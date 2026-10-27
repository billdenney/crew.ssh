
# crew.ssh

<!-- badges: start -->
[![R-CMD-check](https://github.com/billdenney/crew.ssh/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/billdenney/crew.ssh/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

crew.ssh is a [`crew`](https://wlandau.github.io/crew/) launcher plugin that
runs workers on a remote system over `ssh`. Use it to send the heavy parts of a
[`targets`](https://docs.ropensci.org/targets/) pipeline, or any `crew`
workload, to a bigger machine while the controlling R session stays on your
laptop.

Workers reach the local `mirai` dispatcher through a reverse `ssh` tunnel, so
the remote system never opens a connection to your machine. Nothing has to be
port-forwarded, no inbound firewall rule is needed, and the controlling session
does not need a routable address. Everything travels inside the `ssh`
connection you were already allowed to make.

## Installation

``` r
# install.packages("pak")
pak::pak("billdenney/crew.ssh")
```

## Requirements

* An `ssh` client on the local machine. The OpenSSH client that ships with
  Windows 10/11, macOS, and Linux is what the package expects.
* Non-interactive `ssh` access to the remote system. The launcher uses
  `BatchMode=yes`, so authentication must succeed without a prompt: a key with
  no passphrase, or a passphrase-protected key loaded into `ssh-agent`. Confirm
  it works before using the package:

    ``` bash
    ssh -o BatchMode=yes user@example.com R --version
    ```

* R and the `crew` package on the remote system, plus every package your tasks
  use. Keep `mirai` and `nanonext` at the same versions on both ends.
* `ssh_host` accepts anything the `ssh` command accepts, including a `Host`
  alias from `~/.ssh/config`, so `ProxyJump`, non-standard ports, and per-host
  keys can all be configured there instead of in R.

## Example

``` r
library(crew.ssh)

controller <- crew_controller_ssh(
  ssh_host = "user@example.com",
  workers = 4L
)
controller$start()
controller$push(Sys.info()[["nodename"]])
controller$wait()
controller$pop()$result[[1L]]
#> [1] "example"
controller$terminate()
```

## Use with `targets`

`vignette("crew.ssh")` covers this in full, including routing individual
targets to the remote machine and what to do about long-running pipelines.

Set the controller in `_targets.R`, and set `storage` and `retrieval` to
`"main"`:

``` r
library(targets)

tar_option_set(
  storage = "main",
  retrieval = "main",
  controller = crew.ssh::crew_controller_ssh(
    ssh_host = "user@example.com",
    workers = 4L
  )
)

list(
  tar_target(index, seq_len(8L)),
  tar_target(piece, slow_function(index), pattern = map(index)),
  tar_target(summary, combine(piece))
)
```

**`storage = "main"` and `retrieval = "main"` are required unless the remote
system mounts the same filesystem as the local one.** `targets` otherwise
defaults to `storage = "worker"`, which has each worker read and write
`_targets/objects/` itself. A worker on another machine writes into its own
filesystem, the local process waits for files that never appear, and the
pipeline fails with:

```
Error hashing output: timed out after retrying for 65 seconds.
Path _targets/objects/<name> does not exist or has incorrect hash.
File sync timed out.
```

With `"main"`, target data travels over the `ssh` tunnel with the task and the
local process owns the data store. That also means targets which read or write
files by path only work if that path exists on the remote system; ordinary
in-memory targets need nothing extra.

If the remote system *does* mount the same project directory, pass
`directory = "/path/to/project"` so workers start there, and the `targets`
defaults will work as usual.

## Surviving a disconnect

Without salvage, a remote worker exits as soon as its connection to your
session drops, so a task in progress is lost when your laptop sleeps or your
session dies, and it starts over the next time it runs. Salvage is on by
default (`salvage = TRUE`), so that does not happen. Choose where results are
saved, and turn it off with `salvage = FALSE` for tasks that are quick to
compute and large to store:

``` r
controller <- crew_controller_ssh(
  ssh_host = "user@example.com",
  # Somewhere durable: /tmp, the default, may be cleared when the remote
  # system restarts.
  remote_directory = "/home/user/crew-ssh"
)
```

With salvage, a worker whose connection drops finishes the task in progress
and saves its result on the remote system before it exits. When the same task
is pushed again before anything collected that result, by a later session or
by this one retrying a task whose worker was cut off, there are two cases:

* **The old worker finished.** The new worker hands back the saved result
  instead of running the task again.
* **The old worker is still running.** The new worker attaches to it: it waits
  for the old worker's result and hands that back, rather than starting a
  second copy of a task that may have hours left. `remote_workers()` shows the
  old worker as `"running"` and the new one as `"waiting"`. If the old worker
  dies without finishing (killed, container stopped, machine restarted), its
  heartbeat stops, and after two minutes the new worker takes over and runs
  the task itself. If the new worker's own session then loses its connection,
  the next session to connect stops the waiter, which could no longer hand
  anything back; the old worker still saves its result.

With `targets`, this means re-running `tar_make()` after a dropped connection
picks up the remote targets that were in flight, finished or still running,
instead of starting them over. The same holds when you stop `tar_make()`
yourself, or it stops on an error: terminating the controller stops the idle
workers but leaves the busy ones to finish and save their targets. (To stop
the busy ones too, call `controller$terminate(keep_running = FALSE)` yourself;
`tar_make()` always uses the default.) When the
targets held back for a free worker include ones in flight, those are pushed
first. Until a worker of the new session takes an in-flight target over, its
old worker counts against `workers` (unless that target is all the new session
has waiting, when the next worker can only attach to it), so the remote
machine never runs more tasks at once than `workers`. If the network is still down when `crew.ssh`
reopens its tunnel or launches a worker, it keeps trying for up to
`seconds_reconnect` (an hour by default) instead of stopping the pipeline.

A task is matched by its name and by everything it computes from, so a target
whose code or data changed in between runs again; for a `targets` target,
that includes the functions and objects of `_targets.R` it uses, including
functions it reaches by name or holds in lists and R6 objects, and not the
others, so editing an unrelated function leaves an in-flight target to be
picked up. When a target in flight has changed since its run began, the new
session from the same project stops the old run as soon as it pushes the
target, rather than letting it finish a result nothing will collect. A saved error is never
reused, and neither is a result computed under different versions of R or of
any package. Results are written atomically, and a saved file that cannot be
read fails the task with an error naming the file rather than being mistaken
for a missing one.

Saved results are deleted once a controller collects them. Results nothing
collects stay on the remote system; the launcher lists and removes them, and
lists the workers of every session, including ones that outlived their
session:

``` r
controller$launcher$salvage_list()      # saved results and running tasks
controller$launcher$salvage_remove(key) # delete results nothing will collect
controller$launcher$remote_workers()    # workers of every session
controller$launcher$terminate_orphans() # stop workers whose session is gone
```

See `?crew_controller_ssh` for the details.

## Running workers in a container

`launch_prefix` runs each worker under another command, such as a container
runtime. Include `--init`:

``` r
crew_controller_ssh(
  ssh_host = "user@example.com",
  launch_prefix = c(
    "docker", "run", "--init", "--rm", "--network=host",
    "--volume", "/tmp:/tmp", "my-image"
  )
)
```

Without `--init`, R is the first process in the container, and Linux does not
deliver `SIGTERM` to that process unless it handles the signal, which R does
not. A worker then keeps running after its connection drops, computing a
result nothing can collect. `vignette("crew.ssh")` explains the other options.

## Debugging

Remote workers write their output to a log file on the remote system, one per
worker, under a directory unique to the controller. When a worker starts but
never connects, read the logs:

``` r
cat(controller$launcher$logs(), sep = "\n")
#> ==> /tmp/crew-ssh-9f2c.../worker-1a4b....log <==
#> Error in loadNamespace(...) : there is no package called 'crew'
```

The logs stay on the remote system after the workers exit, so they are still
readable after a crash. `remote_directory` (default `"/tmp"`) controls where
they live, and `verbose = TRUE` prints the `ssh` command for the tunnel and a
line for each worker launch.

## How it works

1. The `mirai` dispatcher listens on the local loopback interface.
2. `crew_controller_ssh()` opens one long-lived `ssh` process with
   `-R 0:127.0.0.1:<local port>`. The remote `sshd` picks a free port and `ssh`
   reports it, so nothing collides with other users on the remote system.
3. Each worker launch is a short `ssh` call that pipes the worker's R script
   over stdin, starts it detached with `nohup`, and returns its remote process
   ID. The worker code never touches a command line, so quoting cannot corrupt
   it.
4. Workers dial the remote end of the tunnel instead of the dispatcher's real
   address.
5. The tunnel process holds the write end of its own stdin pipe. When the local
   R session ends — cleanly or by being killed outright — the operating system
   closes that pipe, the tunnel exits, and the remote workers exit with it,
   after they finish their current task and save it (or at once, with
   `salvage = FALSE`).
6. Each session records itself and its workers in its directory on the remote
   system: process IDs, container IDs, and the task each worker is on. A later session reads those records to find workers that outlived
   their session and to stop them, containers included.

## Limitations

* Password authentication is not supported. Use keys.
* The remote system needs a POSIX shell; Windows remotes are not supported.
* Local and remote filesystems are assumed to be different. See the `targets`
  section above.
* Salvage saves results with `saveRDS()`, so a controller with custom
  `serialization` needs `salvage = FALSE`.
