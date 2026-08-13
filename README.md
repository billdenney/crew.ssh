
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
   closes that pipe, the tunnel exits, and the remote workers exit with it. No
   orphaned R processes are left behind on the remote system.

## Limitations

* Password authentication is not supported. Use keys.
* The remote system needs a POSIX shell; Windows remotes are not supported.
* Local and remote filesystems are assumed to be different. See the `targets`
  section above.
