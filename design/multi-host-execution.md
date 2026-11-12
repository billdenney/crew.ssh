# Multi-host execution: one `tar_make()` across every machine

Revision 1, 2026-11-11.

How a `targets` pipeline driven from a Windows laptop spreads its work over
every machine available to it; balances that work without the pipeline author
assigning targets to machines by hand; and keeps computing while the laptop is
away. The worker launcher itself (`crew_controller_ssh()`,
the reverse tunnel, salvage) is not redesigned here. This document covers the
layer above it, which decides *which* machine each task goes to.

---

## 0. Principles

1. **The laptop stays the driver.** The R session that calls `tar_make()` runs
   on the laptop and owns the data store. Nothing here requires logging into a
   server to start work, and nothing requires a batch scheduler.
2. **Dispatched work finishes; new work waits.** While the laptop is
   disconnected, tasks already running continue and their results survive
   (salvage). New tasks do not start until the laptop is back. A deliberate
   limit, not an oversight: see section 1.
3. **No fork of `crew` or `targets`.** Everything here is built from public
   extension points both packages already expose. Section 3.3 shows why that
   is possible.
4. **Placement is the scheduler's job, not the pipeline author's.**
   `_targets.R` says what a target *needs* (a lot of memory, a GPU), not which
   host runs it. Host names belong in configuration.
5. **A wrong placement costs time, never correctness.** Every mechanism here
   degrades to "recompute it", never to "hand back the wrong value".
6. **Capacity is measured, not assumed.** The balancer reads each controller's
   real occupancy rather than modelling it.

---

## 1. Goals and non-goals

### Goals

* **G1.** One `tar_make()` from the laptop uses every configured machine at
  once.
* **G2.** Load balancing: a task goes to a machine with free capacity. The
  author does not name hosts per target.
* **G3.** Queue the whole run and come back to it later, SLURM-style: the run
  accepts all the work up front and drains it as capacity frees up.
* **G4.** Work in flight continues across a laptop disconnect, and its results
  are picked up by the next run. This is today's salvage, extended to more than
  one host.
* **G5.** Placement respects what actually limits a machine, and the limit is
  set per controller rather than inferred. In this workload that limit is CPU:
  a model fit takes several GB while it compiles but is processor-bound
  thereafter, so each machine takes a handful of fits at a time — two to
  eight — and runs out of processor long before memory (section 2). Machines
  that are *not* interchangeable are handled by tiers (section 4.4), not by a
  resource model.
* **G6.** Adding a machine is configuration, not code. Local hosts now; AWS or
  another provider later, as a nice-to-have.

### Non-goals

* **N1.** Starting new tasks while the laptop is disconnected. Accepted
  explicitly. The `targets` driver process *is* its scheduler: the only durable
  state is the data store, so with the driver on the laptop there is nothing
  left to walk the DAG while the laptop is away. Honouring G3 across a
  disconnect would mean moving the driver off the laptop, which principle 1
  rules out.
* **N2.** Two pipelines, or two `tar_make()` runs, sharing the hosts under one
  admission controller. `targets` does not support concurrent `tar_make()`
  instances on one store, and nothing here adds cross-run arbitration beyond
  the existing per-host worker caps. See Q3.
* **N3.** Moving target data out of the laptop's store. Payloads are
  single-digit megabytes, exchanged only at the start and end of a task, so
  `storage = "main"` and `retrieval = "main"` stay. Section 8.2 records what
  would change if that stops holding.
* **N4.** Preemption, backfill, fair-share, or any other scheduler feature
  beyond "put it where there is room".

---

## 2. The deployment this is designed for

The design assumes a fleet of this shape. The named machines of any particular
fleet are deployment configuration and are not recorded here; what follows is
the part that constrains the design.

| | Assumed |
|---|---|
| Machines | two or more, added and removed over time |
| Reachability | each reachable from the laptop by `ssh` with key auth; not necessarily reachable from each other |
| Capacity | unequal, in cores and in memory, and not in proportion |
| Shared filesystem | **none**, between the machines or with the laptop |
| Software | installed per machine, and apt to drift between them |

Two features of it shape everything below.

**The limit is processor, not memory, and it is far below the core count.** The
workload is long model fits that need several GB apiece while they compile and
are processor-bound after that, so a machine is meant to run a handful of fits
at once — single digits — not one per core. A handful of fits uses tens of GB,
which the machines do not come close to exhausting, so the ratio of memory to
cores decides nothing. What that means for the design is that `workers` is a
number chosen per controller from how many fits a machine should run, and the
balancer's job is to keep those few slots full across every machine rather than
to model a resource. With so few slots, static routing is *more* costly, not
less: sending every unlabelled target to one machine idles half a fleet that
has only a dozen or so slots in total.

**No shared filesystem** means a result saved on one machine is invisible from
the others, which is what makes salvage host-affine (section 4.5).

A third feature is not a design constraint but a standing operational one: the
laptop and the machines must agree on the versions of `mirai` and `nanonext`,
and on every package a salvaged result was computed under (section 7).

---

## 3. What the current stack does, and what it does not

### 3.1 A `crew` controller group is a static router, not a scheduler

This is the finding the design turns on. In `crew` 1.3.1,
`crew_class_controller_group` picks the controller for a task like this:

```r
# crew:::crew_class_controller_group, private method .select_single_controller
name <- name %|||% utils::head(names, n = 1L)
```

With no controller named, the task goes to **the first controller in the
group**. There is no occupancy check and no round-robin.

`targets` names that controller from the target's resources, and nothing else:

```r
# targets:::crew_class, public method run_worker
name_controller <- resources$controller
if (self$controller$saturated(controller = name_controller)) {
  self$controller$push_backlog(name = name, controller = name_controller)
  return()
}
# ...
self$controller$push(..., name = name, controller = name_controller, ...)
```

`resources$controller` comes from `tar_resources_crew(controller = ...)` and is
`NULL` unless the author sets it per target. So a group holding one
`crew_controller_ssh()` per machine, with no per-target resources, sends
**every task to the first machine and leaves the rest idle**, and `saturated()`
consults that one machine alone. Group membership by itself buys nothing.

Static routing is usable — name a controller on every target — but it fails G2
and G3: the author hand-partitions the DAG, and a target pinned to a busy host
waits while another host sits idle.

### 3.2 What already works across every controller

Only three group methods select a single controller: **`push()`**,
**`saturated()`** and **`push_backlog()`**. Every other method already spans the
whole group through `.select_controllers(NULL)`:

`pop()`, `collect()`, `wait()`, `unresolved()`, `nonempty()`, `size()`,
`pop_backlog()`, `scale()`, `autoscale()`, `descale()`, `launch()`,
`crashes()`, `summary()`, `pids()`, `terminate()`.

So collection, scaling, waiting and shutdown are multi-host already. **The gap
is three methods wide**, and all three are placement decisions. That is the
whole of the work.

### 3.3 `targets` accepts any object that quacks like a controller

```r
# targets:::validate_crew_controller
tar_assert_envir(controller, msg = "invalid crew controller")
tar_assert_function(controller$validate, msg = "invalid crew controller")
controller$validate()
```

No class check. `tar_option_set(controller = ...)` takes any environment with a
`validate()` method, so a subclass of `crew_class_controller_group` drops in
with no change to `targets` and no change to `crew` (principle 3).

### 3.4 The capacity signal is already public

```r
# crew:::crew_class_controller, public method saturated
as.logical(size() >= .subset2(.launcher, "workers"))
```

`size()` is the number of tasks the controller holds — pushed and not yet
popped. So free capacity is

```r
free <- function(controller) controller$launcher$workers - controller$size()
```

with both terms public. `pop_backlog()` already releases at most
`workers - size()` names per controller, so it uses the same quantity.

---

## 4. Architecture

### 4.1 Three independent axes

Keeping these apart is what makes G6 cheap:

| Axis | Today | Later |
|---|---|---|
| **Placement** | new: balancing controller group | unchanged when hosts are added |
| **Worker launch** | `crew.ssh`, one controller per host | `crew.aws.batch` or another plugin, in the same group |
| **Storage** | laptop store, `storage = "main"` | S3-compatible object store if N3 stops holding |

Adding AWS later means adding a controller to the group. No redesign.

### 4.2 The balancing controller group

A new R6 class inheriting from `crew_class_controller_group`, overriding
exactly the three placement methods of section 3.1 and nothing else:

The configuration this workload calls for is the plain one: one controller per
machine, no tiers, no caps.

```r
controller <- crew_controller_balanced(
  crew.ssh::crew_controller_ssh(
    name = "host1", ssh_host = "user@host1.example.com",
    workers = 6L, launch_prefix = image
  ),
  crew.ssh::crew_controller_ssh(
    name = "host2", ssh_host = "user@host2.example.com",
    workers = 8L, launch_prefix = image
  )
)

tar_option_set(controller = controller, storage = "main", retrieval = "main")
```

`workers` is how many fits that machine should run at once, not how many cores
it has (section 2). Every target goes to whichever machine has a free slot,
with no `resources` on any target.

A machine that is not interchangeable with the others gets a tier (section
4.4), and only then does a target name anything:

```r
controller <- crew_controller_balanced(
  crew.ssh::crew_controller_ssh(name = "host1", ssh_host = "user@host1.example.com", workers = 6L),
  crew.ssh::crew_controller_ssh(name = "host2", ssh_host = "user@host2.example.com", workers = 8L),
  crew.ssh::crew_controller_ssh(name = "gpu", ssh_host = "user@host1.example.com", workers = 1L),
  tiers = c(gpu = "gpu"),
  host_workers = c("user@host1.example.com" = 6L)
)

tar_target(
  fit, run_on_gpu(data),
  resources = tar_resources(crew = tar_resources_crew(controller = "gpu"))
)
```

Targets with no `controller` resource go to the `"default"` tier, which here is
`host1` and `host2`. `host_workers` appears only because two controllers now
share the first machine and between them could start seven workers on a machine
meant to run six.

Tier and host membership are arguments of the balancer, not of the controllers,
per decision 5. Host membership defaults to each launcher's `ssh_host`, which
needs a read-only accessor on the launcher before the balancer can see it
(section 6.2); without one, `host_workers` has to be given as an explicit
controller-to-host mapping.

**Naming hazard.** `crew_controller_ssh()` already has a `host` argument, and it
means the *local* address of the `mirai` dispatcher, not the remote machine. The
balancer must not add a second `host` with the opposite meaning. This document
says **host identity** where it means the remote machine, and the configuration
argument is `host_workers`, keyed by `ssh_host`.

### 4.3 Placement policy

On `push(name = , controller = tier)`:

1. **Affinity.** If any controller's host already holds a saved result or a
   running task for `name`, place it there (section 4.5). This outranks load:
   reusing hours of finished work beats balancing.
2. **Candidates.** Controllers whose `tier` matches; or, when the name matches a
   controller rather than a tier, that controller alone. That keeps
   `tar_resources_crew(controller = "host2")` working as an escape hatch for
   pinning one target to one machine.
3. **Host cap.** Drop candidates whose host is at `host_workers`, counted as the
   sum of `size()` over that host's controllers. This is what stops two
   controllers on one machine from oversubscribing it.
4. **Least loaded.** Among the rest, the largest `free` (section 3.4). Ties
   break on a configured host order, so a cheap local host is preferred to a
   metered cloud one.
5. **Nowhere free.** Push to the affinity host if there was one, else to the
   candidate with the smallest overshoot. A task pushed past `workers` waits in
   the `mirai` queue; `workers` stays a hard cap on worker *processes*, which is
   what protects the machine, and only a soft cap on queued pushes.

`saturated(controller = tier)` returns `TRUE` only when **every** candidate is
saturated. That is what makes `targets` keep feeding the group while any host
has room, instead of backing off because controller one happens to be full.

### 4.4 Tiers, for when machines are not interchangeable

For this workload the machines *are* interchangeable: all of them run the same
fits, all are processor-bound, and none runs out of memory at the handful
of concurrent fits intended. So the configuration that fits section 2 is the
plain one — one controller per machine, every controller in the `"default"`
tier, no tiers configured at all — and the balancer simply keeps both machines'
slots full.

Tiers exist for the cases where that stops being true, and the design keeps
them because adding a machine later is the point (G6). A tier is warranted when
some targets can only run in some places:

* a machine with a GPU, or with a dataset mounted locally, that others lack;
* a class of target that should get fewer concurrent slots than the rest, such
  as one that really is memory-hungry;
* a metered cloud machine that should only take work the local ones cannot.

Within a tier the balancer balances; across tiers it does not choose at all,
because the author already did. That stops deliberately short of a SLURM-style
`--mem` or `--gres` model (N4): a tier is a name the author picks once, whereas
a resource model needs a per-target estimate nobody has.

A machine may carry more than one tier. Its controllers then each launch their
own workers, which is what `host_workers` is for: it caps the machine as a
whole so its tiers cannot oversubscribe it between them. With one controller
per machine, `host_workers` is redundant with that controller's `workers` and
can be left out.

### 4.5 Salvage affinity

No shared filesystem means a salvaged result is readable only on the host that
wrote it: `crew.ssh` saves to `remote_directory/crew-ssh-salvage/<key>.rds`.
With balancing, a target salvaged on one machine could be re-pushed to another
on the next run, and the saved work recomputed. Correct, but it throws away
what G4 exists to protect.

The hook already exists and is public. `crew_class_launcher_ssh` has

```r
salvage_backlog(names)   # which of these names this host has saved or running
salvage_passing()        # whether such a task is still to be pushed
```

`salvage_backlog()` answers by task name, which is all `targets` supplies at
push time, so the balancer asks each controller's launcher and routes to the
host that answers. The salvage key itself is content-derived and
host-independent, so the same target gets the same key wherever it ran; only the
saved *file* is host-local.

If affinity is ever wrong — the host is gone, the image changed — `crew.ssh`
refuses the reuse (a result computed under different package versions is never
reused) and the target runs again. Principle 5 holds.

### 4.6 How much work survives a disconnect

Concurrent in-flight tasks equal the sum of `workers` across every controller,
and salvage carries all of them across a disconnect. So **balancing and G4
reinforce each other**: adding a second machine raises both the throughput
while connected and the amount of work that keeps running while the laptop is
away. At six and eight fits respectively that is fourteen
long fits surviving a closed laptop rather than six — and because each fit is
hours, that ratio is the whole value of the second machine.

---

## 5. How this composes with `crew.ssh` salvage

`crew_class_controller_ssh` already overrides three controller methods, and they
are the same three the balancer touches at group level. They compose, and the
order matters:

| Method | `crew.ssh` controller does | Balanced group must do |
|---|---|---|
| `push()` | wraps the command in `salvage_run()` via `salvage_task()` | choose the controller, then delegate — never bypass the subclass |
| `saturated()` | returns `FALSE` while `salvage_passing()`, so in-flight targets get pushed past the cap | `all()` over candidates' own `saturated()`, inheriting the pass-through for free |
| `pop_backlog()` | front-loads names this host has saved or running, past the cap | already spans all controllers and concatenates; leave it alone |

Two consequences are worth stating because they are easy to get wrong:

* The balancer must call `controller$push()`, not `super$push()` on some other
  object. Salvage lives in the subclass. Calling the parent would silently turn
  salvage off, which is a correctness-shaped bug wearing a performance costume.
* The balancer must not reimplement `saturated()` as "count tasks and compare".
  Asking each controller's own `saturated()` is what preserves the salvage
  pass-through that lets an in-flight target be re-pushed while the host is
  full.

### 5.1 The backlog stays per-controller, for now

`targets` calls `push_backlog(name, controller = tier)` when the group reports
saturation, and `crew.ssh` has just built its "push in-flight targets first"
ordering on top of the *per-controller* backlog. A group-level backlog would
balance better — a name backlogged against a busy host waits for that host even
if another frees up first — but it would bypass that ordering and would have to
reimplement it.

So revision 1 delegates the backlog to the chosen controller and accepts the
mild imbalance. It self-corrects: `targets` re-pushes a dequeued name through
`run_worker()`, so the backlogged name is re-placed on its next pass. Revisit if
measurement shows hosts idling with a non-empty backlog (Q1).

---

## 6. What has to change in `crew.ssh`

### 6.1 Where tier and host membership live

The balancer needs to group controllers by host (for the cap) and by tier (for
candidacy). Two options:

* **(a) Arguments on `crew_controller_ssh()`**: add `tier`, and something for
  host identity. This puts scheduling vocabulary into a launcher package that
  otherwise knows nothing about scheduling, and the obvious name for the second
  argument is taken: `host` already means the local `mirai` dispatcher address.
* **(b) Configuration in the balancer**: `crew_controller_balanced()` takes
  `tiers` and `host_workers`, keyed by controller name and `ssh_host`.

**Prefer (b)**, which is decision 5. It keeps `crew.ssh` free of scheduler
concepts, avoids the `host` collision, and lets the balancer work unchanged over
`crew.aws.batch` controllers, which will never grow a `tier` argument.

So `crew.ssh` gains **no new arguments**. It gains two small read-only pieces of
surface, in 6.2 and 6.3.

### 6.2 A public `ssh_host` accessor

`ssh_host` is stored as `private$.ssh_host` and the launcher has no active
bindings, so nothing outside the object can read which machine a controller
talks to. The balancer needs it to group controllers by host for the cap in
section 4.3 step 3.

A read-only active binding on `crew_class_launcher_ssh` is enough. Without it,
`crew_controller_balanced()` has to be handed an explicit controller-to-host
mapping, which is redundant configuration the user can get wrong.

### 6.3 An affinity query that does not consume state

`salvage_backlog(names)` is the right signal but it has a side effect: it sets
`.salvage_passes`, which `salvage_passing()` then reports, which changes what
`saturated()` returns. Calling it speculatively from the balancer for every
candidate host on every push would corrupt that state.

So `crew.ssh` needs a **pure** query alongside it — something like
`salvage_holds(names)` returning the same answer with no state change — with
`salvage_backlog()` reimplemented on top of it. This is the only change to
`crew.ssh` behaviour the design requires; 6.2 is pure addition.

### 6.4 Documentation

The README and vignette describe a single remote host throughout. They need a
multi-host section covering: tiers, why a salvaged result is host-local, and
that identical images across hosts are a prerequisite for cross-host reuse
rather than a nicety (section 7).

### 6.5 Not changing

The reverse tunnel, the worker launch path, the salvage protocol, the session
records and `terminate_orphans()` all work unchanged with several hosts. Each
controller already has its own launcher, its own tunnel and its own remote
directory.

### 6.6 As built, 2026-11-11

Sections 6.1 to 6.3 are implemented in `R/balanced.R` and `R/crew_ssh.R`, with
tests in `tests/testthat/test-balanced.R`. Two things differ from the design
above, both discovered while building it.

**The balancer's methods cannot call this package's functions.** The controller
group is a non-portable `R6` class, and a non-portable class resolves the names
in *every* method it has — the inherited ones included — in one `parent_env`.
The methods inherited from `crew_class_controller_group` call crew's internals
(`crew_assert`, `map_lgl`, `crew_retry`), so that environment has to be crew's
namespace, and `crew.ssh`'s own functions are then invisible from inside any
method of the class. Everything a method needs is therefore either computed by
`crew_controller_balanced()` and passed to `new()`, or inlined as a private
method. Anyone extending the class has to keep to that; a helper that works
when tested in isolation will fail as soon as a method calls it.

**`host` was already taken.** Host identity is configured as `host_workers`,
keyed by `ssh_host`, because `crew_controller_ssh()` has a `host` argument
meaning the local `mirai` dispatcher address. See the naming hazard in 4.2.

---

## 7. What every machine has to have

| Where | What | Why |
|---|---|---|
| each machine | a container runtime, or R and every package the tasks use | workers run one way or the other |
| each machine | **the same worker image**, or the same package versions | a result salvaged under different package versions is refused and recomputed, so drift silently turns salvage off |
| each machine | non-interactive `ssh` from the laptop | how workers are launched at all |
| laptop | `mirai` and `nanonext` at the machines' versions | the two ends of the tunnel have to agree |
| a machine hosting anything long-lived of its own | user lingering enabled | a user service otherwise stops at logout |

The image parity row is the one that bites quietly. Salvage compares R and
package versions on the worker before reusing a result, so when two machines
drift apart, every cross-machine reuse is refused and the work is recomputed
with no error anywhere to say so. Building one image and pulling it on every
machine — from a registry, which the balancer neither knows nor cares about —
is what keeps that from happening.

This is the one part of the design that is pure deployment, and it is the part
most worth automating: the failure it guards against is silent, and the cost of
it is a recomputed long fit rather than an error anyone would notice.

---

## 8. Extension

### 8.1 More hosts

A new local machine is one more `crew_controller_ssh()` in the group plus a row
in `host_workers`. A cloud host is one more controller of a different class —
`crew.aws.batch::crew_controller_aws_batch()` — in the same group. The balancer
treats them alike because it only uses `size()`, `launcher$workers` and
`saturated()`, which every `crew` controller has. Tie-breaking on host order
(section 4.3, step 4) is what keeps a metered host idle until the free ones are
full.

The only thing a non-`crew.ssh` controller lacks is salvage, so affinity simply
never selects it and those tasks restart after a disconnect. Degrades cleanly.

### 8.2 If N3 stops holding

If target payloads grow past the point where routing everything through the
laptop is sensible, the fix is a store both hosts can reach, not a redesign of
placement. `targets` supports `repository = "aws"` against any S3-compatible
endpoint — the `tar_target()` docs say it "Can be configured with a non-AWS S3
bucket using the `endpoint` argument of `tar_resources_aws()`, but versioning
capabilities may be lost in doing so" — which covers a MinIO instance on one of
the machines as well as a cloud bucket. `storage = "worker"` then means the
worker uploads
directly, and `repository_meta` plus `tar_meta_download()` make the metadata
portable. Placement is untouched: that is the point of the three axes.

---

## 9. Alternatives considered and rejected

| Alternative | Why not |
|---|---|
| Static routing with `tar_resources_crew(controller = ...)` on every target | Works today and needs no code, but fails G2 and G3: the author hand-partitions the DAG and targets queue behind busy hosts while others idle. Kept as the escape hatch of section 4.3 step 2. |
| Patch `crew` to balance inside `crew_class_controller_group` | Would serve everyone, and is worth proposing upstream, but blocks this work on someone else's release. The subclass is the same code and ships now. |
| Move the driver to one of the machines | Rejected by principle 1. Would deliver G3 across disconnects (N1), at the cost of the laptop-as-origin requirement. |
| `rrq` or another Redis-backed durable queue | Decouples workers from the laptop properly, but the `targets` DAG walker still lives in the laptop session, so it buys nothing N1 has not already conceded, and it adds a broker to operate. |
| `batchtools`, `clustermq`, `future` | None has salvage. `batchtools` forbids concurrent registry access and is copy-based across machines; `tar_make_clustermq()` and `tar_make_future()` are legacy paths next to `crew`. |
| A memory-aware scheduler with per-target `--mem` | N4. Needs an estimate per target that nobody has; tiers get most of the benefit from a name the author already picks. |

---

## 10. Open questions

* **Q1.** Does the per-controller backlog (section 5.1) actually leave hosts
  idle in a real pipeline? Needs measurement before a group-level backlog is
  worth its complexity.
* **Q2.** *Largely answered by decision 11.* `host_workers` has no default and
  needs none: with one controller per machine it is redundant with that
  controller's `workers`, and a machine carrying several tiers is a deliberate
  choice whose cap the author is in the best position to state. Neither cores
  nor memory would give a useful default, since the real number is how many
  fits that machine should run at once.
* **Q3.** Two people, or two pipelines, against the same hosts (N2). Per-host
  caps are per-run, so two runs can oversubscribe. `remote_workers()` can see
  every session's workers; whether the balancer should subtract them is
  undecided.
* **Q4.** *Resolved 2026-11-11: the future infrastructure named was AWS and
  DigitalOcean Droplets.* Both are section 8.1: one more controller in the
  group. AWS Batch has a launcher plugin already (`crew.aws.batch`); Droplets
  have none, so they would be reached either by `crew_controller_ssh()` against
  a running droplet — which works today, since a droplet is just an ssh host —
  or by a launcher plugin that creates and destroys droplets per run. The
  former needs no new code and is where to start.
* **Q5.** *Answered for now by decision 10: it ships inside `crew.ssh`. The
  question stays open because the reason to split it has not gone away.*
  Does the balancer belong in `crew.ssh`, or in its own package? It is
  launcher-agnostic by construction, which argues for its own package; shipping
  it inside `crew.ssh` first and splitting later is the cheaper order.

---

## 11. Decisions record

| # | Date | Decision |
|---|---|---|
| 1 | 2026-11-11 | The driver stays on the laptop. New tasks do not start while it is disconnected (N1); in-flight work continues via salvage (G4). |
| 2 | 2026-11-11 | Placement is a subclass of `crew_class_controller_group` overriding `push()`, `saturated()` and `push_backlog()`. No fork of `crew` or `targets`; `targets` validates controllers by duck typing (3.3). |
| 3 | 2026-11-11 | *Superseded by decision 11 the same day.* Memory asymmetry is handled by named tiers, not a resource model (4.4, N4). |
| 4 | 2026-11-11 | Salvage affinity outranks load balancing (4.3 step 1), because recomputing a finished long target costs more than an imbalance. |
| 5 | 2026-11-11 | Tier and host membership are configured in the balancer, not added as arguments to `crew_controller_ssh()` (6.1b), so the balancer works over non-`crew.ssh` controllers. |
| 6 | 2026-11-11 | The backlog stays per-controller in revision 1 (5.1), to avoid reimplementing the in-flight-first ordering. Revisit under Q1. |
| 7 | 2026-11-11 | `storage` and `retrieval` stay `"main"` (N3); payloads are single-digit megabytes exchanged twice per task. |
| 8 | 2026-11-11 | Identical worker images across hosts are a correctness prerequisite for cross-host salvage reuse, not a convenience (section 7). |
| 9 | 2026-11-11 | The balancer is a non-portable `R6` subclass with `parent_env = asNamespace("crew")`, so its methods call no function of this package (6.6). Found while building it; it constrains anyone extending the class. |
| 10 | 2026-11-11 | The balancer ships inside `crew.ssh` rather than its own package (Q5), to be split later if it earns it. |
| 11 | 2026-11-11 | Supersedes 3. Memory is not the binding constraint: a model fit needs several GB only while it compiles and is processor-bound after that, so each machine runs two to eight fits and has memory to spare. `workers` is therefore set per controller from how many fits a machine should run, the machines are interchangeable, and the configuration this project needs has no tiers and no caps (2, 4.2, 4.4). Tiers stay in the design and the code for machines that genuinely differ, such as a GPU or a metered cloud host. No code changed: the three axes of 4.1 meant this was a change of rationale and defaults only. |
