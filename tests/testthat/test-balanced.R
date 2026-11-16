# Stand-in controllers ----
#
# Placement reads only a controller's size(), its launcher's workers and
# ssh_host, and the launcher's salvage_holds() when it has one. These stubs
# offer exactly that, so a placement test needs no ssh, no worker and no
# network, and can set an occupancy that a real controller would need running
# tasks to reach.
#
# They go through crew_class_controller_balanced$new() rather than
# crew_controller_balanced(), because crew_controller_group() rejects anything
# that is not a crew controller. The group's own validate() is therefore not
# exercised by them; the tests at the end of this file use real controllers
# for that.

stub_controller <- function(
  name,
  workers,
  size = 0L,
  holds = character(0L),
  salvage = TRUE,
  ssh_host = NULL
) {
  launcher <- new.env(parent = emptyenv())
  launcher$name <- name
  launcher$workers <- workers
  launcher$ssh_host <- ssh_host
  if (salvage) {
    launcher$salvage_holds <- function(names) intersect(names, holds)
    # The real one also caps how many pass at once and records them for the
    # controller's saturated() to read; the drain only needs which it holds.
    launcher$salvage_backlog <- function(names) intersect(names, holds)
  }
  out <- new.env(parent = emptyenv())
  out$launcher <- launcher
  out$pushed <- character(0L)
  out$size <- function() size
  out$saturated <- function() size >= workers
  out$push_backlog <- function(name) {
    out$pushed <- c(out$pushed, name)
    invisible()
  }
  out$terminate <- function() {
    out$terminated <- TRUE
    invisible()
  }
  out
}

stub_group <- function(..., tiers = NULL, hosts = NULL, host_workers = NULL) {
  controllers <- list(...)
  names(controllers) <- vapply(
    controllers,
    function(x) x$launcher$name,
    character(1L)
  )
  # Qualified because this is a function body, where lintr resolves names
  # without the package loaded and would call these undefined.
  crew.ssh::crew_class_controller_balanced$new(
    controllers = controllers,
    relay = NULL,
    tiers = crew.ssh:::balanced_tiers(
      tiers = tiers,
      names = names(controllers)
    ),
    hosts = crew.ssh:::balanced_hosts(
      hosts = hosts,
      controllers = controllers
    ),
    host_workers = crew.ssh:::balanced_host_workers(
      host_workers = host_workers
    )
  )
}

place <- function(group, name = "task", controller = NULL) {
  group$.__enclos_env__$private$place(name = name, controller = controller)
}

candidates <- function(group, controller = NULL) {
  group$.__enclos_env__$private$candidates(controller = controller)
}

backlog <- function(group) {
  group$.__enclos_env__$private$.backlog
}

# Placement ----

test_that("a task goes to the candidate with the most free capacity", {
  group <- stub_group(
    stub_controller("a", workers = 4L, size = 3L),
    stub_controller("b", workers = 4L, size = 0L),
    stub_controller("c", workers = 4L, size = 2L)
  )
  expect_identical(place(group), "b")
})

test_that("free capacity counts workers against tasks held, not workers alone", {
  # "big" allows more workers but is nearly full; "small" allows fewer and is
  # empty, so the task belongs on "small".
  group <- stub_group(
    stub_controller("big", workers = 40L, size = 39L),
    stub_controller("small", workers = 4L, size = 0L)
  )
  expect_identical(place(group), "small")
})

test_that("ties go to the controller supplied first", {
  group <- stub_group(
    stub_controller("first", workers = 4L, size = 1L),
    stub_controller("second", workers = 4L, size = 1L)
  )
  expect_identical(place(group), "first")
})

test_that("placement is confined to the tier the push names", {
  group <- stub_group(
    stub_controller("small", workers = 40L, size = 0L),
    stub_controller("big", workers = 2L, size = 1L),
    tiers = c(big = "bigmem")
  )
  # "small" has far more room, but the task asked for the bigmem tier.
  expect_identical(place(group, controller = "bigmem"), "big")
  expect_identical(place(group), "small")
})

test_that("a push may name one controller instead of a tier", {
  group <- stub_group(
    stub_controller("a", workers = 4L, size = 0L),
    stub_controller("b", workers = 4L, size = 3L)
  )
  expect_identical(place(group, controller = "b"), "b")
})

test_that("an unknown tier or controller is an error", {
  group <- stub_group(stub_controller("a", workers = 1L))
  expect_error(place(group, controller = "nowhere"), "controller not found")
  expect_error(place(group, controller = 1L), "must be NULL or a character")
})

# Salvage affinity ----

test_that("a task goes to the system holding its work, however loaded", {
  group <- stub_group(
    stub_controller("empty", workers = 40L, size = 0L),
    stub_controller("holder", workers = 2L, size = 2L, holds = "task")
  )
  # "holder" is saturated and "empty" has 40 free slots, but only "holder"
  # can hand back the saved result, so the task goes there anyway.
  expect_identical(place(group, name = "task"), "holder")
  # A different task has no work saved anywhere, so capacity decides.
  expect_identical(place(group, name = "other"), "empty")
})

test_that("affinity is confined to the tier, like any other placement", {
  group <- stub_group(
    stub_controller("a", workers = 4L, size = 0L),
    stub_controller("b", workers = 4L, size = 0L, holds = "task"),
    tiers = c(b = "bigmem")
  )
  # "b" holds the work but is not in the default tier.
  expect_identical(place(group, name = "task"), "a")
  expect_identical(place(group, name = "task", controller = "bigmem"), "b")
})

test_that("a launcher without salvage holds nothing and is still placed", {
  group <- stub_group(
    stub_controller("cloud", workers = 10L, size = 0L, salvage = FALSE),
    stub_controller("ssh", workers = 2L, size = 1L)
  )
  expect_identical(place(group, name = "task"), "cloud")
})

test_that("an unnamed task skips affinity", {
  group <- stub_group(
    stub_controller("empty", workers = 10L, size = 0L),
    stub_controller("holder", workers = 2L, size = 2L, holds = "task")
  )
  expect_identical(place(group, name = NULL), "empty")
  expect_identical(place(group, name = NA_character_), "empty")
  expect_identical(place(group, name = ""), "empty")
})

# Host caps ----

test_that("two controllers of one machine share its cap", {
  group <- stub_group(
    stub_controller("host1-big", workers = 8L, size = 6L, ssh_host = "host1"),
    stub_controller("host1-small", workers = 24L, size = 6L, ssh_host = "host1"),
    stub_controller("host2", workers = 4L, size = 2L, ssh_host = "host2"),
    host_workers = c(host1 = 12L, host2 = 44L)
  )
  # host1 holds 12 tasks over its two controllers and is at its cap, even
  # though host1-small alone has 18 free slots. host2 takes the task.
  expect_identical(place(group), "host2")
})

test_that("a machine below its cap is placed on normally", {
  group <- stub_group(
    stub_controller("host1", workers = 24L, size = 6L, ssh_host = "host1"),
    stub_controller("host2", workers = 4L, size = 2L, ssh_host = "host2"),
    host_workers = c(host1 = 12L, host2 = 44L)
  )
  expect_identical(place(group), "host1")
})

test_that("a machine the cap does not name is uncapped", {
  group <- stub_group(
    stub_controller("a", workers = 40L, size = 30L, ssh_host = "elsewhere"),
    stub_controller("b", workers = 4L, size = 3L, ssh_host = "host2"),
    host_workers = c(host2 = 1L)
  )
  expect_identical(place(group), "a")
})

test_that("when every machine is capped the least overshooting one wins", {
  group <- stub_group(
    stub_controller("a", workers = 4L, size = 8L, ssh_host = "a"),
    stub_controller("b", workers = 4L, size = 5L, ssh_host = "b"),
    host_workers = c(a = 1L, b = 1L)
  )
  expect_identical(place(group), "b")
})

test_that("a capped machine still takes a task whose work it holds", {
  group <- stub_group(
    stub_controller("holder", workers = 2L, size = 9L, holds = "task", ssh_host = "h"),
    stub_controller("free", workers = 40L, size = 0L, ssh_host = "f"),
    host_workers = c(h = 1L, f = 40L)
  )
  expect_identical(place(group, name = "task"), "holder")
})

# Saturation ----

test_that("the group is saturated only when every candidate is", {
  group <- stub_group(
    stub_controller("a", workers = 2L, size = 2L),
    stub_controller("b", workers = 2L, size = 1L)
  )
  expect_false(group$saturated())
  full <- stub_group(
    stub_controller("a", workers = 2L, size = 2L),
    stub_controller("b", workers = 2L, size = 2L)
  )
  expect_true(full$saturated())
})

test_that("saturation is judged within the tier, not across the group", {
  group <- stub_group(
    stub_controller("small", workers = 4L, size = 0L),
    stub_controller("big", workers = 2L, size = 2L),
    tiers = c(big = "bigmem")
  )
  expect_true(group$saturated(controller = "bigmem"))
  expect_false(group$saturated())
})

test_that("saturation asks each controller rather than counting tasks here", {
  # A controller that lets a task through while full, as the ssh controller
  # does for a task an earlier session left in flight, must be believed.
  group <- stub_group(
    stub_controller("a", workers = 2L, size = 2L),
    stub_controller("b", workers = 2L, size = 2L)
  )
  expect_true(group$saturated())
  # Read the controller out first: `controllers` is an active binding, so
  # assigning through it would call a setter that does not exist.
  b <- group$controllers$b
  b$saturated <- function() FALSE
  expect_false(group$saturated())
})

# Backlog ----
#
# `targets` parks a task only once the whole group reports itself saturated,
# so the park cannot choose a controller: every candidate's free capacity is
# zero at that moment. The drain chooses how many go out, and the push that
# follows places each one.

test_that("push_backlog() parks a name without giving it to a controller", {
  group <- stub_group(
    stub_controller("a", workers = 4L, size = 4L),
    stub_controller("b", workers = 4L, size = 2L)
  )
  group$push_backlog(name = "task")
  expect_identical(group$controllers$a$pushed, character(0L))
  expect_identical(group$controllers$b$pushed, character(0L))
  expect_identical(backlog(group), "task")
})

test_that("pop_backlog() holds a task back while every controller is full", {
  group <- stub_group(
    stub_controller("a", workers = 4L, size = 4L),
    stub_controller("b", workers = 4L, size = 4L)
  )
  group$push_backlog(name = "task")
  expect_identical(group$pop_backlog(), character(0L))
  expect_identical(backlog(group), "task")
})

test_that("pop_backlog() releases on any controller's free capacity", {
  # The drain reads the whole group's room. Draining each controller's own
  # backlog at its own rate instead strands every task behind the single
  # worker of "a", because a park while saturated had put them all there.
  group <- stub_group(
    stub_controller("a", workers = 1L, size = 1L),
    stub_controller("b", workers = 2L, size = 0L)
  )
  for (name in c("task1", "task2", "task3")) {
    group$push_backlog(name = name)
  }
  expect_identical(group$pop_backlog(), c("task1", "task2"))
  expect_identical(backlog(group), "task3")
})

test_that("pop_backlog() spends each free slot once", {
  group <- stub_group(
    stub_controller("a", workers = 3L, size = 1L),
    stub_controller("b", workers = 1L, size = 1L)
  )
  for (name in c("task1", "task2", "task3")) {
    group$push_backlog(name = name)
  }
  # Two slots free on "a" and none on "b", so the third task waits instead
  # of being counted against a slot already given away.
  expect_identical(group$pop_backlog(), c("task1", "task2"))
  expect_identical(backlog(group), "task3")
})

test_that("pop_backlog() counts only the controllers a task may run on", {
  group <- stub_group(
    stub_controller("a", workers = 1L, size = 1L),
    stub_controller("b", workers = 2L, size = 0L)
  )
  group$push_backlog(name = "pinned", controller = "a")
  group$push_backlog(name = "anywhere")
  # "b" has room, but "pinned" may only run on "a", which is full.
  expect_identical(group$pop_backlog(), "anywhere")
  expect_identical(backlog(group), "pinned")
})

test_that("pop_backlog() releases a held task past a full group", {
  group <- stub_group(
    stub_controller("a", workers = 1L, size = 1L),
    stub_controller("b", workers = 1L, size = 1L, holds = "salvaged")
  )
  group$push_backlog(name = "ordinary")
  group$push_backlog(name = "salvaged")
  # The launcher holding the work grants the pass that its own saturated()
  # reads, so the task goes out past capacity and ahead of the queue order.
  expect_identical(group$pop_backlog(), "salvaged")
  expect_identical(backlog(group), "ordinary")
})

test_that("pop_backlog() drains a group whose launchers have no salvage", {
  # A cluster or cloud controller has no salvage_backlog(), so the drain must
  # not depend on one being there.
  group <- stub_group(
    stub_controller("a", workers = 1L, size = 1L, salvage = FALSE),
    stub_controller("b", workers = 1L, size = 0L, salvage = FALSE)
  )
  group$push_backlog(name = "task")
  expect_identical(group$pop_backlog(), "task")
  expect_identical(backlog(group), character(0L))
})

test_that("terminate() forgets what the group parked", {
  group <- stub_group(
    stub_controller("a", workers = 1L, size = 1L),
    stub_controller("b", workers = 1L, size = 1L)
  )
  group$push_backlog(name = "task")
  group$terminate()
  expect_identical(backlog(group), character(0L))
  expect_true(group$controllers$a$terminated)
})

test_that("terminate() of named controllers keeps what the group parked", {
  group <- stub_group(
    stub_controller("a", workers = 1L, size = 1L),
    stub_controller("b", workers = 1L, size = 1L)
  )
  group$push_backlog(name = "task")
  group$terminate(controllers = "a")
  # "b" is staying up, so the task has somewhere left to run.
  expect_identical(backlog(group), "task")
})

test_that("push_backlog() refuses a scope the group does not have", {
  group <- stub_group(stub_controller("a", workers = 1L))
  expect_error(
    group$push_backlog(name = "task", controller = "nowhere"),
    "controller not found"
  )
  expect_identical(backlog(group), character(0L))
})

# Configuration ----

test_that("balanced_tiers() defaults every unnamed controller to default", {
  expect_identical(
    balanced_tiers(NULL, c("a", "b")),
    c(a = "default", b = "default")
  )
  expect_identical(
    balanced_tiers(c(b = "bigmem"), c("a", "b")),
    c(a = "default", b = "bigmem")
  )
})

test_that("balanced_tiers() refuses a controller it does not know", {
  expect_error(
    balanced_tiers(c(nope = "bigmem"), c("a", "b")),
    "not in the group"
  )
  expect_error(balanced_tiers(c(a = NA_character_), "a"), "tiers must be")
  expect_error(balanced_tiers(c(a = ""), "a"), "tiers must be")
  expect_error(balanced_tiers("bigmem", "a"), "tiers must be")
})

test_that("balanced_hosts() reads ssh_host and falls back to the name", {
  controllers <- list(
    a = stub_controller("a", workers = 1L, ssh_host = "host1"),
    b = stub_controller("b", workers = 1L)
  )
  expect_identical(
    balanced_hosts(NULL, controllers),
    c(a = "host1", b = "b")
  )
  expect_identical(
    balanced_hosts(c(b = "host1"), controllers),
    c(a = "host1", b = "host1")
  )
})

test_that("balanced_hosts() refuses a controller it does not know", {
  controllers <- list(a = stub_controller("a", workers = 1L))
  expect_error(balanced_hosts(c(nope = "x"), controllers), "not in the group")
  expect_error(balanced_hosts(c(a = NA_character_), controllers), "hosts must be")
})

test_that("balanced_host_workers() refuses anything but positive named numbers", {
  expect_null(balanced_host_workers(NULL))
  expect_identical(balanced_host_workers(c(a = 2L)), c(a = 2L))
  expect_error(balanced_host_workers(c(a = 0L)), "host_workers must be")
  expect_error(balanced_host_workers(c(a = -1L)), "host_workers must be")
  expect_error(balanced_host_workers(2L), "host_workers must be")
  expect_error(balanced_host_workers(c(a = NA_real_)), "host_workers must be")
})

# Construction and validation with real controllers ----

test_that("crew_controller_balanced() builds a usable group", {
  group <- crew_controller_balanced(
    crew::crew_controller_local(name = "a", workers = 2L),
    crew::crew_controller_local(name = "b", workers = 5L),
    crew::crew_controller_local(name = "c", workers = 1L),
    tiers = c(c = "bigmem")
  )
  on.exit(group$terminate(), add = TRUE)
  expect_s3_class(group, "crew_class_controller_balanced")
  expect_s3_class(group, "crew_class_controller_group")
  expect_identical(names(group$controllers), c("a", "b", "c"))
  expect_identical(candidates(group), c("a", "b"))
  expect_identical(candidates(group, "bigmem"), "c")
  # A local launcher has no ssh_host, so each controller is its own machine.
  expect_identical(
    group$.__enclos_env__$private$.hosts,
    c(a = "a", b = "b", c = "c")
  )
  expect_identical(place(group), "b")
})

test_that("a group with no default tier is refused", {
  expect_error(
    crew_controller_balanced(
      crew::crew_controller_local(name = "a", workers = 1L),
      tiers = c(a = "bigmem")
    ),
    "default"
  )
})

test_that("a tier named after a controller is refused", {
  expect_error(
    crew_controller_balanced(
      crew::crew_controller_local(name = "a", workers = 1L),
      crew::crew_controller_local(name = "b", workers = 1L),
      tiers = c(b = "a")
    ),
    "same name as a controller"
  )
})

test_that("a cap on a machine that does not exist is refused", {
  expect_error(
    crew_controller_balanced(
      crew::crew_controller_local(name = "a", workers = 1L),
      host_workers = c(nowhere = 2L)
    ),
    "must name machines"
  )
})

# The ssh launcher's side of the contract ----

test_that("the ssh launcher reports its host and holds nothing when idle", {
  controller <- crew_controller_ssh(
    ssh_host = "user@example.com",
    workers = 2L
  )
  expect_identical(controller$launcher$ssh_host, "user@example.com")
  # Before the controller has read the remote system there is nothing to
  # hold, and asking must not error or start anything.
  expect_identical(controller$launcher$salvage_holds("task"), character(0L))
  expect_identical(controller$launcher$salvage_holds(character(0L)), character(0L))
})

test_that("salvage_holds() reports nothing when salvage is off", {
  controller <- crew_controller_ssh(
    ssh_host = "user@example.com",
    workers = 2L,
    salvage = FALSE
  )
  expect_identical(controller$launcher$salvage_holds("task"), character(0L))
})

test_that("crew_controller_balanced() reads ssh_host from ssh controllers", {
  group <- crew_controller_balanced(
    crew_controller_ssh(name = "big", ssh_host = "host1", workers = 2L),
    crew_controller_ssh(name = "small", ssh_host = "host1", workers = 8L),
    crew_controller_ssh(name = "other", ssh_host = "host2", workers = 4L),
    host_workers = c(host1 = 10L, host2 = 4L)
  )
  expect_identical(
    group$.__enclos_env__$private$.hosts,
    c(big = "host1", small = "host1", other = "host2")
  )
})
