# Balanced controller group ----
#
# A `crew` controller group routes a task to the controller named in the push,
# and to the first controller in the group when the push names none. That is a
# static router: with several remote systems and a `targets` pipeline that
# does not name a controller per target, every task goes to one system and the
# rest stay idle.
#
# This group chooses instead. On each push it picks among the controllers that
# match the tier the push asked for: the one whose remote system already holds
# the task's work, if there is one, and otherwise the one with the most free
# capacity, within a cap on each machine.
#
# Only placement is new. Collecting, scaling, waiting, draining the backlog
# and terminating already span every controller of a `crew` group, and the
# three methods overridden here are the only ones that pick a single
# controller.

#' @title Balanced controller group
#' @export
#' @family balanced
#' @description Create a controller group that chooses which of its
#'   controllers runs each task, instead of using the one the task names.
#' @details [crew::crew_controller_group()] sends a task to the controller
#'   named in the push, and to the first controller in the group when the
#'   push names none. A `targets` pipeline names a controller through
#'   `targets::tar_resources_crew()`, one target at a time, so spreading a
#'   pipeline over several machines means labelling every target by hand, and
#'   a target labelled for a busy machine waits while another machine is idle.
#'
#'   This group places tasks itself. A target names a *tier*, a group of
#'   interchangeable controllers, or names nothing at all and gets the
#'   `"default"` tier. Among the controllers of that tier, a task goes to:
#'
#'   1. the remote system that already holds a saved result or a running task
#'      for it, if there is one. A salvaged result is readable only on the
#'      system that wrote it, so sending the task elsewhere would recompute
#'      work that is already done or already running. This outranks capacity,
#'      because recomputing a long target costs more than an imbalance.
#'   2. otherwise the controller with the most free capacity, counting the
#'      workers it is allowed and the tasks it holds, among the controllers
#'      whose machine is below its `host_workers` cap. Ties go to the
#'      controller supplied first, so listing cheap machines before metered
#'      ones keeps the metered ones idle until the cheap ones fill.
#'   3. and when every candidate is full, the machine holding its work, or
#'      else the one that overshoots least. A task pushed past a controller's
#'      `workers` waits in the queue; `workers` still caps the worker
#'      processes that machine runs, which is what protects it.
#'
#'   `saturated()` reports `TRUE` only when *every* candidate is saturated,
#'   which is what keeps `targets` feeding the group while any machine has
#'   room instead of holding targets back because the first controller is
#'   full.
#' @section Tiers:
#'   Interchangeable machines need no tiers at all: give one controller per
#'   machine, leave `tiers` and `host_workers` alone, and every task is placed
#'   for you.
#'
#'   Tiers are for the machines that are not interchangeable, when some tasks
#'   can only run in some places: a machine with a GPU or a locally mounted
#'   dataset the others lack, a class of task that should get fewer slots than
#'   the rest, or a metered cloud machine that should only take work the local
#'   ones cannot. Put those controllers in their own tier and name that tier on
#'   the tasks that need it. Within a tier the group balances; across tiers it
#'   does not choose, because you already did.
#'
#'   Two controllers may share a machine, which is how one machine offers more
#'   than one tier. `host_workers` then caps the whole machine, so the tiers
#'   on it cannot oversubscribe it between them.
#' @section Choosing `workers`:
#'   Each controller's `workers` is how many tasks that machine should run at
#'   once, not how many cores it has. A task that compiles before it runs may
#'   need several GB briefly and a processor for a long while, so the number
#'   that matters is usually how many such tasks the machine should carry. A
#'   handful per machine is usual. `workers` caps the worker processes the
#'   machine runs, and this group keeps those slots full across every machine.
#' @return A balanced controller group object.
#' @param ... `crew` controller objects, or lists of them, as in
#'   [crew::crew_controller_group()]. Controllers are tried in the order
#'   given when their free capacity ties.
#' @param tiers `NULL`, or a named character vector giving the tier of each
#'   controller: the names are controller names and the values are tier
#'   names. A controller the vector does not name is in the `"default"`
#'   tier, which is where a task that names no controller goes, so at least
#'   one controller must end up there. Tier names and controller names must
#'   not overlap, since a push names one or the other.
#' @param hosts `NULL`, or a named character vector giving the machine of
#'   each controller, to decide which controllers share one. Names are
#'   controller names. The default for an `ssh` controller is its `ssh_host`,
#'   and for any other controller its own name, which makes it a machine of
#'   its own.
#' @param host_workers `NULL`, or a named numeric vector giving the greatest
#'   number of tasks to place on each machine at once, over all the
#'   controllers that share it. Names are machine names as `hosts` gives
#'   them. A machine the vector does not name is not capped, and each of its
#'   controllers is still held to its own `workers`.
#' @param seconds_interval Number of seconds between polls of the group's
#'   relay, as in [crew::crew_controller_group()].
#' @examples
#' \dontrun{
#' controller <- crew_controller_balanced(
#'   crew_controller_ssh(
#'     name = "host1", ssh_host = "user@host1.example.com", workers = 6L
#'   ),
#'   crew_controller_ssh(
#'     name = "host2", ssh_host = "user@host2.example.com", workers = 8L
#'   )
#' )
#' targets::tar_option_set(
#'   controller = controller,
#'   storage = "main",
#'   retrieval = "main"
#' )
#' }
crew_controller_balanced <- function(
  ...,
  tiers = NULL,
  hosts = NULL,
  host_workers = NULL,
  seconds_interval = 0.25
) {
  # crew_controller_group() does the work of collecting the controllers,
  # naming them, refusing ones already started, and wiring their relays
  # together, so the group it builds is taken apart rather than rebuilt here.
  group <- crew::crew_controller_group(
    ...,
    seconds_interval = seconds_interval
  )
  controllers <- group$controllers
  out <- crew_class_controller_balanced$new(
    controllers = controllers,
    relay = group$relay,
    tiers = balanced_tiers(tiers = tiers, names = names(controllers)),
    hosts = balanced_hosts(hosts = hosts, controllers = controllers),
    host_workers = balanced_host_workers(host_workers = host_workers)
  )
  out$validate()
  out
}

#' @title Balanced controller group class
#' @export
#' @family balanced
#' @description `R6` class for a controller group that places tasks itself.
#' @details See [crew_controller_balanced()].
crew_class_controller_balanced <- R6::R6Class(
  classname = "crew_class_controller_balanced",
  inherit = crew::crew_class_controller_group,
  portable = FALSE,
  cloneable = FALSE,
  # A non-portable R6 class resolves the names in every method it has, the
  # inherited ones included, in this environment. The methods inherited from
  # the controller group call crew's internals, so it has to be crew's
  # namespace, which in turn is why the methods below call no function of
  # this package: they would not be found. Anything they need from this
  # package is computed by crew_controller_balanced() and passed to new().
  parent_env = asNamespace("crew"),
  public = list(
    #' @description Create a balanced controller group object.
    #' @return A balanced controller group object.
    #' @param controllers Named list of `crew` controller objects.
    #' @param relay A `crew` relay object.
    #' @param tiers Named character vector, the tier of each controller.
    #' @param hosts Named character vector, the machine of each controller.
    #' @param host_workers `NULL`, or a named numeric vector capping the
    #'   tasks placed on each machine at once.
    initialize = function(
      controllers = NULL,
      relay = NULL,
      tiers = NULL,
      hosts = NULL,
      host_workers = NULL
    ) {
      super$initialize(controllers = controllers, relay = relay)
      private$.tiers <- tiers
      private$.hosts <- hosts
      private$.host_workers <- host_workers
      invisible()
    },
    #' @description Validate the group.
    #' @return `NULL` (invisibly).
    validate = function() {
      super$validate()
      names <- names(private$.controllers)
      for (field in c("tiers", "hosts")) {
        value <- private[[paste0(".", field)]]
        crew::crew_assert(
          value,
          is.character(.),
          !anyNA(.),
          nzchar(.),
          identical(names(.), names),
          message = paste(
            field,
            "must be a character vector naming every controller once."
          )
        )
      }
      crew::crew_assert(
        any(private$.tiers == "default"),
        message = paste(
          "at least one controller must be in the \"default\" tier, because",
          "a task that names no controller is placed there."
        )
      )
      overlap <- intersect(unique(private$.tiers), names)
      crew::crew_assert(
        !length(setdiff(overlap, "default")),
        message = paste(
          "a tier must not have the same name as a controller, because a",
          "push names one or the other. Overlapping:",
          paste(setdiff(overlap, "default"), collapse = ", ")
        )
      )
      caps <- private$.host_workers
      crew::crew_assert(
        is.null(caps) || all(names(caps) %in% unique(private$.hosts)),
        message = paste(
          "host_workers must name machines that hosts gives. Unknown:",
          paste(setdiff(names(caps), unique(private$.hosts)), collapse = ", ")
        )
      )
      invisible()
    },
    #' @description Push a task to the controller this group chooses for it.
    #' @return `NULL` (invisibly).
    #' @param command Language object or character string, the R command to
    #'   run, as in [crew::crew_controller_group()].
    #' @param data Named list of local data objects for `command`.
    #' @param globals Named list of objects to assign to the global
    #'   environment of the worker.
    #' @param substitute Logical of length 1, whether to call
    #'   `base::substitute()` on `command`.
    #' @param seed `NULL` or an integer of length 1, the pseudo-random number
    #'   generator seed.
    #' @param algorithm `NULL` or a character of length 1, the pseudo-random
    #'   number generator algorithm.
    #' @param packages Character vector of packages to load on the worker.
    #' @param library `NULL` or a character vector of library paths.
    #' @param seconds_timeout `NULL` or a number, the task timeout.
    #' @param scale Logical of length 1, whether to scale workers.
    #' @param throttle Logical of length 1, whether to skip scaling when it
    #'   happened too recently.
    #' @param name `NULL` or a character of length 1, the task name. The
    #'   group uses it to find a machine that already holds the task's work.
    #' @param save_command Logical of length 1, whether to keep `command` as
    #'   a string in the task's result.
    #' @param controller `NULL`, a tier name, or a controller name. `NULL`
    #'   means the `"default"` tier.
    push = function(
      command,
      data = list(),
      globals = list(),
      substitute = TRUE,
      seed = NULL,
      algorithm = NULL,
      packages = character(0),
      library = NULL,
      seconds_timeout = NULL,
      scale = TRUE,
      throttle = TRUE,
      name = NULL,
      save_command = NULL,
      controller = NULL
    ) {
      if (substitute) {
        command <- substitute(command)
      }
      # The chosen controller's own push() runs, not the parent class's, so a
      # launcher that wraps the task (as the ssh launcher does for salvage)
      # still gets to wrap it.
      control <- private$.controllers[[
        private$place(name = name, controller = controller)
      ]]
      control$push(
        command = command,
        data = data,
        globals = globals,
        substitute = FALSE,
        seed = seed,
        algorithm = algorithm,
        packages = packages,
        library = library,
        seconds_timeout = seconds_timeout,
        scale = scale,
        throttle = throttle,
        name = name,
        save_command = save_command
      )
    },
    #' @description Whether every controller that could take the task is
    #'   saturated.
    #' @details Each controller is asked with its own `saturated()` method
    #'   rather than by counting tasks here, so a controller that lets a task
    #'   through while it is full, as the `ssh` controller does for a task an
    #'   earlier session left in flight, still does.
    #' @return `TRUE` if no candidate controller can take another task.
    #' @param collect Deprecated in `crew`, not used.
    #' @param throttle Deprecated in `crew`, not used.
    #' @param controller `NULL`, a tier name, or a controller name.
    saturated = function(collect = NULL, throttle = NULL, controller = NULL) {
      names <- private$candidates(controller = controller)
      all(
        vapply(
          names,
          function(name) {
            isTRUE(private$.controllers[[name]]$saturated())
          },
          logical(1L)
        )
      )
    },
    #' @description Add a task name to the backlog of the controller this
    #'   group chooses for it.
    #' @return `NULL` (invisibly).
    #' @param name Character of length 1, the task name.
    #' @param controller `NULL`, a tier name, or a controller name.
    push_backlog = function(name, controller = NULL) {
      control <- private$.controllers[[
        private$place(name = name, controller = controller)
      ]]
      control$push_backlog(name = name)
      invisible()
    }
  ),
  private = list(
    .tiers = NULL,
    .hosts = NULL,
    .host_workers = NULL,
    # The controllers a push may go to: the one it names, or every controller
    # of the tier it names, or the "default" tier when it names nothing.
    candidates = function(controller) {
      names <- names(private$.controllers)
      if (is.null(controller)) {
        controller <- "default"
      }
      crew::crew_assert(
        controller,
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        message = "controller must be NULL or a character string."
      )
      if (controller %in% names) {
        return(controller)
      }
      out <- names[private$.tiers[names] == controller]
      crew::crew_assert(
        length(out) > 0L,
        message = sprintf("controller not found: %s", controller)
      )
      out
    },
    # Which controller takes the task: see the ranking in
    # crew_controller_balanced().
    place = function(name, controller) {
      names <- private$candidates(controller = controller)
      if (length(names) == 1L) {
        return(names)
      }
      held <- private$affinity(name = name, names = names)
      if (!is.na(held)) {
        return(held)
      }
      open <- names[!private$capped(names = names)]
      pool <- if (length(open)) open else names
      free <- vapply(pool, private$free, numeric(1L))
      pool[[which.max(free)]]
    },
    # How many more tasks a controller would take before it is saturated,
    # which crew defines as holding as many tasks as its launcher allows
    # workers. It goes negative for a controller pushed past that, so the
    # least overshooting controller still wins when every candidate is full.
    free = function(name) {
      controller <- private$.controllers[[name]]
      workers <- controller$launcher$workers
      if (!is.numeric(workers) || length(workers) != 1L || anyNA(workers)) {
        return(0)
      }
      as.numeric(workers) - as.numeric(controller$size())
    },
    # The first candidate whose remote system already holds the task's work,
    # or NA when none does.
    affinity = function(name, names) {
      if (
        !is.character(name) ||
          length(name) != 1L ||
          is.na(name) ||
          !nzchar(name)
      ) {
        return(NA_character_)
      }
      for (candidate in names) {
        # A launcher with no salvage, such as a local or a cloud one, holds
        # nothing, so placement falls through to free capacity for it.
        holds <- private$.controllers[[candidate]]$launcher$salvage_holds
        if (is.function(holds) && length(holds(name))) {
          return(candidate)
        }
      }
      NA_character_
    },
    # Whether each candidate's machine is at its cap, counting the tasks of
    # every controller that shares the machine, not just the candidate's.
    capped = function(names) {
      caps <- private$.host_workers
      if (!length(caps)) {
        return(rep(FALSE, length(names)))
      }
      vapply(
        names,
        function(name) {
          host <- private$.hosts[[name]]
          cap <- caps[host]
          !is.na(cap) && private$host_load(host = host) >= cap
        },
        logical(1L),
        USE.NAMES = FALSE
      )
    },
    # The tasks held by every controller of one machine.
    host_load = function(host) {
      names <- names(private$.controllers)
      names <- names[private$.hosts[names] == host]
      sum(
        vapply(
          names,
          function(name) as.numeric(private$.controllers[[name]]$size()),
          numeric(1L)
        )
      )
    }
  )
)

# The tier of each controller, "default" for the ones `tiers` does not name.
balanced_tiers <- function(tiers, names) {
  out <- rep("default", length(names))
  names(out) <- names
  if (is.null(tiers)) {
    return(out)
  }
  balanced_assert_named(value = tiers, what = "tiers")
  balanced_assert_known(value = tiers, names = names, what = "tiers")
  out[names(tiers)] <- unname(tiers)
  out
}

# The machine of each controller: the one `hosts` gives, else the controller's
# ssh_host when it has one, else the controller's own name, which makes it a
# machine of its own and so shares a cap with nothing.
balanced_hosts <- function(hosts, controllers) {
  names <- names(controllers)
  out <- vapply(
    names,
    function(name) {
      host <- controllers[[name]]$launcher$ssh_host
      if (
        is.character(host) &&
          length(host) == 1L &&
          !is.na(host) &&
          nzchar(host)
      ) {
        host
      } else {
        name
      }
    },
    character(1L),
    USE.NAMES = FALSE
  )
  names(out) <- names
  if (is.null(hosts)) {
    return(out)
  }
  balanced_assert_named(value = hosts, what = "hosts")
  balanced_assert_known(value = hosts, names = names, what = "hosts")
  out[names(hosts)] <- unname(hosts)
  out
}

# The cap on each machine, checked here rather than at the first push so that
# a typo stops the pipeline before it starts.
balanced_host_workers <- function(host_workers) {
  if (is.null(host_workers)) {
    return(NULL)
  }
  crew::crew_assert(
    is.numeric(host_workers) &&
      length(host_workers) > 0L &&
      !anyNA(host_workers) &&
      all(host_workers > 0L) &&
      !is.null(names(host_workers)) &&
      all(nzchar(names(host_workers))),
    message = paste(
      "host_workers must be a non-empty named numeric vector of positive",
      "non-missing values."
    )
  )
  host_workers
}

# The shape both `tiers` and `hosts` must have: a named character vector with
# nothing missing or empty on either side.
balanced_assert_named <- function(value, what) {
  crew::crew_assert(
    is.character(value) &&
      length(value) > 0L &&
      !anyNA(value) &&
      all(nzchar(value)) &&
      !is.null(names(value)) &&
      all(nzchar(names(value))),
    message = paste(
      what,
      "must be a non-empty named character vector with no missing or empty",
      "values."
    )
  )
  invisible()
}

# Both `tiers` and `hosts` are keyed by controller name, so a key that names
# no controller is a typo that would otherwise go unnoticed.
balanced_assert_known <- function(value, names, what) {
  unknown <- setdiff(names(value), names)
  crew::crew_assert(
    !length(unknown),
    message = paste(
      what,
      "names controllers that are not in the group:",
      paste(unknown, collapse = ", ")
    )
  )
  invisible()
}
