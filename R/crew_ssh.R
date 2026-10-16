# Launcher ----

#' @title `ssh` launcher class
#' @export
#' @family plugin_ssh
#' @description `R6` class to launch and manage `crew` workers on a remote
#'   system over `ssh`.
#' @details Workers are ordinary `crew` workers started with `Rscript` on the
#'   remote system. They reach the local `mirai` dispatcher through a reverse
#'   `ssh` tunnel (`ssh -R`), so the remote system never needs to open a
#'   connection to the local machine directly and the local machine needs no
#'   inbound firewall rule or public address.
#'
#'   The launcher shells out to the system `ssh` client rather than using an
#'   in-process `ssh` library, so `~/.ssh/config` entries, `ssh-agent`,
#'   `known_hosts`, `ProxyJump`, and preshared keys all behave exactly as they
#'   do on the command line. Authentication must be non-interactive
#'   (`BatchMode=yes` is used), which in practice means key-based
#'   authentication.
#' @examples
#' if (identical(Sys.getenv("CREW_SSH_EXAMPLES"), "true")) {
#' controller <- crew_controller_ssh(ssh_host = "user@example.com")
#' controller$start()
#' controller$push(Sys.info()[["nodename"]])
#' controller$wait()
#' controller$pop()
#' controller$terminate()
#' }
crew_class_launcher_ssh <- R6::R6Class(
  classname = "crew_class_launcher_ssh",
  inherit = crew::crew_class_launcher,
  cloneable = FALSE,
  public = list(
    #' @description Create an `ssh` launcher object.
    #' @return An `ssh` launcher object.
    #' @param ... Arguments passed to
    #'   `crew::crew_class_launcher$initialize()`.
    #' @param ssh_host Character of length 1, the `ssh` destination of the
    #'   remote system, e.g. `"user@example.com"`. Anything the `ssh` command
    #'   accepts works here, including a `Host` alias from `~/.ssh/config`.
    #' @param ssh_port Positive integer of length 1 or `NULL`, the port of the
    #'   remote `ssh` server (`ssh -p`). `NULL` uses the `ssh` default.
    #' @param ssh_keyfile Character of length 1 or `NULL`, path to the private
    #'   key file to authenticate with (`ssh -i`). `NULL` uses the keys `ssh`
    #'   would choose on its own.
    #' @param ssh_options Character vector of additional arguments for the
    #'   `ssh` command, e.g. `c("-o", "ProxyJump=bastion")`. These are placed
    #'   ahead of the options the launcher sets, and `ssh` honors the first
    #'   occurrence of an option, so anything given here takes precedence.
    #' @param ssh_command Character of length 1, the `ssh` executable to run.
    #' @param rscript Character of length 1, path to `Rscript` on the remote
    #'   system. The default assumes `Rscript` is on the remote `PATH` for
    #'   non-interactive `ssh` sessions.
    #' @param launch_prefix Character vector, a command and its arguments to
    #'   run the worker under, e.g.
    #'   `c("docker", "run", "--init", "--rm", "--network=host", "my-image")`.
    #'   Empty by default, which runs `rscript` directly. Each element is
    #'   quoted for the remote shell separately, so an element may contain
    #'   spaces without being split into two arguments. When the prefix starts
    #'   with `docker run` or `podman run`, the launcher adds
    #'   `--cidfile` so that it can stop the container itself.
    #' @param directory Character of length 1 or `NULL`, working directory of
    #'   the workers on the remote system. `NULL` means the directory `ssh`
    #'   lands in, usually the remote home directory. This is the directory the
    #'   command named by `launch_prefix` starts in, which for a container
    #'   runtime is not the same as the working directory inside the container.
    #' @param remote_directory Character of length 1, absolute path to a
    #'   directory on the remote system where the launcher may write worker
    #'   scripts, logs, and records. Each controller gets its own uniquely
    #'   named subdirectory of `remote_directory`.
    #' @param salvage Logical of length 1, whether workers save the result of
    #'   each task on the remote system so that it survives a disconnect.
    #'   `TRUE` by default. See the "Surviving a disconnect" section of
    #'   [crew_controller_ssh()].
    #' @param salvage_directory Character of length 1 or `NULL`, absolute path
    #'   to the directory on the remote system where workers save results.
    #'   `NULL` means the `crew-ssh-salvage` subdirectory of
    #'   `remote_directory`. Unlike the session directories, it is shared by
    #'   every session, which is how a later session finds the results of an
    #'   earlier one.
    #' @param seconds_reconnect Non-negative number of length 1, how long to
    #'   keep trying when the network fails while reopening the tunnel or
    #'   launching a worker, in seconds of pauses between attempts. See the
    #'   "Surviving a disconnect" section of [crew_controller_ssh()].
    #' @param verbose Logical of length 1, whether to print messages about the
    #'   tunnel and each worker launch.
    initialize = function(
      ...,
      ssh_host,
      ssh_port = NULL,
      ssh_keyfile = NULL,
      ssh_options = character(0L),
      ssh_command = "ssh",
      rscript = "Rscript",
      launch_prefix = character(0L),
      directory = NULL,
      remote_directory = "/tmp",
      salvage = TRUE,
      salvage_directory = NULL,
      seconds_reconnect = 3600,
      verbose = FALSE
    ) {
      super$initialize(...)
      private$.ssh_host <- ssh_host
      private$.ssh_port <- ssh_port
      private$.ssh_keyfile <- ssh_keyfile
      private$.ssh_options <- ssh_options
      private$.ssh_command <- ssh_command
      private$.rscript <- rscript
      private$.launch_prefix <- launch_prefix
      private$.directory <- directory
      private$.remote_directory <- remote_directory
      private$.salvage <- salvage
      private$.salvage_directory <- salvage_directory
      private$.seconds_reconnect <- seconds_reconnect
      private$.verbose <- verbose
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly). Called for its side effect of throwing an
    #'   error if the launcher is misconfigured.
    validate = function() {
      super$validate()
      for (field in c("ssh_host", "ssh_command", "rscript", "remote_directory")) {
        crew::crew_assert(
          private[[paste0(".", field)]],
          is.character(.),
          length(.) == 1L,
          !anyNA(.),
          nzchar(.),
          message = paste(
            field,
            "must be a non-empty non-missing character string."
          )
        )
      }
      crew::crew_assert(
        substr(private$.remote_directory, 1L, 1L) == "/",
        message = paste(
          "remote_directory must be an absolute path on the remote system,",
          "not", shQuote(private$.remote_directory, type = "sh"), ".",
          "Paths beginning with \"~\" are not expanded."
        )
      )
      for (field in c("ssh_keyfile", "directory")) {
        value <- private[[paste0(".", field)]]
        crew::crew_assert(
          is.null(value) ||
            (is.character(value) && length(value) == 1L && !anyNA(value)),
          message = paste(
            field,
            "must be NULL or a non-missing character string."
          )
        )
      }
      crew::crew_assert(
        private$.ssh_port,
        is.null(.) || (length(.) == 1L && !anyNA(.) && . > 0L),
        message = "ssh_port must be NULL or a single positive number."
      )
      crew::crew_assert(
        private$.ssh_options,
        is.character(.),
        !anyNA(.),
        message = "ssh_options must be a character vector without NAs."
      )
      # An empty string would reach the remote shell as '', an empty argument,
      # which the container runtime or wrapper would reject in a way that is
      # hard to read back from a worker log.
      crew::crew_assert(
        private$.launch_prefix,
        is.character(.),
        !anyNA(.),
        all(nzchar(.)),
        message = paste(
          "launch_prefix must be a character vector with no NAs and no",
          "empty strings."
        )
      )
      crew::crew_assert(
        is.null(private$container_runtime()) ||
          !any(grepl("^--cidfile(=|$)", private$.launch_prefix)),
        message = paste(
          "launch_prefix must not set --cidfile: crew.ssh sets it itself to",
          "record the container ID of each worker."
        )
      )
      crew::crew_assert(
        private$.salvage,
        isTRUE(.) || isFALSE(.),
        message = "salvage must be TRUE or FALSE."
      )
      crew::crew_assert(
        private$.salvage_directory,
        is.null(.) ||
          (is.character(.) && length(.) == 1L && !anyNA(.) &&
             substr(., 1L, 1L) == "/"),
        message = paste(
          "salvage_directory must be NULL or an absolute path on the remote",
          "system."
        )
      )
      crew::crew_assert(
        private$.seconds_reconnect,
        is.numeric(.),
        length(.) == 1L,
        !anyNA(.),
        . >= 0,
        message = "seconds_reconnect must be a single non-negative number."
      )
      crew::crew_assert(
        private$.verbose,
        isTRUE(.) || isFALSE(.),
        message = "verbose must be TRUE or FALSE."
      )
      invisible()
    },
    #' @description Start the launcher and open the reverse `ssh` tunnel that
    #'   remote workers use to reach the local `mirai` dispatcher.
    #' @return `NULL` (invisibly).
    #' @param url Character of length 1, `mirai` URL of the local dispatcher.
    #' @param profile Character of length 1, `mirai` compute profile.
    start = function(url = NULL, profile = NULL) {
      super$start(url = url, profile = profile)
      private$.session <- paste0("crew-ssh-", crew::crew_random_name())
      private$.session_directory <- paste0(
        private$.remote_directory,
        "/",
        private$.session
      )
      private$.handles <- list()
      private$.disconnected <- character(0L)
      private$.unreachable <- remote_unreachable_none()
      private$.unreachable_checked <- -Inf
      private$.obsolete_tried <- -Inf
      private$.salvage_saved <- character(0L)
      private$.salvage_passes <- character(0L)
      # Hashed environments, because a pipeline may push and collect hundreds
      # of thousands of tasks, and growing a vector costs a copy each time.
      private$.salvage_keys <- new.env(hash = TRUE, parent = emptyenv())
      private$.salvage_collected <- new.env(hash = TRUE, parent = emptyenv())
      private$.salvage_shipped <- salvage_shipped(
        directory = private$salvage_root(),
        session = private$.session,
        session_directory = private$.session_directory,
        controller = private$.name,
        seconds = private$.salvage_seconds
      )
      private$tunnel_start()
      invisible()
    },
    #' @description `mirai` daemon settings for a remote worker.
    #' @details Identical to the settings of the parent class except that the
    #'   URL points at the remote end of the reverse `ssh` tunnel instead of
    #'   the dispatcher's local address, and that with `salvage = TRUE` a
    #'   worker that loses its connection finishes the task in progress
    #'   before it exits.
    #' @return A named list of `mirai` daemon settings.
    settings = function() {
      out <- super$settings()
      if (!is.null(private$.tunnel_port)) {
        out$url <- private$worker_url()
      }
      # mirai reads autoexit = NA as "complete the task in progress, then
      # exit" and the crew default, a signal, as "exit now". Exiting now would
      # throw away the work salvage exists to keep.
      if (isTRUE(private$.salvage)) {
        out$autoexit <- NA
      }
      out
    },
    #' @description Launch workers to match the tasks pushed so far.
    #' @details As in `crew`, except that with `salvage = TRUE`, tasks still
    #'   running on workers this controller cannot reach count against
    #'   `workers` until they finish or a worker of this controller takes them
    #'   over, and those that run an earlier version of a task this controller
    #'   has pushed are stopped. See the "Surviving a disconnect" section of
    #'   [crew_controller_ssh()].
    #' @return `TRUE` if it launched workers, `FALSE` otherwise, invisibly.
    #' @param status A list of task and connection counts from the `crew`
    #'   client.
    #' @param throttle Logical of length 1 or `NULL`, whether to throttle
    #'   scaling.
    scale = function(status, throttle = NULL) {
      busy <- private$unreachable_busy(status)
      if (busy < 1L) {
        return(super$scale(status = status, throttle = throttle))
      }
      workers <- private$.workers
      on.exit(private$.workers <- workers, add = TRUE)
      private$.workers <- max(0L, workers - busy)
      super$scale(status = status, throttle = throttle)
    },
    #' @description Launch a worker on the remote system.
    #' @return A handle: a named list with the worker `name`, the remote
    #'   process ID `pid`, and the remote paths of the worker `script`, its
    #'   `log`, and the `container` ID file, which is `NULL` unless the worker
    #'   runs under `docker run` or `podman run`.
    #' @param call Character of length 1, R code the worker should run.
    launch_worker = function(call) {
      name <- paste0("worker-", crew::crew_random_name())
      paths <- private$worker_paths(name)
      local_script <- tempfile(pattern = "crew_ssh_", fileext = ".R")
      on.exit(unlink(local_script), add = TRUE)
      command <- private$worker_command(name)
      if (private$.verbose) {
        message("crew.ssh: launching ", name, ", log at ", paths$log)
      }
      pid <- private$retry(
        what = paste("launch a worker on", private$.ssh_host),
        attempt = "launch_attempt",
        call = call,
        name = name,
        command = command,
        local_script = local_script
      )
      handle <- list(
        name = name,
        pid = pid,
        script = paths$script,
        log = paths$log,
        container = if (is.null(private$container_runtime())) {
          NULL
        } else {
          paths$cid
        }
      )
      private$.handles[[name]] <- handle
      handle
    },
    #' @description Read the remote log files of the workers this launcher
    #'   started.
    #' @details Worker output is the first place to look when a worker starts
    #'   but never connects back to the controller. The logs live on the remote
    #'   system and outlive the workers, so this method still works after a
    #'   worker crashes.
    #' @return A character vector of log file contents, with a header line
    #'   naming each file. Returns `character(0)` if the launcher has not
    #'   started any workers.
    #' @param lines Positive integer of length 1, number of trailing lines to
    #'   read from each log file.
    logs = function(lines = 100L) {
      if (!length(private$.handles)) {
        return(character(0L))
      }
      out <- private$run(
        command = paste0(
          "tail -v -n ", as.integer(lines), " ",
          shQuote(private$.session_directory, type = "sh"), "/*.log"
        ),
        timeout = private$.seconds_timeout
      )
      strsplit(paste0(out$stdout, out$stderr), split = "\n", fixed = TRUE)[[1L]]
    },
    #' @description List the workers of every session under
    #'   `remote_directory`, including sessions that ended or lost their
    #'   connection.
    #' @details This reads the records each session keeps in its directory on
    #'   the remote system, so it finds workers that outlived the session that
    #'   launched them: with `salvage = TRUE`, a worker whose connection drops
    #'   finishes its task before it exits, and so does a container started
    #'   without `--init`, salvage or not.
    #' @return A data frame with one row per worker: the `host`, the
    #'   `session` and its `session_directory`, the `worker` name, the remote
    #'   process ID `pid` (the client of the container runtime for a container
    #'   worker), the `container` ID and `runtime`, whether the worker is
    #'   `alive`, whether its session is still connected (`session_alive`),
    #'   whether the session is this launcher's (`current`), the machine
    #'   (`local_host`), process ID (`local_pid`), and process creation time
    #'   in seconds since the epoch (`local_created`) of the R session that
    #'   started it, and, with `salvage = TRUE`, the key, name, and state of
    #'   its latest task with the times it started, ended, and was last
    #'   updated. `task_state` is one of `"running"`, `"waiting"` (for a worker
    #'   of another session running the same task), `"reused"`, `"success"`,
    #'   `"error"`, or `"interrupted"` (canceled or timed out).
    #'
    #'   A session counts as connected while the remote end of its tunnel
    #'   runs. The remote `sshd` can keep that end for hours after the local
    #'   machine dropped off the network, so a session started on this machine
    #'   whose R process has ended counts as gone whatever the remote end says.
    remote_workers = function() {
      out <- private$run(
        command = remote_workers_script(private$.remote_directory),
        timeout = private$.seconds_timeout
      )
      private$assert_remote(out, "list the remote workers")
      private$workers_frame(out$stdout)
    },
    #' @description Stop the workers of other sessions that are still running
    #'   after their session ended or lost its connection.
    #' @details An orphaned worker is often still busy with a task. With
    #'   `salvage = TRUE` it saves the result when it finishes, and a later
    #'   push of the same task picks it up, so stopping it throws that work
    #'   away. Check `remote_workers()` first.
    #' @return A data frame of the workers it stopped, in the format of
    #'   `remote_workers()`, invisibly.
    terminate_orphans = function() {
      workers <- self$remote_workers()
      orphans <- workers[
        workers$alive & !workers$session_alive & !workers$current,
        ,
        drop = FALSE
      ]
      if (nrow(orphans)) {
        out <- private$run(
          command = remote_stop_script(
            pids = orphans$pid,
            sessions = orphans$session,
            cid_files = paste0(
              orphans$session_directory, "/", orphans$worker, ".cid"
            ),
            runtimes = orphans$runtime
          ),
          timeout = private$.seconds_timeout
        )
        private$assert_remote(out, "stop the orphaned workers")
      }
      invisible(orphans)
    },
    #' @description List the results saved in the salvage directory and the
    #'   tasks running under a lock there.
    #' @return A data frame with one row per saved result or lock: the task
    #'   `key`; its `state`, which is `"saved"` for a result, `"running"` for
    #'   a task a live worker holds the lock of, and `"abandoned"` for a lock
    #'   whose worker died; the size in `bytes` of a result; when it was last
    #'   `modified`; the seconds since a lock's last heartbeat
    #'   (`heartbeat_age`); the `worker` and `session` holding a lock; and the
    #'   remote `path`.
    salvage_list = function() {
      out <- private$run(
        command = remote_salvage_script(private$salvage_root()),
        timeout = private$.seconds_timeout
      )
      private$assert_remote(out, "list the saved results")
      remote_salvage_frame(
        text = out$stdout,
        directory = private$salvage_root(),
        seconds_stale = private$.salvage_seconds[["stale"]]
      )
    },
    #' @description Delete saved results from the salvage directory.
    #' @details Results are deleted on their own once a controller collects
    #'   them. Use this for results nothing will collect, such as those of a
    #'   task whose code has changed since, which no longer match any task.
    #' @return `keys`, invisibly.
    #' @param keys Character vector of task keys, as in the `key` column of
    #'   `salvage_list()`.
    salvage_remove = function(keys) {
      crew::crew_assert(
        keys,
        is.character(.),
        !anyNA(.),
        all(grepl("^[A-Za-z0-9._-]+$", .)),
        message = paste(
          "keys must be task keys as salvage_list() reports them: letters,",
          "digits, dots, underscores, and dashes."
        )
      )
      if (length(keys)) {
        out <- private$run(
          command = paste(
            "rm -f --",
            paste(
              shQuote(private$salvage_path(keys), type = "sh"),
              collapse = " "
            )
          ),
          timeout = private$.seconds_timeout
        )
        private$assert_remote(out, "delete the saved results")
      }
      invisible(keys)
    },
    #' @description Prepare a task for salvage. The controller calls this for
    #'   every task it pushes; it is not meant to be called directly.
    #' @details Wraps the command of a named task so the worker saves its
    #'   result on the remote system, or reuses the result a worker saved
    #'   before. A task with no name has no identity that a later push could
    #'   match, so it runs unchanged, as does every task with
    #'   `salvage = FALSE`.
    #' @return A list with the `command` and `data` to push.
    #' @param command Language object, the command of the task.
    #' @param data Named list, the data of the task.
    #' @param globals Named list, the globals of the task.
    #' @param seed,algorithm,packages,library Arguments of the task as given
    #'   to `push()`.
    #' @param name Character of length 1 or `NULL`, the name of the task.
    salvage_task = function(
      command,
      data,
      globals,
      seed,
      algorithm,
      packages,
      library,
      name
    ) {
      if (!isTRUE(private$.salvage) || is.null(name) || !nzchar(name)) {
        return(list(command = command, data = data))
      }
      crew::crew_assert(
        !is.null(private$.salvage_shipped),
        message = paste(
          "the crew.ssh launcher must be started before tasks are pushed."
        )
      )
      crew::crew_assert(
        !(".crew_ssh_salvage" %in% names(data)),
        message = "the data of a task must not contain .crew_ssh_salvage."
      )
      cache <- private$.salvage_globals
      if (is.null(cache) || !identical(globals, cache$globals)) {
        private$.salvage_globals <- salvage_globals_cache(globals)
      }
      private$.salvage_passes <- setdiff(private$.salvage_passes, name)
      key <- salvage_key(
        command = command,
        data = data,
        globals = private$.salvage_globals,
        seed = seed,
        algorithm = algorithm,
        packages = packages,
        library = library,
        name = name
      )
      # A pushed task owns its key again, so a deletion still waiting from an
      # earlier collection would delete the new result.
      private$.salvage_pending <- setdiff(
        private$.salvage_pending,
        private$salvage_path(key)
      )
      assign(name, key, envir = private$.salvage_keys)
      # A result this session has already collected was delivered. Running a
      # task again after that asks for a new run, not for the same result.
      reuse <- !exists(
        key,
        envir = private$.salvage_collected,
        inherits = FALSE
      )
      command <- substitute(
        .crew_ssh_salvage$run(
          expr = quote(command),
          envir = environment(),
          key = key,
          name = name,
          reuse = reuse,
          salvage = .crew_ssh_salvage
        ),
        list(command = command, key = key, name = name, reuse = reuse)
      )
      data <- c(data, list(.crew_ssh_salvage = private$.salvage_shipped))
      private$salvage_flush(wait = FALSE)
      list(command = command, data = data)
    },
    #' @description Note that the controller collected a task. The controller
    #'   calls this for every task it hands back; it is not meant to be called
    #'   directly.
    #' @details A task that was collected with its result or an error from
    #'   its own code has nothing left to salvage, so its saved result is
    #'   deleted from the remote system, in the background and in batches. A
    #'   task that crashed keeps its saved result: its worker may still be
    #'   finishing it. So does a task `crew` reports as an error without an
    #'   error message, which is how `crew` hands back a task that has not
    #'   resolved yet.
    #' @return `NULL` (invisibly).
    #' @param name Character of length 1, the name of the task.
    #' @param status Character of length 1, the status `crew` reports for the
    #'   task.
    #' @param error Character of length 1, the error message `crew` reports
    #'   for the task, or `NA`.
    salvage_collect = function(name, status, error = NA_character_) {
      keys <- private$.salvage_keys
      key <- if (is.null(keys)) {
        NULL
      } else {
        get0(name, envir = keys, inherits = FALSE)
      }
      if (is.null(key) || identical(status, "crash")) {
        return(invisible())
      }
      rm(list = name, envir = keys)
      # A task that raised an error has one, as crew_eval() records it. An
      # error status with none comes from crew itself, which reports a task
      # it returned before the task resolved that way, and that task is still
      # running.
      delivered <- identical(status, "success") ||
        (identical(status, "error") && !is.na(error))
      if (delivered) {
        assign(key, TRUE, envir = private$.salvage_collected)
        private$.salvage_pending <- union(
          private$.salvage_pending,
          private$salvage_path(key)
        )
        private$salvage_flush(wait = FALSE)
      }
      invisible()
    },
    #' @description Choose the tasks of a backlog that have something saved
    #'   or running from an earlier session, so they are pushed first. The
    #'   controller calls this from `pop_backlog()`; it is not meant to be
    #'   called directly.
    #' @details A task of this kind is cheap to push: a saved result comes back
    #'   at once, a run of the same task is waited for instead of repeated, and
    #'   a run of an earlier version of it is stopped as soon as the new version
    #'   is pushed, which frees its slot. Until the task is pushed, none of that
    #'   can happen, because only the push gives its key. The tasks chosen are
    #'   pushed even when the controller is saturated (see `salvage_passing()`).
    #'   The choice uses only what the controller last read from the remote
    #'   system, so it costs no `ssh` call.
    #' @return Character vector, the names in `names` to push first, in their
    #'   order there.
    #' @param names Character vector, the names of the tasks in the backlog,
    #'   in order.
    salvage_backlog = function(names) {
      private$.salvage_passes <- character(0L)
      if (!isTRUE(private$.salvage) || !length(names)) {
        return(character(0L))
      }
      first <- backlog_first(
        names = names,
        running = private$.unreachable$task_name,
        saved = private$.salvage_saved,
        keys = private$.salvage_keys
      )
      private$.salvage_passes <- first
      first
    },
    #' @description Whether a task chosen by `salvage_backlog()` is still to
    #'   be pushed. The controller calls this from `saturated()`; it is not
    #'   meant to be called directly.
    #' @details `targets` asks whether the controller is saturated before it
    #'   pushes each task, and keeps the task back if it is. A chosen task is
    #'   let through, and its push clears it.
    #' @return `TRUE` or `FALSE`.
    salvage_passing = function() {
      length(private$.salvage_passes) > 0L
    },
    #' @description Terminate the launcher: stop the remote workers that are
    #'   not busy, delete the saved results this session collected, and close
    #'   the reverse `ssh` tunnel.
    #' @details Without salvage, every worker is stopped. With
    #'   `salvage = TRUE`, a worker in the middle of a task is left to finish
    #'   it and save the result, as after a disconnect, and the rest are
    #'   stopped. A pipeline stopped part way through, whether interrupted or
    #'   halted by an error, therefore keeps its remote work: a later session
    #'   reuses each saved result, waits for a task still running, and stops a
    #'   run whose task has changed since (see the "Surviving a disconnect"
    #'   section of [crew_controller_ssh()]). `terminate_orphans()`, called
    #'   from a later session, stops the workers left running.
    #' @return `NULL` (invisibly).
    terminate = function() {
      private$workers_stop(keep_running = isTRUE(private$.salvage))
      private$salvage_flush(wait = TRUE)
      private$tunnel_stop()
      super$terminate()
      invisible()
    }
  ),
  private = list(
    .ssh_host = NULL,
    .ssh_port = NULL,
    .ssh_keyfile = NULL,
    .ssh_options = NULL,
    .ssh_command = NULL,
    .rscript = NULL,
    .launch_prefix = NULL,
    .directory = NULL,
    .remote_directory = NULL,
    .salvage = NULL,
    .salvage_directory = NULL,
    .seconds_reconnect = NULL,
    # The first and the longest pause between attempts of retry(), in
    # seconds (see reconnect_pause()).
    .reconnect_pauses = c(first = 5, longest = 300),
    .verbose = NULL,
    .session = NULL,
    .session_directory = NULL,
    .handles = list(),
    # Workers of this session launched before its tunnel was last reopened.
    .disconnected = character(0L),
    # Tasks running on workers this controller cannot reach (see
    # unreachable_busy()), when they were last read, and how often to read
    # them, in seconds; and when this controller last tried to stop the ones
    # that run an earlier version of a task it pushed (see
    # unreachable_obsolete()).
    .unreachable = NULL,
    .unreachable_checked = -Inf,
    .unreachable_seconds = 30,
    .obsolete_tried = -Inf,
    .tunnel = NULL,
    .tunnel_port = NULL,
    .tunnel_output = character(0L),
    # How often a busy worker touches the heartbeat of its lock, how long a
    # silent lock counts as held, and how often a worker waiting for another
    # one checks back, in seconds.
    .salvage_seconds = c(heartbeat = 15, stale = 120, poll = 10),
    .salvage_shipped = NULL,
    # A salvage_globals_cache() of the globals last pushed.
    .salvage_globals = NULL,
    # Keys of the results saved in the salvage directory, as last read with
    # the unreachable tasks.
    .salvage_saved = character(0L),
    # Tasks salvage_backlog() chose that are still to be pushed.
    .salvage_passes = character(0L),
    .salvage_keys = NULL,
    .salvage_collected = NULL,
    .salvage_pending = character(0L),
    .salvage_deleting = character(0L),
    .salvage_cleanup = NULL,
    ssh_args = function() {
      c(
        private$.ssh_options,
        "-o", "BatchMode=yes",
        if (is.null(private$.ssh_port)) {
          character(0L)
        } else {
          c("-p", as.character(as.integer(private$.ssh_port)))
        },
        if (is.null(private$.ssh_keyfile)) {
          character(0L)
        } else {
          c("-i", private$.ssh_keyfile)
        }
      )
    },
    run = function(command, stdin = NULL, timeout) {
      ssh_run(
        ssh_command = private$.ssh_command,
        args = c(private$ssh_args(), private$.ssh_host, command),
        stdin = stdin,
        timeout = timeout
      )
    },
    assert_remote = function(out, task) {
      crew::crew_assert(
        identical(out$status, 0L),
        message = paste0(
          "crew.ssh could not ", task, " on ", private$.ssh_host, ". ",
          "ssh exited with status ", out$status,
          if (isTRUE(out$timeout)) " (timed out)" else "",
          ".\nstderr: ", trimws(out$stderr)
        )
      )
    },
    # The container runtime the workers run under, if launch_prefix starts one
    # this launcher knows how to stop.
    container_runtime = function() {
      prefix <- private$.launch_prefix
      if (
        length(prefix) >= 2L &&
          basename(prefix[1L]) %in% c("docker", "podman") &&
          identical(prefix[2L], "run")
      ) {
        prefix[1L]
      } else {
        NULL
      }
    },
    # The runtime as the records spell it: empty when there is none.
    runtime_name = function() {
      runtime <- private$container_runtime()
      if (is.null(runtime)) "" else runtime
    },
    worker_paths = function(name) {
      base <- paste0(private$.session_directory, "/", name)
      list(
        script = paste0(base, ".R"),
        log = paste0(base, ".log"),
        pid = paste0(base, ".pid"),
        cid = paste0(base, ".cid"),
        task = paste0(base, ".task")
      )
    },
    # Shell command that receives the worker script on stdin, detaches an
    # Rscript process to run it, records that process's PID in the session
    # directory, and reports it. Everything that varies is quoted for the
    # remote POSIX shell; the worker code itself never reaches the command
    # line, so no amount of quoting in the deparsed call can confuse the
    # shell.
    worker_command = function(name) {
      paths <- private$worker_paths(name)
      prefix <- private$.launch_prefix
      if (!is.null(private$container_runtime())) {
        prefix <- append(prefix, c("--cidfile", paths$cid), after = 2L)
      }
      paste0(
        "set -e; ",
        "mkdir -p ", shQuote(private$.session_directory, type = "sh"), "; ",
        "cat > ", shQuote(paths$script, type = "sh"), "; ",
        if (is.null(private$.directory)) {
          ""
        } else {
          paste0("cd ", shQuote(private$.directory, type = "sh"), "; ")
        },
        "nohup ",
        paste(
          shQuote(
            c(prefix, private$.rscript, private$.r_arguments, paths$script),
            type = "sh"
          ),
          collapse = " "
        ),
        " > ", shQuote(paths$log, type = "sh"), " 2>&1 < /dev/null & ",
        "pid=$!; echo \"$pid\" > ", shQuote(paths$pid, type = "sh"), "; ",
        "echo \"$pid\""
      )
    },
    # Split "tcp://127.0.0.1:5000" into its scheme, host, and port. The port is
    # whatever follows the last colon, so bracketed IPv6 hosts survive too.
    parse_url = function(url) {
      crew::crew_assert(
        grepl("^[^:]+://.+:[0-9]+$", url),
        message = paste(
          "crew.ssh cannot parse the dispatcher URL", shQuote(url),
          "into a scheme, host, and port."
        )
      )
      parts <- strsplit(url, split = "://", fixed = TRUE)[[1L]]
      rest <- paste(parts[-1L], collapse = "://")
      colon <- max(gregexpr(":", rest, fixed = TRUE)[[1L]])
      list(
        scheme = parts[1L],
        host = substr(rest, 1L, colon - 1L),
        port = substr(rest, colon + 1L, nchar(rest))
      )
    },
    # Remote workers dial the remote end of the tunnel. The host stays
    # loopback so that automatic TLS certificates, whose common name comes
    # from the dispatcher host, still verify.
    worker_url = function() {
      parsed <- private$parse_url(private$.url)
      paste0(parsed$scheme, "://127.0.0.1:", private$.tunnel_port)
    },
    # What the tunnel's ssh session runs on the remote system. It writes the
    # session's records, reports that it is ready, and then waits on stdin,
    # under a command line that names the session so that tunnel.pid can be
    # checked against it. The trailing ":" keeps sh from replacing itself
    # with cat, whose command line would not name the session.
    #
    # The salvage directory is created here, by the ssh user, because a
    # worker in a container that runs as root would otherwise create it as
    # root, and the ssh user could not delete the results in it afterwards.
    #
    # With salvage, the workers left waiting on a task by a session whose
    # tunnel is gone are stopped here too (see remote_stop_waiters_script()),
    # before tunnel.pid names the new tunnel. `dead` names the sessions known
    # to have ended although their tunnel's remote end may linger.
    #
    # The record names the local R process by its PID and its creation time,
    # which together tell a later session on this machine whether that
    # process still runs (see local_session_alive()).
    tunnel_command = function(dead = character(0L)) {
      directory <- private$.session_directory
      record <- c(
        paste("session:", private$.session),
        paste("controller:", private$.name),
        paste("runtime:", private$runtime_name()),
        paste("local_host:", Sys.info()[["nodename"]]),
        paste("local_pid:", Sys.getpid()),
        paste(
          "local_created:",
          sprintf("%.3f", as.numeric(ps::ps_create_time(ps::ps_handle())))
        )
      )
      directories <- c(
        directory,
        if (isTRUE(private$.salvage)) private$salvage_root()
      )
      stop_waiters <- if (isTRUE(private$.salvage)) {
        paste0(
          "{\n",
          remote_stop_waiters_script(
            remote_directory = private$.remote_directory,
            session = private$.session,
            dead = dead
          ),
          "\n:; } && "
        )
      }
      paste0(
        "mkdir -p ", paste(shQuote(directories, type = "sh"), collapse = " "),
        " && ",
        stop_waiters,
        "printf '%s\\n' ", paste(shQuote(record, type = "sh"), collapse = " "),
        " \"started: $(date +%s)\" > ",
        shQuote(paste0(directory, "/session.dcf"), type = "sh"), " && ",
        "echo $$ > ", shQuote(paste0(directory, "/tunnel.pid"), type = "sh"),
        " && echo crew-ssh-tunnel-ready && ",
        "exec sh -c 'cat > /dev/null; :' ",
        shQuote(private$.session, type = "sh")
      )
    },
    tunnel_start = function() {
      parsed <- private$parse_url(private$.url)
      dead <- if (isTRUE(private$.salvage)) {
        private$dead_sessions()
      } else {
        character(0L)
      }
      # Remote port 0 asks the remote sshd for any free port and avoids
      # colliding with whatever else is listening there. ssh reports the port
      # it was given on stderr.
      args <- c(
        private$ssh_args(),
        "-o", "ExitOnForwardFailure=yes",
        "-o", "ServerAliveInterval=30",
        "-o", "ServerAliveCountMax=3",
        "-R", paste0("0:", parsed$host, ":", parsed$port),
        private$.ssh_host,
        # Remote workers stay alive as long as they can reach the dispatcher,
        # so the tunnel must close even when the local session is killed
        # outright and no R finalizer ever runs. Holding the write end of the
        # ssh process's stdin makes the operating system do it: killing this
        # session closes the pipe, "cat" reads EOF and exits, and ssh follows.
        private$tunnel_command(dead = dead)
      )
      if (private$.verbose) {
        message(
          "crew.ssh: opening tunnel: ",
          paste(c(private$.ssh_command, args), collapse = " ")
        )
      }
      private$.tunnel <- processx::process$new(
        command = private$.ssh_command,
        args = args,
        stdin = "|",
        stdout = "|",
        stderr = "|",
        cleanup = TRUE
      )
      private$.tunnel_output <- character(0L)
      private$.tunnel_port <- NULL
      pattern <- "^Allocated port ([0-9]+) for remote forward.*$"
      deadline <- as.numeric(Sys.time()) + private$.seconds_timeout
      ready <- FALSE
      while (is.null(private$.tunnel_port) || !ready) {
        private$.tunnel$poll_io(250L)
        private$tunnel_drain()
        match <- grep(pattern, private$.tunnel_output, value = TRUE)
        ready <- "crew-ssh-tunnel-ready" %in% trimws(private$.tunnel_output)
        if (length(match) && is.null(private$.tunnel_port)) {
          port <- sub(pattern, "\\1", match[1L])
          crew::crew_assert(
            grepl("^[0-9]+$", port),
            message = paste(
              "crew.ssh could not read the forwarded port out of the ssh",
              "message", shQuote(match[1L])
            )
          )
          private$.tunnel_port <- port
        } else if (!is.null(private$.tunnel_port) && ready) {
          break
        } else if (!private$.tunnel$is_alive()) {
          private$tunnel_fail("The ssh tunnel exited before it was ready.")
        } else if (as.numeric(Sys.time()) > deadline) {
          private$tunnel_stop()
          private$tunnel_fail(
            paste(
              "The ssh tunnel did not report a forwarded port within",
              private$.seconds_timeout, "seconds."
            )
          )
        }
      }
      stopped <- remote_stopped_waiters(private$.tunnel_output)
      if (length(stopped)) {
        message(
          "crew.ssh: stopped ", length(stopped), " worker(s) on ",
          private$.ssh_host, " that were waiting for a task running on ",
          "another worker after their own session lost its connection. The ",
          "worker running each task saves its result for the next push. ",
          "Stopped: ", paste(stopped, collapse = ", ")
        )
      }
      closed <- remote_closed_tunnels(private$.tunnel_output)
      if (length(closed)) {
        message(
          "crew.ssh: closed the remote end of the tunnel of ", length(closed),
          " session(s) on ", private$.ssh_host, " that this machine started ",
          "and whose R process has ended: ", paste(closed, collapse = ", ")
        )
      }
      if (private$.verbose) {
        message(
          "crew.ssh: tunnel open, remote workers will dial ",
          private$worker_url()
        )
      }
      invisible()
    },
    # processx closes a pipe once it has read to the end, after which reading
    # it again is an error, so only pipes with more to come are read.
    tunnel_drain = function() {
      tunnel <- private$.tunnel
      if (is.null(tunnel)) {
        return(invisible())
      }
      new <- c(
        if (tunnel$is_incomplete_output()) tunnel$read_output_lines(),
        if (tunnel$is_incomplete_error()) tunnel$read_error_lines()
      )
      if (length(new)) {
        private$.tunnel_output <- c(private$.tunnel_output, new)
      }
      invisible()
    },
    tunnel_fail = function(message) {
      crew::crew_assert(
        FALSE,
        message = paste0(
          message, " ssh output:\n",
          paste(private$.tunnel_output, collapse = "\n")
        )
      )
    },
    # If the tunnel drops, every worker loses the dispatcher and exits, so
    # reopening it and letting subsequent workers use the new port is the only
    # way to recover without restarting the controller.
    tunnel_check = function() {
      private$tunnel_drain()
      if (is.null(private$.tunnel) || !private$.tunnel$is_alive()) {
        message(
          "crew.ssh: the ssh tunnel to ", private$.ssh_host,
          " is down. Reopening it. Previous ssh output:\n",
          paste(private$.tunnel_output, collapse = "\n")
        )
        # Every worker launched so far lost its connection with the tunnel.
        private$.disconnected <- union(
          private$.disconnected,
          names(private$.handles)
        )
        private$retry(
          what = paste("reopen the ssh tunnel to", private$.ssh_host),
          attempt = "tunnel_attempt"
        )
      }
      invisible()
    },
    tunnel_stop = function() {
      if (!is.null(private$.tunnel)) {
        private$tunnel_drain()
        private$.tunnel$kill()
        private$.tunnel <- NULL
      }
      private$.tunnel_port <- NULL
      invisible()
    },
    # Run `attempt`, a private method that makes one try and returns a list
    # with either the `value` it got or the `failure` it met, until a try
    # succeeds or the pauses between tries add up to seconds_reconnect. Only
    # the pauses count, so the time this machine spends asleep does not use up
    # the allowance, and a session that wakes before the network does keeps
    # going once it is back. The arguments in `...` go to `attempt`.
    retry = function(what, attempt, ...) {
      waited <- 0
      tries <- 0L
      repeat {
        tries <- tries + 1L
        outcome <- private[[attempt]](...)
        if (is.null(outcome$failure)) {
          if (tries > 1L) {
            message(
              "crew.ssh: managed to ", what, " at attempt ", tries, "."
            )
          }
          return(outcome$value)
        }
        failure <- gsub("\\s+", " ", trimws(outcome$failure))
        remaining <- private$.seconds_reconnect - waited
        crew::crew_assert(
          remaining > 0,
          message = paste0(
            "crew.ssh could not ", what, " in ", tries, " attempt(s) over ",
            round(waited), " seconds of retrying (seconds_reconnect = ",
            private$.seconds_reconnect, "). Last failure: ", failure
          )
        )
        pause <- reconnect_pause(
          tries = tries,
          remaining = remaining,
          first = private$.reconnect_pauses[["first"]],
          longest = private$.reconnect_pauses[["longest"]],
          jitter = as.numeric(Sys.time()) %% 1
        )
        message(
          "crew.ssh: could not ", what, " (", failure, "). Trying again in ",
          round(pause, 1L), " seconds."
        )
        Sys.sleep(pause)
        waited <- waited + pause
      }
    },
    # The sessions this machine started whose R process has ended, read
    # before a tunnel opens so its command can treat them as gone. A read that
    # fails, as when the network is still down, finds none, and the next
    # tunnel start reads again.
    dead_sessions = function() {
      out <- private$run(
        command = remote_sessions_script(private$.remote_directory),
        timeout = min(private$.seconds_timeout, private$.unreachable_seconds)
      )
      if (!identical(out$status, 0L)) {
        return(character(0L))
      }
      remote_dead_sessions(out$stdout, current = private$.session)
    },
    # remote_workers_frame() of `text`, with a session started on this
    # machine whose R process has ended counted as gone, whatever its
    # tunnel's remote end says.
    workers_frame = function(text) {
      workers <- remote_workers_frame(
        text = text,
        host = private$.ssh_host,
        remote_directory = private$.remote_directory,
        session = private$.session
      )
      local <- local_session_alive(
        local_host = workers$local_host,
        local_pid = workers$local_pid,
        local_created = workers$local_created
      )
      workers$session_alive <- workers$session_alive & !(local %in% FALSE)
      workers
    },
    tunnel_attempt = function() {
      error <- tryCatch(private$tunnel_start(), error = identity)
      if (inherits(error, "error")) {
        private$tunnel_stop()
        return(list(failure = conditionMessage(error)))
      }
      list(value = NULL)
    },
    # One launch. The worker code travels over the ssh session's stdin instead
    # of the command line so that no amount of quoting in the deparsed call
    # can confuse the remote shell. The variable tells the worker its name,
    # which the salvage records use, and marks it as a crew.ssh worker.
    #
    # ssh exits with status 255 when ssh itself fails, as when the network is
    # down, which is worth another try, as is a launch that timed out. Any
    # other failure is the remote command's, and an error.
    launch_attempt = function(call, name, command, local_script) {
      private$tunnel_check()
      writeLines(
        c(
          sprintf("Sys.setenv(CREW_SSH_WORKER = %s)", deparse(name)),
          private$current_url(call)
        ),
        con = local_script
      )
      out <- private$run(
        command = command,
        stdin = local_script,
        timeout = private$.seconds_launch
      )
      pid <- trimws(out$stdout)
      if (identical(out$status, 0L) && grepl("^[0-9]+$", pid)) {
        return(list(value = pid))
      }
      failure <- paste0(
        "ssh exited with status ", out$status,
        if (isTRUE(out$timeout)) " (timed out)" else "",
        ".\nstdout: ", trimws(out$stdout),
        "\nstderr: ", trimws(out$stderr)
      )
      if (identical(out$status, 255L) || isTRUE(out$timeout)) {
        return(list(failure = failure))
      }
      crew::crew_assert(
        FALSE,
        message = paste0(
          "Could not launch a crew worker on ", private$.ssh_host, ". ",
          failure
        )
      )
    },
    # The number of tasks running on workers this controller cannot reach, as
    # remote_unreachable_tasks() finds them, that count against `workers` (see
    # unreachable_count()). A task pushed again with a different key is
    # stopped instead (see unreachable_obsolete()). The remote records, and
    # with them the keys of the saved results, are read only when crew might
    # launch a worker, with more tasks than connected workers, or while a
    # task pushed again under the same key has not been seen taken over, and
    # at most every .unreachable_seconds. A read that fails, as when the
    # network is down, leaves the last count standing.
    unreachable_busy = function(status) {
      if (!isTRUE(private$.salvage) || is.null(private$.session)) {
        return(0L)
      }
      tasks <- .subset2(status, "awaiting") + .subset2(status, "executing")
      due <- as.numeric(Sys.time()) - private$.unreachable_checked >=
        private$.unreachable_seconds
      wanted <- tasks > .subset2(status, "connections") ||
        private$unreachable_pending()
      if (due && wanted) {
        private$.unreachable_checked <- as.numeric(Sys.time())
        out <- private$run(
          command = remote_workers_script(
            private$.remote_directory,
            states = c("running", "waiting"),
            saved = private$salvage_root()
          ),
          timeout = min(private$.seconds_timeout, private$.unreachable_seconds)
        )
        if (identical(out$status, 0L)) {
          parts <- remote_workers_split(out$stdout)
          private$.salvage_saved <- parts$saved
          private$unreachable_update(
            runs = remote_unreachable_tasks(
              workers = private$workers_frame(parts$workers),
              disconnected = private$.disconnected
            ),
            tasks = tasks
          )
        }
      }
      private$unreachable_obsolete()
      private$unreachable_count(tasks)
    },
    # How many of the unreachable runs count against `workers`, with `tasks`
    # the number of tasks this session has yet to finish. A run counts until
    # a reachable worker of this session takes its task over, waiting for it
    # or handing back its saved result, and that worker then holds the slot.
    # A run whose task this session has pushed again, under the same key,
    # gives its slot to that push only when every task this session has yet
    # to finish is such a push: the next worker crew launches then takes one
    # of them and waits instead of starting other work. While other tasks
    # wait too, mirai may give that worker one of those, and the old run goes
    # on all the same, so it counts until its own push is taken.
    unreachable_count = function(tasks) {
      runs <- private$.unreachable
      if (is.null(runs) || !nrow(runs)) {
        return(0L)
      }
      counted <- !runs$claimed
      repushed <- private$unreachable_repushed()
      if (tasks > 0L && tasks == sum(repushed)) {
        counted <- counted & !repushed
      }
      sum(counted)
    },
    # Which unreachable runs this session has pushed again under the same key.
    unreachable_repushed = function() {
      runs <- private$.unreachable
      pushed <- vapply(
        runs$task_name,
        pushed_key,
        character(1L),
        keys = private$.salvage_keys,
        USE.NAMES = FALSE
      )
      !is.na(pushed) & pushed == runs$task_key
    },
    # Whether this session has pushed a task again, under the same key, that
    # no worker of it has been seen to take over: a worker may have taken it
    # since, which only the remote records tell.
    unreachable_pending = function() {
      runs <- private$.unreachable
      if (is.null(runs) || !nrow(runs)) {
        return(FALSE)
      }
      any(!runs$claimed & private$unreachable_repushed())
    },
    # Stop the unreachable workers that run an earlier version of a task this
    # controller has pushed: the same name under a different key computed the
    # same way. A worker of another session is unreachable only once that
    # session is gone, so this never stops the work of a live session. A task
    # whose key was computed another way is left to finish, since its key
    # says nothing about whether the task changed. A stop that fails is tried
    # again after .unreachable_seconds.
    unreachable_obsolete = function() {
      tasks <- private$.unreachable
      if (!nrow(tasks)) {
        return(invisible())
      }
      pushed <- vapply(
        tasks$task_name,
        pushed_key,
        character(1L),
        keys = private$.salvage_keys,
        USE.NAMES = FALSE
      )
      obsolete <- !is.na(pushed) &
        pushed != tasks$task_key &
        tasks$task_scheme %in% salvage_scheme
      due <- as.numeric(Sys.time()) - private$.obsolete_tried >=
        private$.unreachable_seconds
      if (!any(obsolete) || !due) {
        return(invisible())
      }
      private$.obsolete_tried <- as.numeric(Sys.time())
      out <- private$run(
        command = remote_stop_obsolete_script(
          tasks = tasks[obsolete, , drop = FALSE],
          salvage_directory = private$salvage_root()
        ),
        timeout = private$.seconds_timeout
      )
      if (!identical(out$status, 0L)) {
        return(invisible())
      }
      private$.unreachable <- tasks[!obsolete, , drop = FALSE]
      stopped <- remote_stopped_obsolete(out$stdout)
      if (length(stopped)) {
        message(
          "crew.ssh: stopped ", paste(stopped, collapse = ", "), " on ",
          private$.ssh_host, ": each ran an earlier version of a task that ",
          "this controller has pushed again with different inputs, so ",
          "nothing would collect its result."
        )
      }
      invisible()
    },
    unreachable_update = function(runs, tasks) {
      same <- identical(runs$task_key, private$.unreachable$task_key)
      private$.unreachable <- runs
      if (same) {
        return(invisible())
      }
      if (nrow(runs)) {
        message(
          "crew.ssh: ", nrow(runs), " task(s) are still running on workers ",
          "of ", private$.ssh_host, " that this controller cannot reach (",
          paste(runs$task_name, collapse = ", "), "). Until each finishes or ",
          "a worker of this controller takes its task over, this controller ",
          "launches at most ",
          max(0L, private$.workers - private$unreachable_count(tasks)),
          " of its ", private$.workers, " worker(s)."
        )
      } else {
        message(
          "crew.ssh: no tasks are running on unreachable workers of ",
          private$.ssh_host, " any more, so this controller launches up to ",
          private$.workers, " worker(s) again."
        )
      }
      invisible()
    },
    # crew builds `call` once per launch() with the URL of the tunnel at the
    # time, and a reopened tunnel listens on a new port. crew passes `call` as
    # a promise that launch_attempt() forces after its tunnel check, so only
    # a later try, or a later worker of the same launch(), can hold an old
    # port, which this replaces with the port of the tunnel open now.
    current_url = function(call) {
      if (is.null(private$.tunnel_port)) {
        return(call)
      }
      sub(
        "url = \"[A-Za-z+]+://127\\.0\\.0\\.1:[0-9]+\"",
        paste0("url = \"", private$worker_url(), "\""),
        call
      )
    },
    # Stop this session's workers, or with keep_running = TRUE, those not in
    # the middle of a task.
    workers_stop = function(keep_running = FALSE) {
      handles <- private$.handles
      if (!length(handles)) {
        return(invisible())
      }
      names <- names(handles)
      paths <- private$worker_paths(names)
      command <- remote_stop_script(
        pids = vapply(handles, "[[", character(1L), "pid"),
        sessions = rep(private$.session, length(names)),
        cid_files = paths$cid,
        runtimes = rep(private$runtime_name(), length(names)),
        task_files = if (keep_running) paths$task else NULL,
        ended = if (keep_running) {
          paste0(private$.session_directory, "/ended")
        } else {
          NULL
        }
      )
      out <- try(
        private$run(command = command, timeout = private$.seconds_timeout),
        silent = TRUE
      )
      private$.handles <- list()
      kept <- if (inherits(out, "try-error")) {
        character(0L)
      } else {
        remote_kept_workers(out$stdout)
      }
      if (length(kept)) {
        message(
          "crew.ssh: left ", paste(kept, collapse = ", "), " running on ",
          private$.ssh_host, " to finish and save its task. A later session ",
          "reuses each result, or stops a run whose task has changed by ",
          "then; terminate_orphans() from a later session stops them now."
        )
      }
      invisible()
    },
    salvage_root = function() {
      if (is.null(private$.salvage_directory)) {
        paste0(private$.remote_directory, "/crew-ssh-salvage")
      } else {
        private$.salvage_directory
      }
    },
    salvage_path = function(keys) {
      paste0(private$salvage_root(), "/", keys, ".rds")
    },
    # Delete collected results in batches, each with one ssh call in the
    # background, so collecting a task never waits on the network. With
    # wait = TRUE, as when the launcher terminates, it deletes everything
    # still pending before it returns.
    salvage_flush = function(wait = FALSE) {
      repeat {
        if (!private$salvage_reap(wait)) {
          return(invisible())
        }
        paths <- private$.salvage_pending
        if (!length(paths)) {
          return(invisible())
        }
        # Batches stay well within the length of a command line, which
        # Windows limits to 32767 characters.
        batch <- utils::head(paths, 100L)
        private$.salvage_pending <- paths[-seq_along(batch)]
        private$.salvage_deleting <- batch
        private$.salvage_cleanup <- processx::process$new(
          command = private$.ssh_command,
          args = c(
            private$ssh_args(),
            private$.ssh_host,
            paste(
              "rm -f --",
              paste(shQuote(batch, type = "sh"), collapse = " ")
            )
          ),
          stdout = NULL,
          stderr = NULL,
          cleanup = TRUE
        )
        if (!wait) {
          return(invisible())
        }
      }
    },
    # Collect the background deletion if it finished. Returns FALSE if it is
    # still running and the caller should not wait for it.
    salvage_reap = function(wait) {
      process <- private$.salvage_cleanup
      if (is.null(process)) {
        return(TRUE)
      }
      if (wait) {
        process$wait(timeout = private$.seconds_timeout * 1000)
      }
      if (process$is_alive()) {
        if (!wait) {
          return(FALSE)
        }
        process$kill()
      }
      status <- process$get_exit_status()
      if (!identical(status, 0L)) {
        warning(
          "crew.ssh could not delete ", length(private$.salvage_deleting),
          " collected result(s) from ", private$salvage_root(), " on ",
          private$.ssh_host, " (ssh exit status ", format(status), "). ",
          "They stay there until salvage_remove() deletes them; a saved ",
          "result only stands in for the task that saved it.",
          call. = FALSE
        )
      }
      private$.salvage_cleanup <- NULL
      private$.salvage_deleting <- character(0L)
      TRUE
    }
  )
)

# Controller ----

# crew's controller is a non-portable R6 class: every method, inherited ones
# included, gets the enclosing environment of the class that is instantiated.
# The inherited methods call crew's internal functions, so that environment
# has to descend from crew's namespace, which is what parent_env is for. The
# methods below call nothing from crew.ssh except through the launcher.
crew_class_controller_ssh <- R6::R6Class(
  classname = "crew_class_controller_ssh",
  inherit = crew::crew_class_controller,
  portable = FALSE,
  cloneable = FALSE,
  parent_env = asNamespace("crew"),
  public = list(
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
      # Salvage needs the launcher's session, which starts with the
      # controller.
      start()
      task <- .subset2(.launcher, "salvage_task")(
        command = command,
        data = data,
        globals = globals,
        seed = seed,
        algorithm = algorithm,
        packages = packages,
        library = library,
        name = name
      )
      super$push(
        command = .subset2(task, "command"),
        data = .subset2(task, "data"),
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
        save_command = save_command,
        controller = controller
      )
    },
    # The tasks of the backlog that an earlier session has saved or still
    # runs go out first, ahead of the backlog's order and even when the
    # controller is saturated (see the launcher's salvage_backlog()). The
    # backlog keeps the rest in their order.
    pop_backlog = function(controllers = NULL) {
      backlog <- .subset2(.queue_backlog, "as_list")()
      names <- as.character(unlist(backlog, use.names = FALSE))
      first <- .subset2(.launcher, "salvage_backlog")(names)
      if (length(first)) {
        .subset2(.queue_backlog, "clear")()
        for (name in names[!(names %in% first)]) {
          .subset2(.queue_backlog, "push")(name)
        }
      }
      c(first, super$pop_backlog(controllers = controllers))
    },
    saturated = function(collect = NULL, throttle = NULL, controller = NULL) {
      if (.subset2(.launcher, "salvage_passing")()) {
        return(FALSE)
      }
      super$saturated(
        collect = collect,
        throttle = throttle,
        controller = controller
      )
    }
  ),
  private = list(
    # pop(), collect(), and map() each call this once for every task they
    # hand back, which makes it the one place that sees every collection.
    .scan_crash = function(name, task) {
      .subset2(.launcher, "salvage_collect")(
        name = name,
        status = .subset2(task, "status"),
        error = .subset2(task, "error")
      )
      super$.scan_crash(name = name, task = task)
    }
  )
)

#' @title Create a controller with an `ssh` launcher.
#' @export
#' @family plugin_ssh
#' @description Create an `R6` object to submit tasks and launch workers on a
#'   remote system over `ssh`.
#' @details Remote workers connect back to the local `mirai` dispatcher through
#'   a reverse `ssh` tunnel, so the local machine does not need to be reachable
#'   from the remote system. The dispatcher listens on the loopback interface
#'   by default and only the `ssh` connection leaves the machine.
#'
#'   The remote system needs R with the `crew` package installed, and the
#'   `mirai` and `nanonext` versions on both ends should match. Any package a
#'   task uses must be installed on the remote system too.
#' @section Use with `targets`:
#'   Set `tar_option_set(storage = "main", retrieval = "main")` unless the
#'   remote system mounts the same filesystem as the local one. The `targets`
#'   default of `storage = "worker"` has each worker read and write
#'   `_targets/objects/` itself, which a worker on another machine cannot do:
#'   the local process waits for files that never appear and the pipeline fails
#'   with "File sync timed out". With `"main"`, target data travels over the
#'   `ssh` tunnel along with the task. If the remote system does mount the
#'   project directory, pass `directory` instead so workers start there and the
#'   `targets` defaults keep working.
#' @section Running workers in a container:
#'   `launch_prefix` runs each worker under another command, which is how a
#'   worker runs inside a container instead of directly on the remote system:
#'
#'   ```r
#'   crew_controller_ssh(
#'     ssh_host = "user@example.com",
#'     launch_prefix = c(
#'       "docker", "run", "--init", "--rm", "--network=host",
#'       "--volume", "/tmp:/tmp", "my-image"
#'     )
#'   )
#'   ```
#'
#'   Three details of the surrounding design decide what the prefix has to
#'   contain.
#'
#'   The reverse `ssh` tunnel listens on the *remote system's* loopback
#'   interface, and a container on a bridge network has a loopback of its own,
#'   so a worker there would dial a port nothing is listening on. The
#'   controller would then report a worker that launched but never connected.
#'   The container must share the remote system's network namespace:
#'   `--network=host` for `docker` and `podman`, which is already the default
#'   for `apptainer`.
#'
#'   The launcher writes each worker's R script into `remote_directory` on the
#'   remote system and the worker reads it back by that same path, so
#'   `remote_directory` must be visible inside the container at the path it has
#'   outside. Worker logs are written by the remote shell rather than by the
#'   worker itself, so `logs()` keeps working whatever the prefix does. With
#'   `salvage = TRUE` the worker also writes its results and records there.
#'
#'   Without `--init`, R is the first process of the container, and Linux
#'   does not deliver a signal to that process unless it installed a handler
#'   for it, which R does not for `SIGTERM`. A worker then ignores the
#'   signal `mirai` sends it when its connection drops, keeps computing a
#'   result nothing can collect, and outlives the session that started it.
#'   `--init` makes a small init process the first process instead, which
#'   passes the signal on. Use it (or an image whose entry point is an init
#'   process such as `tini`).
#'
#'   When the prefix starts with `docker run` or `podman run`, the launcher
#'   adds `--cidfile` to record each container's ID and stops a worker by
#'   killing its container through the runtime. The process ID the launch
#'   reports belongs to the runtime's client, and stopping the client alone
#'   leaves the container running.
#' @section Surviving a disconnect:
#'   Without salvage, remote workers exit as soon as their connection to the
#'   controller drops, so a task in progress is lost when the local session
#'   ends or the network goes down, and runs again from the start next time.
#'   With `salvage = TRUE`, the default:
#'
#'   1. A worker whose connection drops finishes the task in progress before
#'      it exits.
#'   2. Every worker saves the result of each task in `salvage_directory` on
#'      the remote system before handing it back, writing to a temporary file
#'      and renaming it, so a reader never sees a partial file. The record
#'      holds the value or the error, the task's key, the worker, the remote
#'      host, start and end times, and the versions of R and of every loaded
#'      package.
#'   3. When a later push of the same task, from this session or a later one,
#'      finds a saved result, the worker hands that back instead of running
#'      the task. It does so only for a task that succeeded and ran under the
#'      same versions of R and of every package the worker has installed. An
#'      error is never reused, and a saved file that cannot be read fails the
#'      task with an error that names the file.
#'   4. When the same task is still running on a worker of an earlier session,
#'      the new worker attaches to it: it waits for that worker's result
#'      instead of starting the task a second time, and hands the result back
#'      when it appears. A running worker proves it is alive by touching a
#'      heartbeat file every 15 seconds, even while R is busy. If a task's
#'      worker dies without finishing, because it was killed, its container
#'      was stopped, or the machine restarted, the heartbeat stops, and after
#'      two minutes the new worker takes over and runs the task itself. While
#'      this goes on, `remote_workers()` shows the old worker as `"running"`
#'      and the new one as `"waiting"`. A waiting worker whose own session then
#'      loses its connection can no longer hand the result back, so the next
#'      session to open a tunnel to the same `remote_directory`, or the same
#'      session when it reconnects, stops it with a message. The worker it
#'      waited for still saves the result.
#'   5. A task still running on a worker the controller cannot reach, one of
#'      a session that is gone or one of this session from before its tunnel
#'      was reopened, takes up the remote system as much as a connected
#'      worker does, so it counts against `workers` until it finishes: the
#'      controller launches only as many workers as are left, and says so.
#'      Once a worker of this controller takes the same task over, waiting
#'      for the running one or handing back its saved result, that worker
#'      holds the slot instead. When pushes of such tasks are all the work
#'      this controller has waiting, the slot goes to them at once, since the
#'      next worker launched can only take one of them; while other tasks
#'      wait too, that worker might take one of those, so the old run counts
#'      until its own push is taken. The controller reads the remote records
#'      for this at most every 30 seconds.
#'   6. When the controller pushes a task under the name of a task that such
#'      a worker runs, but with a different key, the task has changed since
#'      that run began: its command, the data it depends on, or a global it
#'      uses. Nothing will collect the old run's result, so the controller
#'      stops that worker, says so, and frees its slot. A run whose key was
#'      computed by a crew.ssh from before keys recorded how they were
#'      computed is left to finish.
#'   7. Tasks an earlier session has saved or still runs go first. When
#'      `targets` holds tasks back because the controller is saturated, the
#'      controller pushes those ahead of the rest, even while saturated, so a
#'      saved result comes back, a run in progress is waited for, and a
#'      changed one is stopped without waiting for a free slot. Which tasks
#'      these are comes from the same remote records.
#'   8. Terminating the controller, which `targets` does when a pipeline ends,
#'      is interrupted, or stops on an error, stops the idle workers but
#'      leaves a worker in the middle of a task to finish and save it, as
#'      after a disconnect. The next session then reuses, waits for, or stops
#'      each of these runs by the rules above. `terminate_orphans()`, called
#'      from a later session, stops them all at once.
#'
#'   A session is gone when the remote end of its tunnel has stopped, or,
#'   for a session started on the same machine, when the R process that
#'   started it has ended. The second matters after a laptop drops off the
#'   network: the remote `sshd` can keep the tunnel's remote end for hours,
#'   which would otherwise make the session look connected. Each session
#'   records its machine, process ID, and process creation time, so a process
#'   ID the system has since given to another process does not count. When a
#'   tunnel opens, the controller also closes the lingering remote ends of the
#'   sessions it knows have ended, and stops their waiting workers.
#'
#'   Whether or not salvage is on, a failure of the network while the
#'   controller reopens its tunnel or launches a worker is retried, with
#'   pauses that double from about 5 seconds to at most 5 minutes, each
#'   varied by up to 20% so that sessions cut off together do not all retry
#'   at once, until the pauses add up to `seconds_reconnect`; only then is it
#'   an error. Time the local machine spends asleep does not count, so a
#'   laptop that wakes before its network does carries on once the network is
#'   back.
#'
#'   A task is identified by its name and by everything it computes from: the
#'   command, data, globals, seed, and packages, or for a `targets` target,
#'   its command, seed, settings, the hashes of the data it depends on, and
#'   the globals it uses, directly or through the global functions it calls,
#'   found the way `targets` finds them. A change to any other global, such as
#'   an unrelated function in `_targets.R`, leaves the key as it was, just as
#'   it leaves the target up to date for `targets`; so does a change to the
#'   pipeline's options, such as its controller. A task pushed without a name,
#'   as `push()` allows, runs without salvage. Salvage assumes a task gives
#'   the same result each time it runs with the same inputs, which `targets`
#'   arranges by seeding every target.
#'
#'   A saved result is deleted once a controller collects it with `pop()`,
#'   `collect()`, or `map()`. Results nothing collects stay until the
#'   launcher's `salvage_remove()` deletes them; `salvage_list()` lists them.
#'   The launcher's `remote_workers()` lists the workers of every session with
#'   the task each is on, and `terminate_orphans()` stops the workers whose
#'   session is gone.
#'
#'   Salvage writes every result to the remote disk until it is collected,
#'   so it suits tasks that are slow to compute more than tasks that produce
#'   a lot of data; `salvage = FALSE` turns it off. It cannot be combined
#'   with custom `serialization`, which the saved files would not apply, so
#'   a controller with `serialization` needs `salvage = FALSE`.
#' @return An `R6` controller object with an `ssh` launcher.
#' @param ssh_host Character of length 1, the `ssh` destination of the remote
#'   system, e.g. `"user@example.com"`. Anything the `ssh` command accepts
#'   works here, including a `Host` alias from `~/.ssh/config`.
#' @param ssh_port Positive integer of length 1 or `NULL`, the port of the
#'   remote `ssh` server (`ssh -p`). `NULL` uses the `ssh` default.
#' @param ssh_keyfile Character of length 1 or `NULL`, path to the private key
#'   file to authenticate with (`ssh -i`). `NULL` uses the keys `ssh` would
#'   choose on its own.
#' @param ssh_options Character vector of additional arguments for the `ssh`
#'   command, e.g. `c("-o", "ProxyJump=bastion")`. Options given here take
#'   precedence over the ones the launcher sets.
#' @param ssh_command Character of length 1, the `ssh` executable to run.
#' @param rscript Character of length 1, path to `Rscript` on the remote
#'   system.
#' @param launch_prefix Character vector, a command and its arguments to run
#'   the worker under, e.g.
#'   `c("docker", "run", "--init", "--rm", "--network=host", "my-image")`.
#'   Empty by default, which runs `rscript` directly. See the "Running
#'   workers in a container" section.
#' @param directory Character of length 1 or `NULL`, working directory of the
#'   workers on the remote system. `NULL` means the directory `ssh` lands in,
#'   usually the remote home directory. This is the directory the command named
#'   by `launch_prefix` starts in, which for a container runtime is not the
#'   same as the working directory inside the container.
#' @param remote_directory Character of length 1, absolute path to a directory
#'   on the remote system where the launcher may write worker scripts, logs,
#'   and records.
#' @param salvage Logical of length 1, whether workers save the result of
#'   each task on the remote system so that it survives a disconnect. `TRUE`
#'   by default; `FALSE` makes workers exit as soon as their connection drops
#'   and saves nothing. See the "Surviving a disconnect" section.
#' @param salvage_directory Character of length 1 or `NULL`, absolute path to
#'   the directory on the remote system where workers save results, shared by
#'   every session. `NULL` means the `crew-ssh-salvage` subdirectory of
#'   `remote_directory`. `/tmp`, the default `remote_directory`, may be
#'   cleared when the remote system restarts, so choose a durable directory
#'   for results that must outlast that.
#' @param seconds_reconnect Non-negative number of length 1, how long to keep
#'   trying when the network fails while reopening the tunnel or launching a
#'   worker, counted in seconds of pauses between attempts. `0` gives up at
#'   the first failure and `Inf` never does. See the "Surviving a disconnect"
#'   section.
#' @param verbose Logical of length 1, whether to print messages about the
#'   tunnel and each worker launch.
#' @param host Character of length 1, local address of the `mirai` dispatcher.
#'   The default binds to the loopback interface, which is all the reverse
#'   `ssh` tunnel needs.
#' @inheritParams crew::crew_controller_local
#' @examples
#' if (identical(Sys.getenv("CREW_SSH_EXAMPLES"), "true")) {
#' controller <- crew_controller_ssh(ssh_host = "user@example.com")
#' controller$start()
#' controller$push(Sys.info()[["nodename"]])
#' controller$wait()
#' controller$pop()
#' controller$terminate()
#' }
crew_controller_ssh <- function(
  ssh_host,
  ssh_port = NULL,
  ssh_keyfile = NULL,
  ssh_options = character(0L),
  ssh_command = "ssh",
  rscript = "Rscript",
  launch_prefix = character(0L),
  directory = NULL,
  remote_directory = "/tmp",
  salvage = TRUE,
  salvage_directory = NULL,
  seconds_reconnect = 3600,
  verbose = FALSE,
  name = NULL,
  workers = 1L,
  host = "127.0.0.1",
  port = NULL,
  tls = crew::crew_tls(),
  serialization = NULL,
  profile = crew::crew_random_name(),
  seconds_interval = 0.25,
  seconds_timeout = 60,
  seconds_launch = 30,
  seconds_idle = 300,
  seconds_wall = Inf,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  r_arguments = c("--no-save", "--no-restore"),
  options_metrics = crew::crew_options_metrics(),
  crashes_max = 5L,
  backup = NULL
) {
  crew::crew_assert(
    !(isTRUE(salvage) && !is.null(serialization)),
    message = paste(
      "salvage = TRUE (the default) cannot be combined with custom",
      "serialization: workers save results with saveRDS(), which would not",
      "apply it, and a value read back would be broken. Set salvage = FALSE",
      "to use serialization."
    )
  )
  client <- crew::crew_client(
    host = host,
    port = port,
    tls = tls,
    serialization = serialization,
    profile = profile,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  launcher <- crew_class_launcher_ssh$new(
    ssh_host = ssh_host,
    ssh_port = ssh_port,
    ssh_keyfile = ssh_keyfile,
    ssh_options = ssh_options,
    ssh_command = ssh_command,
    rscript = rscript,
    launch_prefix = launch_prefix,
    directory = directory,
    remote_directory = remote_directory,
    salvage = salvage,
    salvage_directory = salvage_directory,
    seconds_reconnect = seconds_reconnect,
    verbose = verbose,
    name = name,
    workers = workers,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    tls = tls,
    r_arguments = r_arguments,
    options_metrics = options_metrics
  )
  controller <- crew_class_controller_ssh$new(
    client = client,
    launcher = launcher,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection,
    crashes_max = crashes_max,
    backup = backup
  )
  controller$validate()
  controller
}

# Utilities ----

# The pause before the next try of retry(), after `tries` tries, in seconds:
# `first` doubled for every try after the first, up to `longest`, then spread
# over 80% to 120% of that by `jitter`, a number in [0, 1), so that sessions
# that lost the network together do not all retry at once. It never passes
# `longest`, or what is `remaining` of seconds_reconnect. retry() takes the
# jitter from the clock rather than from R's random number generator, which
# would move the session's random stream.
reconnect_pause <- function(tries, remaining, first, longest, jitter) {
  pause <- min(longest, first * 2^(tries - 1L)) * (0.8 + 0.4 * jitter)
  min(remaining, longest, pause)
}

# The names in a backlog, `names`, that this session has not pushed (`keys`,
# as in pushed_key()) and that have a run going on a worker of an earlier
# session (`running`, task names) or a saved result (`saved`, keys). A saved
# result counts by the prefix of its key, so it may be from an earlier version
# of the task, which then runs again; pushing it first costs nothing either
# way.
backlog_first <- function(names, running, saved, keys) {
  prefixes <- paste0(
    vapply(names, salvage_prefix, character(1L), USE.NAMES = FALSE),
    "-"
  )
  has_saved <- vapply(
    prefixes,
    prefix_found,
    logical(1L),
    keys = saved,
    USE.NAMES = FALSE
  )
  pushed <- !is.na(vapply(
    names,
    pushed_key,
    character(1L),
    keys = keys,
    USE.NAMES = FALSE
  ))
  names[(names %in% running | has_saved) & !pushed]
}

prefix_found <- function(prefix, keys) {
  any(startsWith(keys, prefix))
}

# The salvage key under which this session pushed the task called `name` and
# has not collected it yet, or NA.
pushed_key <- function(name, keys) {
  if (is.null(keys) || is.na(name) || !nzchar(name)) {
    return(NA_character_)
  }
  get0(name, envir = keys, inherits = FALSE, ifnotfound = NA_character_)
}

# Run one command on the remote system and hand the whole result back. The
# caller decides what a failure means, so nothing is thrown here.
ssh_run <- function(ssh_command, args, stdin = NULL, timeout) {
  processx::run(
    command = ssh_command,
    args = args,
    stdin = stdin,
    timeout = timeout,
    error_on_status = FALSE
  )
}
