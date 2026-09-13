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
    #' @param directory Character of length 1 or `NULL`, working directory of
    #'   the workers on the remote system. `NULL` means the directory `ssh`
    #'   lands in, usually the remote home directory.
    #' @param remote_directory Character of length 1, absolute path to a
    #'   directory on the remote system where the launcher may write worker
    #'   scripts and logs. Each controller gets its own uniquely named
    #'   subdirectory of `remote_directory`.
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
      directory = NULL,
      remote_directory = "/tmp",
      verbose = FALSE
    ) {
      super$initialize(...)
      private$.ssh_host <- ssh_host
      private$.ssh_port <- ssh_port
      private$.ssh_keyfile <- ssh_keyfile
      private$.ssh_options <- ssh_options
      private$.ssh_command <- ssh_command
      private$.rscript <- rscript
      private$.directory <- directory
      private$.remote_directory <- remote_directory
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
      private$tunnel_start()
      invisible()
    },
    #' @description `mirai` daemon settings for a remote worker.
    #' @details Identical to the settings of the parent class except that the
    #'   URL points at the remote end of the reverse `ssh` tunnel instead of
    #'   the dispatcher's local address.
    #' @return A named list of `mirai` daemon settings.
    settings = function() {
      out <- super$settings()
      if (!is.null(private$.tunnel_port)) {
        out$url <- private$worker_url()
      }
      out
    },
    #' @description Launch a worker on the remote system.
    #' @return A handle: a named list with the worker `name`, the remote
    #'   process ID `pid`, and the remote paths of the worker `script` and
    #'   `log`.
    #' @param call Character of length 1, R code the worker should run.
    launch_worker = function(call) {
      private$tunnel_check()
      name <- paste0("worker-", crew::crew_random_name())
      script <- paste0(private$.session_directory, "/", name, ".R")
      log <- paste0(private$.session_directory, "/", name, ".log")
      # The worker code travels over the ssh session's stdin instead of the
      # command line so that no amount of quoting in the deparsed call can
      # confuse the remote shell.
      local_script <- tempfile(pattern = "crew_ssh_", fileext = ".R")
      on.exit(unlink(local_script), add = TRUE)
      writeLines(call, con = local_script)
      command <- private$worker_command(script = script, log = log)
      if (private$.verbose) {
        message("crew.ssh: launching ", name, ", log at ", log)
      }
      out <- private$run(
        command = command,
        stdin = local_script,
        timeout = private$.seconds_launch
      )
      pid <- trimws(out$stdout)
      if (!identical(out$status, 0L) || !grepl("^[0-9]+$", pid)) {
        crew::crew_assert(
          FALSE,
          message = paste0(
            "Could not launch a crew worker on ", private$.ssh_host, ". ",
            "ssh exited with status ", out$status,
            if (isTRUE(out$timeout)) " (timed out)" else "",
            ".\nstdout: ", trimws(out$stdout),
            "\nstderr: ", trimws(out$stderr)
          )
        )
      }
      handle <- list(name = name, pid = pid, script = script, log = log)
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
    #' @description Terminate the launcher: stop remote workers and close the
    #'   reverse `ssh` tunnel.
    #' @return `NULL` (invisibly).
    terminate = function() {
      private$workers_stop()
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
    .directory = NULL,
    .remote_directory = NULL,
    .verbose = NULL,
    .session = NULL,
    .session_directory = NULL,
    .handles = list(),
    .tunnel = NULL,
    .tunnel_port = NULL,
    .tunnel_output = character(0L),
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
    # Shell command that receives the worker script on stdin, detaches an
    # Rscript process to run it, and reports that process's PID. Everything
    # that varies is quoted for the remote POSIX shell; the worker code itself
    # never reaches the command line, so no amount of quoting in the deparsed
    # call can confuse the shell.
    worker_command = function(script, log) {
      paste0(
        "set -e; ",
        "mkdir -p ", shQuote(private$.session_directory, type = "sh"), "; ",
        "cat > ", shQuote(script, type = "sh"), "; ",
        if (is.null(private$.directory)) {
          ""
        } else {
          paste0("cd ", shQuote(private$.directory, type = "sh"), "; ")
        },
        "nohup ",
        paste(
          shQuote(
            c(private$.rscript, private$.r_arguments, script),
            type = "sh"
          ),
          collapse = " "
        ),
        " > ", shQuote(log, type = "sh"), " 2>&1 < /dev/null & echo $!"
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
    tunnel_start = function() {
      parsed <- private$parse_url(private$.url)
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
        "cat > /dev/null"
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
      while (is.null(private$.tunnel_port)) {
        private$.tunnel$poll_io(250L)
        private$tunnel_drain()
        match <- grep(pattern, private$.tunnel_output, value = TRUE)
        if (length(match)) {
          port <- sub(pattern, "\\1", match[1L])
          crew::crew_assert(
            grepl("^[0-9]+$", port),
            message = paste(
              "crew.ssh could not read the forwarded port out of the ssh",
              "message", shQuote(match[1L])
            )
          )
          private$.tunnel_port <- port
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
      if (private$.verbose) {
        message(
          "crew.ssh: tunnel open, remote workers will dial ",
          private$worker_url()
        )
      }
      invisible()
    },
    tunnel_drain = function() {
      if (is.null(private$.tunnel)) {
        return(invisible())
      }
      new <- c(
        private$.tunnel$read_output_lines(),
        private$.tunnel$read_error_lines()
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
        private$tunnel_start()
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
    workers_stop = function() {
      pids <- vapply(private$.handles, function(x) x$pid, character(1L))
      if (!length(pids)) {
        return(invisible())
      }
      # PIDs get recycled, so kill one only if its command line still shows
      # this controller's session directory.
      command <- sprintf(
        paste0(
          "for pid in %s; do ",
          "case \"$(ps -p $pid -o args= 2>/dev/null)\" in ",
          "*%s*) kill $pid 2>/dev/null ;; ",
          "esac; ",
          "done; exit 0"
        ),
        paste(pids, collapse = " "),
        private$.session
      )
      try(
        private$run(command = command, timeout = private$.seconds_timeout),
        silent = TRUE
      )
      private$.handles <- list()
      invisible()
    }
  )
)

# Controller ----

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
#' @param directory Character of length 1 or `NULL`, working directory of the
#'   workers on the remote system. `NULL` means the directory `ssh` lands in,
#'   usually the remote home directory.
#' @param remote_directory Character of length 1, absolute path to a directory
#'   on the remote system where the launcher may write worker scripts and
#'   logs.
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
  directory = NULL,
  remote_directory = "/tmp",
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
    directory = directory,
    remote_directory = remote_directory,
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
  controller <- crew::crew_controller(
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
