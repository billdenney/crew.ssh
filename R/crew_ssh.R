# Launcher ----

#' Create an ssh job launcher object
#'
#' @export
ssh_launcher_class <- R6::R6Class(
  classname = "ssh_launcher_class",
  inherit = crew::crew_class_launcher,
  public = list(
    #' @field ssh_host Passed to [ssh::ssh_connect()]
    ssh_host = NA_character_,
    #' @field ssh_keyfile Passed to [ssh::ssh_connect()]
    ssh_keyfile = NULL,
    #' @field ssh_passwd Passed to [ssh::ssh_connect()]
    ssh_passwd = "",
    #' @field ssh_verbose Passed to [ssh::ssh_connect()]
    ssh_verbose = FALSE,

    #' @param ... passed to `crew::crew_class_launcher$initialize()`
    #' @param ssh_host,ssh_keyfile,ssh_passwd,ssh_verbose Passed to
    #'   [ssh::ssh_connect()]
    initialize = function(..., ssh_host, ssh_keyfile = NULL, ssh_passwd = "", ssh_verbose = FALSE) {
      super$initialize(...)
      self$ssh_host <- ssh_host
      self$ssh_keyfile <- ssh_keyfile
      self$ssh_passwd <- ssh_passwd
      self$ssh_verbose <- ssh_verbose
    },
    #' @param call,name,launcher,worker,instance As used with
    #'   `crew::crew_class_launcher$launch_worker()`
    launch_worker = function(call) {
      if (self$ssh_verbose) {
        message("Connecting to: ", self$ssh_host)
      }
      private$session <-
        ssh::ssh_connect(
          host = self$ssh_host,
          keyfile = self$ssh_keyfile,
          passwd = self$ssh_passwd,
          verbose = self$ssh_verbose
        )
      # Dameonize the R session
      # TODO: Put the pidfile in a better location
      pidfile_path <- "/tmp/rcrew.pid"
      command_to_run <-
        sprintf(
          "start-stop-daemon --start --background --make-pidfile --pidfile %s --user $(whoami) --name $(whoami) --startas /usr/local/bin/R -- -e %s &",
          pidfile_path,
          shQuote(call)
        )
      if (self$ssh_verbose) {
        message("Running: ", command_to_run)
      }
      daemon_start <-
        ssh::ssh_exec_internal(
          session = private$session,
          command = command_to_run
        )
      if (length(daemon_start$out) > 0) {
        message("stdout output from starting R daemon:\n", rawToChar(daemon_start$stderr))
      }
      if (length(daemon_start$stderr) > 0) {
        message("stderr output from starting R daemon:\n", rawToChar(daemon_start$stderr))
      }
      # Track the PID to ensure that only the correct job is killed.
      pid_lstart <-
        ssh::ssh_exec_internal(
          session = private$session,
          command = paste("cat", pidfile_path)
        )
      trimws(rawToChar(pid_lstart$stdout))
    },
    #' @param handle As used with `crew::crew_class_launcher$terminate_worker()`
    terminate_worker = function(handle) {
      # Need to clean up the processes on the other end of the connection first,
      # if possible.
      # TODO: Try to kill the process on the server; verify PID and start time, first
      try({
        ssh::ssh_disconnect(private$session)
      }, silent = TRUE)
    }
  ),
  private = list(
    session = NULL
  ),
  cloneable = FALSE
)

# Controller ----

#' Create a controller with the ssh launcher.
#'
#' Create an `R6` object to submit tasks and launch workers.
#'
#' @inheritParams crew::crew_controller_local
#' @inheritParams ssh::ssh_connect
#' @export
crew_controller_ssh <- function(
    name = "ssh controller",
    ssh_host,
    ssh_keyfile = NULL,
    ssh_passwd = "",
    ssh_verbose = FALSE,
    workers = 1L,
    host = NULL,
    port = NULL,
    tls = crew::crew_tls(),
    serialization = NULL,
    profile = crew::crew_random_name(),
    seconds_interval = 0.5,
    seconds_timeout = 30,
    seconds_launch = 30,
    seconds_idle = Inf,
    seconds_wall = Inf,
    tasks_max = Inf,
    tasks_timers = 0L,
    reset_globals = TRUE,
    reset_packages = FALSE,
    reset_options = FALSE,
    garbage_collection = FALSE,
    r_arguments = NULL,
    options_metrics = crew::crew_options_metrics(),
    crashes_max = 5L,
    backup = NULL) {
  client <- crew::crew_client(
    host = host,
    port = port,
    tls = tls,
    serialization = serialization,
    profile = profile,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  launcher <- ssh_launcher_class$new(
    ssh_host = ssh_host,
    ssh_keyfile = ssh_keyfile,
    ssh_passwd = ssh_passwd,
    ssh_verbose = ssh_verbose,
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
