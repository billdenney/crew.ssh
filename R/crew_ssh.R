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
    launch_worker = function(call, name, launcher, worker, instance) {
      private$session <-
        ssh::ssh_connect(
          host = self$ssh_host,
          keyfile = self$ssh_keyfile,
          passwd = self$ssh_passwd,
          verbose = self$ssh_verbose
        )
      # Track the PID and PID start time to ensure that only the correct job is
      # killed.  Solution based on
      # https://stackoverflow.com/questions/5731234/how-to-get-the-start-time-of-a-long-running-linux-process
      ps_start_time <- 'date --date="$(ps -p $! -o lstart=)" "+%s"'
      pid_lstart <-
        ssh::ssh_exec_internal(
          session = private$session,
          command = sprintf("R -e %s & echo $! $(%s)", shQuote(call), ps_start_time)
        )
      setNames(
        strsplit(x = rawToChar(pid_lstart$stdout), split = " ")[[1]],
        c("pid", "start_time")
      )
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
  )
)

#' Create a controller with the ssh launcher.
#'
#' Create an `R6` object to submit tasks and launch workers.
#'
#' @inheritParams crew::crew_controller_local
#' @inheritParams ssh::ssh_connect
#' @export
crew_controller_ssh <- function(
    name = "ssh controller name",
    ssh_host,
    ssh_keyfile = NULL,
    ssh_passwd = "",
    ssh_verbose = FALSE,
    workers = 1L,
    host = NULL,
    port = NULL,
    tls = crew::crew_tls(),
    seconds_interval = 0.5,
    seconds_timeout = 10,
    seconds_launch = 30,
    seconds_idle = Inf,
    seconds_wall = Inf,
    tasks_max = Inf,
    tasks_timers = 0L,
    reset_globals = TRUE,
    reset_packages = FALSE,
    reset_options = FALSE,
    garbage_collection = FALSE,
    launch_max = 5L
) {
  client <- crew::crew_client(
    name = name,
    workers = workers,
    host = host,
    port = port,
    tls = tls,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  launcher <- ssh_launcher_class$new(
    name = name,
    ssh_host = ssh_host,
    ssh_keyfile = ssh_keyfile,
    ssh_passwd = ssh_passwd,
    ssh_verbose = ssh_verbose,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection,
    launch_max = 5L,
    tls = tls
  )
  controller <- crew::crew_controller(client = client, launcher = launcher)
  controller$validate()
  controller
}
