# Fake ssh ----
#
# Tests that need a remote system use this stand-in for ssh, which runs the
# remote command on the local machine. It reports a reverse forward the way
# ssh does, with the dispatcher's own port as the remote one, since remote and
# local are the same machine. Everything else, from the launch command to the
# tunnel's records, runs exactly as it would over ssh.

fake_ssh_lines <- c(
  "#!/bin/sh",
  "forward=\"\"",
  "while [ $# -gt 0 ]; do",
  "  case \"$1\" in",
  "    -o|-p|-i) shift 2 ;;",
  "    -R) forward=\"$2\"; shift 2 ;;",
  "    -*) shift ;;",
  "    *) break ;;",
  "  esac",
  "done",
  "shift",
  "if [ -n \"$forward\" ]; then",
  "  echo \"Allocated port ${forward##*:} for remote forward to ${forward#*:}\" >&2",
  "fi",
  "exec sh -c \"$1\""
)

write_executable <- function(lines) {
  path <- tempfile("crew_ssh_test_")
  writeLines(lines, path)
  Sys.chmod(path, mode = "0755")
  path
}

fake_ssh <- function() {
  write_executable(fake_ssh_lines)
}

skip_without_fake_ssh <- function() {
  testthat::skip_on_cran()
  testthat::skip_on_os("windows")
  testthat::skip_if_not_installed("ps")
}

# A controller whose "remote system" is a fresh directory on this machine.
# Salvage timings are shortened so that tests wait seconds, not minutes.
fake_controller <- function(
  remote_directory,
  ...,
  rscript = file.path(R.home("bin"), "Rscript")
) {
  controller <- crew_controller_ssh(
    ssh_host = "fakehost",
    ssh_command = fake_ssh(),
    remote_directory = remote_directory,
    rscript = rscript,
    seconds_idle = 60,
    ...
  )
  private <- controller$launcher$.__enclos_env__$private
  private$.salvage_seconds <- c(heartbeat = 1, stale = 6, poll = 0.25)
  controller
}

remote_tempdir <- function() {
  path <- tempfile("crew_ssh_remote_")
  dir.create(path)
  normalizePath(path)
}

# Waiting ----

# Evaluate `condition` in the caller's frame until it is TRUE, failing the
# test if it is not within `seconds`.
wait_until <- function(condition, seconds = 60, interval = 0.1) {
  expression <- substitute(condition)
  envir <- parent.frame()
  deadline <- Sys.time() + seconds
  while (!isTRUE(eval(expression, envir = envir))) {
    if (Sys.time() > deadline) {
      testthat::fail(
        paste("timed out waiting for", deparse1(expression))
      )
      return(invisible(FALSE))
    }
    Sys.sleep(interval)
  }
  invisible(TRUE)
}

pid_alive <- function(pid) {
  as.integer(pid) %in% ps::ps_pids()
}

# Other R sessions ----
#
# A test of a disconnect runs the first controller in another R process and
# kills that process, the way a session dies. That process loads crew.ssh as
# it is being tested: the installed package under R CMD check, the source tree
# under devtools::test().

crew_ssh_source <- function() {
  if (
    requireNamespace("pkgload", quietly = TRUE) &&
      pkgload::is_dev_package("crew.ssh")
  ) {
    normalizePath(pkgload::pkg_path(testthat::test_path()))
  } else {
    NULL
  }
}

crew_ssh_load <- function(source) {
  if (is.null(source)) {
    loadNamespace("crew.ssh")
  } else {
    pkgload::load_all(source, quiet = TRUE, attach = FALSE, helpers = FALSE)
  }
  invisible()
}

# Start a session that pushes one task and waits on it until it is killed.
# callr runs it in a fresh session, where the helpers of these tests do not
# exist, so it loads crew.ssh itself.
push_in_session <- function(source, args, seconds, command, data, name) {
  if (is.null(source)) {
    loadNamespace("crew.ssh")
  } else {
    pkgload::load_all(source, quiet = TRUE, attach = FALSE, helpers = FALSE)
  }
  controller <- do.call(crew.ssh::crew_controller_ssh, args)
  private <- controller$launcher$.__enclos_env__$private
  private$.salvage_seconds <- seconds
  controller$start()
  controller$push(
    command = command,
    data = data,
    name = name,
    substitute = FALSE
  )
  controller$wait(mode = "all", seconds_timeout = 600)
}

session_in_background <- function(
  remote_directory,
  command,
  data,
  name,
  launch_prefix = character(0L),
  rscript = file.path(R.home("bin"), "Rscript")
) {
  callr::r_bg(
    push_in_session,
    args = list(
      source = crew_ssh_source(),
      args = list(
        ssh_host = "fakehost",
        ssh_command = fake_ssh(),
        remote_directory = remote_directory,
        rscript = rscript,
        launch_prefix = launch_prefix,
        seconds_idle = 60,
        seconds_launch = 300,
        salvage = TRUE
      ),
      seconds = c(heartbeat = 1, stale = 6, poll = 0.25),
      command = command,
      data = data,
      name = name
    ),
    stdout = file.path(remote_directory, "session.out"),
    stderr = "2>&1"
  )
}

# A task that counts its runs in a file, then sleeps, then answers.
counted_task <- function(seconds) {
  substitute(
    {
      cat("run\n", file = counter, append = TRUE)
      Sys.sleep(seconds)
      6L * 7L
    },
    list(seconds = seconds)
  )
}

# A task that counts its runs, then holds until the file `release` exists, so a
# test decides exactly when it finishes.
held_task <- function() {
  quote({
    cat("run\n", file = counter, append = TRUE)
    while (!file.exists(release)) {
      Sys.sleep(0.2)
    }
    6L * 7L
  })
}

read_task_state <- function(path) {
  unname(read.dcf(path)[1L, "task_state"])
}

# The state of the latest task of every worker under `remote`.
task_states <- function(remote) {
  files <- list.files(remote, "\\.task$", recursive = TRUE, full.names = TRUE)
  out <- vapply(files, read_task_state, character(1L), USE.NAMES = FALSE)
  names(out) <- sub("\\.task$", "", basename(files))
  out
}

count_runs <- function(counter) {
  if (file.exists(counter)) length(readLines(counter)) else 0L
}

# Run `fun` in a fresh R session, with `helpers` defined there. Functions
# travel without the environment of these tests, which would carry the
# crew.ssh namespace along with them.
in_another_session <- function(fun, args = list(), helpers = list()) {
  callr::r(
    call_in_session,
    args = list(
      fun = global_function(fun),
      args = args,
      helpers = lapply(helpers, global_function)
    )
  )
}

call_in_session <- function(fun, args, helpers) {
  list2env(helpers, envir = globalenv())
  do.call(fun, args)
}

global_function <- function(fun) {
  environment(fun) <- globalenv()
  fun
}

# Containers ----
#
# Set CREW_SSH_TEST_DOCKER_IMAGE to an image with R and crew to run workers in
# containers. The tests start their own containers with --rm, so they leave
# nothing behind.

skip_without_docker <- function() {
  image <- Sys.getenv("CREW_SSH_TEST_DOCKER_IMAGE", unset = "")
  testthat::skip_if(
    !nzchar(image),
    "CREW_SSH_TEST_DOCKER_IMAGE is not set, so container tests are skipped."
  )
  skip_without_fake_ssh()
  image
}

# The container shares the remote directory at the same path, and runs as the
# user running the tests, so that the files it writes can be cleaned up.
docker_prefix <- function(image, remote_directory, init = TRUE) {
  user <- paste0(
    system2("id", "-u", stdout = TRUE),
    ":",
    system2("id", "-g", stdout = TRUE)
  )
  c(
    "docker", "run",
    if (init) "--init",
    "--rm", "--network=host", "--user", user,
    "--volume", paste0(remote_directory, ":", remote_directory),
    image
  )
}

container_running <- function(cid) {
  cid %in% system2("docker", c("ps", "-q", "--no-trunc"), stdout = TRUE)
}

container_ids <- function(remote_directory) {
  files <- list.files(
    remote_directory,
    pattern = "^worker-.*\\.cid$",
    recursive = TRUE,
    full.names = TRUE
  )
  # The runtime writes the ID without a final newline.
  vapply(files, readLines, character(1L), warn = FALSE, USE.NAMES = FALSE)
}

# A stand-in for docker that honors --cidfile, answers inspect, and logs kill,
# so the paths that depend on a container runtime run without one. It must be
# called docker for crew.ssh to treat it as one.
fake_docker <- function(log) {
  directory <- tempfile("crew_ssh_fake_docker_")
  dir.create(directory)
  path <- file.path(directory, "docker")
  writeLines(
    c(
      "#!/bin/sh",
      "case \"$1\" in",
      "  run)",
      "    shift",
      "    if [ \"$1\" = --cidfile ]; then printf 'fakecid-%s' \"$$\" > \"$2\"; shift 2; fi",
      "    shift",
      "    exec \"$@\" ;;",
      "  inspect) echo true ;;",
      paste0("  kill) echo \"$@\" >> ", shQuote(log, type = "sh"), " ;;"),
      "esac"
    ),
    path
  )
  Sys.chmod(path, mode = "0755")
  path
}
