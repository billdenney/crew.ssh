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

# The sections below serve one test file each. They live here because lintr
# checks every file on its own, so a function that calls a helper of this file
# has to be defined in this file too.

# Remote records ----

# A container runtime that answers `inspect` with a canned state and logs
# every call.
fake_runtime <- function(log, running = "true") {
  write_executable(
    c(
      "#!/bin/sh",
      paste0("echo \"$@\" >> ", shQuote(log, type = "sh")),
      "case \"$1\" in",
      paste0("  inspect) echo ", running, " ;;"),
      "esac"
    )
  )
}

# Launchers ----

# A launcher with salvage whose session is set up without opening a tunnel.
# The remote system's paths are POSIX paths, which a Windows temporary
# directory is not, and Windows cannot be the remote system.
salvage_launcher <- function(remote_directory = remote_tempdir()) {
  testthat::skip_on_os("windows")
  launcher <- crew_controller_ssh(
    ssh_host = "fakehost",
    ssh_command = fake_ssh(),
    remote_directory = remote_directory,
    salvage = TRUE
  )$launcher
  private <- launcher$.__enclos_env__$private
  private$.session <- "crew-ssh-abc"
  private$.session_directory <- file.path(remote_directory, "crew-ssh-abc")
  private$.salvage_keys <- new.env(hash = TRUE, parent = emptyenv())
  private$.salvage_collected <- new.env(hash = TRUE, parent = emptyenv())
  private$.salvage_shipped <- salvage_shipped(
    directory = private$salvage_root(),
    session = private$.session,
    session_directory = private$.session_directory,
    controller = "ctl",
    seconds = private$.salvage_seconds
  )
  launcher
}

# A stand-in for ssh that fails the way ssh does while the network is down,
# with exit status 255, as many times as the number in the file `failures`
# says, counting it down, and then behaves like fake_ssh().
flaky_ssh <- function(failures) {
  path <- shQuote(failures, type = "sh")
  write_executable(
    c(
      "#!/bin/sh",
      paste0("n=$(cat ", path, " 2>/dev/null || echo 0)"),
      "if [ \"$n\" -gt 0 ]; then",
      paste0("  echo $((n - 1)) > ", path),
      "  echo 'ssh: Could not resolve hostname fakehost: Name or service not known' >&2",
      "  exit 255",
      "fi",
      fake_ssh_lines[-1L]
    )
  )
}

# Salvage ----

# A payload with every kind of object that hashes by identity, built from the
# same code in this session and in another one. The closures are made by
# evaluating code rather than written out, so that each has the environment a
# real one would: the global environment, or a factory's frame.
identity_payload <- function() {
  environment <- new.env()
  environment$self <- environment
  environment$value <- 1:3
  factory <- eval(quote(local({
    k <- 2
    function(x) x * k
  })), envir = globalenv())
  list(
    command = quote({
      fit(data)
    }),
    data = list(
      data = data.frame(x = 1:3),
      environment = environment,
      formula = y ~ x,
      pointer = structure(1:2, selfref = methods::new("externalptr"))
    ),
    globals = list(
      fit = eval(quote(function(data) stats::lm(x ~ 1, data)), envir = globalenv()),
      factory = factory,
      sourced = eval(
        parse(text = "function(x) {\n  x + 1\n}", keep.source = TRUE),
        envir = globalenv()
      ),
      primitive = sum,
      .Random.seed = sample.int(1000L, 3L)
    )
  )
}

identity_key <- function(source) {
  if (!("crew.ssh" %in% loadedNamespaces())) {
    crew_ssh_load(source)
  }
  key <- get("salvage_key", envir = asNamespace("crew.ssh"))
  cache <- get("salvage_globals_cache", envir = asNamespace("crew.ssh"))
  payload <- identity_payload()
  key(
    command = payload$command,
    data = payload$data,
    globals = cache(payload$globals),
    seed = 1L,
    algorithm = NULL,
    packages = character(0L),
    library = NULL,
    name = "identity"
  )
}

# The lines every test pipeline starts with: load crew.ssh and point targets
# at a controller whose "remote system" is a directory of this machine.
pipeline_header <- function(remote) {
  source <- crew_ssh_source()
  load <- if (is.null(source)) {
    "loadNamespace('crew.ssh')"
  } else {
    sprintf(
      "pkgload::load_all(%s, quiet = TRUE, attach = FALSE, helpers = FALSE)",
      deparse(source)
    )
  }
  c(
    load,
    "library(targets)",
    "tar_option_set(",
    "  storage = 'main', retrieval = 'main', error = 'continue',",
    "  packages = character(0),",
    "  controller = crew.ssh::crew_controller_ssh(",
    sprintf("    ssh_host = 'fakehost', ssh_command = %s,", deparse(fake_ssh())),
    sprintf("    remote_directory = %s, salvage = TRUE,", deparse(remote)),
    sprintf("    rscript = %s,", deparse(file.path(R.home("bin"), "Rscript"))),
    "    seconds_idle = 60, workers = 2L",
    "  )",
    ")"
  )
}

# A pipeline with one remote target that holds until the file `release`
# exists. `unrelated` changes a function that no target uses.
write_pipeline <- function(project, remote, multiplier, unrelated = 1) {
  writeLines(
    c(
      pipeline_header(remote),
      sprintf("unrelated_helper <- function(x) x + %s", unrelated),
      sprintf(
        paste(
          "slow_fit <- function(x) {",
          "cat('run\\n', file = %s, append = TRUE);",
          "while (!file.exists(%s)) Sys.sleep(0.2);",
          "sum(x) * %s }"
        ),
        deparse(file.path(project, "fits.txt")),
        deparse(file.path(project, "release")),
        multiplier
      ),
      "list(",
      "  tar_target(data, 1:10, deployment = 'main'),",
      "  tar_target(fit, slow_fit(data)),",
      "  tar_target(report, fit + 1, deployment = 'main')",
      ")"
    ),
    file.path(project, "_targets.R")
  )
}

# A pipeline whose remote target fails the first time it runs.
write_flaky_pipeline <- function(project, remote) {
  counter <- deparse(file.path(project, "flaky.txt"))
  writeLines(
    c(
      pipeline_header(remote),
      sprintf(
        paste(
          "flaky_fit <- function() {",
          "cat('run\\n', file = %s, append = TRUE);",
          "if (length(readLines(%s)) < 2L) stop('transient failure');",
          "'recovered' }"
        ),
        counter,
        counter
      ),
      "list(tar_target(flaky, flaky_fit()))"
    ),
    file.path(project, "_targets.R")
  )
}

make_pipeline <- function(project, background = FALSE) {
  targets::tar_make(
    script = file.path(project, "_targets.R"),
    store = file.path(project, "_targets"),
    reporter = "silent",
    callr_function = if (background) callr::r_bg else callr::r
  )
}

# Run the pipeline until its remote target is computing, then kill the session.
# The target holds until the file `release` exists.
interrupted_pipeline <- function(project) {
  first <- make_pipeline(project, background = TRUE)
  wait_until(file.exists(file.path(project, "fits.txt")), seconds = 120)
  first$kill()
}

# Remote records ----

# One record in the format remote_workers_script() prints.
worker_record <- function(session, session_alive, worker, alive, state,
                          key = NA, name = NA, scheme = 2L) {
  c(
    paste("session:", session),
    paste("session_alive:", session_alive),
    paste("worker:", worker),
    "pid: 1",
    paste("process_alive:", alive),
    if (!is.na(key)) paste("task_key:", key),
    if (!is.na(name)) paste("task_name:", name),
    paste("task_state:", state),
    if (!is.na(scheme)) paste("task_scheme:", scheme),
    ""
  )
}

# Runs left behind ----

# The process IDs of the workers under `remote`, named by worker.
worker_pids <- function(remote) {
  files <- list.files(remote, "^worker-.*\\.pid$", recursive = TRUE, full.names = TRUE)
  stats::setNames(
    as.integer(vapply(files, readLines, character(1L))),
    sub("\\.pid$", "", basename(files))
  )
}

# The session directories under `remote` other than `current`.
other_session <- function(remote, current) {
  sessions <- list.files(remote, pattern = "^crew-ssh-[0-9a-f]+$")
  setdiff(sessions, current)
}

# Start a held task named "slow" in another session, then end that session,
# leaving its worker running. Returns that worker's process ID.
dead_session_task <- function(remote, counter, release, data = list()) {
  session <- session_in_background(
    remote,
    command = held_task(),
    data = c(list(counter = counter, release = release), data),
    name = "slow"
  )
  wait_until(file.exists(counter))
  session$kill()
  old <- other_session(remote, current = NULL)
  tunnel <- readLines(file.path(remote, old, "tunnel.pid"))
  wait_until(!pid_alive(tunnel), seconds = 30)
  worker_pids(remote)[[1L]]
}

# The globals of a pipeline whose target `fit` calls slow_fit(), which calls
# helper(), which reads scale_by. No function of `fit` reaches `other`.
pipeline_globals <- function(helper = function(x) x * scale_by,
                             scale_by = 2,
                             other = function(x) x - 1,
                             slow_fit = function(x) helper(sum(x))) {
  list(
    slow_fit = global_function(slow_fit),
    helper = global_function(helper),
    scale_by = scale_by,
    other = global_function(other),
    .Random.seed = sample.int(1000L, 3L)
  )
}

# The process ID of the worker running a task under `remote`.
fit_worker_pid <- function(remote) {
  states <- task_states(remote)
  worker <- names(states)[states == "running"]
  path <- list.files(
    remote, paste0("^", worker, "\\.pid$"),
    recursive = TRUE, full.names = TRUE
  )
  readLines(path)
}
