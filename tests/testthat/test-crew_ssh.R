ssh_host    <- "bill@picasso.humanpredictions.local"
ssh_keyfile <- file.path(Sys.getenv("USERPROFILE"), ".ssh", "id_ed25519")

# Unit tests (no SSH required) ----

test_that("ssh_launcher_class stores all fields on initialize", {
  launcher <- ssh_launcher_class$new(
    name             = "test",
    ssh_host         = "user@host",
    ssh_keyfile      = "/path/to/key",
    ssh_passwd       = "pw",
    ssh_verbose      = TRUE,
    rscript_path     = "/usr/bin/Rscript",
    remote_log_dir   = "/var/log",
    seconds_interval = 0.25
  )
  expect_equal(launcher$ssh_host,       "user@host")
  expect_equal(launcher$ssh_keyfile,    "/path/to/key")
  expect_equal(launcher$ssh_passwd,     "pw")
  expect_true(launcher$ssh_verbose)
  expect_equal(launcher$rscript_path,   "/usr/bin/Rscript")
  expect_equal(launcher$remote_log_dir, "/var/log")
})

test_that("terminate() with no launched workers returns without error", {
  launcher <- ssh_launcher_class$new(name = "test", ssh_host = "user@host", seconds_interval = 0.25)
  expect_no_error(launcher$terminate())
})

test_that("launch_worker returns a correctly shaped handle", {
  fake_session <- structure(list(), class = "ssh_session")

  local_mocked_bindings(
    ssh_connect = function(...) fake_session,
    scp_upload  = function(...) invisible(NULL),
    ssh_exec_internal = function(session, command) {
      list(status = 0L, stdout = charToRaw("12345\n"), stderr = raw(0))
    },
    .package = "ssh"
  )

  launcher <- ssh_launcher_class$new(name = "test", ssh_host = "user@host", seconds_interval = 0.25)
  handle <- launcher$launch_worker(call = 'crew::crew_worker(settings = list())')

  expect_named(handle, c("id", "pid", "session", "remote_script", "remote_log"),
               ignore.order = TRUE)
  expect_equal(handle$pid, "12345")
  expect_identical(handle$session, fake_session)
  expect_match(handle$remote_script, "^/tmp/rcrew_worker_")
  expect_match(handle$remote_log,    "^/tmp/rcrew_worker_")
})

test_that("launch_worker uses remote_log_dir in remote paths", {
  fake_session  <- structure(list(), class = "ssh_session")
  uploaded_to   <- NULL

  local_mocked_bindings(
    ssh_connect = function(...) fake_session,
    scp_upload  = function(session, files, to) { uploaded_to <<- to; invisible(NULL) },
    ssh_exec_internal = function(session, command) {
      list(status = 0L, stdout = charToRaw("55555\n"), stderr = raw(0))
    },
    .package = "ssh"
  )

  launcher <- ssh_launcher_class$new(
    name             = "test",
    ssh_host         = "user@host",
    remote_log_dir   = "/var/tmp",
    seconds_interval = 0.25
  )
  handle <- launcher$launch_worker(call = 'crew::crew_worker(settings = list())')

  expect_match(uploaded_to, "^/var/tmp/rcrew_worker_.*\\.R$")
  expect_equal(handle$remote_script, uploaded_to)
  expect_match(handle$remote_log,    "^/var/tmp/rcrew_worker_.*\\.log$")
})

test_that("launch_worker stops on non-zero remote exit status", {
  fake_session <- structure(list(), class = "ssh_session")
  disconnected <- FALSE

  local_mocked_bindings(
    ssh_connect = function(...) fake_session,
    scp_upload  = function(...) invisible(NULL),
    ssh_exec_internal = function(session, command) {
      list(status = 1L, stdout = raw(0), stderr = charToRaw("Rscript: not found\n"))
    },
    ssh_disconnect = function(...) { disconnected <<- TRUE; invisible(NULL) },
    .package = "ssh"
  )

  launcher <- ssh_launcher_class$new(name = "test", ssh_host = "user@host", seconds_interval = 0.25)
  expect_error(
    launcher$launch_worker(call = 'crew::crew_worker(settings = list())'),
    "Failed to launch remote R worker"
  )
  expect_true(disconnected)
})

test_that("launch_worker stops when PID output is non-numeric", {
  fake_session <- structure(list(), class = "ssh_session")

  local_mocked_bindings(
    ssh_connect = function(...) fake_session,
    scp_upload  = function(...) invisible(NULL),
    ssh_exec_internal = function(session, command) {
      list(status = 0L, stdout = charToRaw("nohup: ignoring input\n"), stderr = raw(0))
    },
    ssh_disconnect = function(...) invisible(NULL),
    .package = "ssh"
  )

  launcher <- ssh_launcher_class$new(name = "test", ssh_host = "user@host", seconds_interval = 0.25)
  expect_error(
    launcher$launch_worker(call = 'crew::crew_worker(settings = list())'),
    "Could not determine PID"
  )
})

test_that("launch_worker stops when PID output is empty", {
  fake_session <- structure(list(), class = "ssh_session")

  local_mocked_bindings(
    ssh_connect = function(...) fake_session,
    scp_upload  = function(...) invisible(NULL),
    ssh_exec_internal = function(session, command) {
      list(status = 0L, stdout = charToRaw("   \n"), stderr = raw(0))
    },
    ssh_disconnect = function(...) invisible(NULL),
    .package = "ssh"
  )

  launcher <- ssh_launcher_class$new(name = "test", ssh_host = "user@host", seconds_interval = 0.25)
  expect_error(
    launcher$launch_worker(call = 'crew::crew_worker(settings = list())'),
    "Could not determine PID"
  )
})

test_that("terminate() disconnects all SSH sessions opened by launch_worker", {
  fake_session    <- structure(list(), class = "ssh_session")
  disconnect_count <- 0L

  local_mocked_bindings(
    ssh_connect = function(...) fake_session,
    scp_upload  = function(...) invisible(NULL),
    ssh_exec_internal = function(session, command) {
      list(status = 0L, stdout = charToRaw("12345\n"), stderr = raw(0))
    },
    ssh_disconnect = function(...) { disconnect_count <<- disconnect_count + 1L; invisible(NULL) },
    .package = "ssh"
  )

  launcher <- ssh_launcher_class$new(name = "test", ssh_host = "user@host", seconds_interval = 0.25)
  launcher$launch_worker(call = 'crew::crew_worker(settings = list())')
  launcher$launch_worker(call = 'crew::crew_worker(settings = list())')

  launcher$terminate()
  expect_equal(disconnect_count, 2L)
})

test_that("launch_worker with ssh_verbose emits messages", {
  fake_session <- structure(list(), class = "ssh_session")

  local_mocked_bindings(
    ssh_connect = function(...) fake_session,
    scp_upload  = function(...) invisible(NULL),
    ssh_exec_internal = function(session, command) {
      list(status = 0L, stdout = charToRaw("77777\n"), stderr = raw(0))
    },
    .package = "ssh"
  )

  launcher <- ssh_launcher_class$new(
    name             = "test",
    ssh_host         = "user@host",
    ssh_verbose      = TRUE,
    seconds_interval = 0.25
  )
  expect_message(
    launcher$launch_worker(call = 'crew::crew_worker(settings = list())'),
    "Connecting to"
  )
})

test_that("crew_controller_ssh constructs and validates without error", {
  skip_if_not_installed("crew")

  controller <- crew_controller_ssh(
    name        = "test-ssh",
    ssh_host    = "user@host",
    workers     = 1L,
    host        = "127.0.0.1"
  )
  expect_true(inherits(controller, "R6"))
  controller$terminate()
})

# Integration tests (require SSH to picasso with crew installed) ----

can_ssh <- tryCatch({
  s <- ssh::ssh_connect(ssh_host, keyfile = ssh_keyfile)
  r <- ssh::ssh_exec_internal(s, "Rscript -e 'cat(nzchar(system.file(package=\"crew\")))'")
  crew_available <- identical(trimws(rawToChar(r$stdout)), "TRUE")
  ssh::ssh_disconnect(s)
  crew_available
}, error = function(e) FALSE)

test_that("remote task runs on picasso and returns correct hostname", {
  skip_on_cran()
  skip_if(!can_ssh, "Cannot reach picasso via SSH or crew not installed on remote")
  skip_if_not_installed("ps")
  skip_if_not_installed("nanonext")

  local_hostname <- Sys.info()[["nodename"]]

  local_host <- nanonext::ip_addr()[1]
  message("Using host IP for controller: ", local_host)

  controller <- crew_controller_ssh(
    ssh_host       = ssh_host,
    ssh_keyfile    = ssh_keyfile,
    workers        = 1L,
    seconds_idle   = 30,
    seconds_launch = 120,
    host           = local_host
  )
  on.exit(try(controller$terminate(), silent = TRUE), add = TRUE)

  controller$start()
  controller$push(
    name    = "remote_info",
    command = paste(Sys.info()[["nodename"]], ps::ps_pid())
  )
  controller$wait(mode = "all", seconds_timeout = 300)
  result <- controller$pop()

  expect_false(is.null(result))
  remote_info     <- result$result[[1]]
  remote_hostname <- strsplit(remote_info, " ")[[1]][1]
  expect_false(identical(remote_hostname, local_hostname),
               label = "Worker should run on remote host, not locally")
})

test_that("multiple workers on picasso each run tasks", {
  skip_on_cran()
  skip_if(!can_ssh, "Cannot reach picasso via SSH or crew not installed on remote")
  skip_if_not_installed("ps")
  skip_if_not_installed("nanonext")

  controller <- crew_controller_ssh(
    ssh_host       = ssh_host,
    ssh_keyfile    = ssh_keyfile,
    workers        = 2L,
    seconds_idle   = 30,
    seconds_launch = 120,
    host           = nanonext::ip_addr()[1]
  )
  on.exit(try(controller$terminate(), silent = TRUE), add = TRUE)

  controller$start()
  for (i in seq_len(4L)) {
    controller$push(
      name    = paste0("task_", i),
      command = ps::ps_pid(),
      data    = list()
    )
  }
  controller$wait(mode = "all", seconds_timeout = 300)
  results <- controller$collect()

  expect_equal(nrow(results), 4L)
  pids <- unlist(results$result)
  expect_equal(length(pids), 4L)
})
