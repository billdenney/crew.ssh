# The scripts that read records and stop workers are shell code, so these
# tests run them with sh against real directories and real processes.

skip_without_sh <- function() {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("ps")
}

run_sh <- function(script) {
  out <- processx::run("sh", c("-c", script), error_on_status = FALSE)
  expect_identical(out$status, 0L)
  out$stdout
}

# A process whose command line names the session, as a worker's or a tunnel's
# does.
process_naming <- function(session, seconds = 60) {
  processx::process$new(
    "sh",
    c("-c", paste("sleep", seconds), session),
    cleanup = TRUE
  )
}

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

test_that("remote_dcf() and remote_field() read records and fill gaps with NA", {
  expect_null(remote_dcf(""))
  expect_null(remote_dcf("\n  \n"))
  records <- remote_dcf("a: 1\nb: x\n\na: 2\n")
  expect_identical(nrow(records), 2L)
  expect_identical(remote_field(records, "a", 2L), c("1", "2"))
  expect_identical(remote_field(records, "b", 2L), c("x", NA))
  expect_identical(remote_field(records, "missing", 2L), c(NA_character_, NA))
  expect_identical(remote_field(NULL, "a", 0L), character(0L))
  expect_identical(
    remote_field(remote_dcf("a: \n"), "a", 1L),
    NA_character_
  )
})

test_that("remote_workers_frame() types every column, even with no workers", {
  empty <- remote_workers_frame("", "host", "/tmp", "crew-ssh-a")
  expect_identical(nrow(empty), 0L)
  expect_identical(
    vapply(lapply(empty, class), "[[", character(1L), 1L),
    c(
      host = "character", session = "character",
      session_directory = "character", worker = "character",
      pid = "integer", container = "character", runtime = "character",
      alive = "logical", session_alive = "logical", current = "logical",
      task_key = "character", task_name = "character",
      task_state = "character", task_started = "POSIXct",
      task_ended = "POSIXct", task_updated = "POSIXct"
    )
  )
  text <- paste(
    "session: crew-ssh-a",
    "session_alive: FALSE",
    "worker: worker-1",
    "pid: 123",
    "process_alive: FALSE",
    "container: abc",
    "container_running: true",
    "runtime: docker",
    "task_key: fit-1",
    "task_name: fit",
    "task_state: running",
    "task_updated: 1790000000.500",
    "task_started: 1790000000.250",
    "",
    sep = "\n"
  )
  frame <- remote_workers_frame(text, "user@host", "/remote", NULL)
  expect_identical(frame$session_directory, "/remote/crew-ssh-a")
  expect_identical(frame$pid, 123L)
  # The client died, the container did not.
  expect_true(frame$alive)
  expect_false(frame$session_alive)
  expect_false(frame$current)
  expect_identical(as.numeric(frame$task_started), 1790000000.25)
  expect_true(is.na(frame$task_ended))
})

test_that("remote_workers_script() reads the records of every session", {
  skip_without_sh()
  remote <- remote_tempdir()
  live <- file.path(remote, "crew-ssh-live")
  gone <- file.path(remote, "crew-ssh-gone")
  dir.create(live)
  dir.create(gone)
  dir.create(file.path(remote, "crew-ssh-salvage"))
  log <- tempfile()
  runtime <- fake_runtime(log)
  writeLines(
    c("session: crew-ssh-live", paste("runtime:", runtime)),
    file.path(live, "session.dcf")
  )
  tunnel <- process_naming("crew-ssh-live")
  worker <- process_naming("crew-ssh-live")
  on.exit({
    tunnel$kill()
    worker$kill()
  })
  writeLines(as.character(tunnel$get_pid()), file.path(live, "tunnel.pid"))
  writeLines(as.character(worker$get_pid()), file.path(live, "worker-a.pid"))
  writeLines("cid-a", file.path(live, "worker-a.cid"))
  writeLines(
    c("task_key: fit-1", "task_name: fit", "task_state: running"),
    file.path(live, "worker-a.task")
  )
  # A session whose tunnel and worker are gone, and whose pid files now name
  # processes of something else.
  writeLines(as.character(Sys.getpid()), file.path(gone, "tunnel.pid"))
  writeLines(as.character(Sys.getpid()), file.path(gone, "worker-b.pid"))
  frame <- remote_workers_frame(
    run_sh(remote_workers_script(remote)),
    host = "user@host",
    remote_directory = remote,
    session = "crew-ssh-live"
  )
  frame <- frame[order(frame$worker), ]
  expect_identical(frame$worker, c("worker-a", "worker-b"))
  expect_identical(frame$session, c("crew-ssh-live", "crew-ssh-gone"))
  expect_identical(frame$pid, c(worker$get_pid(), Sys.getpid()))
  expect_identical(frame$alive, c(TRUE, FALSE))
  expect_identical(frame$session_alive, c(TRUE, FALSE))
  expect_identical(frame$current, c(TRUE, FALSE))
  expect_identical(frame$container, c("cid-a", NA))
  expect_identical(frame$runtime, c(runtime, NA))
  expect_identical(frame$task_state, c("running", NA))
  expect_identical(readLines(log), "inspect --format {{.State.Running}} cid-a")
})

test_that("remote_workers_script() finds nothing in an empty directory", {
  skip_without_sh()
  expect_identical(run_sh(remote_workers_script(remote_tempdir())), "")
})

test_that("remote_stop_script() stops only processes that still name their session", {
  skip_without_sh()
  remote <- remote_tempdir()
  log <- tempfile()
  runtime <- fake_runtime(log)
  mine <- process_naming("crew-ssh-mine")
  other <- process_naming("crew-ssh-other")
  on.exit({
    mine$kill()
    other$kill()
  })
  cid <- file.path(remote, "worker-a.cid")
  writeLines("cid-a", cid)
  script <- remote_stop_script(
    pids = c(mine$get_pid(), other$get_pid(), 999999L),
    sessions = c("crew-ssh-mine", "crew-ssh-mine", "crew-ssh-mine"),
    cid_files = c(cid, file.path(remote, "missing.cid"), cid),
    runtimes = c(runtime, runtime, NA)
  )
  run_sh(script)
  wait_until(!mine$is_alive(), seconds = 10)
  # Its PID names another session, so it is not this worker any more.
  expect_true(other$is_alive())
  expect_identical(readLines(log), "kill cid-a")
})

test_that("remote_salvage_script() lists saved results and locks", {
  skip_without_sh()
  directory <- file.path(remote_tempdir(), "salvage")
  expect_identical(run_sh(remote_salvage_script(directory)), "")
  dir.create(directory)
  writeBin(as.raw(1:10), file.path(directory, "fit-1.rds"))
  writeLines("half", file.path(directory, "fit-1.rds.tmp.1.2"))
  dir.create(file.path(directory, "fit-2.lock"))
  write.dcf(
    data.frame(worker = "worker-a", session = "crew-ssh-a", token = "1.2"),
    file.path(directory, "fit-2.lock", "owner.dcf")
  )
  file.create(file.path(directory, "fit-2.lock", "heartbeat"))
  dir.create(file.path(directory, "fit-3.lock"))
  Sys.setFileTime(file.path(directory, "fit-3.lock"), Sys.time() - 600)
  frame <- remote_salvage_frame(
    run_sh(remote_salvage_script(directory)),
    directory = directory,
    seconds_stale = 120
  )
  expect_identical(frame$key, c("fit-1", "fit-2", "fit-3"))
  expect_identical(frame$state, c("saved", "running", "abandoned"))
  expect_identical(frame$bytes, c(10, NA, NA))
  expect_identical(frame$worker, c(NA, "worker-a", NA))
  expect_identical(frame$session, c(NA, "crew-ssh-a", NA))
  expect_identical(
    frame$path,
    file.path(directory, c("fit-1.rds", "fit-2.lock", "fit-3.lock"))
  )
  expect_lt(frame$heartbeat_age[2L], 60)
  expect_gt(frame$heartbeat_age[3L], 500)
  expect_true(is.na(frame$heartbeat_age[1L]))
})

test_that("remote_salvage_frame() is empty with typed columns when nothing is saved", {
  frame <- remote_salvage_frame("", "/remote/salvage", 120)
  expect_identical(nrow(frame), 0L)
  expect_identical(
    names(frame),
    c("key", "state", "bytes", "modified", "heartbeat_age", "worker", "session", "path")
  )
  expect_s3_class(frame$modified, "POSIXct")
  expect_identical(frame$path, character(0L))
})
