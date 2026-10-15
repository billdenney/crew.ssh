# The scripts that read records and stop workers are shell code, so these
# tests run them with sh against real directories and real processes.

skip_without_sh <- function() {
  testthat::skip_on_cran()
  testthat::skip_on_os("windows")
  testthat::skip_if_not_installed("ps")
}

run_sh <- function(script) {
  out <- processx::run("sh", c("-c", script), error_on_status = FALSE)
  testthat::expect_identical(out$status, 0L)
  out$stdout
}

# A process whose command line names the session, as a worker's or a tunnel's
# does. The trailing ":" matters: a shell given one command, as macOS's bash
# is, replaces itself with it and the name is gone from the command line.
process_naming <- function(session, seconds = 60) {
  processx::process$new(
    "sh",
    c("-c", paste("sleep", seconds, "; :"), session),
    cleanup = TRUE
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
      local_host = "character", local_pid = "integer",
      local_created = "numeric",
      task_key = "character", task_name = "character",
      task_state = "character", task_started = "POSIXct",
      task_ended = "POSIXct", task_updated = "POSIXct"
    )
  )
  text <- paste(
    "session: crew-ssh-a",
    "session_alive: FALSE",
    "local_host: LAPTOP",
    "local_pid: 4567",
    "local_created: 1790000000.125",
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
  expect_identical(frame$local_host, "LAPTOP")
  expect_identical(frame$local_pid, 4567L)
  expect_identical(frame$local_created, 1790000000.125)
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

# The records of a session as its tunnel command writes them. `tunnel_pid`
# stands for the remote end of its tunnel, whose command line names the
# session exactly while the tunnel is up.
write_session_records <- function(remote, session, tunnel_pid, runtime = "") {
  directory <- file.path(remote, session)
  dir.create(directory)
  writeLines(
    c(paste("session:", session), paste("runtime:", runtime)),
    file.path(directory, "session.dcf")
  )
  writeLines(as.character(tunnel_pid), file.path(directory, "tunnel.pid"))
  directory
}

write_worker_records <- function(directory, worker, pid, state, task,
                                 cid = NULL) {
  if (!is.null(pid)) {
    writeLines(as.character(pid), file.path(directory, paste0(worker, ".pid")))
  }
  if (!is.null(cid)) {
    writeLines(cid, file.path(directory, paste0(worker, ".cid")))
  }
  writeLines(
    c(paste("task_name:", task), paste("task_state:", state)),
    file.path(directory, paste0(worker, ".task"))
  )
}

test_that("remote_stop_waiters_script() stops only waiters cut off from their session", {
  skip_without_sh()
  remote <- remote_tempdir()
  log <- tempfile()
  runtime <- fake_runtime(log)
  live_tunnel <- process_naming("crew-ssh-live")
  # The reconnecting session's old tunnel, still up on the remote side because
  # sshd has not noticed that its client is gone.
  old_tunnel <- process_naming("crew-ssh-current")
  waiter_dead <- process_naming("crew-ssh-dead")
  runner_dead <- process_naming("crew-ssh-dead")
  waiter_live <- process_naming("crew-ssh-live")
  waiter_current <- process_naming("crew-ssh-current")
  processes <- list(
    live_tunnel, old_tunnel, waiter_dead, runner_dead, waiter_live,
    waiter_current
  )
  on.exit(for (process in processes) process$kill())
  # A live process whose command line names no session stands for a tunnel
  # that is gone and a PID the system has given to something else.
  unrelated <- Sys.getpid()
  # A container worker of a dead session: the runtime says it is running.
  directory <- write_session_records(remote, "crew-ssh-container", unrelated, runtime)
  write_worker_records(directory, "worker-c", NULL, "waiting", "fit_c", cid = "cid-c")
  directory <- write_session_records(remote, "crew-ssh-current", old_tunnel$get_pid())
  write_worker_records(directory, "worker-w", waiter_current$get_pid(), "waiting", "fit_w")
  directory <- write_session_records(remote, "crew-ssh-dead", unrelated)
  write_worker_records(directory, "worker-g", unrelated, "waiting", "fit_g")
  write_worker_records(directory, "worker-r", runner_dead$get_pid(), "running", "fit_r")
  write_worker_records(directory, "worker-w", waiter_dead$get_pid(), "waiting", "fit_d")
  directory <- write_session_records(remote, "crew-ssh-live", live_tunnel$get_pid())
  write_worker_records(directory, "worker-w", waiter_live$get_pid(), "waiting", "fit_l")
  out <- run_sh(
    paste(
      remote_stop_waiters_script(remote, session = "crew-ssh-current"),
      "exit 0",
      sep = "\n"
    )
  )
  expect_identical(
    strsplit(out, "\n", fixed = TRUE)[[1L]],
    c(
      "crew-ssh-stopped-waiter: crew-ssh-container worker-c fit_c",
      "crew-ssh-stopped-waiter: crew-ssh-current worker-w fit_w",
      "crew-ssh-stopped-waiter: crew-ssh-dead worker-w fit_d"
    )
  )
  wait_until(!waiter_dead$is_alive(), seconds = 10)
  wait_until(!waiter_current$is_alive(), seconds = 10)
  # The worker running the task saves its result, so it stays.
  expect_true(runner_dead$is_alive())
  # The waiter of a connected session can still hand its result back.
  expect_true(waiter_live$is_alive())
  # Tunnels are left alone, even the stale end of the reconnecting session's.
  expect_true(old_tunnel$is_alive())
  expect_true(live_tunnel$is_alive())
  expect_identical(
    readLines(log),
    c("inspect --format {{.State.Running}} cid-c", "kill cid-c")
  )
})

test_that("remote_stop_waiters_script() does nothing without sessions", {
  skip_without_sh()
  script <- remote_stop_waiters_script(remote_tempdir(), session = "crew-ssh-x")
  expect_identical(run_sh(paste(script, "exit 0", sep = "\n")), "")
})

test_that("remote_workers_script() can skip workers by the state of their task", {
  skip_without_sh()
  remote <- remote_tempdir()
  directory <- write_session_records(remote, "crew-ssh-a", Sys.getpid())
  for (state in c("running", "waiting", "success")) {
    write_worker_records(
      directory, paste0("worker-", state), Sys.getpid(), state,
      paste0("task_", state)
    )
  }
  # A worker that has not started a task has no task file.
  writeLines(as.character(Sys.getpid()), file.path(directory, "worker-none.pid"))
  all <- remote_workers_frame(
    run_sh(remote_workers_script(remote)), "host", remote, "crew-ssh-x"
  )
  expect_identical(
    sort(all$worker),
    c("worker-none", "worker-running", "worker-success", "worker-waiting")
  )
  some <- remote_workers_frame(
    run_sh(remote_workers_script(remote, states = c("running", "waiting"))),
    "host", remote, "crew-ssh-x"
  )
  expect_identical(sort(some$worker), c("worker-running", "worker-waiting"))
  expect_identical(
    some$task_state[order(some$worker)],
    c("running", "waiting")
  )
})

# One record in the format remote_workers_script() prints.
worker_record <- function(session, session_alive, worker, alive, state,
                          key = NA, name = NA) {
  c(
    paste("session:", session),
    paste("session_alive:", session_alive),
    paste("worker:", worker),
    "pid: 1",
    paste("process_alive:", alive),
    if (!is.na(key)) paste("task_key:", key),
    if (!is.na(name)) paste("task_name:", name),
    paste("task_state:", state),
    ""
  )
}

test_that("remote_unreachable_tasks() finds the running tasks no connection reaches", {
  text <- paste(
    c(
      # A running worker of a session that is gone: counted.
      worker_record("crew-ssh-dead", "FALSE", "worker-a", "TRUE", "running", "fit-1", "fit"),
      # Its process is gone too: not running any more.
      worker_record("crew-ssh-dead", "FALSE", "worker-b", "FALSE", "running", "gone-2", "gone"),
      # A second record of the same task counts once.
      worker_record("crew-ssh-dead", "FALSE", "worker-c", "TRUE", "running", "fit-1", "fit"),
      # A waiter computes nothing.
      worker_record("crew-ssh-dead", "FALSE", "worker-d", "TRUE", "waiting", "wait-3", "wait"),
      # A connected session's worker is that session's to count.
      worker_record("crew-ssh-live", "TRUE", "worker-e", "TRUE", "running", "live-4", "live"),
      # This session's worker from before the tunnel was reopened: counted.
      worker_record("crew-ssh-me", "TRUE", "worker-f", "TRUE", "running", "old-5", "old"),
      # This session's connected worker is already counted by crew.
      worker_record("crew-ssh-me", "TRUE", "worker-g", "TRUE", "running", "new-6", "new"),
      # A running record without a key cannot be matched to anything.
      worker_record("crew-ssh-dead", "FALSE", "worker-h", "TRUE", "running"),
      # A finished task takes up nothing.
      worker_record("crew-ssh-me", "TRUE", "worker-i", "TRUE", "success", "done-7", "done")
    ),
    collapse = "\n"
  )
  workers <- remote_workers_frame(text, "host", "/remote", session = "crew-ssh-me")
  expect_identical(
    remote_unreachable_tasks(workers, disconnected = c("worker-f", "worker-i")),
    data.frame(
      task_key = c("fit-1", "old-5"),
      task_name = c("fit", "old"),
      stringsAsFactors = FALSE
    )
  )
  # Without the reopen, this session's worker is reachable.
  expect_identical(
    remote_unreachable_tasks(workers, disconnected = character(0L))$task_key,
    "fit-1"
  )
  none <- remote_workers_frame("", "host", "/remote", session = "crew-ssh-me")
  expect_identical(
    remote_unreachable_tasks(none, disconnected = character(0L)),
    data.frame(
      task_key = character(0L),
      task_name = character(0L),
      stringsAsFactors = FALSE
    )
  )
})

test_that("remote_stopped_waiters() reads the workers the tunnel reports stopping", {
  output <- c(
    "crew-ssh-tunnel-ready",
    "crew-ssh-stopped-waiter: crew-ssh-a worker-1 fit_pk_1",
    "  crew-ssh-stopped-waiter: crew-ssh-b worker-2 ",
    "Allocated port 1234 for remote forward to 127.0.0.1:5678"
  )
  expect_identical(
    remote_stopped_waiters(output),
    c("worker-1 of crew-ssh-a (fit_pk_1)", "worker-2 of crew-ssh-b")
  )
  expect_identical(remote_stopped_waiters(character(0L)), character(0L))
  expect_identical(remote_stopped_waiters("crew-ssh-tunnel-ready"), character(0L))
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

# The PID and creation time of an R process that ran and has ended.
ended_r_process <- function() {
  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  process <- processx::process$new(rscript, c("-e", "Sys.sleep(60)"))
  out <- list(
    pid = process$get_pid(),
    created = as.numeric(ps::ps_create_time(process$as_ps_handle()))
  )
  process$kill()
  process$wait(10000)
  out
}

test_that("local_session_alive() tells a running R process from an ended or reused one", {
  host <- Sys.info()[["nodename"]]
  me <- Sys.getpid()
  created <- as.numeric(ps::ps_create_time(ps::ps_handle()))
  ended <- ended_r_process()
  expect_identical(
    local_session_alive(
      local_host = c(host, tolower(host), host, host, "elsewhere", NA, host),
      local_pid = c(me, me, me, ended$pid, me, me, NA),
      local_created = c(
        created, created, created + 100, ended$created, created, created,
        created
      )
    ),
    # This process, under any case of its host name; its PID with another
    # creation time, as when the PID was given to a later process; an ended
    # process; and three sessions only their tunnel can speak for.
    c(TRUE, TRUE, FALSE, FALSE, NA, NA, NA)
  )
  # A record from an earlier crew.ssh, without a creation time, goes by PID.
  expect_identical(local_session_alive(host, me, NA_real_), TRUE)
  expect_identical(
    local_session_alive(character(0L), integer(0L), numeric(0L)),
    logical(0L)
  )
})

test_that("remote_dead_sessions() names this machine's sessions whose R process ended", {
  host <- Sys.info()[["nodename"]]
  created <- sprintf("%.3f", as.numeric(ps::ps_create_time(ps::ps_handle())))
  ended <- ended_r_process()
  text <- paste(
    c(
      "session: crew-ssh-mine",
      paste("local_host:", host),
      paste("local_pid:", Sys.getpid()),
      paste("local_created:", created),
      "",
      "session: crew-ssh-ended",
      paste("local_host:", tolower(host)),
      paste("local_pid:", ended$pid),
      paste("local_created:", sprintf("%.3f", ended$created)),
      "",
      "session: crew-ssh-elsewhere",
      "local_host: another-machine",
      paste("local_pid:", ended$pid),
      paste("local_created:", sprintf("%.3f", ended$created)),
      "",
      # The session asking is never reported, even with an ended process.
      "session: crew-ssh-current",
      paste("local_host:", host),
      paste("local_pid:", ended$pid),
      paste("local_created:", sprintf("%.3f", ended$created)),
      "",
      "session: crew-ssh-before-local-records",
      "controller: x"
    ),
    collapse = "\n"
  )
  expect_identical(
    remote_dead_sessions(text, current = "crew-ssh-current"),
    "crew-ssh-ended"
  )
  expect_identical(remote_dead_sessions("", current = "x"), character(0L))
})

test_that("remote_closed_tunnels() reads the sessions whose tunnel end was closed", {
  output <- c(
    "crew-ssh-closed-tunnel: crew-ssh-a",
    "crew-ssh-stopped-waiter: crew-ssh-a worker-1 fit",
    "  crew-ssh-closed-tunnel: crew-ssh-b  ",
    "crew-ssh-tunnel-ready"
  )
  expect_identical(remote_closed_tunnels(output), c("crew-ssh-a", "crew-ssh-b"))
  expect_identical(remote_closed_tunnels(character(0L)), character(0L))
})

test_that("remote_sessions_script() prints the record of every session", {
  skip_without_sh()
  remote <- remote_tempdir()
  write_session_records(remote, "crew-ssh-a", 1L)
  write_session_records(remote, "crew-ssh-b", 2L, runtime = "docker")
  # A session directory without a record yet is left out.
  dir.create(file.path(remote, "crew-ssh-c"))
  dir.create(file.path(remote, "crew-ssh-salvage"))
  records <- remote_dcf(run_sh(remote_sessions_script(remote)))
  expect_identical(unname(records[, "session"]), c("crew-ssh-a", "crew-ssh-b"))
  expect_identical(unname(records[, "runtime"]), c("", "docker"))
})

test_that("remote_workers_script() reports the local process of each session", {
  skip_without_sh()
  remote <- remote_tempdir()
  directory <- file.path(remote, "crew-ssh-a")
  dir.create(directory)
  writeLines(
    c(
      "session: crew-ssh-a",
      "local_host: LAPTOP",
      "local_pid: 4567",
      "local_created: 1790000000.125"
    ),
    file.path(directory, "session.dcf")
  )
  writeLines(as.character(Sys.getpid()), file.path(directory, "worker-a.pid"))
  frame <- remote_workers_frame(
    run_sh(remote_workers_script(remote)), "host", remote, "crew-ssh-x"
  )
  expect_identical(frame$local_host, "LAPTOP")
  expect_identical(frame$local_pid, 4567L)
  expect_identical(frame$local_created, 1790000000.125)
})

test_that("remote_stop_waiters_script() treats sessions known to have ended as gone", {
  skip_without_sh()
  remote <- remote_tempdir()
  # The ended session's tunnel end, kept by sshd after its client vanished.
  lingering <- process_naming("crew-ssh-ended")
  waiter <- process_naming("crew-ssh-ended")
  live_tunnel <- process_naming("crew-ssh-live")
  live_waiter <- process_naming("crew-ssh-live")
  processes <- list(lingering, waiter, live_tunnel, live_waiter)
  on.exit(for (process in processes) process$kill())
  directory <- write_session_records(remote, "crew-ssh-ended", lingering$get_pid())
  write_worker_records(directory, "worker-w", waiter$get_pid(), "waiting", "fit_e")
  directory <- write_session_records(remote, "crew-ssh-live", live_tunnel$get_pid())
  write_worker_records(directory, "worker-w", live_waiter$get_pid(), "waiting", "fit_l")
  out <- run_sh(
    paste(
      remote_stop_waiters_script(
        remote,
        session = "crew-ssh-new",
        dead = "crew-ssh-ended"
      ),
      "exit 0",
      sep = "\n"
    )
  )
  expect_identical(
    strsplit(out, "\n", fixed = TRUE)[[1L]],
    c(
      "crew-ssh-closed-tunnel: crew-ssh-ended",
      "crew-ssh-stopped-waiter: crew-ssh-ended worker-w fit_e"
    )
  )
  wait_until(!lingering$is_alive(), seconds = 10)
  wait_until(!waiter$is_alive(), seconds = 10)
  # A session not known to have ended still goes by its tunnel.
  expect_true(live_tunnel$is_alive())
  expect_true(live_waiter$is_alive())
})
