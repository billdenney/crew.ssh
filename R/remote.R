# Remote records ----
#
# Each controller session keeps a durable record of itself and its workers in
# its session directory on the remote system:
#
# * session.dcf: the controller, the local machine and process that started
#   it, and the container runtime its workers run under, if any;
# * tunnel.pid: the remote end of the session's tunnel, alive exactly as long
#   as the session is connected;
# * worker-*.pid: the process the launch started, which for a container
#   runtime is its client and not the container;
# * worker-*.cid: the container ID, written by the container runtime;
# * worker-*.task: the task the worker is on and its state (with salvage).
#
# The functions below write the shell scripts that read these records back and
# stop workers. A worker that runs in a container needs nothing but a shell
# and the container runtime on the remote system, so the scripts use POSIX sh
# and not R.

remote_workers_script <- function(remote_directory) {
  paste(
    paste0(
      "for d in ", shQuote(remote_directory, type = "sh"), "/crew-ssh-*/; do"
    ),
    "  s=$(basename \"$d\")",
    "  rt=",
    "  if [ -f \"${d}session.dcf\" ]; then",
    "    rt=$(sed -n 's/^runtime: //p' \"${d}session.dcf\")",
    "  fi",
    "  sa=FALSE",
    "  if [ -s \"${d}tunnel.pid\" ]; then",
    "    tp=$(cat \"${d}tunnel.pid\")",
    "    case \"$(ps -p \"$tp\" -o args= 2>/dev/null)\" in",
    "      *\"$s\"*) sa=TRUE ;;",
    "    esac",
    "  fi",
    "  for p in \"$d\"worker-*.pid; do",
    "    [ -f \"$p\" ] || continue",
    "    w=$(basename \"$p\" .pid)",
    "    pid=$(cat \"$p\")",
    "    alive=FALSE",
    "    case \"$(ps -p \"$pid\" -o args= 2>/dev/null)\" in",
    "      *\"$s\"*) alive=TRUE ;;",
    "    esac",
    "    cid=",
    "    [ -s \"$d$w.cid\" ] && cid=$(cat \"$d$w.cid\")",
    "    running=",
    "    if [ -n \"$cid\" ] && [ -n \"$rt\" ]; then",
    "      running=$(\"$rt\" inspect --format '{{.State.Running}}' \\",
    "        \"$cid\" 2>/dev/null) || running=false",
    "    fi",
    "    printf 'session: %s\\nsession_alive: %s\\n' \"$s\" \"$sa\"",
    "    printf 'worker: %s\\npid: %s\\nprocess_alive: %s\\n' \\",
    "      \"$w\" \"$pid\" \"$alive\"",
    "    printf 'container: %s\\ncontainer_running: %s\\nruntime: %s\\n' \\",
    "      \"$cid\" \"$running\" \"$rt\"",
    "    [ -s \"$d$w.task\" ] && cat \"$d$w.task\"",
    "    echo",
    "  done",
    "done",
    "exit 0",
    sep = "\n"
  )
}

remote_salvage_script <- function(directory) {
  paste(
    paste0("d=", shQuote(directory, type = "sh")),
    "[ -d \"$d\" ] || exit 0",
    "now=$(date +%s)",
    # GNU stat first, then BSD stat as on macOS.
    "mtime() {",
    "  stat -c %Y \"$1\" 2>/dev/null || stat -f %m \"$1\" 2>/dev/null",
    "}",
    "size() {",
    "  stat -c %s \"$1\" 2>/dev/null || stat -f %z \"$1\" 2>/dev/null",
    "}",
    "for f in \"$d\"/*.rds; do",
    "  [ -f \"$f\" ] || continue",
    "  printf 'key: %s\\nstate: saved\\nbytes: %s\\nmodified: %s\\n\\n' \\",
    "    \"$(basename \"$f\" .rds)\" \"$(size \"$f\")\" \"$(mtime \"$f\")\"",
    "done",
    "for l in \"$d\"/*.lock; do",
    "  [ -d \"$l\" ] || continue",
    "  beat=$(mtime \"$l/heartbeat\")",
    "  [ -n \"$beat\" ] || beat=$(mtime \"$l\")",
    "  printf 'key: %s\\nstate: lock\\nmodified: %s\\nheartbeat_age: %s\\n' \\",
    "    \"$(basename \"$l\" .lock)\" \"$beat\" \"$((now - beat))\"",
    "  [ -s \"$l/owner.dcf\" ] && sed -n -e 's/^worker: /owner_worker: /p' \\",
    "    -e 's/^session: /owner_session: /p' \"$l/owner.dcf\"",
    "  echo",
    "done",
    "exit 0",
    sep = "\n"
  )
}

# Stop each worker the way it was started. Signalling the recorded process
# stops a worker that runs directly. For a container it reaches only the
# runtime's client, which forwards SIGTERM to the container's first process,
# and R ignores SIGTERM there unless the container was started with --init. So
# the container itself is killed through the runtime, by the ID it recorded.
# A process ID only counts while its command line still names its session,
# because the system reuses process IDs.
remote_stop_script <- function(pids, sessions, cid_files, runtimes) {
  runtimes[is.na(runtimes)] <- ""
  calls <- paste(
    "crew_ssh_stop",
    shQuote(pids, type = "sh"),
    shQuote(sessions, type = "sh"),
    shQuote(cid_files, type = "sh"),
    shQuote(runtimes, type = "sh")
  )
  paste(
    "crew_ssh_stop() {",
    "  case \"$(ps -p \"$1\" -o args= 2>/dev/null)\" in",
    "    *\"$2\"*) kill \"$1\" 2>/dev/null ;;",
    "  esac",
    "  if [ -n \"$4\" ] && [ -s \"$3\" ]; then",
    "    \"$4\" kill \"$(cat \"$3\")\" > /dev/null 2>&1",
    "  fi",
    "  return 0",
    "}",
    paste(calls, collapse = "\n"),
    "exit 0",
    sep = "\n"
  )
}

remote_dcf <- function(text) {
  if (!nzchar(trimws(text))) {
    return(NULL)
  }
  connection <- textConnection(text)
  on.exit(close(connection))
  read.dcf(connection, all = TRUE)
}

remote_epoch <- function(x) {
  as.POSIXct(as.numeric(x), origin = "1970-01-01")
}

# A field that some records lack, or that no record has at all, is NA.
remote_field <- function(records, name, rows) {
  if (is.null(records) || !(name %in% names(records))) {
    return(rep(NA_character_, rows))
  }
  out <- as.character(records[[name]])
  out[!is.na(out) & !nzchar(out)] <- NA_character_
  out
}

remote_workers_frame <- function(text, host, remote_directory, session) {
  records <- remote_dcf(text)
  rows <- if (is.null(records)) 0L else nrow(records)
  sessions <- remote_field(records, "session", rows)
  container_running <- remote_field(records, "container_running", rows)
  process_alive <- remote_field(records, "process_alive", rows) %in% "TRUE"
  data.frame(
    host = rep(host, rows),
    session = sessions,
    session_directory = paste0(
      remote_directory, "/", sessions,
      recycle0 = TRUE
    ),
    worker = remote_field(records, "worker", rows),
    pid = as.integer(remote_field(records, "pid", rows)),
    container = remote_field(records, "container", rows),
    runtime = remote_field(records, "runtime", rows),
    alive = process_alive | container_running %in% "true",
    session_alive = remote_field(records, "session_alive", rows) %in% "TRUE",
    current = sessions %in% session,
    task_key = remote_field(records, "task_key", rows),
    task_name = remote_field(records, "task_name", rows),
    task_state = remote_field(records, "task_state", rows),
    task_started = remote_epoch(remote_field(records, "task_started", rows)),
    task_ended = remote_epoch(remote_field(records, "task_ended", rows)),
    task_updated = remote_epoch(remote_field(records, "task_updated", rows)),
    stringsAsFactors = FALSE
  )
}

remote_salvage_frame <- function(text, directory, seconds_stale) {
  records <- remote_dcf(text)
  rows <- if (is.null(records)) 0L else nrow(records)
  keys <- remote_field(records, "key", rows)
  state <- remote_field(records, "state", rows)
  heartbeat_age <- as.numeric(remote_field(records, "heartbeat_age", rows))
  lock <- !is.na(state) & state == "lock"
  state[lock] <- ifelse(
    heartbeat_age[lock] < seconds_stale,
    "running",
    "abandoned"
  )
  data.frame(
    key = keys,
    state = state,
    bytes = as.numeric(remote_field(records, "bytes", rows)),
    modified = remote_epoch(remote_field(records, "modified", rows)),
    heartbeat_age = heartbeat_age,
    worker = remote_field(records, "owner_worker", rows),
    session = remote_field(records, "owner_session", rows),
    path = paste0(
      directory, "/", keys, ifelse(lock, ".lock", ".rds"),
      recycle0 = TRUE
    ),
    stringsAsFactors = FALSE
  )
}
