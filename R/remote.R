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

#
# `states`, when given, limits the workers to those whose latest task is in one
# of these states, and skips the others before the process and container
# checks, which over many old sessions are what take the time.
remote_workers_script <- function(remote_directory, states = NULL) {
  filter <- if (is.null(states)) {
    character(0L)
  } else {
    c(
      "    st=",
      "    [ -s \"$d$w.task\" ] && st=$(sed -n 's/^task_state: //p' \"$d$w.task\")",
      paste0(
        "    case \"$st\" in ",
        paste(shQuote(states, type = "sh"), collapse = "|"),
        ") ;; *) continue ;; esac"
      )
    )
  }
  lines <- c(
    paste0(
      "for d in ", shQuote(remote_directory, type = "sh"), "/crew-ssh-*/; do"
    ),
    "  s=$(basename \"$d\")",
    "  rt=",
    "  lh=",
    "  lp=",
    "  lc=",
    "  if [ -f \"${d}session.dcf\" ]; then",
    "    rt=$(sed -n 's/^runtime: //p' \"${d}session.dcf\")",
    "    lh=$(sed -n 's/^local_host: //p' \"${d}session.dcf\")",
    "    lp=$(sed -n 's/^local_pid: //p' \"${d}session.dcf\")",
    "    lc=$(sed -n 's/^local_created: //p' \"${d}session.dcf\")",
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
    filter,
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
    "    printf 'local_host: %s\\nlocal_pid: %s\\nlocal_created: %s\\n' \\",
    "      \"$lh\" \"$lp\" \"$lc\"",
    "    printf 'worker: %s\\npid: %s\\nprocess_alive: %s\\n' \\",
    "      \"$w\" \"$pid\" \"$alive\"",
    "    printf 'container: %s\\ncontainer_running: %s\\nruntime: %s\\n' \\",
    "      \"$cid\" \"$running\" \"$rt\"",
    "    [ -s \"$d$w.task\" ] && cat \"$d$w.task\"",
    "    echo",
    "  done",
    "done",
    "exit 0"
  )
  paste(lines, collapse = "\n")
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
    remote_stop_function(),
    paste(calls, collapse = "\n"),
    "exit 0",
    sep = "\n"
  )
}

# crew_ssh_stop PID SESSION CID_FILE RUNTIME, as remote_stop_script() and
# remote_stop_waiters_script() use it.
remote_stop_function <- function() {
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
    sep = "\n"
  )
}

# Stop the workers that wait, under salvage, for a task another worker is
# running, once the tunnel of their own session is gone. Such a worker can no
# longer hand a result back, and the worker it waits for saves the result
# itself, so the waiter only holds a container and its memory until that task
# ends, which can be hours.
#
# This runs in the tunnel command of every session start and reconnect, before
# the session records its new tunnel, so it sees only tunnels opened earlier.
# `session` names the session opening the tunnel: when it reconnects, its own
# waiters lost their connection with the old tunnel, even while the remote end
# of that tunnel lingers because sshd has not yet noticed the client is gone.
# A worker's task file is read just before the stop, so a waiter that has
# since taken the task over is left running. Each stopped worker is reported on
# a line beginning "crew-ssh-stopped-waiter:".
#
# `dead` names sessions the controller knows have ended, because they were
# started on its own machine by an R process that is no longer running (see
# local_session_alive()). sshd can keep the remote end of such a session's
# tunnel for hours after its client is gone, which would make the session
# look connected, so for these the tunnel is not consulted: their waiters are
# stopped, and the lingering end of their tunnel is closed and reported on a
# line beginning "crew-ssh-closed-tunnel:".
remote_stop_waiters_script <- function(remote_directory, session,
                                       dead = character(0L)) {
  paste(
    remote_stop_function(),
    paste0("current=", shQuote(session, type = "sh")),
    paste0("dead=", shQuote(paste(dead, collapse = " "), type = "sh")),
    paste0(
      "for d in ", shQuote(remote_directory, type = "sh"), "/crew-ssh-*/; do"
    ),
    "  [ -d \"$d\" ] || continue",
    "  s=$(basename \"$d\")",
    "  ended=",
    "  case \" $dead \" in *\" $s \"*) ended=TRUE ;; esac",
    "  if [ -n \"$ended\" ] && [ -s \"${d}tunnel.pid\" ]; then",
    "    tp=$(cat \"${d}tunnel.pid\")",
    "    case \"$(ps -p \"$tp\" -o args= 2>/dev/null)\" in",
    "      *\"$s\"*) kill \"$tp\" 2>/dev/null &&",
    "        printf 'crew-ssh-closed-tunnel: %s\\n' \"$s\" ;;",
    "    esac",
    "  fi",
    "  if [ \"$s\" != \"$current\" ] && [ -z \"$ended\" ] && [ -s \"${d}tunnel.pid\" ]; then",
    "    case \"$(ps -p \"$(cat \"${d}tunnel.pid\")\" -o args= 2>/dev/null)\" in",
    "      *\"$s\"*) continue ;;",
    "    esac",
    "  fi",
    "  rt=",
    "  if [ -f \"${d}session.dcf\" ]; then",
    "    rt=$(sed -n 's/^runtime: //p' \"${d}session.dcf\")",
    "  fi",
    "  for t in \"$d\"worker-*.task; do",
    "    [ -f \"$t\" ] || continue",
    "    grep -qx 'task_state: waiting' \"$t\" || continue",
    "    w=$(basename \"$t\" .task)",
    "    pid=",
    "    [ -s \"$d$w.pid\" ] && pid=$(cat \"$d$w.pid\")",
    "    alive=",
    "    case \"$(ps -p \"$pid\" -o args= 2>/dev/null)\" in",
    "      *\"$s\"*) alive=TRUE ;;",
    "    esac",
    "    if [ -n \"$rt\" ] && [ -s \"$d$w.cid\" ]; then",
    "      r=$(\"$rt\" inspect --format '{{.State.Running}}' \\",
    "        \"$(cat \"$d$w.cid\")\" 2>/dev/null) && [ \"$r\" = true ] && alive=TRUE",
    "    fi",
    "    [ -n \"$alive\" ] || continue",
    "    crew_ssh_stop \"$pid\" \"$s\" \"$d$w.cid\" \"$rt\"",
    "    printf 'crew-ssh-stopped-waiter: %s %s %s\\n' \"$s\" \"$w\" \\",
    "      \"$(sed -n 's/^task_name: //p' \"$t\")\"",
    "  done",
    "done",
    sep = "\n"
  )
}

# The tasks still running on workers the controller cannot reach, as a data
# frame of their `task_key` and `task_name`: tasks of workers of other
# sessions whose tunnel is gone, and of this session's workers named in
# `disconnected`, which it launched before its tunnel was last reopened.
# `workers` is a remote_workers_frame().
remote_unreachable_tasks <- function(workers, disconnected) {
  cut_off <- ifelse(
    workers$current,
    workers$worker %in% disconnected,
    !workers$session_alive
  )
  running <- workers$alive &
    workers$task_state %in% "running" &
    !is.na(workers$task_key) &
    cut_off
  out <- workers[running, c("task_key", "task_name"), drop = FALSE]
  out <- out[!duplicated(out$task_key), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# The workers remote_stop_waiters_script() reports in `output`, each as
# "<worker> of <session> (<task>)", or without the task when its record
# names none.
remote_stopped_waiters <- function(output) {
  pattern <- "^crew-ssh-stopped-waiter: (\\S+) (\\S+) ?(.*)$"
  lines <- grep(pattern, trimws(output), value = TRUE, perl = TRUE)
  labels <- sub(pattern, "\\2 of \\1", lines, perl = TRUE)
  tasks <- sub(pattern, "\\3", lines, perl = TRUE)
  named <- nzchar(tasks)
  labels[named] <- paste0(labels[named], " (", tasks[named], ")")
  labels
}

# The sessions whose lingering tunnel end remote_stop_waiters_script()
# reports closing in `output`.
remote_closed_tunnels <- function(output) {
  pattern <- "^crew-ssh-closed-tunnel: (\\S+)$"
  lines <- grep(pattern, trimws(output), value = TRUE, perl = TRUE)
  sub(pattern, "\\1", lines, perl = TRUE)
}

# The record of every session under `remote_directory`, as written by its
# tunnel command, one DCF record per session.
remote_sessions_script <- function(remote_directory) {
  paste(
    paste0(
      "for d in ", shQuote(remote_directory, type = "sh"), "/crew-ssh-*/; do"
    ),
    "  [ -s \"${d}session.dcf\" ] || continue",
    "  cat \"${d}session.dcf\"",
    "  echo",
    "done",
    "exit 0",
    sep = "\n"
  )
}

# The sessions in remote_sessions_script() output `text` that were started on
# this machine by an R process that has ended, other than `current`.
remote_dead_sessions <- function(text, current,
                                 nodename = Sys.info()[["nodename"]]) {
  records <- remote_dcf(text)
  rows <- if (is.null(records)) 0L else nrow(records)
  sessions <- remote_field(records, "session", rows)
  alive <- local_session_alive(
    local_host = remote_field(records, "local_host", rows),
    local_pid = as.integer(remote_field(records, "local_pid", rows)),
    local_created = as.numeric(remote_field(records, "local_created", rows)),
    nodename = nodename
  )
  dead <- sessions[alive %in% FALSE & !is.na(sessions)]
  setdiff(dead, current)
}

# Whether the R process that started each session is still running, judged on
# this machine: TRUE or FALSE for a session started here (`nodename`), and NA
# for one started on another machine or recorded without its process, which
# only the remote end of its tunnel can speak for. Host names compare without
# regard to case, as Windows treats them.
local_session_alive <- function(local_host, local_pid, local_created,
                                nodename = Sys.info()[["nodename"]]) {
  out <- rep(NA, length(local_host))
  here <- !is.na(local_host) &
    toupper(local_host) == toupper(nodename) &
    !is.na(local_pid)
  if (any(here)) {
    out[here] <- mapply(
      local_process_alive,
      local_pid[here],
      local_created[here],
      USE.NAMES = FALSE
    )
  }
  out
}

# Whether process `pid` is running and is the one created at `created`
# (seconds since the epoch). The system gives the PID of a process that ended
# to a later one, whose creation time differs. A record without a creation
# time, from a session started by an earlier crew.ssh, is judged by its PID
# alone.
local_process_alive <- function(pid, created) {
  handle <- tryCatch(ps::ps_handle(as.integer(pid)), error = identity)
  if (inherits(handle, "error")) {
    return(FALSE)
  }
  if (is.na(created)) {
    return(isTRUE(ps::ps_is_running(handle)))
  }
  started <- tryCatch(as.numeric(ps::ps_create_time(handle)), error = identity)
  if (inherits(started, "error")) {
    return(FALSE)
  }
  abs(started - created) < 1
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
    local_host = remote_field(records, "local_host", rows),
    local_pid = as.integer(remote_field(records, "local_pid", rows)),
    local_created = as.numeric(remote_field(records, "local_created", rows)),
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
