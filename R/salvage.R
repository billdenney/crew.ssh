# Salvage ----
#
# With `salvage = TRUE`, every named task runs inside salvage_run() on the
# worker, which saves the outcome of the task on the remote system before
# handing it back. A result whose delivery fails, because the local session or
# the ssh connection died while the task ran, therefore survives on the remote
# system, and a later push of the same task reads it instead of running the
# task again.
#
# The code below comes in two halves.
#
# * The controller side runs locally. salvage_key() names a task by its
#   content, so the same task pushed by a later session gets the same key.
# * The worker side runs on the remote system. salvage_run() and the functions
#   it calls travel inside each task (see salvage_shipped()), so the remote
#   system does not need crew.ssh installed. These functions may only use base
#   R, plus packages that every crew worker has and that they call with `::`.
#   A test checks this.

# Controller side ----

# The key of a task: a readable prefix from the task name and a hash of
# everything the task computes from. Two pushes get the same key exactly when
# they would compute the same thing, so a saved result can stand in for a run.
salvage_key <- function(
  command,
  data,
  globals_hash,
  seed,
  algorithm,
  packages,
  library,
  name
) {
  visited <- salvage_visited()
  fingerprint <- if (salvage_is_targets(command, data)) {
    salvage_fingerprint_targets(
      data = data,
      name = name,
      globals_hash = globals_hash,
      visited = visited
    )
  } else {
    list(
      kind = "crew",
      name = name,
      command = salvage_canonical(command, visited),
      data = salvage_canonical(data, visited),
      globals = globals_hash,
      seed = seed,
      algorithm = algorithm,
      packages = packages,
      library = library
    )
  }
  paste0(salvage_prefix(name), "-", rlang::hash(fingerprint))
}

# targets ships the whole global environment of `_targets.R` with every task,
# so the hash of the globals is computed once and reused while they stay the
# same object. `.Random.seed` changes whenever the local session draws a
# random number and does not change what a task computes: crew and targets
# seed each task themselves.
salvage_globals_hash <- function(globals) {
  globals <- globals[setdiff(names(globals), ".Random.seed")]
  rlang::hash(salvage_canonical(globals, salvage_visited()))
}

salvage_is_targets <- function(command, data) {
  is.call(command) &&
    identical(command[[1L]], quote(targets::target_run_worker)) &&
    inherits(.subset2(data, "target"), "tar_target")
}

# A targets task carries the whole target object, whose upstream targets
# record when their data was last written. That time changes whenever an
# upstream target reruns, even with an identical value (for example under
# `tar_cue(mode = "always")`), so the target object is not hashed as a whole.
# The fingerprint takes the fields that decide what the target computes:
# its command, seed, storage settings, and the hashes of the data it depends
# on.
salvage_fingerprint_targets <- function(data, name, globals_hash, visited) {
  target <- .subset2(data, "target")
  command <- .subset2(target, "command")
  settings <- .subset2(target, "settings")
  hash <- .subset2(command, "hash")
  crew::crew_assert(
    is.character(hash) && length(hash) == 1L && !is.na(hash) && nzchar(hash),
    message = paste(
      "crew.ssh salvage cannot read the command hash of target",
      shQuote(name), "from the task targets pushed. This version of",
      "targets may store targets differently; set salvage = FALSE."
    )
  )
  list(
    kind = "targets",
    name = name,
    target = .subset2(settings, "name"),
    command = hash,
    packages = .subset2(command, "packages"),
    library = .subset2(command, "library"),
    seed = .subset2(target, "seed"),
    deps = .subset2(target, "deps"),
    index = .subset2(target, "index"),
    format = .subset2(settings, "format"),
    repository = .subset2(settings, "repository"),
    iteration = .subset2(settings, "iteration"),
    storage = .subset2(settings, "storage"),
    retrieval = .subset2(settings, "retrieval"),
    pattern = .subset2(settings, "pattern"),
    upstream = salvage_upstream(target),
    envir = salvage_canonical(.subset2(data, "envir"), visited),
    path_store = .subset2(data, "path_store"),
    fun = .subset2(data, "fun"),
    options = salvage_canonical(.subset2(data, "options"), visited),
    envvars = .subset2(data, "envvars"),
    globals = globals_hash
  )
}

salvage_upstream <- function(target) {
  subpipeline <- .subset2(target, "subpipeline")
  if (is.null(subpipeline)) {
    return(list())
  }
  upstream <- .subset2(subpipeline, "targets")
  names <- sort(ls(upstream, all.names = TRUE), method = "radix")
  lapply(names, salvage_upstream_hash, upstream = upstream)
}

salvage_upstream_hash <- function(name, upstream) {
  target <- get(name, envir = upstream, inherits = FALSE)
  hash <- .subset2(.subset2(target, "file"), "hash")
  if (
    is.character(hash) && length(hash) == 1L && !is.na(hash) && nzchar(hash)
  ) {
    return(c(name, hash))
  }
  # A bud is one slice of an upstream value and a pattern stands for its
  # branches, which the subpipeline lists separately. targets derives the
  # names of both from the data, so the name alone identifies them.
  crew::crew_assert(
    inherits(target, c("tar_bud", "tar_pattern")),
    message = paste(
      "crew.ssh salvage cannot find the hash of the data of upstream target",
      shQuote(name), "in the task targets pushed. This version of targets",
      "may store targets differently; set salvage = FALSE."
    )
  )
  c(name, class(target)[1L])
}

# Saved results are files named after their key, so the prefix keeps to
# characters that need no quoting anywhere and cannot start an option.
salvage_prefix <- function(name) {
  prefix <- substr(gsub("[^A-Za-z0-9._-]", "_", name), 1L, 64L)
  prefix <- sub("^[.-]+", "", prefix)
  if (nzchar(prefix)) prefix else "task"
}

# Canonical form ----
#
# rlang::hash() hashes environments by identity, and with them every closure
# and every formula, so the hash of the same task differs from one session to
# the next. salvage_canonical() rewrites an object into plain data with the
# same content: environments become their sorted bindings, closures their
# formals, body and environment, and the global environment, base, namespaces
# and attached packages their names. The result hashes the same in every
# session.
#
# Some objects become a fixed marker instead of their content:
#
# * crew controllers, which hold sockets and process handles that differ in
#   every session and never change what a task computes;
# * external pointers, which reach a worker as null pointers whatever they
#   pointed to (salvage refuses custom serialization, the one way they could
#   travel).
#
# Calls stay as they are. A call with an object inlined into it hashes by
# identity and so never matches across sessions, which can only cost a rerun.

salvage_visited <- function() {
  visited <- new.env(hash = TRUE, parent = emptyenv())
  visited$.count <- 0L
  visited
}

salvage_canonical <- function(x, visited) {
  type <- typeof(x)
  if (identical(type, "environment")) {
    return(salvage_canonical_environment(x, visited))
  }
  if (identical(type, "closure")) {
    return(salvage_canonical_closure(x, visited))
  }
  if (type %in% c("builtin", "special")) {
    return(list(type = type, name = deparse(x)))
  }
  if (type %in% c("externalptr", "weakref")) {
    return(list(type = type))
  }
  # Code parsed with keep.source = TRUE, as at the console, carries the file
  # it came from, with a timestamp.
  if (is.language(x)) {
    x <- utils::removeSource(x)
  }
  if (!salvage_needs_canonical(x)) {
    return(x)
  }
  value <- if (identical(type, "list")) {
    unname(lapply(unclass(x), salvage_canonical, visited = visited))
  } else if (identical(type, "S4")) {
    NULL
  } else {
    `attributes<-`(x, NULL)
  }
  list(
    type = type,
    value = value,
    attributes = salvage_canonical_attributes(x, visited)
  )
}

# Whether an object holds anything rlang::hash() would hash by identity. Plain
# data, the common case, goes to rlang::hash() untouched, which avoids copying
# large vectors.
salvage_needs_canonical <- function(x) {
  type <- typeof(x)
  if (
    type %in%
      c(
        "environment", "closure", "builtin", "special",
        "externalptr", "weakref", "S4"
      )
  ) {
    return(TRUE)
  }
  if (identical(type, "list")) {
    for (index in seq_along(x)) {
      if (salvage_needs_canonical(.subset2(x, index))) {
        return(TRUE)
      }
    }
  }
  attributes <- attributes(x)
  for (name in setdiff(names(attributes), "names")) {
    if (salvage_needs_canonical(attributes[[name]])) {
      return(TRUE)
    }
  }
  FALSE
}

salvage_canonical_attributes <- function(x, visited) {
  attributes <- attributes(x)
  if (is.null(attributes)) {
    return(NULL)
  }
  attributes <- attributes[order(names(attributes), method = "radix")]
  lapply(attributes, salvage_canonical, visited = visited)
}

salvage_canonical_closure <- function(x, visited) {
  x <- utils::removeSource(x)
  list(
    type = "closure",
    formals = salvage_canonical(formals(x), visited),
    body = salvage_canonical(body(x), visited),
    environment = salvage_canonical(environment(x), visited),
    attributes = salvage_canonical_attributes(x, visited)
  )
}

salvage_canonical_environment <- function(x, visited) {
  name <- salvage_environment_name(x)
  if (!is.null(name)) {
    return(list(type = "environment", name = name))
  }
  if (inherits(x, c("crew_class_controller", "crew_class_controller_group"))) {
    return(list(type = "crew controller"))
  }
  # Environments can refer to themselves, so each is expanded once and later
  # visits record the order of the first one, which is the same in every
  # session because bindings are visited in sorted order.
  address <- rlang::obj_address(x)
  seen <- visited[[address]]
  if (!is.null(seen)) {
    return(list(type = "cycle", index = seen))
  }
  visited$.count <- visited$.count + 1L
  assign(address, visited$.count, envir = visited)
  names <- sort(ls(x, all.names = TRUE), method = "radix")
  list(
    type = "environment",
    names = names,
    values = lapply(
      names,
      salvage_canonical_binding,
      environment = x,
      visited = visited
    ),
    attributes = salvage_canonical_attributes(x, visited)
  )
}

# Reading an active binding runs code, possibly with side effects, so the
# binding counts as present without being read. The `...` of a function's
# frame holds promises, which a closure made in that frame sees as their
# values. Reading a promise runs the code behind it, whose warnings would only
# confuse at push time.
salvage_canonical_binding <- function(name, environment, visited) {
  if (bindingIsActive(name, environment)) {
    return(list(type = "active binding"))
  }
  value <- suppressWarnings(
    tryCatch(
      if (identical(name, "...")) {
        eval(quote(list(...)), envir = environment)
      } else {
        get(name, envir = environment, inherits = FALSE)
      },
      error = identity
    )
  )
  salvage_canonical(value, visited)
}

salvage_environment_name <- function(x) {
  if (identical(x, globalenv())) {
    return("R_GlobalEnv")
  }
  if (identical(x, baseenv())) {
    return("base")
  }
  if (identical(x, emptyenv())) {
    return("R_EmptyEnv")
  }
  if (isNamespace(x)) {
    return(paste0("namespace:", getNamespaceName(x)))
  }
  name <- environmentName(x)
  if (startsWith(name, "package:")) {
    return(name)
  }
  NULL
}

# Shipping ----

# The worker-side functions and the settings they need, as one list that
# travels inside each task. Each function loses its bytecode, which could
# refer to the crew.ssh namespace, and gets the base environment as its
# enclosure, so it runs on a worker that does not have crew.ssh.
salvage_shipped <- function(
  directory,
  session,
  session_directory,
  controller,
  seconds
) {
  functions <- list(
    run = salvage_run,
    read = salvage_read,
    problem = salvage_problem,
    outcome = salvage_outcome,
    reason = salvage_reason,
    write = salvage_write,
    lock = salvage_lock,
    lock_age = salvage_lock_age,
    lock_owner = salvage_lock_owner,
    unlock = salvage_unlock,
    heartbeat = salvage_heartbeat,
    state = salvage_state,
    versions = salvage_versions,
    installed_version = salvage_installed_version,
    stamp = salvage_stamp
  )
  c(
    lapply(functions, salvage_detach),
    list(
      directory = directory,
      session = session,
      session_directory = session_directory,
      controller = controller,
      seconds_heartbeat = seconds[["heartbeat"]],
      seconds_stale = seconds[["stale"]],
      seconds_poll = seconds[["poll"]]
    )
  )
}

salvage_detach <- function(fun) {
  body(fun) <- body(fun)
  environment(fun) <- baseenv()
  fun
}

# Worker side ----
#
# Everything from here on runs on the worker. See the note at the top of the
# file about what these functions may call.

# Run one task, or hand back its saved result.
#
# * A saved result that ran successfully under the same R and package versions
#   is returned without running the task.
# * A task another worker is running right now, which the heartbeat of its
#   lock shows, is waited for rather than run twice. This is what happens when
#   a session reconnects while a worker of the previous session is still busy.
# * Otherwise the task runs under a lock, and its value or error is saved
#   before it is returned or rethrown.
salvage_run <- function(expr, envir, key, name, reuse, salvage) {
  worker <- Sys.getenv("CREW_SSH_WORKER")
  # A task can also reach a worker crew.ssh did not launch, for example one of
  # a backup controller, which has no remote directory to save into.
  if (!nzchar(worker)) {
    return(eval(expr, envir = envir))
  }
  directory <- salvage$directory
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  if (!dir.exists(directory)) {
    stop(
      "crew.ssh cannot create the salvage directory ", directory,
      " on ", Sys.info()[["nodename"]], ".",
      call. = FALSE
    )
  }
  path <- file.path(directory, paste0(key, ".rds"))
  lock <- file.path(directory, paste0(key, ".lock"))
  explained <- ""
  waiting <- FALSE
  repeat {
    record <- if (reuse) salvage$read(path, key, salvage) else NULL
    reason <- if (is.null(record)) "" else salvage$reason(record, salvage)
    if (!is.null(record) && !nzchar(reason)) {
      message(
        "crew.ssh: task ", name, " reuses the result worker ", record$worker,
        " saved at ", format(record$ended, usetz = TRUE), "."
      )
      salvage$state(salvage, worker, key, name, "reused")
      return(record$value)
    }
    if (nzchar(reason) && !identical(reason, explained)) {
      message(
        "crew.ssh: task ", name, " runs again instead of reusing its saved ",
        "result because ", reason, "."
      )
      explained <- reason
    }
    owner <- salvage$lock(lock, worker, salvage)
    if (isTRUE(owner)) {
      break
    }
    if (!waiting) {
      message(
        "crew.ssh: task ", name, " is already running on worker ",
        owner[["worker"]], " of session ", owner[["session"]],
        ", so this worker waits for its result."
      )
      salvage$state(salvage, worker, key, name, "waiting")
      waiting <- TRUE
    }
    Sys.sleep(salvage$seconds_poll)
  }
  token <- attr(owner, "token")
  previous <- attr(owner, "previous")
  if (!is.null(previous)) {
    message(
      "crew.ssh: task ", name, " was running on worker ", previous[["worker"]],
      " of session ", previous[["session"]], ", which went silent ",
      previous[["silent"]], " seconds ago, so this worker runs it."
    )
  }
  # A task interrupted while it runs, as crew's cancel() and timeouts do,
  # leaves through here without a result, so its worker's record must say so.
  running <- FALSE
  on.exit(
    {
      if (running) {
        salvage$state(salvage, worker, key, name, "interrupted")
      }
      salvage$unlock(lock, token)
    },
    add = TRUE
  )
  # The worker this one waited for may have saved its result between the last
  # read and the lock.
  if (reuse) {
    record <- salvage$read(path, key, salvage)
    if (!is.null(record) && !nzchar(salvage$reason(record, salvage))) {
      salvage$state(salvage, worker, key, name, "reused")
      return(record$value)
    }
  }
  # Temporary files left by a writer that died midway are never read, only
  # cleared here.
  leftovers <- list.files(directory, full.names = TRUE)
  unlink(leftovers[startsWith(basename(leftovers), paste0(key, ".rds.tmp."))])
  salvage$heartbeat(lock, salvage$seconds_heartbeat, salvage$seconds_stale)
  started <- Sys.time()
  salvage$state(salvage, worker, key, name, "running", started = started)
  running <- TRUE
  out <- tryCatch(list(value = eval(expr, envir = envir)), error = identity)
  running <- FALSE
  ended <- Sys.time()
  failed <- inherits(out, "error")
  outcome <- if (failed) {
    list(status = "error", error = conditionMessage(out), class = class(out))
  } else {
    salvage$outcome(out$value)
  }
  record <- list(
    format = "crew.ssh.salvage",
    version = 1L,
    key = key,
    name = name,
    status = outcome$status,
    value = if (identical(outcome$status, "success")) out$value,
    error = outcome$error,
    error_class = outcome$class,
    controller = salvage$controller,
    session = salvage$session,
    worker = worker,
    nodename = Sys.info()[["nodename"]],
    pid = Sys.getpid(),
    started = started,
    ended = ended,
    r_version = R.version.string,
    packages = salvage$versions()
  )
  # The task itself is done, so a result that cannot be saved still goes back
  # to the controller, with a warning that it would not survive a disconnect.
  saved <- tryCatch(salvage$write(record, path, salvage), error = identity)
  if (inherits(saved, "error")) {
    warning(
      "crew.ssh could not save the result of task ", name, " to ", path,
      ", so it would be lost if the connection dropped: ",
      conditionMessage(saved),
      call. = FALSE
    )
  }
  salvage$state(
    salvage, worker, key, name, record$status,
    started = started,
    ended = ended
  )
  if (failed) {
    stop(out)
  }
  out$value
}

# How a task that returned ended. targets catches an error in a target's
# command and returns the target with the error in its metrics, so to crew the
# task succeeded; to salvage it failed, and must not stand in for a rerun.
salvage_outcome <- function(value) {
  if (inherits(value, "tar_target")) {
    metrics <- .subset2(value, "metrics")
    error <- .subset2(metrics, "error")
    if (!is.null(error)) {
      return(
        list(
          status = "error",
          error = paste(error, collapse = "\n"),
          class = .subset2(metrics, "error_class")
        )
      )
    }
    if (!is.null(.subset2(metrics, "cancel"))) {
      return(
        list(
          status = "error",
          error = "the target was canceled",
          class = NA_character_
        )
      )
    }
  }
  list(status = "success", error = NA_character_, class = NA_character_)
}

# NULL when nothing is saved, the record when a valid one is, and an error when
# something is saved that cannot be read: a damaged file is never mistaken for
# a missing one.
salvage_read <- function(path, key, salvage) {
  if (!file.exists(path)) {
    return(NULL)
  }
  record <- tryCatch(readRDS(path), error = identity)
  problem <- salvage$problem(record, key)
  if (nzchar(problem)) {
    stop(
      structure(
        class = c("crew_ssh_salvage_unreadable", "error", "condition"),
        list(
          message = paste0(
            "crew.ssh cannot use the saved result at ", path, ": ", problem,
            ". Delete the file to run the task again."
          ),
          call = NULL
        )
      )
    )
  }
  record
}

salvage_problem <- function(record, key) {
  if (inherits(record, "error")) {
    return(paste("reading it failed with", conditionMessage(record)))
  }
  if (!is.list(record) || !identical(record$format, "crew.ssh.salvage")) {
    return("it is not a crew.ssh salvage record")
  }
  if (!identical(record$version, 1L)) {
    return(
      paste(
        "it has record version", format(record$version),
        "and this version of crew.ssh reads version 1"
      )
    )
  }
  if (!identical(record$key, key)) {
    return(paste("it belongs to task key", format(record$key)))
  }
  if (identical(record$status, "success")) {
    if (!("value" %in% names(record))) {
      return("it has no value")
    }
  } else if (!identical(record$status, "error")) {
    return(paste("its status is", format(record$status)))
  }
  if (
    !inherits(record$ended, "POSIXct") || !inherits(record$started, "POSIXct")
  ) {
    return("it has no start and end time")
  }
  # salvage_reason() compares these with the worker before any reuse.
  if (
    !is.character(record$r_version) ||
      length(record$r_version) != 1L ||
      !is.character(record$packages) ||
      is.null(names(record$packages))
  ) {
    return("it does not record the R and package versions it ran with")
  }
  ""
}

# Why a valid record cannot stand in for running the task, or "" if it can. A
# saved error is never reused, the same as targets, which reruns a target that
# errored. Neither is a result from other software: the remote system may have
# been updated since, for example to fix a bug the task depends on.
salvage_reason <- function(record, salvage) {
  if (!identical(record$status, "success")) {
    return(paste("its last run failed with", record$error))
  }
  if (!identical(record$r_version, R.version.string)) {
    return(
      paste0(
        "it ran under ", record$r_version, " and this worker has ",
        R.version.string
      )
    )
  }
  recorded <- record$packages
  current <- vapply(
    names(recorded),
    salvage$installed_version,
    character(1L),
    USE.NAMES = FALSE
  )
  changed <- is.na(current) | current != recorded
  if (!any(changed)) {
    return("")
  }
  current[is.na(current)] <- "not installed"
  paste(
    "it ran with other package versions than this worker has:",
    paste0(
      names(recorded)[changed], " ", recorded[changed], " then, ",
      current[changed], " now",
      collapse = "; "
    )
  )
}

salvage_installed_version <- function(package) {
  version <- suppressWarnings(
    utils::packageDescription(package, fields = "Version")
  )
  if (is.character(version)) version else NA_character_
}

salvage_versions <- function() {
  namespaces <- sort(loadedNamespaces(), method = "radix")
  vapply(namespaces, getNamespaceVersion, character(1L))
}

# Write to a temporary file and rename it into place. A rename within one
# directory is atomic, so a reader sees the whole record or none of it, even if
# the writer dies midway. The temporary name avoids R's random number
# generator, which the task it saves may depend on.
salvage_write <- function(record, path, salvage) {
  temporary <- paste0(path, ".tmp.", salvage$stamp())
  on.exit(unlink(temporary))
  # A file that cannot be opened is an error, and the warning that comes with
  # it says nothing more.
  suppressWarnings(saveRDS(record, temporary))
  if (!suppressWarnings(file.rename(temporary, path))) {
    stop("cannot rename ", temporary, " to ", path, call. = FALSE)
  }
  invisible(path)
}

salvage_stamp <- function() {
  paste0(Sys.getpid(), ".", sprintf("%.0f", as.numeric(Sys.time()) * 1e6))
}

# Take the lock directory of a task, which mkdir creates atomically. Returns
# TRUE with the lock's token as an attribute, or, if a live worker holds the
# lock, a named character vector describing that worker. A lock whose heartbeat
# went quiet belongs to a worker that died, and is taken over: renaming it
# away is atomic, so of several workers trying, exactly one succeeds. That one
# gets the dead worker's owner record as the "previous" attribute, with the
# seconds it was silent.
salvage_lock <- function(lock, worker, salvage) {
  previous <- NULL
  for (attempt in seq_len(10L)) {
    if (dir.create(lock, showWarnings = FALSE)) {
      token <- salvage$stamp()
      owner <- data.frame(
        worker = worker,
        session = salvage$session,
        controller = salvage$controller,
        nodename = Sys.info()[["nodename"]],
        pid = Sys.getpid(),
        started = sprintf("%.3f", as.numeric(Sys.time())),
        token = token
      )
      write.dcf(owner, file = file.path(lock, "owner.dcf"))
      file.create(file.path(lock, "heartbeat"))
      return(structure(TRUE, token = token, previous = previous))
    }
    age <- salvage$lock_age(lock)
    if (is.na(age)) {
      if (!dir.exists(dirname(lock)) || file.access(dirname(lock), 2L) != 0L) {
        break
      }
      next
    }
    if (age < salvage$seconds_stale) {
      return(salvage$lock_owner(lock))
    }
    dead <- salvage$lock_owner(lock)
    abandoned <- paste0(lock, ".abandoned.", salvage$stamp())
    if (suppressWarnings(file.rename(lock, abandoned))) {
      previous <- c(dead, silent = sprintf("%.0f", age))
      unlink(abandoned, recursive = TRUE)
    }
  }
  stop("crew.ssh cannot lock ", lock, ".", call. = FALSE)
}

# Seconds since the lock last showed a sign of life, or NA if it is gone.
salvage_lock_age <- function(lock) {
  times <- file.mtime(c(lock, file.path(lock, "heartbeat")))
  times <- times[!is.na(times)]
  if (!length(times)) {
    return(NA_real_)
  }
  as.numeric(difftime(Sys.time(), max(times), units = "secs"))
}

salvage_lock_owner <- function(lock) {
  owner <- tryCatch(
    suppressWarnings(read.dcf(file.path(lock, "owner.dcf")))[1L, ],
    error = identity
  )
  # The lock is new and its owner has not written the file yet.
  if (inherits(owner, "error")) {
    owner <- c(worker = "unknown", session = "unknown")
  }
  owner
}

# Remove the lock only if this worker still holds it. A worker whose heartbeat
# was taken for dead may finish after another worker took the lock over, and
# must leave that worker's lock alone.
salvage_unlock <- function(lock, token) {
  owner <- tryCatch(
    suppressWarnings(
      read.dcf(file.path(lock, "owner.dcf"), fields = "token")
    )[1L, 1L],
    error = identity
  )
  if (identical(unname(owner), token)) {
    unlink(lock, recursive = TRUE)
  }
  invisible()
}

# R cannot update a file while it is busy in compiled code, which is where a
# long task spends its time, so a background shell touches the heartbeat
# instead. It stops when the lock is gone or the R process exits. Without it a
# session that reconnects would take the lock of a task still running for a
# dead one, so a worker that cannot start it says so.
salvage_heartbeat <- function(lock, seconds, stale) {
  script <- paste(
    "while [ -d \"$1\" ] && kill -0 \"$2\" 2>/dev/null; do",
    "touch \"$1/heartbeat\" 2>/dev/null; sleep \"$3\"; done"
  )
  started <- if (nzchar(Sys.which("sh"))) {
    tryCatch(
      system2(
        command = "sh",
        args = c(
          "-c",
          shQuote(script, type = "sh"),
          "crew-ssh-heartbeat",
          shQuote(lock, type = "sh"),
          Sys.getpid(),
          seconds
        ),
        stdout = FALSE,
        stderr = FALSE,
        wait = FALSE
      ),
      error = identity
    )
  } else {
    simpleError("there is no sh on this worker")
  }
  if (inherits(started, "error")) {
    warning(
      "crew.ssh cannot keep the lock of this task alive (",
      conditionMessage(started), "), so a session that reconnects while the ",
      "task runs would take the lock for a dead worker and run the task ",
      "again once the lock has been quiet for ", stale, " seconds.",
      call. = FALSE
    )
  }
  invisible()
}

# Record which task a worker is on in the session directory, where
# `remote_workers()` reads it back with nothing but a shell.
salvage_state <- function(
  salvage,
  worker,
  key,
  name,
  state,
  started = NULL,
  ended = NULL
) {
  path <- file.path(salvage$session_directory, paste0(worker, ".task"))
  temporary <- paste0(path, ".tmp.", salvage$stamp())
  fields <- data.frame(
    task_key = key,
    task_name = name,
    task_state = state,
    task_updated = sprintf("%.3f", as.numeric(Sys.time()))
  )
  if (!is.null(started)) {
    fields$task_started <- sprintf("%.3f", as.numeric(started))
  }
  if (!is.null(ended)) {
    fields$task_ended <- sprintf("%.3f", as.numeric(ended))
  }
  written <- tryCatch(
    {
      write.dcf(fields, file = temporary)
      file.rename(temporary, path)
    },
    error = identity,
    warning = identity
  )
  if (!isTRUE(written)) {
    unlink(temporary)
    warning(
      "crew.ssh could not record the state of worker ", worker, " in ", path,
      ".",
      call. = FALSE
    )
  }
  invisible()
}
