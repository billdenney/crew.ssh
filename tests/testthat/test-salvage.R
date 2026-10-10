# Keys ----
#
# A key decides whether a saved result may stand in for running a task, so
# these tests pin both directions: everything a task computes from changes the
# key, and nothing else does, including what changes between sessions.

# Functions in _targets.R and at the console live in the global environment,
# so the test functions do too.
plus_one <- global_function(function(x) x + 1)
plus_two <- global_function(function(x) x + 2)

key_args <- function(...) {
  args <- list(
    command = quote(f(x)),
    data = list(x = 1),
    globals_hash = salvage_globals_hash(list(f = plus_one)),
    seed = 1L,
    algorithm = "L'Ecuyer-CMRG",
    packages = "stats",
    library = NULL,
    name = "fit"
  )
  utils::modifyList(args, list(...), keep.null = TRUE)
}

# quote = TRUE passes the command as a call instead of evaluating it.
key_of <- function(...) {
  do.call(salvage_key, key_args(...), quote = TRUE)
}

test_that("a key is the sanitized name and a 32-character hash", {
  key <- key_of()
  expect_match(key, "^fit-[0-9a-f]{32}$")
  expect_identical(key_of(), key)
})

test_that("every input of a task changes its key", {
  base <- key_of()
  changes <- list(
    command = quote(f(y)),
    data = list(x = 2),
    globals_hash = salvage_globals_hash(list(f = plus_two)),
    seed = 2L,
    algorithm = "Mersenne-Twister",
    packages = c("stats", "utils"),
    library = "/opt/R/library",
    name = "fit2"
  )
  # The contract covers every argument of salvage_key(), so the list of
  # changes has to as well.
  expect_setequal(names(changes), names(formals(salvage_key)))
  for (field in names(changes)) {
    args <- key_args()
    args[field] <- list(changes[[field]])
    expect_false(
      identical(do.call(salvage_key, args, quote = TRUE), base),
      info = field
    )
  }
})

test_that("a NULL seed gives a different key from a set one", {
  expect_false(
    identical(
      key_of(seed = NULL),
      key_of()
    )
  )
})

test_that("salvage_prefix() keeps file names plain and safe", {
  expect_identical(salvage_prefix("fit"), "fit")
  expect_identical(salvage_prefix("fit model 1"), "fit_model_1")
  expect_identical(salvage_prefix("a/b\\c$d"), "a_b_c_d")
  expect_identical(salvage_prefix("x_9.B-z"), "x_9.B-z")
  # A leading dot would hide the file and a leading dash could read as an
  # option.
  expect_identical(salvage_prefix(".hidden"), "hidden")
  expect_identical(salvage_prefix("--rf"), "rf")
  expect_identical(salvage_prefix(""), "task")
  expect_identical(salvage_prefix("..."), "task")
  expect_identical(salvage_prefix(strrep("a", 100L)), strrep("a", 64L))
})

test_that("names that sanitize alike still get different keys", {
  expect_false(
    identical(
      key_of(name = "a b"),
      key_of(name = "a_b")
    )
  )
})

test_that("the globals hash ignores .Random.seed and crew controllers", {
  base <- salvage_globals_hash(list(f = plus_one))
  expect_identical(
    salvage_globals_hash(list(f = plus_one, .Random.seed = 1:3)),
    base
  )
  expect_false(identical(salvage_globals_hash(list(f = plus_two)), base))
  first <- crew::crew_controller_local(name = "first")
  second <- crew::crew_controller_local(name = "second", workers = 3L)
  expect_identical(
    salvage_globals_hash(list(f = plus_one, controller = first)),
    salvage_globals_hash(list(f = plus_one, controller = second))
  )
})

# Canonical form ----

canonical_hash <- function(x) {
  rlang::hash(salvage_canonical(x, salvage_visited()))
}

test_that("plain data passes through untouched", {
  data <- list(a = 1:3, b = data.frame(x = c(1.5, 2), y = c("p", "q")))
  expect_identical(salvage_canonical(data, salvage_visited()), data)
  expect_identical(salvage_canonical(NULL, salvage_visited()), NULL)
})

test_that("environments hash by content, not identity", {
  first <- new.env()
  first$a <- 1
  first$b <- "two"
  second <- new.env()
  second$b <- "two"
  second$a <- 1
  expect_identical(canonical_hash(first), canonical_hash(second))
  second$a <- 2
  expect_false(identical(canonical_hash(first), canonical_hash(second)))
})

loop_two <- global_function(
  function(x) {
    for (i in 1:2) x <- x + 1
    x
  }
)

test_that("closures hash by code and enclosing content, not by bytecode or source", {
  plain <- loop_two
  compiled <- compiler::cmpfun(plain)
  sourced <- eval(
    parse(
      text = "function(x) {\n  for (i in 1:2) x <- x + 1\n  x\n}",
      keep.source = TRUE
    ),
    envir = globalenv()
  )
  environment(plain) <- globalenv()
  environment(compiled) <- globalenv()
  expect_identical(canonical_hash(compiled), canonical_hash(plain))
  expect_identical(canonical_hash(sourced), canonical_hash(plain))
  # Two factories' closures share their code and differ in what they capture.
  first <- local({
    k <- 2
    function(x) x * k
  })
  same <- local({
    k <- 2
    function(x) x * k
  })
  other <- local({
    k <- 3
    function(x) x * k
  })
  expect_identical(canonical_hash(first), canonical_hash(same))
  expect_false(identical(canonical_hash(first), canonical_hash(other)))
})

test_that("commands parsed with source references hash like plain ones", {
  sourced <- parse(text = "{\n  f(1)\n}", keep.source = TRUE)[[1L]]
  plain <- quote({
    f(1)
  })
  expect_true(!is.null(attr(sourced, "srcref")))
  expect_identical(canonical_hash(sourced), canonical_hash(plain))
})

test_that("special environments hash by name", {
  expect_identical(
    salvage_canonical(globalenv(), salvage_visited()),
    list(type = "environment", name = "R_GlobalEnv")
  )
  expect_identical(
    salvage_canonical(baseenv(), salvage_visited()),
    list(type = "environment", name = "base")
  )
  expect_identical(
    salvage_canonical(emptyenv(), salvage_visited()),
    list(type = "environment", name = "R_EmptyEnv")
  )
  expect_identical(
    salvage_canonical(asNamespace("stats"), salvage_visited()),
    list(type = "environment", name = "namespace:stats")
  )
  expect_identical(
    salvage_canonical(as.environment("package:stats"), salvage_visited()),
    list(type = "environment", name = "package:stats")
  )
})

test_that("primitives, external pointers, and formulas hash stably", {
  expect_identical(
    salvage_canonical(sum, salvage_visited()),
    list(type = "builtin", name = ".Primitive(\"sum\")")
  )
  expect_identical(
    salvage_canonical(`if`, salvage_visited()),
    list(type = "special", name = ".Primitive(\"if\")")
  )
  # Objects like data.table keep an external pointer in an attribute, which
  # reaches a worker as a null pointer whatever it pointed to.
  first <- structure(1:2, selfref = methods::new("externalptr"))
  second <- structure(1:2, selfref = methods::new("externalptr"))
  expect_identical(canonical_hash(first), canonical_hash(second))
  expect_identical(
    salvage_canonical(methods::new("externalptr"), salvage_visited()),
    list(type = "externalptr")
  )
  formula <- y ~ x
  local_formula <- local(y ~ x)
  expect_identical(canonical_hash(formula), canonical_hash(y ~ x))
  expect_false(identical(canonical_hash(formula), canonical_hash(y ~ z)))
  # The formula's environment is part of it.
  expect_false(identical(canonical_hash(formula), canonical_hash(local_formula)))
})

test_that("environments that refer to themselves terminate and hash stably", {
  first <- new.env()
  first$self <- first
  first$value <- 1
  second <- new.env()
  second$self <- second
  second$value <- 1
  expect_identical(canonical_hash(first), canonical_hash(second))
  canonical <- salvage_canonical(first, salvage_visited())
  expect_identical(canonical$names, c("self", "value"))
  expect_identical(canonical$values[[1L]], list(type = "cycle", index = 1L))
})

binding_reads <- new.env()

counted_binding <- function() {
  binding_reads$count <- binding_reads$count + 1L
  1
}

test_that("active bindings are never read", {
  binding_reads$count <- 0L
  object <- new.env()
  makeActiveBinding("field", counted_binding, object)
  canonical <- salvage_canonical(object, salvage_visited())
  expect_identical(canonical$values[[1L]], list(type = "active binding"))
  expect_identical(binding_reads$count, 0L)
  expect_identical(object$field, 1)
  expect_identical(binding_reads$count, 1L)
})

test_that("a closure made in a frame with ... sees the values of the dots", {
  make <- eval(quote(function(...) function() sum(...)), envir = globalenv())
  expect_identical(canonical_hash(make(1, 2)), canonical_hash(make(1, 2)))
  expect_false(identical(canonical_hash(make(1, 2)), canonical_hash(make(1, 3))))
})

test_that("a binding that errors when read hashes as its error", {
  object <- new.env()
  delayedAssign("broken", stop("unreadable"), assign.env = object)
  canonical <- salvage_canonical(object, salvage_visited())
  expect_s3_class(canonical$values[[1L]], "error")
  expect_identical(conditionMessage(canonical$values[[1L]]), "unreadable")
  # Reading it again restarts the promise, which R warns about.
  expect_silent(again <- canonical_hash(object))
  expect_identical(canonical_hash(object), again)
})

test_that("attributes in a different order hash the same", {
  first <- structure(1:2, a = "x", b = list(new.env()))
  second <- structure(1:2, b = list(new.env()), a = "x")
  expect_identical(canonical_hash(first), canonical_hash(second))
})

test_that("S4 objects hash by their slots", {
  setClass("CrewSshTest", representation(x = "numeric"), where = globalenv())
  on.exit(removeClass("CrewSshTest", where = globalenv()))
  first <- methods::new("CrewSshTest", x = 1)
  same <- methods::new("CrewSshTest", x = 1)
  other <- methods::new("CrewSshTest", x = 2)
  expect_identical(canonical_hash(first), canonical_hash(same))
  expect_false(identical(canonical_hash(first), canonical_hash(other)))
})

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
  hash <- get("salvage_globals_hash", envir = asNamespace("crew.ssh"))
  payload <- identity_payload()
  key(
    command = payload$command,
    data = payload$data,
    globals_hash = hash(payload$globals),
    seed = 1L,
    algorithm = NULL,
    packages = character(0L),
    library = NULL,
    name = "identity"
  )
}

test_that("the same task gets the same key in another R session", {
  skip_on_cran()
  skip_if_not_installed("callr")
  here <- identity_key(source = NULL)
  there <- in_another_session(
    identity_key,
    args = list(source = crew_ssh_source()),
    helpers = list(
      crew_ssh_load = crew_ssh_load,
      identity_payload = identity_payload
    )
  )
  expect_identical(there, here)
  expect_match(here, "^identity-[0-9a-f]{32}$")
})

# targets fingerprints ----

fake_target <- function(name, class, hash = NULL) {
  target <- new.env()
  target$file <- new.env()
  target$file$hash <- hash
  target$file$time <- "t20000.1s"
  class(target) <- c(class, "tar_builder", "tar_target", "environment")
  target
}

fake_subpipeline <- function(...) {
  upstream <- list(...)
  targets <- new.env()
  for (target in upstream) {
    assign(get("name", envir = target), target, envir = targets)
  }
  subpipeline <- new.env()
  subpipeline$targets <- targets
  subpipeline
}

named_target <- function(name, class, hash = NULL) {
  target <- fake_target(name, class, hash)
  target$name <- name
  target
}

fake_targets_data <- function(hash = "abc123", upstream = NULL) {
  target <- named_target("fit", "tar_stem")
  target$command <- new.env()
  target$command$hash <- hash
  target$command$packages <- "stats"
  target$command$library <- NULL
  target$settings <- new.env()
  target$settings$name <- "fit"
  target$settings$format <- "rds"
  target$settings$storage <- "main"
  target$settings$retrieval <- "main"
  target$seed <- 123L
  target$deps <- c("data", "slow_fit")
  target$subpipeline <- upstream
  list(
    envir = "globalenv",
    path_store = "_targets",
    fun = "tar_make",
    options = list(seed = 0L),
    envvars = c(TAR_PROJECT = "main"),
    target = target
  )
}

targets_command <- quote(
  targets::target_run_worker(
    target = target,
    envir = envir,
    path_store = path_store,
    fun = fun,
    options = options,
    envvars = envvars
  )
)

test_that("salvage_is_targets() recognizes exactly the tasks targets pushes", {
  data <- fake_targets_data()
  expect_true(salvage_is_targets(targets_command, data))
  expect_false(salvage_is_targets(quote(f(target)), data))
  expect_false(salvage_is_targets(targets_command, list(target = 1)))
  expect_false(salvage_is_targets(quote(x), data))
})

test_that("upstream targets count by their data hash, buds and patterns by name", {
  expect_identical(salvage_upstream(new.env()), list())
  target <- new.env()
  target$subpipeline <- fake_subpipeline(
    named_target("b_1f", "tar_bud"),
    named_target("a", "tar_stem", hash = "aa11"),
    named_target("p", "tar_pattern"),
    named_target("p_2c", "tar_branch", hash = "cc22")
  )
  expect_identical(
    salvage_upstream(target),
    list(
      c("a", "aa11"),
      c("b_1f", "tar_bud"),
      c("p", "tar_pattern"),
      c("p_2c", "cc22")
    )
  )
})

test_that("an upstream target without a data hash fails loudly", {
  target <- new.env()
  target$subpipeline <- fake_subpipeline(named_target("a", "tar_stem"))
  expect_error(salvage_upstream(target), class = "crew_error")
  target$subpipeline <- fake_subpipeline(
    named_target("a", "tar_stem", hash = NA_character_)
  )
  expect_error(salvage_upstream(target), class = "crew_error")
})

targets_key <- function(data) {
  salvage_key(
    command = targets_command,
    data = data,
    globals_hash = "g",
    seed = NULL,
    algorithm = NULL,
    packages = character(0L),
    library = NULL,
    name = "fit"
  )
}

test_that("a target's fingerprint ignores when its upstream data was written", {
  first <- fake_targets_data(
    upstream = fake_subpipeline(named_target("a", "tar_stem", hash = "aa11"))
  )
  rewritten <- named_target("a", "tar_stem", hash = "aa11")
  rewritten$file$time <- "t20001.9s"
  second <- fake_targets_data(upstream = fake_subpipeline(rewritten))
  changed <- fake_targets_data(
    upstream = fake_subpipeline(named_target("a", "tar_stem", hash = "bb22"))
  )
  key <- targets_key
  expect_identical(key(first), key(second))
  expect_false(identical(key(first), key(changed)))
  expect_false(identical(key(first), key(fake_targets_data(hash = "other"))))
  expect_match(key(first), "^fit-[0-9a-f]{32}$")
})

test_that("a target without a command hash fails loudly", {
  for (hash in list(NULL, NA_character_, "", c("a", "b"))) {
    expect_error(
      salvage_fingerprint_targets(
        data = fake_targets_data(hash = hash),
        name = "fit",
        globals_hash = "g",
        visited = salvage_visited()
      ),
      class = "crew_error"
    )
  }
})

# Worker side ----

shipped_for <- function(directory, session_directory = dirname(directory)) {
  salvage_shipped(
    directory = directory,
    session = "crew-ssh-test",
    session_directory = session_directory,
    controller = "test-controller",
    seconds = c(heartbeat = 1, stale = 3, poll = 0.1)
  )
}

salvage_tempdir <- function() {
  root <- tempfile("crew_ssh_salvage_")
  dir.create(root)
  file.path(normalizePath(root), "salvage")
}

good_record <- function(key = "k-1", value = 42) {
  list(
    format = "crew.ssh.salvage",
    version = 1L,
    key = key,
    name = "k",
    status = "success",
    value = value,
    error = NA_character_,
    error_class = NA_character_,
    controller = "test-controller",
    session = "crew-ssh-test",
    worker = "worker-other",
    nodename = "remote",
    pid = 1L,
    started = as.POSIXct("2026-10-01 10:00:00", tz = "UTC"),
    ended = as.POSIXct("2026-10-01 11:00:00", tz = "UTC"),
    r_version = R.version.string,
    packages = vapply(c("base", "stats"), getNamespaceVersion, character(1L))
  )
}

test_that("salvage_shipped() ships functions without crew.ssh and the settings", {
  shipped <- shipped_for("/remote/salvage", "/remote/session")
  functions <- Filter(is.function, shipped)
  expect_setequal(
    names(functions),
    c(
      "run", "read", "problem", "outcome", "reason", "write", "lock",
      "lock_age", "lock_owner", "unlock", "heartbeat", "state", "versions",
      "installed_version", "stamp"
    )
  )
  for (name in names(functions)) {
    expect_identical(environment(functions[[name]]), baseenv(), info = name)
  }
  expect_identical(
    shipped[!vapply(shipped, is.function, logical(1L))],
    list(
      directory = "/remote/salvage",
      session = "crew-ssh-test",
      session_directory = "/remote/session",
      controller = "test-controller",
      seconds_heartbeat = 1,
      seconds_stale = 3,
      seconds_poll = 0.1
    )
  )
})

test_that("the functions shipped to workers need nothing but base R", {
  skip_if_not_installed("codetools")
  functions <- Filter(is.function, shipped_for("/remote/salvage"))
  for (name in names(functions)) {
    globals <- codetools::findGlobals(functions[[name]])
    missing <- globals[!vapply(globals, exists, logical(1L), envir = baseenv())]
    expect_identical(missing, character(0L), info = name)
  }
})

run_shipped <- function(blob) {
  salvage <- unserialize(blob)
  Sys.setenv(CREW_SSH_WORKER = "worker-x")
  envir <- new.env()
  envir$x <- 20
  value <- salvage$run(
    expr = quote(x + 1),
    envir = envir,
    key = "k-1",
    name = "k",
    reuse = TRUE,
    salvage = salvage
  )
  list(value = value, loaded = loadedNamespaces())
}

test_that("shipped functions run in an R session without crew.ssh", {
  skip_on_cran()
  # covr rewrites every function of the package to count its lines, and the
  # rewritten functions load crew.ssh wherever they run. The test above still
  # checks what the shipped functions call.
  skip_on_covr()
  skip_if_not_installed("callr")
  directory <- salvage_tempdir()
  shipped <- shipped_for(directory)
  out <- callr::r(
    run_shipped,
    args = list(blob = serialize(shipped, NULL)),
    libpath = .Library
  )
  expect_identical(out$value, 21)
  expect_false("crew.ssh" %in% out$loaded)
  expect_identical(readRDS(file.path(directory, "k-1.rds"))$value, 21)
})

test_that("salvage_read() tells a missing record from an unreadable one", {
  directory <- salvage_tempdir()
  dir.create(directory)
  shipped <- shipped_for(directory)
  path <- file.path(directory, "k-1.rds")
  expect_null(shipped$read(path, "k-1", shipped))
  saveRDS(good_record(), path)
  expect_identical(shipped$read(path, "k-1", shipped), good_record())
  writeLines("not an rds file", path)
  expect_error(
    shipped$read(path, "k-1", shipped),
    class = "crew_ssh_salvage_unreadable"
  )
  expect_error(
    shipped$read(path, "k-1", shipped),
    paste0(
      "crew.ssh cannot use the saved result at ", path,
      ": reading it failed with"
    ),
    fixed = TRUE
  )
  # A record cut short, as by a full disk, fails gzip's checks.
  saveRDS(good_record(value = runif(1000L)), path)
  size <- file.size(path)
  writeBin(readBin(path, "raw", size)[seq_len(size %/% 2L)], path)
  expect_error(
    shipped$read(path, "k-1", shipped),
    class = "crew_ssh_salvage_unreadable"
  )
  saveRDS(good_record(key = "other"), path)
  expect_error(
    shipped$read(path, "k-1", shipped),
    "it belongs to task key other. Delete the file to run the task again.",
    fixed = TRUE
  )
})

# The problem salvage_problem() finds in a good record with some fields changed.
record_problem <- function(...) {
  shipped <- shipped_for("/remote/salvage")
  record <- utils::modifyList(good_record(), list(...), keep.null = TRUE)
  shipped$problem(record, "k-1")
}

test_that("salvage_problem() names what is wrong with every malformed record", {
  shipped <- shipped_for("/remote/salvage")
  problem <- record_problem
  expect_identical(shipped$problem(good_record(), "k-1"), "")
  expect_identical(
    shipped$problem(simpleError("bad gzip"), "k-1"),
    "reading it failed with bad gzip"
  )
  expect_identical(
    shipped$problem(42, "k-1"),
    "it is not a crew.ssh salvage record"
  )
  expect_identical(problem(format = "other"), "it is not a crew.ssh salvage record")
  expect_identical(
    problem(version = 2L),
    "it has record version 2 and this version of crew.ssh reads version 1"
  )
  expect_identical(problem(key = "k-2"), "it belongs to task key k-2")
  expect_identical(problem(status = "partial"), "its status is partial")
  record <- good_record()
  record$value <- NULL
  expect_identical(shipped$problem(record, "k-1"), "it has no value")
  expect_identical(problem(ended = NULL), "it has no start and end time")
  expect_identical(problem(started = "yesterday"), "it has no start and end time")
  expect_identical(
    problem(packages = NULL),
    "it does not record the R and package versions it ran with"
  )
  expect_identical(
    problem(packages = c("1.0", "2.0")),
    "it does not record the R and package versions it ran with"
  )
  expect_identical(
    problem(r_version = NULL),
    "it does not record the R and package versions it ran with"
  )
  # An error record carries no value, and that is not a problem.
  record <- good_record()
  record$status <- "error"
  record$value <- NULL
  expect_identical(shipped$problem(record, "k-1"), "")
})

test_that("salvage_write() replaces a record whole and leaves no temporary file", {
  directory <- salvage_tempdir()
  dir.create(directory)
  shipped <- shipped_for(directory)
  path <- file.path(directory, "k-1.rds")
  shipped$write(good_record(value = 1), path, shipped)
  shipped$write(good_record(value = 2), path, shipped)
  expect_identical(readRDS(path)$value, 2)
  expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "k-1.rds")
  expect_error(
    shipped$write(good_record(), file.path(directory, "missing", "k-1.rds"), shipped)
  )
  expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "k-1.rds")
})

test_that("salvage_reason() reuses only successes from the same software", {
  shipped <- shipped_for("/remote/salvage")
  expect_identical(shipped$reason(good_record(), shipped), "")
  record <- good_record()
  record$status <- "error"
  record$error <- "boom"
  expect_identical(shipped$reason(record, shipped), "its last run failed with boom")
  record <- good_record()
  record$r_version <- "R version 3.6.0 (2019-04-26)"
  expect_identical(
    shipped$reason(record, shipped),
    paste0(
      "it ran under R version 3.6.0 (2019-04-26) and this worker has ",
      R.version.string
    )
  )
  record <- good_record()
  record$packages <- c(
    base = getNamespaceVersion("base")[[1L]],
    stats = "0.0.1",
    crewSshNotInstalled = "1.0"
  )
  expect_identical(
    shipped$reason(record, shipped),
    paste0(
      "it ran with other package versions than this worker has: ",
      "stats 0.0.1 then, ", getNamespaceVersion("stats")[[1L]], " now; ",
      "crewSshNotInstalled 1.0 then, not installed now"
    )
  )
})

fake_finished_target <- function(error = NULL, cancel = NULL) {
  target <- structure(new.env(), class = c("tar_stem", "tar_target", "environment"))
  target$metrics <- new.env()
  target$metrics$error <- error
  target$metrics$error_class <- if (is.null(error)) NULL else "simpleError"
  target$metrics$cancel <- cancel
  target
}

test_that("salvage_outcome() sees a target that ended early as failed", {
  shipped <- shipped_for("/remote/salvage")
  success <- list(status = "success", error = NA_character_, class = NA_character_)
  expect_identical(shipped$outcome(42), success)
  expect_identical(shipped$outcome(fake_finished_target()), success)
  expect_identical(
    shipped$outcome(fake_finished_target(error = "boom")),
    list(status = "error", error = "boom", class = "simpleError")
  )
  expect_identical(
    shipped$outcome(fake_finished_target(cancel = TRUE)),
    list(status = "error", error = "the target was canceled", class = NA_character_)
  )
})

test_that("salvage_versions() lists every loaded namespace with its version", {
  versions <- shipped_for("/remote/salvage")$versions()
  expect_identical(names(versions), sort(loadedNamespaces(), method = "radix"))
  expect_identical(versions[["stats"]], getNamespaceVersion("stats")[[1L]])
  expect_identical(
    shipped_for("/remote/salvage")$installed_version("crewSshNotInstalled"),
    NA_character_
  )
})

test_that("a lock goes to one worker and is taken over once it goes quiet", {
  directory <- salvage_tempdir()
  dir.create(directory)
  shipped <- shipped_for(directory)
  lock <- file.path(directory, "k-1.lock")
  first <- shipped$lock(lock, "worker-a", shipped)
  expect_true(isTRUE(first))
  expect_match(attr(first, "token"), "^[0-9]+\\.[0-9]+$")
  owner <- read.dcf(file.path(lock, "owner.dcf"))[1L, ]
  expect_identical(
    unname(owner[c("worker", "session", "controller", "pid", "token")]),
    c(
      "worker-a", "crew-ssh-test", "test-controller",
      as.character(Sys.getpid()), attr(first, "token")
    )
  )
  held <- shipped$lock(lock, "worker-b", shipped)
  expect_identical(held[["worker"]], "worker-a")
  expect_identical(held[["session"]], "crew-ssh-test")
  # Silence for longer than seconds_stale means the holder died.
  old <- Sys.time() - 60
  Sys.setFileTime(c(file.path(lock, "heartbeat"), lock), old)
  second <- shipped$lock(lock, "worker-b", shipped)
  expect_true(isTRUE(second))
  expect_false(identical(attr(second, "token"), attr(first, "token")))
  expect_identical(
    read.dcf(file.path(lock, "owner.dcf"))[1L, "worker"][[1L]],
    "worker-b"
  )
  expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "k-1.lock")
  # The worker that took over knows whose lock it was, and for how long.
  previous <- attr(second, "previous")
  expect_identical(previous[["worker"]], "worker-a")
  expect_identical(previous[["session"]], "crew-ssh-test")
  expect_gte(as.numeric(previous[["silent"]]), 59)
  expect_null(attr(first, "previous"))
})

test_that("a worker removes only a lock it still holds", {
  directory <- salvage_tempdir()
  dir.create(directory)
  shipped <- shipped_for(directory)
  lock <- file.path(directory, "k-1.lock")
  token <- attr(shipped$lock(lock, "worker-a", shipped), "token")
  shipped$unlock(lock, "someone else's token")
  expect_true(dir.exists(lock))
  shipped$unlock(lock, token)
  expect_false(dir.exists(lock))
  # Unlocking a lock that is already gone is harmless.
  expect_silent(shipped$unlock(lock, token))
})

test_that("salvage_lock_age() and salvage_lock_owner() cope with a missing lock", {
  directory <- salvage_tempdir()
  dir.create(directory)
  shipped <- shipped_for(directory)
  lock <- file.path(directory, "k-1.lock")
  expect_identical(shipped$lock_age(lock), NA_real_)
  dir.create(lock)
  expect_lt(shipped$lock_age(lock), 5)
  expect_identical(
    shipped$lock_owner(lock),
    c(worker = "unknown", session = "unknown")
  )
})

test_that("a lock that cannot be created fails loudly", {
  skip_on_os("windows")
  skip_if(identical(unname(Sys.info()[["effective_user"]]), "root"))
  directory <- salvage_tempdir()
  dir.create(directory)
  Sys.chmod(directory, mode = "0555")
  on.exit(Sys.chmod(directory, mode = "0755"))
  shipped <- shipped_for(directory)
  expect_error(
    shipped$lock(file.path(directory, "k-1.lock"), "worker-a", shipped),
    "crew.ssh cannot lock",
    fixed = TRUE
  )
})

test_that("a lock path that can neither be created nor read fails loudly", {
  skip_on_os("windows")
  directory <- salvage_tempdir()
  dir.create(directory)
  shipped <- shipped_for(directory)
  lock <- file.path(directory, "k-1.lock")
  # A dangling symbolic link: mkdir finds the name taken, and its time cannot
  # be read.
  file.symlink(file.path(directory, "nowhere"), lock)
  expect_error(
    shipped$lock(lock, "worker-a", shipped),
    "crew.ssh cannot lock",
    fixed = TRUE
  )
})

# A process can exit between being listed and being read.
command_line <- function(handle) {
  command <- tryCatch(ps::ps_cmdline(handle), error = identity)
  if (inherits(command, "error")) "" else paste(command, collapse = " ")
}

heartbeat_processes <- function(lock) {
  commands <- vapply(ps::ps()$ps_handle, command_line, character(1L))
  sum(
    grepl("crew-ssh-heartbeat", commands, fixed = TRUE) &
      grepl(lock, commands, fixed = TRUE)
  )
}

test_that("the heartbeat keeps a lock fresh and stops with it", {
  skip_on_os("windows")
  skip_if_not_installed("ps")
  directory <- salvage_tempdir()
  dir.create(directory)
  shipped <- shipped_for(directory)
  lock <- file.path(directory, "k-1.lock")
  dir.create(lock)
  shipped$heartbeat(lock, 0.2, 60)
  wait_until(file.exists(file.path(lock, "heartbeat")), seconds = 10)
  Sys.setFileTime(file.path(lock, "heartbeat"), Sys.time() - 60)
  wait_until(shipped$lock_age(lock) < 5, seconds = 10)
  expect_identical(heartbeat_processes(lock), 1L)
  unlink(lock, recursive = TRUE)
  wait_until(heartbeat_processes(lock) == 0L, seconds = 10)
})

test_that("a worker that cannot keep its lock alive says so", {
  skip_on_os("windows")
  directory <- salvage_tempdir()
  dir.create(directory)
  shipped <- shipped_for(directory)
  lock <- file.path(directory, "k-1.lock")
  dir.create(lock)
  message <- paste0(
    "crew.ssh cannot keep the lock of this task alive (%s), so a session that ",
    "reconnects while the task runs would take the lock for a dead worker and ",
    "run the task again once the lock has been quiet for 90 seconds."
  )
  # No sh on the PATH, as in an image that ships only R.
  withr::with_envvar(
    c(PATH = directory),
    expect_warning(
      shipped$heartbeat(lock, 1, 90),
      sprintf(message, "there is no sh on this worker"),
      fixed = TRUE
    )
  )
})

test_that("salvage_state() records a worker's task for remote_workers()", {
  directory <- salvage_tempdir()
  session <- dirname(directory)
  shipped <- shipped_for(directory, session)
  started <- as.POSIXct(1790000000.25, origin = "1970-01-01")
  ended <- as.POSIXct(1790000060.5, origin = "1970-01-01")
  shipped$state(shipped, "worker-a", "k-1", "k", "success", started, ended)
  state <- read.dcf(file.path(session, "worker-a.task"))
  expect_identical(
    colnames(state),
    c(
      "task_key", "task_name", "task_state", "task_updated", "task_started",
      "task_ended"
    )
  )
  expect_identical(
    unname(state[1L, c("task_key", "task_name", "task_state", "task_started", "task_ended")]),
    c("k-1", "k", "success", "1790000000.250", "1790000060.500")
  )
  shipped$state(shipped, "worker-a", "k-2", "k2", "waiting")
  state <- read.dcf(file.path(session, "worker-a.task"))
  expect_identical(colnames(state), c("task_key", "task_name", "task_state", "task_updated"))
  expect_identical(unname(state[1L, "task_state"]), "waiting")
  expect_identical(
    list.files(session, pattern = "\\.tmp\\."),
    character(0L)
  )
  shipped$session_directory <- file.path(session, "missing")
  expect_warning(
    shipped$state(shipped, "worker-a", "k-1", "k", "running"),
    "crew.ssh could not record the state of worker worker-a",
    fixed = TRUE
  )
  expect_identical(list.files(dirname(session), pattern = "\\.tmp\\."), character(0L))
})

# salvage_run() ----

local_worker <- function(name = "worker-test", envir = parent.frame()) {
  withr::local_envvar(c(CREW_SSH_WORKER = name), .local_envir = envir)
}

run_task <- function(shipped, expr, envir = new.env(), reuse = TRUE, key = "k-1") {
  shipped$run(
    expr = expr,
    envir = envir,
    key = key,
    name = "k",
    reuse = reuse,
    salvage = shipped
  )
}

counting_envir <- function() {
  envir <- new.env()
  envir$runs <- 0L
  envir
}

counted <- quote({
  runs <- runs + 1L
  runs * 10
})

test_that("outside a crew.ssh worker a task just runs", {
  withr::local_envvar(c(CREW_SSH_WORKER = ""))
  directory <- salvage_tempdir()
  shipped <- shipped_for(directory)
  envir <- counting_envir()
  expect_identical(run_task(shipped, counted, envir), 10)
  expect_false(dir.exists(directory))
})

test_that("a task runs once, saves a full record, and is then reused", {
  local_worker()
  directory <- salvage_tempdir()
  shipped <- shipped_for(directory)
  envir <- counting_envir()
  expect_identical(run_task(shipped, counted, envir), 10)
  record <- readRDS(file.path(directory, "k-1.rds"))
  expect_identical(
    record[c(
      "format", "version", "key", "name", "status", "value", "error",
      "error_class", "controller", "session", "worker", "nodename", "pid",
      "r_version"
    )],
    list(
      format = "crew.ssh.salvage",
      version = 1L,
      key = "k-1",
      name = "k",
      status = "success",
      value = 10,
      error = NA_character_,
      error_class = NA_character_,
      controller = "test-controller",
      session = "crew-ssh-test",
      worker = "worker-test",
      nodename = Sys.info()[["nodename"]],
      pid = Sys.getpid(),
      r_version = R.version.string
    )
  )
  expect_identical(names(record$packages), sort(loadedNamespaces(), method = "radix"))
  expect_true(record$started <= record$ended)
  expect_false(dir.exists(file.path(directory, "k-1.lock")))
  expect_identical(
    unname(read.dcf(file.path(dirname(directory), "worker-test.task"))[1L, "task_state"]),
    "success"
  )
  expect_message(
    expect_identical(run_task(shipped, counted, envir), 10),
    "crew.ssh: task k reuses the result worker worker-test saved at",
    fixed = TRUE
  )
  expect_identical(envir$runs, 1L)
  expect_identical(
    unname(read.dcf(file.path(dirname(directory), "worker-test.task"))[1L, "task_state"]),
    "reused"
  )
})

test_that("reuse = FALSE runs the task again over a saved result", {
  local_worker()
  shipped <- shipped_for(salvage_tempdir())
  envir <- counting_envir()
  expect_identical(run_task(shipped, counted, envir), 10)
  expect_identical(run_task(shipped, counted, envir, reuse = FALSE), 20)
  expect_message(
    expect_identical(run_task(shipped, counted, envir), 20),
    "reuses the result",
    fixed = TRUE
  )
  expect_identical(envir$runs, 2L)
})

test_that("an error is saved, rethrown as it was, and never reused", {
  local_worker()
  directory <- salvage_tempdir()
  shipped <- shipped_for(directory)
  envir <- counting_envir()
  failing <- quote({
    runs <- runs + 1L
    stop(
      structure(
        class = c("custom_error", "error", "condition"),
        list(message = "boom", call = NULL)
      )
    )
  })
  expect_error(run_task(shipped, failing, envir), "boom", class = "custom_error")
  record <- readRDS(file.path(directory, "k-1.rds"))
  expect_identical(
    record[c("status", "value", "error", "error_class")],
    list(
      status = "error",
      value = NULL,
      error = "boom",
      error_class = c("custom_error", "error", "condition")
    )
  )
  expect_message(
    expect_error(run_task(shipped, failing, envir), "boom", class = "custom_error"),
    "runs again instead of reusing its saved result because its last run failed with boom",
    fixed = TRUE
  )
  expect_identical(envir$runs, 2L)
  expect_message(
    expect_identical(run_task(shipped, counted, envir), 30),
    "because its last run failed with boom",
    fixed = TRUE
  )
})

test_that("a target that ended in an error is returned, saved as failed, and rerun", {
  local_worker()
  directory <- salvage_tempdir()
  shipped <- shipped_for(directory)
  envir <- counting_envir()
  envir$target <- fake_finished_target(error = "target failed")
  ended_early <- quote({
    runs <- runs + 1L
    target
  })
  expect_identical(run_task(shipped, ended_early, envir), envir$target)
  record <- readRDS(file.path(directory, "k-1.rds"))
  expect_identical(record$status, "error")
  expect_identical(record$error, "target failed")
  expect_null(record$value)
  expect_message(
    expect_identical(run_task(shipped, ended_early, envir), envir$target),
    "because its last run failed with target failed",
    fixed = TRUE
  )
  expect_identical(envir$runs, 2L)
})

test_that("a result from other package versions is not reused", {
  local_worker()
  directory <- salvage_tempdir()
  shipped <- shipped_for(directory)
  envir <- counting_envir()
  run_task(shipped, counted, envir)
  record <- readRDS(file.path(directory, "k-1.rds"))
  record$packages[["stats"]] <- "0.0.1"
  saveRDS(record, file.path(directory, "k-1.rds"))
  expect_message(
    expect_identical(run_task(shipped, counted, envir), 20),
    "it ran with other package versions than this worker has: stats 0.0.1 then",
    fixed = TRUE
  )
})

test_that("a saved result that cannot be read fails the task without running it", {
  local_worker()
  directory <- salvage_tempdir()
  dir.create(directory)
  writeLines("garbage", file.path(directory, "k-1.rds"))
  shipped <- shipped_for(directory)
  envir <- counting_envir()
  expect_error(
    run_task(shipped, counted, envir),
    class = "crew_ssh_salvage_unreadable"
  )
  expect_identical(envir$runs, 0L)
  expect_false(dir.exists(file.path(directory, "k-1.lock")))
})

test_that("an interrupted task releases its lock and says it was interrupted", {
  local_worker()
  directory <- salvage_tempdir()
  shipped <- shipped_for(directory)
  out <- tryCatch(
    run_task(shipped, quote(rlang::interrupt())),
    interrupt = identity
  )
  expect_s3_class(out, "interrupt")
  expect_false(dir.exists(file.path(directory, "k-1.lock")))
  expect_false(file.exists(file.path(directory, "k-1.rds")))
  expect_identical(
    unname(read.dcf(file.path(dirname(directory), "worker-test.task"))[1L, "task_state"]),
    "interrupted"
  )
})

save_later <- function(directory, record, seconds) {
  Sys.sleep(seconds)
  path <- file.path(directory, paste0(record$key, ".rds"))
  saveRDS(record, paste0(path, ".partial"))
  file.rename(paste0(path, ".partial"), path)
  unlink(file.path(directory, paste0(record$key, ".lock")), recursive = TRUE)
}

test_that("a task another live worker runs is waited for, not run twice", {
  skip_on_cran()
  skip_if_not_installed("callr")
  local_worker()
  directory <- salvage_tempdir()
  dir.create(directory)
  shipped <- shipped_for(directory)
  expect_true(isTRUE(shipped$lock(file.path(directory, "k-1.lock"), "worker-other", shipped)))
  shipped$heartbeat(file.path(directory, "k-1.lock"), 0.2, 60)
  saver <- callr::r_bg(
    save_later,
    args = list(directory = directory, record = good_record(value = 99), seconds = 2)
  )
  on.exit(saver$kill())
  envir <- counting_envir()
  messages <- testthat::capture_messages(
    value <- run_task(shipped, counted, envir)
  )
  expect_identical(value, 99)
  expect_identical(envir$runs, 0L)
  expect_match(
    messages[1L],
    paste(
      "crew.ssh: task k is already running on worker worker-other of session",
      "crew-ssh-test, so this worker waits for its result."
    ),
    fixed = TRUE
  )
  expect_match(messages[2L], "reuses the result worker worker-other saved", fixed = TRUE)
})

test_that("a task whose worker went quiet is taken over and run", {
  local_worker()
  directory <- salvage_tempdir()
  dir.create(directory)
  shipped <- shipped_for(directory)
  lock <- file.path(directory, "k-1.lock")
  shipped$lock(lock, "worker-dead", shipped)
  Sys.setFileTime(c(file.path(lock, "heartbeat"), lock), Sys.time() - 600)
  envir <- counting_envir()
  expect_message(
    expect_identical(run_task(shipped, counted, envir), 10),
    paste(
      "crew.ssh: task k was running on worker worker-dead of session",
      "crew-ssh-test, which went silent 600 seconds ago, so this worker runs it."
    ),
    fixed = TRUE
  )
  expect_identical(envir$runs, 1L)
  expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "k-1.rds")
})

second_read <- function(path, key, salvage) {
  salvage$reads$count <- salvage$reads$count + 1L
  if (salvage$reads$count < 2L) NULL else salvage$saved
}

test_that("a result saved just before the lock is taken is reused", {
  local_worker()
  shipped <- shipped_for(salvage_tempdir())
  shipped$reads <- new.env()
  shipped$reads$count <- 0L
  shipped$saved <- good_record(value = 7)
  shipped$read <- second_read
  envir <- counting_envir()
  expect_identical(run_task(shipped, counted, envir), 7)
  expect_identical(envir$runs, 0L)
  expect_identical(shipped$reads$count, 2L)
})

test_that("leftovers of a writer that died midway are cleared before a run", {
  local_worker()
  directory <- salvage_tempdir()
  dir.create(directory)
  leftover <- file.path(directory, "k-1.rds.tmp.123.456")
  writeLines("half a record", leftover)
  unrelated <- file.path(directory, "k-10.rds.tmp.1.2")
  writeLines("another task's", unrelated)
  shipped <- shipped_for(directory)
  run_task(shipped, counted, counting_envir())
  expect_false(file.exists(leftover))
  expect_true(file.exists(unrelated))
})

test_that("a result that cannot be saved still comes back, with a warning", {
  local_worker()
  directory <- salvage_tempdir()
  dir.create(file.path(directory, "k-1.rds"), recursive = TRUE)
  shipped <- shipped_for(directory)
  envir <- counting_envir()
  expect_warning(
    expect_identical(run_task(shipped, counted, envir, reuse = FALSE), 10),
    "crew.ssh could not save the result of task k",
    fixed = TRUE
  )
})

test_that("a salvage directory that cannot be created fails loudly", {
  local_worker()
  blocker <- tempfile()
  writeLines("a file, not a directory", blocker)
  shipped <- shipped_for(file.path(blocker, "salvage"))
  expect_error(
    run_task(shipped, counted, counting_envir()),
    "crew.ssh cannot create the salvage directory",
    fixed = TRUE
  )
})

# targets end to end ----
#
# A pipeline whose remote target is in flight when the R session running it
# dies, then runs again.

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
# exists.
write_pipeline <- function(project, remote, multiplier) {
  writeLines(
    c(
      pipeline_header(remote),
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

saved_fits <- function(remote) {
  list.files(file.path(remote, "crew-ssh-salvage"), "^fit-.*\\.rds$")
}

test_that("a rerun pipeline reuses the target a dead session left finished", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  skip_if_not_installed("targets")
  project <- remote_tempdir()
  remote <- file.path(project, "remote")
  dir.create(remote)
  write_pipeline(project, remote, multiplier = 2)
  interrupted_pipeline(project)
  # The old worker finishes after its session is gone and saves the target.
  file.create(file.path(project, "release"))
  wait_until(length(saved_fits(remote)) == 1L, seconds = 60)
  make_pipeline(project)
  store <- file.path(project, "_targets")
  expect_identical(targets::tar_read(fit, store = store), 110)
  expect_identical(targets::tar_read(report, store = store), 111)
  expect_identical(count_runs(file.path(project, "fits.txt")), 1L)
  expect_identical(targets::tar_meta(fit, error, store = store)$error, NA_character_)
  # Collected, so deleted.
  expect_identical(list.files(file.path(remote, "crew-ssh-salvage")), character(0L))
})

test_that("a target that errored runs again on the next run instead of replaying", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  skip_if_not_installed("targets")
  project <- remote_tempdir()
  remote <- file.path(project, "remote")
  dir.create(remote)
  write_flaky_pipeline(project, remote)
  store <- file.path(project, "_targets")
  make_pipeline(project)
  expect_identical(targets::tar_progress(store = store)$progress, "errored")
  expect_identical(count_runs(file.path(project, "flaky.txt")), 1L)
  make_pipeline(project)
  expect_identical(targets::tar_read(flaky, store = store), "recovered")
  expect_identical(count_runs(file.path(project, "flaky.txt")), 2L)
  expect_identical(
    targets::tar_meta(flaky, error, store = store)$error,
    NA_character_
  )
  expect_identical(list.files(file.path(remote, "crew-ssh-salvage")), character(0L))
})

test_that("a rerun pipeline waits for the target a dead session left running", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  skip_if_not_installed("targets")
  project <- remote_tempdir()
  remote <- file.path(project, "remote")
  dir.create(remote)
  write_pipeline(project, remote, multiplier = 2)
  interrupted_pipeline(project)
  expect_identical(saved_fits(remote), character(0L))
  second <- make_pipeline(project, background = TRUE)
  on.exit(second$kill())
  # The new session pushes the target again, and its worker attaches to the
  # run already under way instead of starting another.
  wait_until("waiting" %in% task_states(remote), seconds = 180)
  expect_true(second$is_alive())
  expect_identical(count_runs(file.path(project, "fits.txt")), 1L)
  file.create(file.path(project, "release"))
  wait_until(!second$is_alive(), seconds = 180)
  expect_identical(second$get_exit_status(), 0L)
  store <- file.path(project, "_targets")
  expect_identical(targets::tar_read(fit, store = store), 110)
  expect_identical(targets::tar_read(report, store = store), 111)
  expect_identical(count_runs(file.path(project, "fits.txt")), 1L)
})

test_that("a target whose code changed runs again instead of reusing", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  skip_if_not_installed("targets")
  project <- remote_tempdir()
  remote <- file.path(project, "remote")
  dir.create(remote)
  write_pipeline(project, remote, multiplier = 2)
  interrupted_pipeline(project)
  file.create(file.path(project, "release"))
  wait_until(length(saved_fits(remote)) == 1L, seconds = 60)
  saved <- saved_fits(remote)
  write_pipeline(project, remote, multiplier = 3)
  make_pipeline(project)
  store <- file.path(project, "_targets")
  expect_identical(targets::tar_read(fit, store = store), 165)
  expect_identical(count_runs(file.path(project, "fits.txt")), 2L)
  # The old result matches no task any more, so nothing collected it.
  expect_identical(
    setdiff(list.files(file.path(remote, "crew-ssh-salvage")), saved),
    character(0L)
  )
  expect_identical(list.files(file.path(remote, "crew-ssh-salvage"), "\\.rds$"), saved)
})
