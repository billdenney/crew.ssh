test_that("crew_controller_ssh() creates a valid controller", {
  controller <- crew_controller_ssh(ssh_host = "user@example.com")
  expect_s3_class(controller, "crew_class_controller")
  expect_s3_class(controller$launcher, "crew_class_launcher_ssh")
  expect_silent(controller$validate())
  # The dispatcher only has to be reachable through the tunnel, so it stays on
  # the loopback interface.
  expect_equal(controller$client$host, "127.0.0.1")
})

test_that("the launcher rejects malformed arguments", {
  expect_error(
    crew_controller_ssh(ssh_host = c("a", "b")),
    class = "crew_error"
  )
  expect_error(
    crew_controller_ssh(ssh_host = "user@example.com", rscript = ""),
    class = "crew_error"
  )
  expect_error(
    crew_controller_ssh(ssh_host = "user@example.com", remote_directory = "tmp"),
    class = "crew_error"
  )
  expect_error(
    crew_controller_ssh(
      ssh_host = "user@example.com",
      remote_directory = "~/tmp"
    ),
    class = "crew_error"
  )
  expect_error(
    crew_controller_ssh(ssh_host = "user@example.com", ssh_port = -1L),
    class = "crew_error"
  )
  expect_error(
    crew_controller_ssh(ssh_host = "user@example.com", ssh_options = NA),
    class = "crew_error"
  )
  expect_error(
    crew_controller_ssh(ssh_host = "user@example.com", verbose = "yes"),
    class = "crew_error"
  )
  expect_error(
    crew_controller_ssh(ssh_host = "user@example.com", directory = c("a", "b")),
    class = "crew_error"
  )
})

test_that("ssh_args() honors ssh_port, ssh_keyfile, and ssh_options", {
  launcher <- crew_controller_ssh(
    ssh_host = "user@example.com",
    ssh_port = 2222,
    ssh_keyfile = "/home/user/.ssh/id_ed25519",
    ssh_options = c("-o", "ProxyJump=bastion")
  )$launcher
  args <- launcher$.__enclos_env__$private$ssh_args()
  expect_equal(args[1:2], c("-o", "ProxyJump=bastion"))
  expect_true(all(c("-p", "2222") %in% args))
  expect_true(all(c("-i", "/home/user/.ssh/id_ed25519") %in% args))
  expect_true("BatchMode=yes" %in% args)
  # ssh honors the first occurrence of an option, so user options must lead.
  expect_lt(match("ProxyJump=bastion", args), match("BatchMode=yes", args))
})

test_that("ssh_args() omits the port and key when they are NULL", {
  launcher <- crew_controller_ssh(ssh_host = "user@example.com")$launcher
  args <- launcher$.__enclos_env__$private$ssh_args()
  expect_equal(args, c("-o", "BatchMode=yes"))
})

test_that("parse_url() splits a dispatcher URL on the last colon", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com"
  )$launcher$.__enclos_env__$private
  expect_equal(
    private$parse_url("tcp://127.0.0.1:5000"),
    list(scheme = "tcp", host = "127.0.0.1", port = "5000")
  )
  expect_equal(
    private$parse_url("tls+tcp://192.168.0.155:44271"),
    list(scheme = "tls+tcp", host = "192.168.0.155", port = "44271")
  )
  expect_equal(
    private$parse_url("tcp://[::1]:5000"),
    list(scheme = "tcp", host = "[::1]", port = "5000")
  )
  expect_error(private$parse_url("tcp://127.0.0.1"), class = "crew_error")
  expect_error(private$parse_url("127.0.0.1:5000"), class = "crew_error")
})

test_that("worker_url() keeps the scheme and loopback host but takes the tunnel port", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com"
  )$launcher$.__enclos_env__$private
  private$.url <- "tcp://127.0.0.1:5000"
  private$.tunnel_port <- "45678"
  expect_equal(private$worker_url(), "tcp://127.0.0.1:45678")
  private$.url <- "tls+tcp://127.0.0.1:5000"
  expect_equal(private$worker_url(), "tls+tcp://127.0.0.1:45678")
})

test_that("the worker command uploads the script, detaches R, and records the PID", {
  launcher <- crew_controller_ssh(
    ssh_host = "user@example.com",
    rscript = "/usr/local/bin/Rscript",
    r_arguments = c("--no-save", "--no-restore")
  )$launcher
  private <- launcher$.__enclos_env__$private
  private$.session_directory <- "/tmp/crew-ssh-abc"
  command <- private$worker_command("worker-1")
  expect_match(command, "^set -e; ", fixed = FALSE)
  expect_match(command, "mkdir -p '/tmp/crew-ssh-abc';", fixed = TRUE)
  expect_match(command, "cat > '/tmp/crew-ssh-abc/worker-1.R';", fixed = TRUE)
  expect_match(
    command,
    "nohup '/usr/local/bin/Rscript' '--no-save' '--no-restore'",
    fixed = TRUE
  )
  # Detaching every standard stream is what lets the ssh session return
  # instead of waiting on the worker.
  expect_match(
    command,
    "> '/tmp/crew-ssh-abc/worker-1.log' 2>&1 < /dev/null & ",
    fixed = TRUE
  )
  # The PID goes to the session directory, where a later session finds it,
  # and to stdout, where this launch reads it.
  expect_true(
    endsWith(
      command,
      "pid=$!; echo \"$pid\" > '/tmp/crew-ssh-abc/worker-1.pid'; echo \"$pid\""
    )
  )
  expect_no_match(command, "cd ", fixed = TRUE)
})

test_that("worker_paths() keeps every record of a worker in the session directory", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com"
  )$launcher$.__enclos_env__$private
  private$.session_directory <- "/tmp/crew-ssh-abc"
  expect_identical(
    private$worker_paths(c("worker-1", "worker-2")),
    list(
      script = c("/tmp/crew-ssh-abc/worker-1.R", "/tmp/crew-ssh-abc/worker-2.R"),
      log = c("/tmp/crew-ssh-abc/worker-1.log", "/tmp/crew-ssh-abc/worker-2.log"),
      pid = c("/tmp/crew-ssh-abc/worker-1.pid", "/tmp/crew-ssh-abc/worker-2.pid"),
      cid = c("/tmp/crew-ssh-abc/worker-1.cid", "/tmp/crew-ssh-abc/worker-2.cid"),
      task = c("/tmp/crew-ssh-abc/worker-1.task", "/tmp/crew-ssh-abc/worker-2.task")
    )
  )
})

test_that("the worker command changes directory when asked", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com",
    directory = "/home/user/project"
  )$launcher$.__enclos_env__$private
  private$.session_directory <- "/tmp/crew-ssh-abc"
  command <- private$worker_command("worker-1")
  expect_match(command, "cd '/home/user/project';", fixed = TRUE)
  # The directory change has to happen before R starts, not after.
  expect_lt(
    regexpr("cd '/home/user/project'", command, fixed = TRUE),
    regexpr("nohup", command, fixed = TRUE)
  )
})

test_that("the worker command quotes paths that would otherwise break the shell", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com",
    rscript = "/opt/R 4.6/bin/Rscript",
    directory = "/home/user/my project"
  )$launcher$.__enclos_env__$private
  private$.session_directory <- "/tmp/crew ssh"
  command <- private$worker_command("worker-1")
  expect_match(command, "mkdir -p '/tmp/crew ssh';", fixed = TRUE)
  expect_match(command, "cd '/home/user/my project';", fixed = TRUE)
  expect_match(command, "nohup '/opt/R 4.6/bin/Rscript'", fixed = TRUE)
  expect_match(command, "'/tmp/crew ssh/worker-1.R'", fixed = TRUE)
  expect_match(command, "> '/tmp/crew ssh/worker-1.pid'", fixed = TRUE)
})

test_that("no launch_prefix leaves the worker command running Rscript directly", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com"
  )$launcher$.__enclos_env__$private
  private$.session_directory <- "/tmp/crew-ssh-abc"
  command <- private$worker_command("worker-1")
  expect_match(command, "nohup 'Rscript' '--no-save'", fixed = TRUE)
  expect_no_match(command, "--cidfile", fixed = TRUE)
})

test_that("launch_prefix runs the worker under a container runtime that records its ID", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com",
    launch_prefix = c(
      "docker", "run", "--init", "--rm", "--network=host",
      "--volume", "/tmp:/tmp", "my-image"
    )
  )$launcher$.__enclos_env__$private
  private$.session_directory <- "/tmp/crew-ssh-abc"
  expect_identical(
    private$worker_command("worker-1"),
    paste0(
      "set -e; mkdir -p '/tmp/crew-ssh-abc'; ",
      "cat > '/tmp/crew-ssh-abc/worker-1.R'; ",
      "nohup 'docker' 'run' '--cidfile' '/tmp/crew-ssh-abc/worker-1.cid' ",
      "'--init' '--rm' '--network=host' ",
      "'--volume' '/tmp:/tmp' 'my-image' ",
      "'Rscript' '--no-save' '--no-restore' ",
      "'/tmp/crew-ssh-abc/worker-1.R' ",
      "> '/tmp/crew-ssh-abc/worker-1.log' 2>&1 < /dev/null & ",
      "pid=$!; echo \"$pid\" > '/tmp/crew-ssh-abc/worker-1.pid'; echo \"$pid\""
    )
  )
})

prefix_runtime <- function(prefix) {
  crew_controller_ssh(
    ssh_host = "user@example.com",
    launch_prefix = prefix
  )$launcher$.__enclos_env__$private$container_runtime()
}

test_that("container_runtime() recognizes docker run and podman run only", {
  runtime <- prefix_runtime
  expect_identical(runtime(c("docker", "run", "img")), "docker")
  expect_identical(runtime(c("podman", "run", "img")), "podman")
  expect_identical(runtime(c("/usr/bin/docker", "run", "img")), "/usr/bin/docker")
  expect_null(runtime(character(0L)))
  expect_null(runtime("docker"))
  expect_null(runtime(c("docker", "container", "run", "img")))
  expect_null(runtime(c("apptainer", "exec", "img.sif")))
  expect_null(runtime(c("sudo", "docker", "run", "img")))
})

test_that("the prefix precedes Rscript, and Rscript's arguments stay with Rscript", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com",
    launch_prefix = c("docker", "run", "my-image"),
    rscript = "/usr/local/bin/Rscript",
    r_arguments = "--vanilla"
  )$launcher$.__enclos_env__$private
  private$.session_directory <- "/tmp/crew-ssh-abc"
  # The arguments belong to the R inside the container, so they have to follow
  # the image name rather than the container runtime.
  expect_match(
    private$worker_command("worker-1"),
    paste(
      "nohup 'docker' 'run' '--cidfile' '/tmp/crew-ssh-abc/worker-1.cid'",
      "'my-image' '/usr/local/bin/Rscript' '--vanilla'",
      "'/tmp/crew-ssh-abc/worker-1.R'"
    ),
    fixed = TRUE
  )
})

test_that("a prefix element containing spaces stays a single argument", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com",
    launch_prefix = c("docker", "run", "--volume", "/home/my data:/data", "img")
  )$launcher$.__enclos_env__$private
  private$.session_directory <- "/tmp/crew-ssh-abc"
  expect_match(
    private$worker_command("worker-1"),
    "'--volume' '/home/my data:/data' 'img'",
    fixed = TRUE
  )
})

test_that("launch_prefix rejects values the remote shell could not use", {
  expect_error(
    crew_controller_ssh(
      ssh_host = "user@example.com",
      launch_prefix = c("docker", NA_character_)
    ),
    class = "crew_error"
  )
  # An empty string would reach the shell as '', an empty argument.
  expect_error(
    crew_controller_ssh(
      ssh_host = "user@example.com",
      launch_prefix = c("docker", "")
    ),
    class = "crew_error"
  )
  expect_error(
    crew_controller_ssh(ssh_host = "user@example.com", launch_prefix = 1L),
    class = "crew_error"
  )
})

test_that("launch_prefix may not set the --cidfile crew.ssh sets", {
  for (prefix in list(
    c("docker", "run", "--cidfile", "/tmp/x", "img"),
    c("podman", "run", "--cidfile=/tmp/x", "img")
  )) {
    expect_error(
      crew_controller_ssh(ssh_host = "user@example.com", launch_prefix = prefix),
      "launch_prefix must not set --cidfile",
      class = "crew_error"
    )
  }
  # Without a container runtime crew.ssh adds nothing, so nothing conflicts.
  expect_silent(
    crew_controller_ssh(
      ssh_host = "user@example.com",
      launch_prefix = c("wrapper", "--cidfile", "x")
    )
  )
})

test_that("an empty launch_prefix means no prefix", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com",
    launch_prefix = character(0L)
  )$launcher$.__enclos_env__$private
  private$.session_directory <- "/tmp/crew-ssh-abc"
  expect_match(
    private$worker_command("worker-1"),
    "nohup 'Rscript'",
    fixed = TRUE
  )
})

test_that("logs() short-circuits before any worker has launched", {
  launcher <- crew_controller_ssh(ssh_host = "user@example.com")$launcher
  expect_equal(launcher$logs(), character(0L))
})

test_that("terminate() is a no-op before the launcher starts", {
  launcher <- crew_controller_ssh(ssh_host = "user@example.com")$launcher
  expect_silent(launcher$terminate())
})

test_that("crew_controller_ssh() returns crew.ssh's controller class", {
  controller <- crew_controller_ssh(ssh_host = "user@example.com")
  expect_identical(
    class(controller),
    c("crew_class_controller_ssh", "crew_class_controller", "R6")
  )
})

test_that("the salvage arguments are validated", {
  expect_error(
    crew_controller_ssh(ssh_host = "user@example.com", salvage = NA),
    "salvage must be TRUE or FALSE.",
    class = "crew_error"
  )
  expect_error(
    crew_controller_ssh(ssh_host = "user@example.com", salvage = c(TRUE, FALSE)),
    class = "crew_error"
  )
  for (directory in list("relative/path", "~/salvage", NA_character_, c("/a", "/b"), 1)) {
    expect_error(
      crew_controller_ssh(
        ssh_host = "user@example.com",
        salvage_directory = directory
      ),
      "salvage_directory must be NULL or an absolute path",
      class = "crew_error"
    )
  }
  expect_error(
    crew_controller_ssh(
      ssh_host = "user@example.com",
      salvage = TRUE,
      serialization = mirai::serial_config("custom", identity, identity)
    ),
    "cannot be combined with custom serialization",
    class = "crew_error"
  )
})

test_that("seconds_reconnect is validated", {
  for (seconds in list(-1, NA_real_, c(1, 2), "60", NULL)) {
    expect_error(
      crew_controller_ssh(
        ssh_host = "user@example.com",
        seconds_reconnect = seconds
      ),
      "seconds_reconnect must be a single non-negative number.",
      class = "crew_error"
    )
  }
  for (seconds in c(0, 30, Inf)) {
    controller <- crew_controller_ssh(
      ssh_host = "user@example.com",
      seconds_reconnect = seconds
    )
    expect_identical(
      controller$launcher$.__enclos_env__$private$.seconds_reconnect,
      seconds
    )
  }
  default <- crew_controller_ssh(ssh_host = "user@example.com")
  expect_identical(
    default$launcher$.__enclos_env__$private$.seconds_reconnect,
    3600
  )
})

test_that("current_url() points a launch at the tunnel open now", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com"
  )$launcher$.__enclos_env__$private
  call <- paste0(
    "crew::crew_worker(settings = list(url = \"tcp://127.0.0.1:38181\", ",
    "dispatcher = TRUE), controller = \"x\")"
  )
  # Before a tunnel is open there is nothing to point at.
  expect_identical(private$current_url(call), call)
  private$.url <- "tcp://127.0.0.1:55365"
  private$.tunnel_port <- "38479"
  expect_identical(
    private$current_url(call),
    sub("38181", "38479", call, fixed = TRUE)
  )
  private$.url <- "tls+tcp://127.0.0.1:55365"
  tls <- sub("tcp://", "tls+tcp://", call, fixed = TRUE)
  expect_identical(
    private$current_url(tls),
    sub("38181", "38479", tls, fixed = TRUE)
  )
})

test_that("pushed_key() finds the key of a pending task by its name", {
  keys <- new.env(parent = emptyenv())
  assign("fit", "fit-abc", envir = keys)
  expect_identical(pushed_key("fit", keys), "fit-abc")
  expect_identical(pushed_key("other", keys), NA_character_)
  expect_identical(pushed_key(NA_character_, keys), NA_character_)
  expect_identical(pushed_key("", keys), NA_character_)
  expect_identical(pushed_key("fit", NULL), NA_character_)
})

# reconnect_pause() with the launcher's own first and longest pauses.
default_pause <- function(tries, jitter, remaining = Inf) {
  reconnect_pause(
    tries = tries,
    remaining = remaining,
    first = 5,
    longest = 300,
    jitter = jitter
  )
}

test_that("reconnect pauses double up to five minutes, with jitter of 20%", {
  # With the jitter in the middle, the pauses are the plain doubling.
  expect_identical(
    vapply(1:9, default_pause, numeric(1L), jitter = 0.5),
    c(5, 10, 20, 40, 80, 160, 300, 300, 300)
  )
  # Jitter spreads a pause over 80% to 120% of it.
  expect_equal(default_pause(1L, jitter = 0), 4)
  expect_equal(default_pause(3L, jitter = 0.75), 22)
  expect_equal(default_pause(7L, jitter = 0), 240)
  # The longest pause stays the longest, whatever the jitter.
  expect_identical(default_pause(7L, jitter = 0.999), 300)
  expect_identical(default_pause(20L, jitter = 0.999), 300)
  # And no pause outlasts what is left of seconds_reconnect.
  expect_identical(default_pause(5L, jitter = 0.5, remaining = 12), 12)
  expect_identical(default_pause(1L, jitter = 0.999, remaining = 3), 3)
  # The launcher's defaults are the ones tested here.
  expect_identical(
    crew_controller_ssh(
      ssh_host = "user@example.com"
    )$launcher$.__enclos_env__$private$.reconnect_pauses,
    c(first = 5, longest = 300)
  )
})

test_that("salvage is on unless it is turned off", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com"
  )$launcher$.__enclos_env__$private
  expect_true(private$.salvage)
  private <- crew_controller_ssh(
    ssh_host = "user@example.com",
    salvage = FALSE
  )$launcher$.__enclos_env__$private
  expect_false(private$.salvage)
})

test_that("custom serialization needs salvage = FALSE, and says so", {
  serialization <- mirai::serial_config("custom", identity, identity)
  expect_error(
    crew_controller_ssh(
      ssh_host = "user@example.com",
      serialization = serialization
    ),
    "Set salvage = FALSE to use serialization.",
    fixed = TRUE,
    class = "crew_error"
  )
  expect_s3_class(
    crew_controller_ssh(
      ssh_host = "user@example.com",
      serialization = serialization,
      salvage = FALSE
    ),
    "crew_class_controller_ssh"
  )
})

test_that("the salvage directory defaults to one shared by all sessions", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com",
    remote_directory = "/scratch"
  )$launcher$.__enclos_env__$private
  expect_identical(private$salvage_root(), "/scratch/crew-ssh-salvage")
  expect_identical(
    private$salvage_path(c("a-1", "b-2")),
    c("/scratch/crew-ssh-salvage/a-1.rds", "/scratch/crew-ssh-salvage/b-2.rds")
  )
  private <- crew_controller_ssh(
    ssh_host = "user@example.com",
    salvage_directory = "/data/results"
  )$launcher$.__enclos_env__$private
  expect_identical(private$salvage_root(), "/data/results")
})

# The creation time of this R process as a session records it.
created_here <- function() {
  sprintf("%.3f", as.numeric(ps::ps_create_time(ps::ps_handle())))
}

test_that("the tunnel command records the session and then waits on stdin", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com",
    name = "ctl",
    launch_prefix = c("docker", "run", "img"),
    salvage = FALSE
  )$launcher$.__enclos_env__$private
  private$.session <- "crew-ssh-abc"
  private$.session_directory <- "/tmp/crew-ssh-abc"
  expect_identical(
    private$tunnel_command(),
    paste0(
      "mkdir -p '/tmp/crew-ssh-abc' && ",
      "printf '%s\\n' 'session: crew-ssh-abc' 'controller: ctl' ",
      "'runtime: docker' 'local_host: ", Sys.info()[["nodename"]], "' ",
      "'local_pid: ", Sys.getpid(), "' ",
      "'local_created: ", created_here(), "' \"started: $(date +%s)\" > ",
      "'/tmp/crew-ssh-abc/session.dcf' && ",
      "echo $$ > '/tmp/crew-ssh-abc/tunnel.pid' && ",
      "echo crew-ssh-tunnel-ready && ",
      "exec sh -c 'cat > /dev/null; :' 'crew-ssh-abc'"
    )
  )
  private$.salvage <- TRUE
  expect_match(
    private$tunnel_command(),
    "^mkdir -p '/tmp/crew-ssh-abc' '/tmp/crew-ssh-salvage' && "
  )
})

test_that("with salvage, the tunnel command stops stranded waiters before recording itself", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com",
    name = "ctl",
    remote_directory = "/scratch"
  )$launcher$.__enclos_env__$private
  private$.session <- "crew-ssh-abc"
  private$.session_directory <- "/scratch/crew-ssh-abc"
  expect_identical(
    private$tunnel_command(),
    paste0(
      "mkdir -p '/scratch/crew-ssh-abc' '/scratch/crew-ssh-salvage' && {\n",
      remote_stop_waiters_script("/scratch", session = "crew-ssh-abc"),
      "\n:; } && ",
      "printf '%s\\n' 'session: crew-ssh-abc' 'controller: ctl' ",
      "'runtime: ' 'local_host: ", Sys.info()[["nodename"]], "' ",
      "'local_pid: ", Sys.getpid(), "' ",
      "'local_created: ", created_here(), "' \"started: $(date +%s)\" > ",
      "'/scratch/crew-ssh-abc/session.dcf' && ",
      "echo $$ > '/scratch/crew-ssh-abc/tunnel.pid' && ",
      "echo crew-ssh-tunnel-ready && ",
      "exec sh -c 'cat > /dev/null; :' 'crew-ssh-abc'"
    )
  )
  # Sessions known to have ended reach the script that stops their waiters.
  expect_true(
    grepl(
      remote_stop_waiters_script(
        "/scratch",
        session = "crew-ssh-abc",
        dead = c("crew-ssh-old", "crew-ssh-older")
      ),
      private$tunnel_command(dead = c("crew-ssh-old", "crew-ssh-older")),
      fixed = TRUE
    )
  )
})

test_that("salvage_task() leaves a task alone without salvage or a name", {
  launcher <- crew_controller_ssh(
    ssh_host = "user@example.com",
    salvage = FALSE
  )$launcher
  task <- launcher$salvage_task(
    command = quote(f(x)),
    data = list(x = 1),
    globals = list(),
    seed = NULL,
    algorithm = NULL,
    packages = character(0L),
    library = NULL,
    name = "fit"
  )
  expect_identical(task, list(command = quote(f(x)), data = list(x = 1)))
  launcher <- crew_controller_ssh(
    ssh_host = "user@example.com",
    salvage = TRUE
  )$launcher
  expect_identical(
    launcher$salvage_task(
      command = quote(f(x)),
      data = list(x = 1),
      globals = list(),
      seed = NULL,
      algorithm = NULL,
      packages = character(0L),
      library = NULL,
      name = NULL
    ),
    list(command = quote(f(x)), data = list(x = 1))
  )
  expect_identical(
    launcher$salvage_task(
      command = quote(f(x)),
      data = list(x = 1),
      globals = list(),
      seed = NULL,
      algorithm = NULL,
      packages = character(0L),
      library = NULL,
      name = ""
    ),
    list(command = quote(f(x)), data = list(x = 1))
  )
  # A collection before the launcher ever started has nothing to note.
  expect_null(launcher$salvage_collect(name = "fit", status = "success"))
  # A named task needs the session's settings, which start() makes.
  expect_error(
    launcher$salvage_task(
      command = quote(f(x)),
      data = list(),
      globals = list(),
      seed = NULL,
      algorithm = NULL,
      packages = character(0L),
      library = NULL,
      name = "fit"
    ),
    "must be started before tasks are pushed",
    class = "crew_error"
  )
})

push_fit <- function(launcher, data = list(x = 1)) {
  launcher$salvage_task(
    command = quote(f(x)),
    data = data,
    globals = list(f = plus_one_global),
    seed = 1L,
    algorithm = NULL,
    packages = character(0L),
    library = NULL,
    name = "fit"
  )
}

plus_one_global <- global_function(function(x) x + 1)

test_that("salvage_task() wraps a named task to run under salvage", {
  launcher <- salvage_launcher()
  private <- launcher$.__enclos_env__$private
  task <- push_fit(launcher)
  key <- salvage_key(
    command = quote(f(x)),
    data = list(x = 1),
    globals = salvage_globals_cache(list(f = plus_one_global)),
    seed = 1L,
    algorithm = NULL,
    packages = character(0L),
    library = NULL,
    name = "fit"
  )
  expect_identical(
    task$command,
    substitute(
      .crew_ssh_salvage$run(
        expr = quote(f(x)),
        envir = environment(),
        key = key,
        name = "fit",
        reuse = TRUE,
        salvage = .crew_ssh_salvage
      ),
      list(key = key)
    )
  )
  expect_identical(names(task$data), c("x", ".crew_ssh_salvage"))
  expect_identical(task$data$.crew_ssh_salvage, private$.salvage_shipped)
  expect_identical(as.list(private$.salvage_keys), list(fit = key))
  expect_error(
    push_fit(launcher, data = list(.crew_ssh_salvage = 1)),
    "must not contain .crew_ssh_salvage",
    class = "crew_error"
  )
})

test_that("collecting a task queues its saved result for deletion", {
  launcher <- salvage_launcher()
  private <- launcher$.__enclos_env__$private
  task <- push_fit(launcher)
  key <- private$.salvage_keys$fit
  # Tasks this launcher never saw, and crashes, leave everything as it was.
  launcher$salvage_collect(name = "other", status = "success")
  launcher$salvage_collect(name = "fit", status = "crash")
  expect_identical(as.list(private$.salvage_keys), list(fit = key))
  expect_identical(private$.salvage_pending, character(0L))
  launcher$salvage_collect(name = "fit", status = "success")
  expect_identical(ls(private$.salvage_keys), character(0L))
  expect_identical(ls(private$.salvage_collected), key)
  # The deletion started in the background with what was pending.
  expect_identical(private$.salvage_deleting, private$salvage_path(key))
  private$salvage_flush(wait = TRUE)
  expect_null(private$.salvage_cleanup)
  # Pushing it again asks for a new run, not for the result just collected.
  task <- push_fit(launcher)
  expect_false(task$command$reuse)
  launcher$salvage_collect(name = "fit", status = "cancel")
  expect_identical(private$.salvage_pending, character(0L))
})

# What the launcher keeps after crew hands back a task with this status.
collect_with_status <- function(launcher, status, error) {
  private <- launcher$.__enclos_env__$private
  push_fit(launcher)
  launcher$salvage_collect(name = "fit", status = status, error = error)
  out <- list(
    key_kept = exists("fit", envir = private$.salvage_keys, inherits = FALSE),
    collected = length(ls(private$.salvage_collected)),
    pending = length(private$.salvage_pending)
  )
  private$.salvage_collected <- new.env(hash = TRUE, parent = emptyenv())
  private$.salvage_pending <- character(0L)
  private$.salvage_cleanup <- NULL
  out
}

test_that("only a task that delivered an outcome has its saved result deleted", {
  launcher <- salvage_launcher()
  delivered <- list(key_kept = FALSE, collected = 1L, pending = 0L)
  # The deletion is already under way in the background, which is why nothing
  # is left pending.
  expect_identical(
    collect_with_status(launcher, "success", NA_character_),
    delivered
  )
  expect_identical(collect_with_status(launcher, "error", "boom"), delivered)
  # crew reports a task it returned before the task resolved as an error with
  # no message. That task is still running, and its result is not collected.
  not_delivered <- list(key_kept = FALSE, collected = 0L, pending = 0L)
  expect_identical(
    collect_with_status(launcher, "error", NA_character_),
    not_delivered
  )
  expect_identical(
    collect_with_status(launcher, "cancel", "canceled"),
    not_delivered
  )
  # A crash keeps the key, because the task is pushed again under the same
  # name.
  expect_identical(
    collect_with_status(launcher, "crash", "worker exited"),
    list(key_kept = TRUE, collected = 0L, pending = 0L)
  )
})

test_that("pushing a task again cancels the deletion of its result", {
  launcher <- salvage_launcher()
  private <- launcher$.__enclos_env__$private
  push_fit(launcher)
  key <- private$.salvage_keys$fit
  private$.salvage_pending <- c(private$salvage_path(key), "/elsewhere.rds")
  # A deletion still running blocks a new one, so the pending list stays.
  private$.salvage_cleanup <- processx::process$new("sleep", "5")
  on.exit(private$.salvage_cleanup$kill())
  push_fit(launcher)
  expect_identical(private$.salvage_pending, "/elsewhere.rds")
})

test_that("a deletion that fails says so", {
  skip_without_fake_ssh()
  launcher <- salvage_launcher()
  private <- launcher$.__enclos_env__$private
  private$.ssh_command <- write_executable(c("#!/bin/sh", "exit 3"))
  private$.salvage_pending <- c("/remote/a.rds", "/remote/b.rds")
  expect_warning(
    private$salvage_flush(wait = TRUE),
    "crew.ssh could not delete 2 collected result(s)",
    fixed = TRUE
  )
  expect_identical(private$.salvage_pending, character(0L))
  expect_null(private$.salvage_cleanup)
})

test_that("deletions go out in batches of 100", {
  skip_without_fake_ssh()
  launcher <- salvage_launcher()
  private <- launcher$.__enclos_env__$private
  log <- tempfile()
  private$.ssh_command <- write_executable(
    c("#!/bin/sh", paste0("echo \"$#\" >> ", shQuote(log, type = "sh")))
  )
  private$.salvage_pending <- sprintf("/remote/%03d.rds", seq_len(250L))
  private$salvage_flush(wait = FALSE)
  expect_identical(length(private$.salvage_pending), 150L)
  private$salvage_flush(wait = TRUE)
  expect_identical(private$.salvage_pending, character(0L))
  # Three ssh calls, each with its options, the host, and one command.
  expect_identical(readLines(log), c("4", "4", "4"))
})

# Integration ----
# These run real workers through the fake ssh above, so the "remote" system is
# this machine.

test_that("a session records itself and its tunnel on the remote system", {
  skip_without_fake_ssh()
  remote <- remote_tempdir()
  controller <- fake_controller(remote, salvage = TRUE, name = "ctl")
  on.exit(controller$terminate())
  controller$start()
  private <- controller$launcher$.__enclos_env__$private
  session <- private$.session
  record <- read.dcf(file.path(remote, session, "session.dcf"))
  expect_identical(
    colnames(record),
    c(
      "session", "controller", "runtime", "local_host", "local_pid",
      "local_created", "started"
    )
  )
  expect_identical(
    unname(record[1L, c("session", "controller", "local_pid", "local_created")]),
    c(session, "ctl", as.character(Sys.getpid()), created_here())
  )
  expect_true(is.na(record[1L, "runtime"]) || !nzchar(record[1L, "runtime"]))
  expect_identical(
    as.integer(readLines(file.path(remote, session, "tunnel.pid"))),
    private$.tunnel$get_pid()
  )
  expect_true(dir.exists(file.path(remote, "crew-ssh-salvage")))
  settings <- controller$launcher$settings()
  # mirai finishes the task in progress before exiting when autoexit is NA.
  expect_identical(settings$autoexit, NA)
  expect_identical(settings$url, controller$client$url)
})

test_that("without salvage, workers keep crew's exit-at-once setting", {
  skip_without_fake_ssh()
  controller <- fake_controller(remote_tempdir(), salvage = FALSE)
  on.exit(controller$terminate())
  controller$start()
  expect_identical(
    controller$launcher$settings()$autoexit,
    crew::crew_terminate_signal()
  )
  private <- controller$launcher$.__enclos_env__$private
  expect_false(
    dir.exists(file.path(private$.remote_directory, "crew-ssh-salvage"))
  )
})

test_that("a tunnel that exits before it is ready fails loudly", {
  skip_without_fake_ssh()
  controller <- crew_controller_ssh(
    ssh_host = "fakehost",
    ssh_command = write_executable(
      c("#!/bin/sh", "echo 'ssh: connect to host fakehost: refused' >&2", "exit 255")
    ),
    remote_directory = remote_tempdir()
  )
  expect_error(
    controller$start(),
    paste0(
      "The ssh tunnel exited before it was ready. ssh output:\n",
      "ssh: connect to host fakehost: refused"
    ),
    fixed = TRUE
  )
  controller$terminate()
})

test_that("a tunnel that never gets ready times out", {
  skip_without_fake_ssh()
  controller <- crew_controller_ssh(
    ssh_host = "fakehost",
    ssh_command = write_executable(c("#!/bin/sh", "cat > /dev/null")),
    remote_directory = remote_tempdir(),
    seconds_timeout = 1
  )
  expect_error(
    controller$start(),
    "The ssh tunnel did not report a forwarded port within 1 seconds.",
    fixed = TRUE
  )
  controller$terminate()
})

test_that("a tunnel that dropped is reopened at the next launch", {
  skip_without_fake_ssh()
  controller <- fake_controller(remote_tempdir())
  on.exit(controller$terminate())
  controller$start()
  private <- controller$launcher$.__enclos_env__$private
  old <- private$.tunnel$get_pid()
  # A tunnel dies on its own, as when the network drops, not through the
  # handle crew.ssh holds.
  tools::pskill(old)
  wait_until(!private$.tunnel$is_alive(), seconds = 10)
  expect_message(
    controller$launcher$launch_worker(controller$launcher$call()),
    "is down. Reopening it.",
    fixed = TRUE
  )
  expect_false(identical(private$.tunnel$get_pid(), old))
  expect_true(private$.tunnel$is_alive())
})

test_that("a tunnel that cannot be reopened yet is tried again until the network is back", {
  skip_without_fake_ssh()
  failures <- tempfile()
  controller <- fake_controller(remote_tempdir(), salvage = FALSE)
  on.exit(controller$terminate())
  private <- controller$launcher$.__enclos_env__$private
  private$.ssh_command <- flaky_ssh(failures)
  private$.reconnect_pauses <- c(first = 0.1, longest = 0.2)
  controller$start()
  tools::pskill(private$.tunnel$get_pid())
  wait_until(!private$.tunnel$is_alive(), seconds = 10)
  writeLines("2", failures)
  messages <- capture_messages({
    controller$push(command = 6L * 7L)
    controller$wait(mode = "all", seconds_timeout = 60)
  })
  expect_identical(
    sum(
      grepl(
        paste0(
          "crew.ssh: could not reopen the ssh tunnel to fakehost (The ssh ",
          "tunnel exited before it was ready. ssh output: ssh: Could not ",
          "resolve hostname fakehost: Name or service not known). Trying ",
          "again in "
        ),
        messages,
        fixed = TRUE
      )
    ),
    2L
  )
  expect_true(
    any(
      grepl(
        "crew.ssh: managed to reopen the ssh tunnel to fakehost at attempt 3.",
        messages,
        fixed = TRUE
      )
    )
  )
  expect_identical(readLines(failures), "0")
  expect_true(private$.tunnel$is_alive())
  expect_identical(controller$pop()$result[[1L]], 42L)
})

test_that("seconds_reconnect = 0 gives up at the first failure", {
  skip_without_fake_ssh()
  failures <- tempfile()
  controller <- fake_controller(
    remote_tempdir(),
    salvage = FALSE,
    seconds_reconnect = 0
  )
  on.exit(controller$terminate())
  private <- controller$launcher$.__enclos_env__$private
  private$.ssh_command <- flaky_ssh(failures)
  controller$start()
  tools::pskill(private$.tunnel$get_pid())
  wait_until(!private$.tunnel$is_alive(), seconds = 10)
  writeLines("5", failures)
  expect_error(
    suppressMessages(
      controller$launcher$launch_worker(controller$launcher$call())
    ),
    paste0(
      "crew.ssh could not reopen the ssh tunnel to fakehost in 1 attempt(s) ",
      "over 0 seconds of retrying (seconds_reconnect = 0). Last failure: ",
      "The ssh tunnel exited before it was ready. ssh output: ",
      "ssh: Could not resolve hostname fakehost: Name or service not known"
    ),
    fixed = TRUE
  )
  expect_identical(readLines(failures), "4")
})

test_that("a launch that fails on the network is tried again", {
  skip_without_fake_ssh()
  failures <- tempfile()
  controller <- fake_controller(remote_tempdir(), salvage = FALSE)
  on.exit(controller$terminate())
  private <- controller$launcher$.__enclos_env__$private
  private$.ssh_command <- flaky_ssh(failures)
  private$.reconnect_pauses <- c(first = 0.1, longest = 0.2)
  controller$start()
  writeLines("2", failures)
  messages <- capture_messages({
    controller$push(command = 6L * 7L)
    controller$wait(mode = "all", seconds_timeout = 60)
  })
  expect_identical(
    sum(
      grepl(
        paste0(
          "crew.ssh: could not launch a worker on fakehost (ssh exited with ",
          "status 255. stdout: stderr: ssh: Could not resolve hostname ",
          "fakehost: Name or service not known). Trying again in "
        ),
        messages,
        fixed = TRUE
      )
    ),
    2L
  )
  expect_true(
    any(
      grepl(
        "crew.ssh: managed to launch a worker on fakehost at attempt 3.",
        messages,
        fixed = TRUE
      )
    )
  )
  expect_identical(readLines(failures), "0")
  expect_length(private$.handles, 1L)
  expect_identical(controller$pop()$result[[1L]], 42L)
})

test_that("a saved result lives until its task is collected", {
  skip_without_fake_ssh()
  remote <- remote_tempdir()
  controller <- fake_controller(remote, salvage = TRUE)
  on.exit(controller$terminate())
  controller$start()
  controller$push(command = sqrt(x), data = list(x = 49), name = "seven")
  controller$push(command = sqrt(x), data = list(x = 64))
  controller$wait(mode = "all", seconds_timeout = 60)
  saved <- list.files(file.path(remote, "crew-ssh-salvage"))
  # The unnamed task has no identity a later push could match, so nothing of
  # it is saved.
  expect_identical(length(saved), 1L)
  expect_match(saved, "^seven-[0-9a-f]{32}\\.rds$")
  results <- list(controller$pop(), controller$pop())
  values <- vapply(lapply(results, "[[", "result"), "[[", numeric(1L), 1L)
  expect_setequal(values, c(7, 8))
  expect_identical(vapply(results, "[[", character(1L), "status"), c("success", "success"))
  workers <- controller$launcher$remote_workers()
  expect_identical(nrow(workers), 1L)
  expect_identical(workers$task_name, "seven")
  expect_identical(workers$task_state, "success")
  expect_true(workers$alive)
  expect_true(workers$session_alive)
  expect_true(workers$current)
  expect_true(any(grepl(workers$worker, controller$launcher$logs(lines = 5L))))
  controller$terminate()
  expect_identical(list.files(file.path(remote, "crew-ssh-salvage")), character(0L))
  # terminate() stopped the worker.
  wait_until(!pid_alive(workers$pid), seconds = 10)
})

test_that("collect() and controller groups delete what they collect", {
  skip_without_fake_ssh()
  remote <- remote_tempdir()
  controller <- fake_controller(remote, salvage = TRUE, name = "remote")
  group <- crew::crew_controller_group(controller)
  on.exit(group$terminate())
  group$start()
  group$push(command = x + 1, data = list(x = 1), name = "pushed")
  group$wait(mode = "all", seconds_timeout = 60)
  expect_identical(group$pop()$result[[1L]], 2)
  controller$push(command = x + 1, data = list(x = 2), name = "collected")
  controller$wait(mode = "all", seconds_timeout = 60)
  expect_identical(controller$collect()$result[[1L]], 3)
  # walk() pushes one task per element and collect() hands them back. map()
  # does the same in one call, but crew 1.3.2 can return from it before its
  # tasks resolve, which a loaded machine makes common.
  controller$walk(
    command = x + 1,
    iterate = list(x = c(3, 4), name = c("w1", "w2")),
    names = "name",
    verbose = FALSE
  )
  controller$wait(mode = "all", seconds_timeout = 60)
  walked <- controller$collect()
  expect_setequal(walked$name, c("w1", "w2"))
  expect_setequal(unlist(walked$result), c(4, 5))
  private <- controller$launcher$.__enclos_env__$private
  expect_identical(length(ls(private$.salvage_collected)), 4L)
  private$salvage_flush(wait = TRUE)
  expect_identical(list.files(file.path(remote, "crew-ssh-salvage")), character(0L))
})

test_that("without salvage, terminate() stops a worker in the middle of a task", {
  skip_without_fake_ssh()
  remote <- remote_tempdir()
  started <- file.path(remote, "started")
  controller <- fake_controller(remote, salvage = FALSE)
  on.exit(controller$terminate())
  controller$start()
  controller$push(
    command = {
      file.create(started)
      Sys.sleep(120)
    },
    data = list(started = started)
  )
  wait_until({
    controller$scale()
    file.exists(started)
  })
  pid <- as.integer(
    readLines(list.files(remote, "\\.pid$", recursive = TRUE, full.names = TRUE)[
      grepl("worker-", list.files(remote, "\\.pid$", recursive = TRUE))
    ])
  )
  expect_true(pid_alive(pid))
  controller$terminate()
  wait_until(!pid_alive(pid), seconds = 10)
})

test_that("with salvage, terminate() leaves a busy worker to finish and save its task", {
  skip_without_fake_ssh()
  remote <- remote_tempdir()
  counter <- file.path(remote, "runs.txt")
  release <- file.path(remote, "release")
  controller <- fake_controller(remote, salvage = TRUE, workers = 2L)
  on.exit(controller$terminate())
  controller$start()
  controller$push(
    command = held_task(),
    data = list(counter = counter, release = release),
    name = "slow",
    substitute = FALSE
  )
  controller$push(command = 6L * 7L, name = "quick")
  wait_until(
    {
      controller$scale()
      file.exists(counter) && controller$unresolved() < 2L
    },
    seconds = 60
  )
  wait_until(length(worker_pids(remote)) == 2L, seconds = 60)
  pids <- worker_pids(remote)
  states <- task_states(remote)
  busy <- names(states)[states == "running"]
  idle <- setdiff(names(pids), busy)
  expect_length(busy, 1L)
  expect_length(idle, 1L)
  session <- other_session(remote, current = NULL)
  expect_message(
    controller$terminate(),
    paste0("left ", busy, " (slow) running on fakehost to finish and save its task"),
    fixed = TRUE
  )
  wait_until(!pid_alive(pids[[idle]]), seconds = 10)
  expect_true(pid_alive(pids[[busy]]))
  # Marked as gone, so later sessions count the worker left running.
  expect_true(file.exists(file.path(remote, session, "ended")))
  file.create(release)
  wait_until(
    length(list.files(file.path(remote, "crew-ssh-salvage"), "^slow-.*\\.rds$")) == 1L,
    seconds = 60
  )
  wait_until(!pid_alive(pids[[busy]]), seconds = 60)
  expect_identical(count_runs(counter), 1L)
})

test_that("a worker under a container runtime is found and stopped by its container ID", {
  skip_without_fake_ssh()
  remote <- remote_tempdir()
  log <- tempfile()
  started <- file.path(remote, "started")
  docker <- fake_docker(log)
  controller <- fake_controller(
    remote,
    launch_prefix = c(docker, "run", "image")
  )
  on.exit(controller$terminate())
  controller$start()
  controller$push(
    command = {
      file.create(started)
      Sys.sleep(120)
    },
    data = list(started = started)
  )
  wait_until(
    {
      controller$scale()
      file.exists(started)
    },
    seconds = 120
  )
  handles <- controller$launcher$.__enclos_env__$private$.handles
  expect_length(handles, 1L)
  cid_file <- handles[[1L]]$container
  expect_match(cid_file, "/worker-[0-9a-f]+\\.cid$")
  cid <- readLines(cid_file, warn = FALSE)
  expect_match(cid, "^fakecid-[0-9]+$")
  workers <- controller$launcher$remote_workers()
  expect_identical(workers$container, cid)
  expect_identical(workers$runtime, docker)
  expect_true(workers$alive)
  pid <- as.integer(handles[[1L]]$pid)
  controller$terminate()
  # The runtime is asked to kill the container, whose ID it wrote, and the
  # runtime's own client process is signalled too.
  expect_identical(readLines(log), paste("kill", cid))
  wait_until(!pid_alive(pid), seconds = 10)
})

worker_pid <- function(remote, session) {
  files <- list.files(
    file.path(remote, session),
    pattern = "^worker-.*\\.pid$",
    full.names = TRUE
  )
  as.integer(readLines(files))
}

test_that("a worker that loses its session finishes, saves, and hands over", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  remote <- remote_tempdir()
  counter <- file.path(remote, "runs.txt")
  session <- session_in_background(
    remote,
    command = counted_task(seconds = 4),
    data = list(counter = counter),
    name = "slow"
  )
  on.exit(session$kill())
  wait_until(file.exists(counter))
  # The session dies the way a closed laptop's does, with no chance to clean
  # up.
  session$kill()
  old <- other_session(remote, current = NULL)
  wait_until(
    length(list.files(file.path(remote, "crew-ssh-salvage"), "^slow-.*\\.rds$")) == 1L
  )
  wait_until(!pid_alive(worker_pid(remote, old)), seconds = 30)
  log <- readLines(list.files(file.path(remote, old), "\\.log$", full.names = TRUE))
  expect_true(any(grepl("crew worker finished normally", log, fixed = TRUE)))
  controller <- fake_controller(remote, salvage = TRUE)
  on.exit(controller$terminate(), add = TRUE)
  controller$start()
  workers <- controller$launcher$remote_workers()
  expect_identical(workers$session, old)
  expect_identical(workers$alive, FALSE)
  expect_identical(workers$session_alive, FALSE)
  expect_identical(workers$task_state, "success")
  controller$push(
    command = counted_task(seconds = 4),
    data = list(counter = counter),
    name = "slow",
    substitute = FALSE
  )
  controller$wait(mode = "all", seconds_timeout = 60)
  out <- controller$pop()
  expect_identical(out$result[[1L]], 42L)
  expect_identical(count_runs(counter), 1L)
  controller$terminate()
  expect_identical(list.files(file.path(remote, "crew-ssh-salvage")), character(0L))
})

test_that("a reconnecting session waits for a task still running, then collects it", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  remote <- remote_tempdir()
  counter <- file.path(remote, "runs.txt")
  release <- file.path(remote, "release")
  session <- session_in_background(
    remote,
    command = held_task(),
    data = list(counter = counter, release = release),
    name = "slow"
  )
  on.exit(session$kill())
  wait_until(file.exists(counter))
  # The session dies the way a closed laptop's does, with the task running.
  session$kill()
  old <- other_session(remote, current = NULL)
  controller <- fake_controller(remote, salvage = TRUE)
  on.exit(controller$terminate(), add = TRUE)
  controller$start()
  controller$push(
    command = held_task(),
    data = list(counter = counter, release = release),
    name = "slow",
    substitute = FALSE
  )
  # The new session's worker finds the task running and attaches to it.
  wait_until(
    {
      controller$scale()
      identical(sort(unname(task_states(remote))), c("running", "waiting"))
    },
    seconds = 120
  )
  workers <- controller$launcher$remote_workers()
  workers <- workers[order(workers$session_alive), ]
  expect_identical(workers$task_state, c("running", "waiting"))
  expect_identical(workers$session_alive, c(FALSE, TRUE))
  expect_identical(workers$alive, c(TRUE, TRUE))
  expect_identical(workers$task_key[1L], workers$task_key[2L])
  expect_identical(controller$launcher$salvage_list()$state, "running")
  # While the old worker computes, the new session has nothing to collect.
  Sys.sleep(2)
  expect_identical(controller$unresolved(), 1L)
  expect_null(controller$pop())
  file.create(release)
  controller$wait(mode = "all", seconds_timeout = 120)
  out <- controller$pop()
  expect_identical(out$status, "success")
  expect_identical(out$result[[1L]], 42L)
  expect_identical(count_runs(counter), 1L)
  log <- controller$launcher$logs()
  expect_true(any(grepl("so this worker waits for its result", log, fixed = TRUE)))
  expect_true(any(grepl("reuses the result worker", log, fixed = TRUE)))
  controller$terminate()
  expect_identical(list.files(file.path(remote, "crew-ssh-salvage")), character(0L))
  wait_until(!pid_alive(worker_pid(remote, old)), seconds = 30)
})

test_that("a task left running by a dead session counts against workers", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  remote <- remote_tempdir()
  counter <- file.path(remote, "runs.txt")
  release <- file.path(remote, "release")
  session <- session_in_background(
    remote,
    command = held_task(),
    data = list(counter = counter, release = release),
    name = "slow"
  )
  on.exit(session$kill())
  wait_until(file.exists(counter))
  session$kill()
  old <- other_session(remote, current = NULL)
  tunnel <- readLines(file.path(remote, old, "tunnel.pid"))
  wait_until(!pid_alive(tunnel), seconds = 30)
  controller <- fake_controller(remote, salvage = TRUE)
  on.exit(controller$terminate(), add = TRUE)
  private <- controller$launcher$.__enclos_env__$private
  private$.unreachable_seconds <- 0.5
  controller$start()
  expect_message(
    {
      controller$push(command = 6L * 7L, name = "quick")
      controller$wait(mode = "all", seconds_timeout = 3)
    },
    paste0(
      "1 task(s) are still running on workers of fakehost that this ",
      "controller cannot reach (slow). Until each finishes or a worker of ",
      "this controller takes its task over, this controller launches at ",
      "most 0 of its 1 worker(s)."
    ),
    fixed = TRUE
  )
  # The old worker holds the only slot, so nothing launched.
  expect_length(private$.handles, 0L)
  expect_identical(controller$unresolved(), 1L)
  file.create(release)
  expect_message(
    controller$wait(mode = "all", seconds_timeout = 120),
    "no tasks are running on unreachable workers of fakehost any more",
    fixed = TRUE
  )
  expect_identical(controller$pop()$result[[1L]], 42L)
  expect_length(private$.handles, 1L)
  # Pushed again, the slow task hands back what the old worker saved.
  controller$push(
    command = held_task(),
    data = list(counter = counter, release = release),
    name = "slow",
    substitute = FALSE
  )
  controller$wait(mode = "all", seconds_timeout = 60)
  expect_identical(controller$pop()$result[[1L]], 42L)
  expect_identical(count_runs(counter), 1L)
})

test_that("a session started here counts as gone once its R process ends, whatever its tunnel says", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  remote <- remote_tempdir()
  counter <- file.path(remote, "runs.txt")
  release <- file.path(remote, "release")
  session <- session_in_background(
    remote,
    command = held_task(),
    data = list(counter = counter, release = release),
    name = "slow"
  )
  on.exit(session$kill())
  wait_until(file.exists(counter))
  session$kill()
  old <- other_session(remote, current = NULL)
  wait_until(!pid_alive(readLines(file.path(remote, old, "tunnel.pid"))), seconds = 30)
  # A stand-in for the remote end of the old tunnel that sshd keeps after the
  # laptop running the session dropped off the network.
  lingering <- processx::process$new(
    "sh",
    c("-c", "sleep 120; :", old),
    cleanup = TRUE
  )
  on.exit(lingering$kill(), add = TRUE)
  writeLines(as.character(lingering$get_pid()), file.path(remote, old, "tunnel.pid"))
  controller <- fake_controller(remote, salvage = TRUE)
  on.exit(controller$terminate(), add = TRUE)
  private <- controller$launcher$.__enclos_env__$private
  private$.unreachable_seconds <- 0.5
  expect_message(
    controller$start(),
    paste0(
      "closed the remote end of the tunnel of 1 session(s) on fakehost that ",
      "this machine started and whose R process has ended: ", old
    ),
    fixed = TRUE
  )
  wait_until(!lingering$is_alive(), seconds = 10)
  workers <- controller$launcher$remote_workers()
  expect_identical(workers$session_alive[workers$session == old], FALSE)
  # The task still running there takes the only slot.
  expect_message(
    {
      controller$push(command = 6L * 7L, name = "quick")
      controller$wait(mode = "all", seconds_timeout = 3)
    },
    paste0(
      "1 task(s) are still running on workers of fakehost that this ",
      "controller cannot reach (slow)."
    ),
    fixed = TRUE
  )
  expect_length(private$.handles, 0L)
  file.create(release)
  controller$wait(mode = "all", seconds_timeout = 120)
  expect_identical(controller$pop()$result[[1L]], 42L)
})

test_that("remote_workers() counts a session started here as gone once its R process ends", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  remote <- remote_tempdir()
  controller <- fake_controller(remote, salvage = TRUE)
  on.exit(controller$terminate())
  controller$start()
  launcher <- controller$launcher
  private <- launcher$.__enclos_env__$private
  # A record of a session this machine started, whose R process has ended,
  # and whose tunnel end lingers.
  ended <- callr::r_bg(Sys.getpid)
  ended$wait(10000)
  directory <- file.path(remote, "crew-ssh-ended")
  dir.create(directory)
  lingering <- processx::process$new(
    "sh",
    c("-c", "sleep 120; :", "crew-ssh-ended"),
    cleanup = TRUE
  )
  on.exit(lingering$kill(), add = TRUE)
  writeLines(
    c(
      "session: crew-ssh-ended",
      paste("local_host:", Sys.info()[["nodename"]]),
      paste("local_pid:", ended$get_pid()),
      "local_created: 1"
    ),
    file.path(directory, "session.dcf")
  )
  writeLines(as.character(lingering$get_pid()), file.path(directory, "tunnel.pid"))
  writeLines(as.character(lingering$get_pid()), file.path(directory, "worker-a.pid"))
  workers <- launcher$remote_workers()
  expect_identical(workers$session, "crew-ssh-ended")
  expect_identical(workers$session_alive, FALSE)
  # Its own session, started by this process, stays connected.
  expect_identical(
    local_session_alive(
      Sys.info()[["nodename"]],
      Sys.getpid(),
      as.numeric(ps::ps_create_time(ps::ps_handle()))
    ),
    TRUE
  )
  expect_identical(private$dead_sessions(), "crew-ssh-ended")
})

test_that("a task pushed again behind other work waits while its old run counts", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  remote <- remote_tempdir()
  counter <- file.path(remote, "runs.txt")
  release <- file.path(remote, "release")
  old_pid <- dead_session_task(remote, counter, release)
  controller <- fake_controller(remote, salvage = TRUE)
  on.exit(controller$terminate(), add = TRUE)
  private <- controller$launcher$.__enclos_env__$private
  private$.unreachable_seconds <- 0.5
  controller$start()
  controller$push(command = 6L * 7L, name = "other")
  controller$push(
    command = held_task(),
    data = list(counter = counter, release = release),
    name = "slow",
    substitute = FALSE
  )
  # The worker launched next might take "other" while the old run goes on,
  # so the old run holds the only slot until it ends and nothing launches.
  controller$wait(mode = "all", seconds_timeout = 3)
  expect_length(private$.handles, 0L)
  expect_identical(private$unreachable_count(2L), 1L)
  file.create(release)
  controller$wait(mode = "all", seconds_timeout = 120)
  results <- controller$collect()
  expect_setequal(results$name, c("other", "slow"))
  expect_identical(results$result[results$name == "slow"][[1L]], 42L)
  expect_identical(count_runs(counter), 1L)
  wait_until(!pid_alive(old_pid), seconds = 30)
})

test_that("a task pushed again holds the slot its old worker counted against", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  remote <- remote_tempdir()
  counter <- file.path(remote, "runs.txt")
  release <- file.path(remote, "release")
  dead_session_task(remote, counter, release)
  controller <- fake_controller(remote, salvage = TRUE, workers = 2L)
  on.exit(controller$terminate(), add = TRUE)
  private <- controller$launcher$.__enclos_env__$private
  private$.unreachable_seconds <- 0.5
  controller$start()
  controller$push(
    command = held_task(),
    data = list(counter = counter, release = release),
    name = "slow",
    substitute = FALSE
  )
  # With a slot to spare, the worker launched takes the task, attaches, and
  # waits instead of starting a second run. Once the records show it, that
  # worker holds the slot and the old run no longer counts.
  wait_until(
    {
      controller$scale()
      identical(sort(unname(task_states(remote))), c("running", "waiting"))
    },
    seconds = 120
  )
  wait_until(
    {
      controller$scale()
      identical(private$unreachable_count(1L), 0L)
    },
    seconds = 30
  )
  file.create(release)
  controller$wait(mode = "all", seconds_timeout = 120)
  expect_identical(controller$pop()$result[[1L]], 42L)
  expect_identical(count_runs(counter), 1L)
})

test_that("a task pushed again with different inputs stops the old run in its place", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  remote <- remote_tempdir()
  counter <- file.path(remote, "runs.txt")
  release <- file.path(remote, "release")
  old_pid <- dead_session_task(remote, counter, release, data = list(version = 1L))
  old_key <- unname(read.dcf(
    list.files(remote, "\\.task$", recursive = TRUE, full.names = TRUE)
  )[1L, "task_key"])
  lock <- file.path(remote, "crew-ssh-salvage", paste0(old_key, ".lock"))
  expect_true(dir.exists(lock))
  controller <- fake_controller(remote, salvage = TRUE)
  on.exit(controller$terminate(), add = TRUE)
  private <- controller$launcher$.__enclos_env__$private
  private$.unreachable_seconds <- 0.5
  controller$start()
  expect_message(
    {
      controller$push(
        command = held_task(),
        data = list(counter = counter, release = release, version = 2L),
        name = "slow",
        substitute = FALSE
      )
      wait_until(
        {
          controller$scale()
          !pid_alive(old_pid)
        },
        seconds = 60
      )
    },
    paste0(
      "of crew-ssh-[0-9a-z]+ \\(slow\\) on fakehost: each ran an earlier ",
      "version of a task that this controller has pushed again"
    )
  )
  expect_false(dir.exists(lock))
  # The slot it held goes to the new version, which runs.
  wait_until(
    {
      controller$scale()
      count_runs(counter) == 2L
    },
    seconds = 120
  )
  file.create(release)
  controller$wait(mode = "all", seconds_timeout = 120)
  expect_identical(controller$pop()$result[[1L]], 42L)
  expect_identical(count_runs(counter), 2L)
})

test_that("a run whose key was computed another way is never taken for a changed task", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  remote <- remote_tempdir()
  counter <- file.path(remote, "runs.txt")
  release <- file.path(remote, "release")
  old_pid <- dead_session_task(remote, counter, release, data = list(version = 1L))
  # As a crew.ssh from before keys recorded how they were computed wrote it.
  task <- list.files(remote, "\\.task$", recursive = TRUE, full.names = TRUE)
  record <- read.dcf(task)
  write.dcf(record[, colnames(record) != "task_scheme", drop = FALSE], task)
  controller <- fake_controller(remote, salvage = TRUE)
  on.exit(controller$terminate(), add = TRUE)
  private <- controller$launcher$.__enclos_env__$private
  private$.unreachable_seconds <- 0.5
  controller$start()
  controller$push(
    command = held_task(),
    data = list(counter = counter, release = release, version = 2L),
    name = "slow",
    substitute = FALSE
  )
  controller$wait(mode = "all", seconds_timeout = 3)
  expect_true(pid_alive(old_pid))
  expect_identical(private$unreachable_count(1L), 1L)
  expect_length(private$.handles, 0L)
  file.create(release)
  controller$wait(mode = "all", seconds_timeout = 120)
  expect_identical(controller$pop()$result[[1L]], 42L)
  expect_identical(count_runs(counter), 2L)
})

test_that("backlog_first() picks the unpushed tasks with a run going or a saved result", {
  keys <- new.env(hash = TRUE, parent = emptyenv())
  assign("e", "e-0", envir = keys)
  expect_identical(
    backlog_first(
      names = c("a", "b", "c", "d", "e", "a b"),
      running = c("b", "e", "zzz"),
      saved = c("d-0123", "a_b-99"),
      keys = keys
    ),
    c("b", "d", "a b")
  )
  expect_identical(
    backlog_first(character(0L), "b", "d-0", keys),
    character(0L)
  )
  expect_identical(
    backlog_first(c("a", "b"), character(0L), character(0L), keys),
    character(0L)
  )
})

# A controller with an empty backlog but no tunnel. crew's terminate() gives
# an unstarted controller the empty backlog that start() would.
backlog_controller <- function(...) {
  controller <- crew_controller_ssh(ssh_host = "fakehost", ...)
  controller$terminate()
  controller
}

backlog_names <- function(controller) {
  queue <- controller$.__enclos_env__$private$.queue_backlog
  as.character(unlist(queue$as_list()))
}

test_that("pop_backlog() sends tasks an earlier session left ahead of the rest, even when saturated", {
  controller <- backlog_controller(workers = 1L)
  launcher <- controller$launcher
  private <- launcher$.__enclos_env__$private
  private$.unreachable <- remote_unreachable_tasks(
    remote_workers_frame(
      paste(
        worker_record("crew-ssh-dead", "FALSE", "worker-a", "TRUE", "running", "b-1", "b"),
        collapse = "\n"
      ),
      "fakehost", "/remote", session = "crew-ssh-me"
    ),
    disconnected = character(0L)
  )
  private$.salvage_saved <- "d-0123"
  private$.salvage_keys <- new.env(hash = TRUE, parent = emptyenv())
  for (name in c("a", "b", "c", "d")) {
    controller$push_backlog(name)
  }
  # One slot is free, which the backlog's first task takes as usual.
  expect_identical(controller$pop_backlog(), c("b", "d", "a"))
  expect_identical(backlog_names(controller), "c")
  # Until the chosen tasks are pushed, targets is told there is room.
  expect_true(launcher$salvage_passing())
  expect_false(controller$saturated())
  expect_identical(controller$pop_backlog(), "c")
  expect_false(launcher$salvage_passing())
  expect_identical(controller$pop_backlog(), character(0L))
})

test_that("an unreachable run gives its slot to its push only when nothing else waits", {
  launcher <- crew_controller_ssh(ssh_host = "fakehost")$launcher
  private <- launcher$.__enclos_env__$private
  private$.unreachable <- remote_unreachable_tasks(
    remote_workers_frame(
      paste(
        c(
          worker_record("crew-ssh-dead", "FALSE", "worker-a", "TRUE", "running", "a-1", "a"),
          worker_record("crew-ssh-dead", "FALSE", "worker-b", "TRUE", "running", "b-1", "b"),
          worker_record("crew-ssh-dead", "FALSE", "worker-c", "TRUE", "running", "c-1", "c"),
          # This session's worker has taken b over.
          worker_record("crew-ssh-me", "TRUE", "worker-d", "TRUE", "waiting", "b-1", "b")
        ),
        collapse = "
"
      ),
      "fakehost", "/remote", session = "crew-ssh-me"
    ),
    disconnected = character(0L)
  )
  private$.salvage_keys <- new.env(hash = TRUE, parent = emptyenv())
  # a is pushed again under its key, c under a new one.
  assign("a", "a-1", envir = private$.salvage_keys)
  assign("c", "c-2", envir = private$.salvage_keys)
  expect_identical(private$unreachable_repushed(), c(TRUE, FALSE, FALSE))
  expect_true(private$unreachable_pending())
  # Only a's push waits, so the next worker takes it: a gives up its slot.
  expect_identical(private$unreachable_count(1L), 1L)
  # Another task waits too and could take that worker: a keeps counting.
  expect_identical(private$unreachable_count(2L), 2L)
  expect_identical(private$unreachable_count(0L), 2L)
  private$.unreachable <- remote_unreachable_none()
  expect_identical(private$unreachable_count(3L), 0L)
  expect_false(private$unreachable_pending())
})

test_that("pushing a chosen task clears its pass", {
  launcher <- salvage_launcher()
  private <- launcher$.__enclos_env__$private
  private$.salvage_passes <- c("fit", "other")
  push_fit(launcher)
  expect_identical(private$.salvage_passes, "other")
  expect_true(launcher$salvage_passing())
})

test_that("without salvage, the backlog keeps its order and nothing passes", {
  controller <- backlog_controller(salvage = FALSE)
  launcher <- controller$launcher
  for (name in c("a", "b")) {
    controller$push_backlog(name)
  }
  expect_identical(launcher$salvage_backlog(c("a", "b")), character(0L))
  expect_false(launcher$salvage_passing())
  expect_identical(controller$pop_backlog(), "a")
  expect_identical(backlog_names(controller), "b")
})

test_that("a task whose worker was killed outright is run again once its lock goes quiet", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  remote <- remote_tempdir()
  counter <- file.path(remote, "runs.txt")
  release <- file.path(remote, "release")
  session <- session_in_background(
    remote,
    command = held_task(),
    data = list(counter = counter, release = release),
    name = "slow"
  )
  on.exit(session$kill())
  wait_until(file.exists(counter))
  session$kill()
  old <- other_session(remote, current = NULL)
  pid <- worker_pid(remote, old)
  # No chance to finish, save, or release the lock.
  tools::pskill(pid, tools::SIGKILL)
  wait_until(!pid_alive(pid), seconds = 10)
  controller <- fake_controller(remote, salvage = TRUE)
  on.exit(controller$terminate(), add = TRUE)
  controller$start()
  file.create(release)
  controller$push(
    command = held_task(),
    data = list(counter = counter, release = release),
    name = "slow",
    substitute = FALSE
  )
  controller$wait(mode = "all", seconds_timeout = 120)
  expect_identical(controller$pop()$result[[1L]], 42L)
  # The first run died with its worker and the second finished the task.
  expect_identical(count_runs(counter), 2L)
  log <- controller$launcher$logs()
  expect_true(
    any(grepl("which went silent", log, fixed = TRUE)),
    info = paste(log, collapse = "\n")
  )
})

test_that("terminate_orphans() stops the workers of sessions that are gone", {
  skip_without_fake_ssh()
  skip_if_not_installed("callr")
  remote <- remote_tempdir()
  counter <- file.path(remote, "runs.txt")
  session <- session_in_background(
    remote,
    command = counted_task(seconds = 120),
    data = list(counter = counter),
    name = "endless"
  )
  on.exit(session$kill())
  wait_until(file.exists(counter))
  session$kill()
  controller <- fake_controller(remote, salvage = TRUE)
  on.exit(controller$terminate(), add = TRUE)
  controller$start()
  wait_until(
    identical(controller$launcher$remote_workers()$session_alive, FALSE)
  )
  orphans <- controller$launcher$remote_workers()
  expect_identical(orphans$alive, TRUE)
  expect_identical(orphans$task_state, "running")
  stopped <- controller$launcher$terminate_orphans()
  expect_identical(stopped$worker, orphans$worker)
  wait_until(!pid_alive(orphans$pid), seconds = 10)
  expect_identical(nrow(controller$launcher$terminate_orphans()), 0L)
  # Its lock outlives it, and goes quiet.
  wait_until(
    identical(controller$launcher$salvage_list()$state, "abandoned"),
    seconds = 30
  )
})

test_that("salvage_list() and salvage_remove() manage results nothing collects", {
  skip_without_fake_ssh()
  remote <- remote_tempdir()
  directory <- file.path(remote, "crew-ssh-salvage")
  controller <- fake_controller(remote, salvage = TRUE)
  on.exit(controller$terminate())
  controller$start()
  expect_identical(nrow(controller$launcher$salvage_list()), 0L)
  saveRDS(list(1), file.path(directory, "old-1.rds"))
  saveRDS(list(2), file.path(directory, "old-2.rds"))
  listed <- controller$launcher$salvage_list()
  expect_identical(listed$key, c("old-1", "old-2"))
  expect_identical(listed$state, c("saved", "saved"))
  expect_identical(
    listed$bytes,
    as.numeric(file.size(file.path(directory, c("old-1.rds", "old-2.rds"))))
  )
  expect_identical(controller$launcher$salvage_remove("old-1"), "old-1")
  expect_identical(list.files(directory), "old-2.rds")
  expect_identical(controller$launcher$salvage_remove(character(0L)), character(0L))
  for (keys in list("../old-2", "a b", NA_character_, 1L, "x;rm")) {
    expect_error(
      controller$launcher$salvage_remove(keys),
      "keys must be task keys",
      class = "crew_error"
    )
  }
  expect_identical(list.files(directory), "old-2.rds")
})

test_that("remote commands that fail raise errors that name the host", {
  skip_without_fake_ssh()
  controller <- crew_controller_ssh(
    ssh_host = "fakehost",
    ssh_command = write_executable(c("#!/bin/sh", "echo 'no route' >&2", "exit 255")),
    remote_directory = remote_tempdir()
  )
  launcher <- controller$launcher
  expect_error(
    launcher$remote_workers(),
    paste0(
      "crew.ssh could not list the remote workers on fakehost. ",
      "ssh exited with status 255.\nstderr: no route"
    ),
    fixed = TRUE
  )
  expect_error(launcher$salvage_list(), "could not list the saved results", fixed = TRUE)
  expect_error(launcher$salvage_remove("a-1"), "could not delete the saved results", fixed = TRUE)
  expect_error(launcher$terminate_orphans(), "could not list the remote workers", fixed = TRUE)
})

test_that("verbose = TRUE reports the tunnel and each launch", {
  skip_without_fake_ssh()
  controller <- fake_controller(remote_tempdir(), verbose = TRUE)
  on.exit(controller$terminate())
  messages <- testthat::capture_messages(controller$start())
  expect_match(messages[1L], "^crew.ssh: opening tunnel: ")
  expect_match(messages[2L], "^crew.ssh: tunnel open, remote workers will dial tcp://127.0.0.1:")
  expect_message(
    controller$launcher$launch_worker(controller$launcher$call()),
    "^crew.ssh: launching worker-[0-9a-f]+, log at "
  )
})

test_that("a launch that fails on the remote system fails loudly", {
  skip_without_fake_ssh()
  # A host that opens the tunnel but cannot start workers, as when the remote
  # directory is not writable.
  broken <- write_executable(
    c(
      fake_ssh_lines[-length(fake_ssh_lines)],
      "case \"$1\" in *nohup*) echo 'mkdir: Permission denied' >&2; exit 1 ;; esac",
      "exec sh -c \"$1\""
    )
  )
  controller <- fake_controller(remote_tempdir())
  private <- controller$launcher$.__enclos_env__$private
  private$.ssh_command <- broken
  on.exit(controller$terminate())
  controller$start()
  expect_error(
    controller$launcher$launch_worker(controller$launcher$call()),
    paste0(
      "Could not launch a crew worker on fakehost. ssh exited with status 1.\n",
      "stdout: \nstderr: mkdir: Permission denied"
    ),
    fixed = TRUE
  )
})

test_that("draining a tunnel that is not open does nothing", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com"
  )$launcher$.__enclos_env__$private
  expect_null(private$tunnel_drain())
  expect_identical(private$.tunnel_output, character(0L))
})

test_that("a deletion that hangs is stopped when the launcher terminates", {
  skip_without_fake_ssh()
  launcher <- salvage_launcher()
  private <- launcher$.__enclos_env__$private
  private$.seconds_timeout <- 0.5
  private$.ssh_command <- write_executable(c("#!/bin/sh", "sleep 30"))
  private$.salvage_pending <- "/remote/a.rds"
  private$salvage_flush(wait = FALSE)
  hung <- private$.salvage_cleanup
  expect_warning(
    private$salvage_flush(wait = TRUE),
    "crew.ssh could not delete 1 collected result(s)",
    fixed = TRUE
  )
  expect_false(hung$is_alive())
  expect_null(private$.salvage_cleanup)
})

# Live tests ----
# These need a remote system that the ssh command can reach non-interactively
# and that has R and crew installed. Point CREW_SSH_TEST_HOST at it, e.g.
# Sys.setenv(CREW_SSH_TEST_HOST = "user@example.com").

skip_without_host <- function() {
  host <- Sys.getenv("CREW_SSH_TEST_HOST", unset = "")
  testthat::skip_if(
    !nzchar(host),
    "CREW_SSH_TEST_HOST is not set, so live ssh tests are skipped."
  )
  host
}

test_that("tasks run on the remote system and results come back", {
  host <- skip_without_host()
  controller <- crew_controller_ssh(ssh_host = host, workers = 2L,
                                    seconds_idle = 30)
  on.exit(controller$terminate(), add = TRUE)
  controller$start()
  # The tunnel gives the workers a loopback URL on the remote system, which is
  # not the URL the dispatcher itself listens on.
  expect_match(controller$launcher$settings()$url, "^tcp://127\\.0\\.0\\.1:[0-9]+$")
  expect_false(identical(controller$launcher$settings()$url, controller$client$url))
  controller$push(
    command = list(node = Sys.info()[["nodename"]], value = 21L * 2L),
    name = "remote"
  )
  controller$wait(mode = "all", seconds_timeout = 120)
  out <- controller$pop()
  expect_equal(out$name, "remote")
  expect_true(is.na(out$error))
  expect_equal(out$result[[1L]]$value, 42L)
  expect_false(identical(out$result[[1L]]$node, Sys.info()[["nodename"]]))
})

test_that("worker errors travel back to the controller", {
  host <- skip_without_host()
  controller <- crew_controller_ssh(ssh_host = host, seconds_idle = 30)
  on.exit(controller$terminate(), add = TRUE)
  controller$start()
  controller$push(command = stop("boom"), name = "bad")
  controller$wait(mode = "all", seconds_timeout = 120)
  out <- controller$pop()
  expect_equal(out$error, "boom")
})

test_that("launch_worker() reports a remote PID and a readable log", {
  host <- skip_without_host()
  controller <- crew_controller_ssh(ssh_host = host, seconds_idle = 30)
  on.exit(controller$terminate(), add = TRUE)
  controller$start()
  handle <- controller$launcher$launch_worker(controller$launcher$call())
  expect_match(handle$pid, "^[0-9]+$")
  expect_match(handle$script, "\\.R$")
  expect_match(handle$log, "\\.log$")
  expect_true(any(grepl(handle$name, controller$launcher$logs(lines = 5L))))
})

test_that("automatic TLS survives the port rewrite", {
  host <- skip_without_host()
  controller <- crew_controller_ssh(
    ssh_host = host,
    tls = crew::crew_tls(mode = "automatic"),
    seconds_idle = 30
  )
  on.exit(controller$terminate(), add = TRUE)
  controller$start()
  # mirai derives the certificate common name from the dispatcher hostname, so
  # only the port may differ between the two URLs.
  expect_match(controller$launcher$settings()$url, "^tls\\+tcp://127\\.0\\.0\\.1:")
  controller$push(command = 21L * 2L, name = "tls")
  controller$wait(mode = "all", seconds_timeout = 120)
  out <- controller$pop()
  expect_true(is.na(out$error))
  expect_equal(out$result[[1L]], 42L)
})

test_that("an unreachable host fails loudly instead of hanging", {
  skip_without_host()
  controller <- crew_controller_ssh(
    ssh_host = "crew-ssh-no-such-host.invalid",
    seconds_timeout = 30
  )
  expect_error(controller$start(), class = "crew_error")
})

test_that("a container worker is stopped through its runtime, even without --init", {
  image <- skip_without_docker()
  remote <- remote_tempdir()
  started <- file.path(remote, "started")
  # Without --init, R is the container's first process and ignores the
  # SIGTERM that reaches it through the runtime's client.
  controller <- fake_controller(
    remote,
    launch_prefix = docker_prefix(image, remote, init = FALSE),
    rscript = "Rscript",
    seconds_launch = 300
  )
  on.exit(controller$terminate())
  controller$start()
  controller$push(
    command = {
      file.create(started)
      Sys.sleep(300)
    },
    data = list(started = started)
  )
  wait_until(
    {
      controller$scale()
      file.exists(started)
    },
    seconds = 300
  )
  cid <- container_ids(remote)
  expect_length(cid, 1L)
  expect_true(container_running(cid))
  workers <- controller$launcher$remote_workers()
  expect_identical(workers$container, cid)
  expect_identical(workers$runtime, "docker")
  expect_true(workers$alive)
  controller$terminate()
  wait_until(!container_running(cid), seconds = 60)
})

test_that("a container worker that loses its session finishes, saves, and exits", {
  image <- skip_without_docker()
  skip_if_not_installed("callr")
  remote <- remote_tempdir()
  counter <- file.path(remote, "runs.txt")
  # This is the configuration of the incident behind salvage: no --init.
  prefix <- docker_prefix(image, remote, init = FALSE)
  session <- session_in_background(
    remote,
    command = counted_task(seconds = 8),
    data = list(counter = counter),
    name = "slow",
    launch_prefix = prefix,
    rscript = "Rscript"
  )
  on.exit(session$kill())
  wait_until(file.exists(counter), seconds = 300)
  cid <- container_ids(remote)
  session$kill()
  wait_until(
    length(list.files(file.path(remote, "crew-ssh-salvage"), "^slow-.*\\.rds$")) == 1L,
    seconds = 60
  )
  wait_until(!container_running(cid), seconds = 60)
  controller <- fake_controller(
    remote,
    salvage = TRUE,
    launch_prefix = prefix,
    rscript = "Rscript",
    seconds_launch = 300
  )
  on.exit(controller$terminate(), add = TRUE)
  controller$start()
  controller$push(
    command = counted_task(seconds = 8),
    data = list(counter = counter),
    name = "slow",
    substitute = FALSE
  )
  controller$wait(mode = "all", seconds_timeout = 300)
  expect_identical(controller$pop()$result[[1L]], 42L)
  expect_identical(count_runs(counter), 1L)
})

test_that("a container worker still running is waited for by a reconnecting session", {
  image <- skip_without_docker()
  skip_if_not_installed("callr")
  remote <- remote_tempdir()
  counter <- file.path(remote, "runs.txt")
  release <- file.path(remote, "release")
  prefix <- docker_prefix(image, remote, init = FALSE)
  session <- session_in_background(
    remote,
    command = held_task(),
    data = list(counter = counter, release = release),
    name = "slow",
    launch_prefix = prefix,
    rscript = "Rscript"
  )
  on.exit(session$kill())
  wait_until(file.exists(counter), seconds = 300)
  cid <- container_ids(remote)
  session$kill()
  controller <- fake_controller(
    remote,
    salvage = TRUE,
    launch_prefix = prefix,
    rscript = "Rscript",
    seconds_launch = 300
  )
  on.exit(controller$terminate(), add = TRUE)
  controller$start()
  controller$push(
    command = held_task(),
    data = list(counter = counter, release = release),
    name = "slow",
    substitute = FALSE
  )
  wait_until(
    {
      controller$scale()
      identical(sort(unname(task_states(remote))), c("running", "waiting"))
    },
    seconds = 300
  )
  # The old container is still computing for a session that no longer exists.
  expect_true(container_running(cid))
  workers <- controller$launcher$remote_workers()
  expect_identical(sum(workers$alive), 2L)
  file.create(release)
  controller$wait(mode = "all", seconds_timeout = 300)
  expect_identical(controller$pop()$result[[1L]], 42L)
  expect_identical(count_runs(counter), 1L)
  wait_until(!container_running(cid), seconds = 60)
})
