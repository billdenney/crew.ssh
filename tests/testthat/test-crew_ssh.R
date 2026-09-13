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

test_that("the worker command uploads the script, detaches R, and reports the PID", {
  launcher <- crew_controller_ssh(
    ssh_host = "user@example.com",
    rscript = "/usr/local/bin/Rscript",
    r_arguments = c("--no-save", "--no-restore")
  )$launcher
  private <- launcher$.__enclos_env__$private
  private$.session_directory <- "/tmp/crew-ssh-abc"
  command <- private$worker_command(
    script = "/tmp/crew-ssh-abc/worker-1.R",
    log = "/tmp/crew-ssh-abc/worker-1.log"
  )
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
    "> '/tmp/crew-ssh-abc/worker-1.log' 2>&1 < /dev/null & echo $!",
    fixed = TRUE
  )
  expect_no_match(command, "cd ", fixed = TRUE)
})

test_that("the worker command changes directory when asked", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com",
    directory = "/home/user/project"
  )$launcher$.__enclos_env__$private
  private$.session_directory <- "/tmp/crew-ssh-abc"
  command <- private$worker_command(script = "s.R", log = "s.log")
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
  command <- private$worker_command(script = "/tmp/crew ssh/w.R", log = "/l.log")
  expect_match(command, "mkdir -p '/tmp/crew ssh';", fixed = TRUE)
  expect_match(command, "cd '/home/user/my project';", fixed = TRUE)
  expect_match(command, "nohup '/opt/R 4.6/bin/Rscript'", fixed = TRUE)
  expect_match(command, "'/tmp/crew ssh/w.R'", fixed = TRUE)
})

test_that("no launch_prefix leaves the worker command running Rscript directly", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com"
  )$launcher$.__enclos_env__$private
  private$.session_directory <- "/tmp/crew-ssh-abc"
  command <- private$worker_command(script = "s.R", log = "s.log")
  expect_match(command, "nohup 'Rscript' '--no-save'", fixed = TRUE)
})

test_that("launch_prefix runs the worker under a container runtime", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com",
    launch_prefix = c(
      "docker", "run", "--rm", "--network=host",
      "--volume", "/tmp:/tmp", "my-image"
    )
  )$launcher$.__enclos_env__$private
  private$.session_directory <- "/tmp/crew-ssh-abc"
  expect_identical(
    private$worker_command(
      script = "/tmp/crew-ssh-abc/worker-1.R",
      log = "/tmp/crew-ssh-abc/worker-1.log"
    ),
    paste0(
      "set -e; mkdir -p '/tmp/crew-ssh-abc'; ",
      "cat > '/tmp/crew-ssh-abc/worker-1.R'; ",
      "nohup 'docker' 'run' '--rm' '--network=host' ",
      "'--volume' '/tmp:/tmp' 'my-image' ",
      "'Rscript' '--no-save' '--no-restore' ",
      "'/tmp/crew-ssh-abc/worker-1.R' ",
      "> '/tmp/crew-ssh-abc/worker-1.log' 2>&1 < /dev/null & echo $!"
    )
  )
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
    private$worker_command(script = "s.R", log = "s.log"),
    "nohup 'docker' 'run' 'my-image' '/usr/local/bin/Rscript' '--vanilla' 's.R'",
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
    private$worker_command(script = "s.R", log = "s.log"),
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

test_that("an empty launch_prefix means no prefix", {
  private <- crew_controller_ssh(
    ssh_host = "user@example.com",
    launch_prefix = character(0L)
  )$launcher$.__enclos_env__$private
  private$.session_directory <- "/tmp/crew-ssh-abc"
  expect_match(
    private$worker_command(script = "s.R", log = "s.log"),
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
