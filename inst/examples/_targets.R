library(targets)

ssh_controller <- crew.ssh::crew_controller_ssh(
  ssh_host       = "bill@picasso.humanpredictions.local",
  ssh_keyfile    = file.path(Sys.getenv("USERPROFILE"), ".ssh", "id_ed25519"),
  workers        = 2L,
  seconds_idle   = 30,
  # host must be the local machine's IP as reachable from the remote server
  host           = nanonext::ip_addr()[1]
)

tar_option_set(controller = ssh_controller)

list(
  tar_target(data,        mtcars),
  tar_target(model1,      stats::lm(mpg ~ cyl, data = data)),
  tar_target(remote_info, paste(Sys.info()[["nodename"]], ps::ps_pid()))
)
