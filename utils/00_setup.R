.onLoad <- function(libname, pkgname) {
  # Set clock for devtools::check() verify current time to pass
  Sys.setenv(R_CHECK_SYSTEM_CLOCK = 0)
}

source("utils/dev_functions.R")

platform <- get_platform()

if (platform == "macOS") {
  source("utils/install_mac_os_system_deps.R")
}

# TODO Only needed for dev-branch where packages need to be installed, rtools detection
# is buggy
# if (platform == "windows") {
#     source("utils/install_windows_system_deps.R")
# }

source("utils/manage_packages.R")

source("utils/set_rstudio_prefs.R")

load_all()

message("🔧 Pas aan voor data van je eigen instelling: config.yml")
message("🔍 Render voor analyse: Instroomprognose_prototype.qmd")
#TODO Starting in R Studio works, interactive is more general, later on might need
#to verify if this works in VS Code / Positron etc
if (rstudioapi::isAvailable()) {
  rstudioapi::navigateToFile("config.yml")
  rstudioapi::navigateToFile("instroomprognose_prototype.qmd")
}

rm(list = ls())
