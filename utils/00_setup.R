.onLoad <- function(libname, pkgname) {
  # Set clock for devtools::check() verify current time to pass
  Sys.setenv(R_CHECK_SYSTEM_CLOCK = 0)
}

# Functions ran before the packages are loaded
source("utils/dev_functions.R")

# Other functions
#source("utils/dev_functions.R")

platform <- get_platform()

## TODO This is needed at the first time, but slows down starting up
# if (platform == "macOS") {
#   source("utils/install_mac_os_system_deps.R")
# }
#
# # TODO Only needed for dev-branch where packages need to be installed, rtools detection
# # is buggy
# if (platform == "windows") {
#     source("utils/install_windows_system_deps.R")
# }

source("utils/manage_packages.R")

source("utils/set_rstudio_prefs.R")

load_all()
