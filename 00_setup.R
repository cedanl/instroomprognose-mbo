.onLoad <- function(libname, pkgname) {
    # Set clock for devtools::check() verify current time to pass
    Sys.setenv(R_CHECK_SYSTEM_CLOCK = 0)
}

# Check and init project files, remove functions from environment afterwards
source("utils/setup_functions.R")
init_project_files()
rm(generate_project_id,init_project_files, make_valid_package_name)

source("utils/dev_functions.R")
source("utils/manage_packages.R")

load_all()

# TODO Change default in own config if needed
Sys.setenv(R_CONFIG_ACTIVE = "default")
# Sys.setenv(R_CONFIG_ACTIVE = "cambo")

