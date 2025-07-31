## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for MBOA Analysis - Cefero
## This source code is licensed under the MIT license found in the
## LICENSE file in the root directory of this repository.
## Copyright 2024 Cefero
## Web Page: www.cefero.nl
## Contact: corneel@cefero.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## 1. SET-UP ####
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# In order for load
packages_base <- c(
    "base",
    "methods",
    "utils",
    "stats",
    "graphics",
    "grDevices",
    "datasets")


# TODO When updating this list, also run outcommented code
packages_cran <- c(

    # develop package
    "slider",
    "devtools",
    "usethis",
    "roxygen2",
    "this.path",
    #"pak",

    # quarto
    "quarto",
    "knitr",

    # visualisation
    "ggplot2" ,       # Create plots
    "scales",
    "gt",
    #"leaflet",
    #"sf",

    # process mining
    "eventdataR",     # Event data
    "bupaR",          # Business process analysis
    "processmapR",    # Process mining
    # main
    "cli",            # Create command line interfaces
    #"LaF",            # Read data files without encoding (like ASCII)
    #"dataReporter",   # Create a data audit report
    # "rlang",          # Enable complex operations
    "forcats",        # Work with factors
    "config",         # Set up configuration files and functions
    "janitor",        # Clean up names from special characters
    "lubridate",      # Work with dates and times
    "purrr",          # Work with functions and vectors
    "readxl",         # Read xlsx
    "readr",          # Read data (csv, tsv, and fwf)
    #"fs",             # Work with file systems
    #"rvest",          # Read html
    #"slackr",         # Send messages in Slack
    #"stringi",        # Work with other strings
    "stringr",        # Work with strings
    "tibble",         # Edit and create tibbles
    "tidyr",          # Tidy data in the tidyverse environment
    #"fst",            # Perform operations with large data files
    "dplyr"#,          # Utilise the dplyr environment
    #"vvmover",
    #"vvconverter",
    #"corrr"           # Correlation matrix
)

# Include both the package name (for loading) and the account name (for renv snapshot)
packages_github <- c(
    #"vusa",            # Utilise packages from the VU team
    #"pal",              # pal for using llm assistants
    #"gander"#,
    #"shinychat"
)

packages_github_with_account <- c(
    #"vusaverse/vusa",
    #"simonpcouch/pal",
    #"simonpcouch/gander"#,
    #"posit-dev/shinychat"
)


# Combine packages, config should not be loaded
packages <- c(packages_base, packages_cran, packages_github)
packages <- packages[packages != "config"]
packages_renv <- c(packages_cran, packages_github)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. EXECUTE ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#'*INFO* Fix for packages from github, install them first
install_if_missing <- function(packages, packages_to_install) {
  missing <- !sapply(packages, requireNamespace, quietly = TRUE)
  if (any(missing)) {
    install.packages(packages_to_install[missing])
  }
}

install_if_missing(packages_github, packages_github_with_account)


options(renv.snapshot.filter = function(project) {
  return(packages_renv)
})

# TODO Un-edit when adding packages above to include them in snapshot
# renv::snapshot(type = "custom")

# TODO Run with clean = TRUE to remove all packages that are added but not in snapshot
# TODO I earlier had code for manually checkign updates due to pak. I think this code can be removed now
# if (are_packages_up_to_date(packages_renv) == TRUE) {
#   message("✅ All packages already at correct versions. No need to restore.")
# } else {
#   # Only run restore if needed
#   renv::restore(confirm = FALSE)
# }
renv::restore(confirm = FALSE)

# TODO Set to TRUE when adding packages to check if there are problematic conflicts
suppressMessages(purrr::walk(packages, ~library(.x,
                                                character.only = TRUE)))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE-AND-CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

clear_script_objects()
