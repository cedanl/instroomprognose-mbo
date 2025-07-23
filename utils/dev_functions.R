


#' Close all tabs that are not already in the Tabs vector and close view panes
#'
#' This function iterates through all tabs in RStudio, closing those that are not
#' already listed in the Tabs vector. It also handles view panes by closing them
#' if they do not have a corresponding document context.
#'
#' @return NULL
#' @export
close_view <- function() {
  Tabs <- c()

  doc <- rstudioapi::getSourceEditorContext()

  while (is.null(doc) || !doc$id %in% Tabs) {
    if (is.null(doc)) {
      rstudioapi::executeCommand("closeSourceDoc")
    }
    rstudioapi::executeCommand("nextTab")

    Tabs <- c(Tabs, doc$id)

    doc <- rstudioapi::getSourceEditorContext()
  }
}

#' clear script objects.
#'
#' Clear objects which are created in the current script.
#' @param ... which object(s) to keep.
#' @param filepath path to script
#' @param list list of objects
#' @param pos what position in the environment to clear
#' @param envir which environment to clear
#' @param line_start from which line
#' @param line_end until which line
#' @param silent whether to mute console output
#' @export
clear_script_objects <- function(..., filepath = NULL, list = character(), pos = -1, envir = as.environment(pos), line_start = 0, line_end = -1L, silent = TRUE) {
  if (missing(filepath)) {
    filepath <- this.path::sys.path()
  }

  # Get added objects and put them in list
  dots <- match.call(expand.dots = FALSE)$...
  if (length(dots) &&
      !all(vapply(dots, function(x) is.symbol(x) || is.character(x), NA, USE.NAMES = FALSE))) {
    stop("... must contain names or character strings")
  }
  names <- vapply(dots, as.character, "")
  if (length(names) == 0L) names <- character()
  objects_not_remove_list <- .Primitive("c")(list, names)

  # Find objects - using a pattern that will reliably find variable assignments
  lines <- readr::read_lines(filepath, skip = line_start, n_max = line_end)

  # Look for variable names followed by <- assignment
  pattern <- "^\\s*([a-zA-Z][a-zA-Z0-9_]*)\\s*<-"
  matches <- stringr::str_match(lines, pattern)

  # Extract just the variable names (in the second column of the match matrix)
  objects_found <- matches[, 2]
  objects_found_unique <- base::unique(objects_found[!base::is.na(objects_found)])

  objects_found_to_remove <- setdiff(objects_found_unique, objects_not_remove_list)

  objects_exist_and_remove <- objects_found_to_remove[vapply(objects_found_to_remove, exists, logical(1), envir = .GlobalEnv)]

  # Remove only existing variables
  if (length(objects_exist_and_remove) > 0 & silent) {
    suppressWarnings(rm(list = objects_exist_and_remove, pos = ".GlobalEnv"))
  }

  if (length(objects_exist_and_remove) > 0 & !silent) {
    rm(list = objects_exist_and_remove, pos = ".GlobalEnv")
    base::cat(cli::style_bold(
      cli::col_red("De volgende variabelen worden verwijderd: \n")
    ))
    base::cat(cli::style_bold(cli::col_red(
      paste(objects_exist_and_remove, collapse =     ", \n")
    )))
    base::cat(paste("\n"))
  }
}


get_required_r_version <- function(lockfile = "renv.lock") {
  lock <- jsonlite::fromJSON(lockfile)
  r_version <- lock$R$Version
  return(r_version)
}


# TODO Experimental code to programmatically detect system dependencies, currently not used
# For now I have harcoded the system dependencies for the packages I use
get_system_dependencies <- function(packages, os = "macos-arm64") {
  # TODO Check the possible and encode them
  #os <- match.arg(os)

  # Create a temporary file for the results
  tmp_file <- tempfile()

  # Use pak to get system requirements
  if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
  }

  # Get dependencies for each package
  deps <- list()

  for (pkg in packages) {
    system_reqs <- pak::pkg_sysreqs(pkg, sysreqs_platform = os)
    if (length(system_reqs) > 0 && !is.null(system_reqs$packages)) {
      deps[[pkg]] <- system_reqs$packages
    } else {
      deps[[pkg]] <- NA
    }
  }

  for (pkg in packages) {
    tryCatch({
      # Get system requirements for the package
      system_reqs <- pak::pkg_sysreqs(pkg, sysreqs_platform = os)
      # if (os == "ubuntu") {
      #     system_reqs <- pak::pkg_sysreqs(pkg, os = "ubuntu-22.04")
      # } else {
      #     system_reqs <- pak::pkg_sysreqs(pkg, os = "macos")
      # }

      # Extract library names from system requirements
      if (length(system_reqs) > 0 && !is.null(system_reqs$packages)) {
        deps[[pkg]] <- system_reqs$packages
      } else {
        deps[[pkg]] <- NA
      }
    }, error = function(e) {
      deps[[pkg]] <<- paste("Error:", e$message)
    })
  }

  # Convert to data frame
  result <- data.frame(
    package = names(deps),
    system_libraries = sapply(deps, function(x) {
      if (length(x) == 0 || all(is.na(x))) {
        return("None (pure R package)")
      } else {
        return(paste(x, collapse = ", "))
      }
    }),
    stringsAsFactors = FALSE
  )

  return(result)
}

# Extract packages from manage_packages.R
extract_packages_from_file <- function(filepath) {
  # Read the file
  lines <- readLines(filepath)

  # Find the packages_cran and packages_github vectors
  start_cran <- grep("packages_cran <- c", lines, fixed = TRUE)
  end_cran_candidates <- grep("^\\s*\\)\\s*$", lines)
  end_cran <- min(end_cran_candidates[end_cran_candidates > start_cran])

  start_github <- grep("packages_github <- c", lines, fixed = TRUE)
  end_github_candidates <- grep("^\\s*\\)\\s*$", lines)
  end_github <- min(end_github_candidates[end_github_candidates > start_github])

  # Extract package names
  cran_pkgs <- lines[(start_cran + 1):(end_cran - 1)]
  github_pkgs <- lines[(start_github + 1):(end_github - 1)]

  # Clean up package names
  clean_pkg <- function(pkg_line) {
    pkg <- gsub("^\\s*\"(.+?)\".*$", "\\1", pkg_line)
    pkg <- gsub(",\\s*$", "", pkg)
    pkg <- gsub("#.*$", "", pkg)  # Remove comments
    pkg <- trimws(pkg)
    return(pkg)
  }

  cran_pkgs <- sapply(cran_pkgs, clean_pkg)
  cran_pkgs
  github_pkgs <- sapply(github_pkgs, clean_pkg)

  # Combine and filter out empty strings
  all_pkgs <- c(cran_pkgs, github_pkgs)
  all_pkgs <- all_pkgs[nchar(all_pkgs) > 0]

  return(all_pkgs)
}


# file_path <- "utils/manage_packages.R"
#
# # Usage
# packages <- extract_packages_from_file("utils/manage_packages.R")
#
# ubuntu_deps <- get_system_dependencies(packages, os = "ubuntu")
#
# # Combine results
# result <- data.frame(
#     package = ubuntu_deps$package,
#     ubuntu_libs = ubuntu_deps$system_libraries
# )
