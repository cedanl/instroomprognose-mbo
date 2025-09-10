


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

install_homebrew <- function() {
    if (Sys.which("brew") == "") {
        message("Installing Homebrew...")
        system(
            '/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"',
            wait = TRUE
        )
        # Ensure brew is in PATH
        if (file.exists("/opt/homebrew/bin/brew")) {
            Sys.setenv(PATH = paste("/opt/homebrew/bin", Sys.getenv("PATH"), sep = ":"))
        }
    }
}

get_installed_brew_aliases <- function() {
    # Use brew list directly instead of JSON parsing
    installed_formulae <- system("brew list --formula", intern = TRUE, ignore.stderr = TRUE)

    # Handle case where no formulae are installed
    if (length(installed_formulae) == 0 || any(grepl("Error", installed_formulae))) {
        return(character())
    }

    return(installed_formulae)
}

install_brew_packages <- function(pkgs) {
    message("🔍 Checking for system dependencies...")

    installed <- get_installed_brew_aliases()

    # Categorize as casks or formulae (once)
    casks <- character()
    formulae <- character()

    for (pkg in pkgs) {
        msg <- paste0("  • Checking type of: ", pkg)
        message(msg)
        is_cask <- tryCatch({
            out <- suppressWarnings(system(paste("brew info --cask", pkg), intern = TRUE, ignore.stderr = TRUE))
            length(out) > 0 && !grepl("^Error", out[1])
        }, error = function(e) FALSE)

        if (is_cask) {
            casks <- c(casks, pkg)
        } else {
            formulae <- c(formulae, pkg)
        }
    }

    # Filter what's actually missing
    missing_formulae <- setdiff(formulae, installed)
    installed_casks <- system("brew list --cask", intern = TRUE)
    missing_casks <- setdiff(casks, installed_casks)

    if (length(missing_formulae)) {
        message("📦 Installing missing formulae: ", paste(missing_formulae, collapse = ", "))
        system(paste("brew install", paste(missing_formulae, collapse = " ")))
    }

    if (length(missing_casks)) {
        message("🖥️ Opening Terminal to install missing casks: ", paste(missing_casks, collapse = ", "))
        for (cask in missing_casks) {
            system(sprintf(
                'osascript -e \'tell application "Terminal" to do script "brew install --cask %s"\'',
                cask
            ))
        }
    }

    message("✅ Done checking/installing dependencies.")
}

get_platform <- function() {
    sysname <- Sys.info()[["sysname"]]
    switch(sysname,
           Darwin = "macOS",
           Linux = "Linux",
           Windows = "Windows",
           sysname)
}

ask_restart_rstudio <- function() {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        ans <- readline("🔄 Rtools installed. Restart R session now? [y/N]: ")
        if (tolower(ans) == "y") {
            message("Restarting R session...")
            rstudioapi::restartSession()
        } else {
            message("❗ Please restart R manually to complete Rtools setup.")
        }
    } else {
        message("⚠️ Not running in RStudio — please restart R manually.")
    }
}

r_version_to_rtools_suffix <- function(r_version) {
    # Extract major and minor version (e.g., "4.3.2" -> "4" and "3")
    parts <- strsplit(r_version, ".", fixed = TRUE)[[1]]
    major <- parts[1]
    minor <- parts[2]

    # Collapse to suffix like "43"
    paste0(major, minor)
}

get_rtools_info_from_lockfile <- function(lockfile = NULL) {
    if (is.null(lockfile)) {
        lockfile <- "renv.lock"
    }

    # Read lockfile manually without jsonlite
    if (!file.exists(lockfile)) {
        stop("Lockfile not found: ", lockfile)
    }

    lines <- readLines(lockfile)

    # Find R version line (looks like: "Version": "4.3.2",)
    r_version_line <- grep('"Version":', lines)
    if (length(r_version_line) == 0) {
        # Fallback to current R version
        r_version <- paste(R.version$major, R.version$minor, sep = ".")
    } else {
        # Extract version from first match (should be R section)
        version_text <- lines[r_version_line[1]]
        # Regex to match: "Version": "4.3.2" (for instance, with optional whitespace)
        version_pattern <- '.*"Version":\\\\s*"([^"]+)".*'
        r_version <- gsub(version_pattern, "\\1", version_text)
    }

    suffix <- r_version_to_rtools_suffix(r_version)

    list(
        r_version = r_version,
        rtools_suffix = suffix,
        rtools_version = paste0("Rtools", suffix),
        rtools_exe = paste0("rtools", suffix, ".exe"),
        download_url = paste0("https://cran.r-project.org/bin/windows/Rtools/rtools", suffix, ".exe")
    )
}

install_rtools <- function() {
    rtools_info <- get_rtools_info_from_lockfile()

    rtools_url <- rtools_info$download_url
    dest <- file.path(tempdir(), rtools_info$rtools_exe)

    message("📥 Downloading Rtools installer...")
    download.file(rtools_url, dest, mode = "wb")

    message("🚀 Launching Rtools installer (will require user interaction)...")
    shell.exec(dest)  # opens installer GUI

    ask_restart_rstudio()
}


# Function to quickly check if packages are installed at correct versions
# Function to check if packages are installed at correct versions
are_packages_up_to_date <- function(packages) {
    library_paths <- .libPaths()
    project_lib <- renv::paths$library()

    # Get the lockfile
    lockfile <- renv::lockfile_read()

    # Check if any packages need updating
    all_up_to_date <- TRUE

    for (pkg in packages) {
        # Skip if package isn't in lockfile
        if (!pkg %in% names(lockfile$Packages)) {
            next
        }

        # Check if installed
        is_installed <- requireNamespace(pkg, quietly = TRUE)
        if (!is_installed) {
            all_up_to_date <- FALSE
            break
        }

        # Check version
        expected_version <- lockfile$Packages[[pkg]]$Version
        installed_version <- as.character(packageVersion(pkg))

        if (expected_version != installed_version) {
            all_up_to_date <- FALSE
            break
        }
    }

    return(all_up_to_date)
}
