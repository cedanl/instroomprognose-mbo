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
