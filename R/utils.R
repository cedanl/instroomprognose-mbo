#' Get Institutions
#'
#' @description
#' Extracts available institutions from the data with BRIN codes and school names
#'
#' @return Data frame with brin_code and school_name columns
#'
#' @importFrom dplyr filter select distinct arrange
#'
#' @export
get_institutions <- function() {

    cat("Loading applications data to extract institutions...\n")

    # Load applications data
    applications <- load_and_enrich_applications()

    # Extract unique institutions with valid names
    institutions <- applications |>
        filter(
            !is.na(instellingserkenningscode),
            !is.na(school),
            nchar(trimws(instellingserkenningscode)) > 0,
            nchar(trimws(school)) > 0
        ) |>
        select(
            brin_code = instellingserkenningscode,
            school_name = school
        ) |>
        distinct() |>
        arrange(brin_code)

    cat("Found", nrow(institutions), "unique institutions\n")

    return(institutions)
}

#' Render Single Institution Report
#'
#' @param brin_code BRIN code of the institution
#' @param school_name Name of the school
#' @param year Numeric value of year to analyze
#' @param input_file_path File path to the Quarto input file
#' @param output_base_dir_relative directory for output files, relative to the input path
#'
#' @return TRUE if rendering succeeded, FALSE otherwise
#'
#' @importFrom quarto quarto_render
#' @importFrom stringr str_replace
#'
#' @export
render_institution_report <- function(
        brin_code,
        school_name,
        year = 2024,
        input_file_path = "analysis/instelling_analysis.qmd",
        output_base_dir_relative = "../output"
) {

    # Create safe filename from school name (remove special characters)
    safe_school_name <- str_replace(school_name, "[^A-Za-z0-9_\\-\\s]", "_")
    safe_school_name <- str_replace(school_name, "\\s+", "_")

    # Create output directory for this institution
    output_full_dir <- file.path(output_base_dir_relative, paste0(brin_code, "_", safe_school_name), year)

    if (!dir.exists(output_full_dir)) {
        dir.create(output_full_dir, recursive = TRUE)
        cat("Created directory:", output_full_dir, "\n")
    }

    output_file <- "Instroomanalyse.html"

    cat("Rendering report for:", school_name, "(", brin_code, ")\n")
    cat("Output file:", output_file, "\n")

    # Render the report using Quarto
    tryCatch({

        quarto::quarto_render(
            input = input_file_path,
            quarto_args = c("--output-dir", output_full_dir),
            output_file = output_file,
            execute_dir = getwd(),
            execute_params = list(
                brin_code = "30RR",
                school_name = "MBO Amersfoort",
                year = 2024
            ),
            quiet = FALSE
        )

        cat("Successfully rendered:", school_name, "\n")
        return(TRUE)

    }, error = function(e) {
        cat("Error rendering", school_name, ":", e$message, "\n")
        return(FALSE)
    })
}

#' Main Rendering Function
#'
#' @param mode Character: "specific" or "all"
#' @param brin_codes Vector of specific BRIN codes (when mode = "specific")
#' @param year Numeric value of year to analyze
#'
#' @return NULL
#'
#' @importFrom dplyr filter
#'
#'
#' @export
render_multiple_institution_reports <- function(
        mode = "all",
        brin_codes = NULL,
        year = 2024
) {

    cat("=== Institution Analysis Report Generator ===\n")
    cat("Mode:", mode, "\n")
    cat("Year:", year, "\n\n")

    # Get list of available institutions
    institutions <- get_institutions()

    if (nrow(institutions) == 0) {
        rlang::abort("No institutions found in the data")
    }

    # Determine which institutions to process
    if (mode == "all") {
        cat("Rendering reports for all", nrow(institutions), "institutions...\n\n")
        target_institutions <- institutions

    } else if (mode == "specific") {
        if (is.null(brin_codes)) {
            rlang::abort("brin_codes must be provided when mode = 'specific'")
        }

        target_institutions <- institutions |>
            filter(brin_code %in% brin_codes)

        if (nrow(target_institutions) == 0) {
            rlang::abort("No matching institutions found for specified BRIN codes")
        }

        cat("Rendering reports for", nrow(target_institutions), "specific institutions...\n\n")

    } else {
        rlang::abort("Invalid mode. Must be 'all' or 'specific'")
    }

    # Render reports
    cat("Processing", nrow(target_institutions), "institutions...\n\n")

    success_count <- 0
    error_count <- 0

    for (i in 1:nrow(target_institutions)) {

        institution <- target_institutions[i, ]

        success <- render_institution_report(
            brin_code = institution$brin_code,
            school_name = institution$school_name,
            year = year
        )

        if (success) {
            success_count <- success_count + 1
        } else {
            error_count <- error_count + 1
        }

        # Add small delay between renders
        Sys.sleep(0.5)
    }

    # Summary
    cat("\n=== Rendering Complete ===\n")
    cat("Successful renders:", success_count, "\n")
    cat("Failed renders:", error_count, "\n")
    cat("Output directory: output/institution_reports/\n")

    if (success_count > 0) {
        cat("\nReports are ready! Check the output/institution_reports/ directory.\n")
    }
}


#' Open Configuration File
#'
#' Opens the project configuration file in RStudio editor if available,
#' otherwise prints the config path. Uses the config package's default
#' behavior to find the config file.
#'
#' @export
open_config <- function() {
    config_path <- Sys.getenv("R_CONFIG_FILE", "config.yml")

    if (!file.exists(config_path)) {
        rlang::abort(paste0("Config file niet gevonden op: ", config_path))
    }

    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rlang::inform(paste0("Opening config file: ", config_path))
        rstudioapi::navigateToFile(config_path)
    } else {
        rlang::inform(paste0("RStudio API niet beschikbaar. Open zelf het config bestand op locatie: ", config_path))
    }

    invisible(config_path)
}

#' Validate Configuration Settings
#'
#' Validates the current configuration settings to ensure all required
#' values are present and valid for pipeline execution.
#'
#' @param config_settings List of configuration settings from config::get()
#' @return Invisibly returns TRUE if validation passes, otherwise aborts with error
#' @importFrom rlang abort inform
#' @export
validate_config <- function(config_settings = config::get()) {
    errors <- character(0)

    # Check prepare setting
    if (is.null(config_settings$prepare)) {
        errors <- c(errors, "'prepare' ontbreekt in configuratie")
    } else if (!is.logical(config_settings$prepare)) {
        errors <- c(errors, "'prepare' moet 'true' of 'false' zijn")
    }

    # Check year setting
    if (is.null(config_settings$year)) {
        errors <- c(errors, "'year' ontbreekt in configuratie")
    } else if (!is.numeric(config_settings$year)) {
        errors <- c(errors, "'year' moet numeriek zijn, zonder aanhalingstekens")
    } else if (nchar(as.character(config_settings$year)) != 4) {
        errors <- c(errors, "'year' moet 4-cijferig zijn")
    } else if (!grepl("^202", as.character(config_settings$year))) {
        errors <- c(errors, "'year' moet beginnen met 202")
    }

    # Check brin setting if not in demo mode
    if (!is.null(config_settings$brin)) {
        institutions <- get_institutions()
        if (!config_settings$brin %in% institutions$brin_code) {
            errors <- c(errors, paste0("BRIN '", config_settings$brin, "' niet gevonden in beschikbare instellingen"))
        }
    }

    # Report validation results
    if (length(errors) > 0) {
        rlang::inform("Configuratie validatie resultaten:")
        for (error in errors) {
            rlang::inform(error)
        }
        rlang::abort("Configuratie validatie gefaald - corrigeer bovenstaande fouten")
    } else {
        rlang::inform("Configuratie validatie succesvol")
    }

    invisible(TRUE)
}
