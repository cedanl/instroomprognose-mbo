#!/usr/bin/env Rscript

#' Render Institution Analysis Reports
#'
#' @description
#' Script to render instelling_analysis.qmd for specific institutions or all institutions.
#' Creates HTML reports in organized subdirectories with proper naming.
#'
#' @author Corneel den Hartogh

# Load required libraries and functions
source("utils/00_setup.R")
source("R/data_pipeline.R")

#' Get Institution List
#'
#' @description
#' Extracts available institutions from the data with BRIN codes and school names
#'
#' @param config_env Environment to use for data loading
#' @return Data frame with brin_code and school_name columns
get_institution_list <- function(config_env = "default") {

    cat("Loading applications data to extract institutions...\n")

    # Load applications data
    applications <- load_and_enrich_applications(config_env)

    # Extract unique institutions with valid names
    institutions <- applications %>%
        filter(
            !is.na(instellingserkenningscode),
            !is.na(school),
            nchar(trimws(instellingserkenningscode)) > 0,
            nchar(trimws(school)) > 0
        ) %>%
        select(
            brin_code = instellingserkenningscode,
            school_name = school
        ) %>%
        distinct() %>%
        arrange(brin_code)

    cat("Found", nrow(institutions), "unique institutions\n")

    return(institutions)
}

#' Render Single Institution Report
#'
#' @param brin_code BRIN code of the institution
#' @param school_name Name of the school
#' @param years Vector of years to analyze
#' @param output_base_dir Base directory for output files
render_institution_report <- function(
    brin_code,
    school_name,
    years = c(2023, 2024),
    output_base_dir = "output/institution_reports"
) {

    # Create safe filename from school name (remove special characters)
    safe_school_name <- str_replace(school_name, "[^A-Za-z0-9_\\-\\s]", "_")
    safe_school_name <- str_replace(school_name, "\\s+", "_")

    # Create output directory for this institution
    institution_dir <- file.path(output_base_dir, paste0(brin_code, "_", safe_school_name))

    if (!dir.exists(institution_dir)) {
        dir.create(institution_dir, recursive = TRUE)
        cat("Created directory:", institution_dir, "\n")
    }

    output_file <- paste0(brin_code, "_", safe_school_name, "_instroomanalyse.html")


    cat("Rendering report for:", school_name, "(", brin_code, ")\n")
    cat("Output file:", output_file, "\n")

    # Render the report using Quarto
    tryCatch({

        quarto::quarto_render(
            input = "instelling_analysis.qmd",
            quarto_args = c("--output-dir", institution_dir),
            #quarto_args = c("--output", paste0(institution_dir, "/", output_file)),
            output_file = output_file,
            #output_dir = institution_dir,
            execute_params = list(
                brin_code = brin_code,
                school_name = school_name,
                years = years
            ),
            quiet = FALSE
        )
        cat("✅ Successfully rendered:", school_name, "\n")
        return(TRUE)

    }, error = function(e) {
        cat("❌ Error rendering", school_name, ":", e$message, "\n")
        return(FALSE)
    })
}

#' Main Rendering Function
#'
#' @param mode Character: "specific" or "all"
#' @param brin_codes Vector of specific BRIN codes (when mode = "specific")
#' @param years Vector of years to analyze
#' @param config_env Configuration environment to use
render_reports <- function(
    mode = "specific",
    brin_codes = NULL,
    years = c(2023, 2024),
    config_env = "default"
) {

    cat("=== Institution Analysis Report Generator ===\n")
    cat("Mode:", mode, "\n")
    cat("Years:", paste(years, collapse = ", "), "\n\n")

    # Get list of available institutions
    institutions <- get_institution_list(config_env)

    if (nrow(institutions) == 0) {
        stop("No institutions found in the data")
    }

    # Determine which institutions to process
    if (mode == "all") {
        cat("Rendering reports for all", nrow(institutions), "institutions...\n\n")
        target_institutions <- institutions

    } else if (mode == "specific") {
        if (is.null(brin_codes)) {
            stop("brin_codes must be provided when mode = 'specific'")
        }

        target_institutions <- institutions %>%
            filter(brin_code %in% brin_codes)

        if (nrow(target_institutions) == 0) {
            stop("No matching institutions found for specified BRIN codes")
        }

        cat("Rendering reports for", nrow(target_institutions), "specific institutions...\n\n")

    } else {
        stop("Invalid mode. Must be 'all' or 'specific'")
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
            years = years
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
        cat("\n✅ Reports are ready! Check the output/institution_reports/ directory.\n")
    }
}

# Simplified command line interface
if (!interactive()) {
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) == 0) {
        cat("Usage: Rscript render_instelling_reports.R [all|BRIN_CODE]\n")
        cat("Examples:\n")
        cat("  Rscript render_instelling_reports.R all\n")
        cat("  Rscript render_instelling_reports.R 30RR\n")
    } else if (args[1] == "all") {
        render_reports(mode = "all")
    } else {
        render_reports(mode = "specific", brin_codes = args[1])
    }
}

# Export functions for use in other scripts
cat("Functions loaded successfully. Use render_reports() to start.\n")
cat("Example usage:\n")
cat("  render_reports(mode = 'all')\n")
cat("  render_reports(mode = 'specific', brin_codes = c('30RR', '27EB'))\n")
cat("  render_reports(mode = 'interactive')\n")

# Uncomment to test:
render_reports(mode = "specific", brin_codes = c("30RR"))
