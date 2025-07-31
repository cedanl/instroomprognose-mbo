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
#' @param comparison_mode Comparison mode ("relative_to_average" or "absolute")
#' @param years Vector of years to analyze
#' @param output_base_dir Base directory for output files
render_institution_report <- function(
    brin_code, 
    school_name, 
    comparison_mode = "relative_to_average",
    years = c(2023, 2024),
    output_base_dir = "output/institution_reports"
) {
    
    # Create safe filename from school name (remove special characters)
    safe_school_name <- gsub("[^A-Za-z0-9_\\-\\s]", "", school_name)
    safe_school_name <- gsub("\\s+", "_", trimws(safe_school_name))
    
    # Create output directory for this institution
    institution_dir <- file.path(output_base_dir, paste0(brin_code, "_", safe_school_name))
    
    if (!dir.exists(institution_dir)) {
        dir.create(institution_dir, recursive = TRUE)
        cat("Created directory:", institution_dir, "\n")
    }
    
    # Define output file path
    output_file <- file.path(
        institution_dir,
        paste0(brin_code, "_", safe_school_name, "_instroomanalyse.html")
    )
    
    cat("Rendering report for:", school_name, "(", brin_code, ")\n")
    cat("Output file:", output_file, "\n")
    
    # Render the report with explicit output format to ensure proper styling
    tryCatch({
        rmarkdown::render(
            "instelling_analysis.qmd",
            output_format = rmarkdown::html_document(
                toc = TRUE,
                toc_title = "Inhoudsopgave",
                code_folding = "hide",
                code_download = TRUE,
                theme = "bootstrap",
                highlight = "tango",
                css = NULL
            ),
            output_file = output_file,
            params = list(
                brin_code = brin_code,
                school_name = school_name,
                comparison_mode = comparison_mode,
                years = years
            ),
            quiet = TRUE
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
#' @param mode Character: "specific", "all", or "interactive"
#' @param brin_codes Vector of specific BRIN codes (when mode = "specific")
#' @param comparison_mode Comparison mode for analysis
#' @param years Vector of years to analyze
#' @param config_env Configuration environment to use
render_reports <- function(
    mode = "interactive",
    brin_codes = NULL,
    comparison_mode = "relative_to_average",
    years = c(2023, 2024),
    config_env = "default"
) {
    
    cat("=== Institution Analysis Report Generator ===\n")
    cat("Mode:", mode, "\n")
    cat("Years:", paste(years, collapse = ", "), "\n")
    cat("Comparison mode:", comparison_mode, "\n\n")
    
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
        
    } else if (mode == "interactive") {
        
        cat("Available institutions:\n")
        for (i in 1:min(10, nrow(institutions))) {
            cat(sprintf("%2d. %s (%s)\n", i, institutions$school_name[i], institutions$brin_code[i]))
        }
        
        if (nrow(institutions) > 10) {
            cat("... and", nrow(institutions) - 10, "more institutions\n")
        }
        
        cat("\nChoose an option:\n")
        cat("1. Render all institutions\n")
        cat("2. Render specific institutions (provide BRIN codes)\n")
        cat("3. Show all institutions and choose\n")
        
        choice <- readline(prompt = "Enter your choice (1-3): ")
        
        if (choice == "1") {
            target_institutions <- institutions
            
        } else if (choice == "2") {
            cat("\nAvailable BRIN codes:", paste(institutions$brin_code, collapse = ", "), "\n")
            brin_input <- readline(prompt = "Enter BRIN codes (comma-separated): ")
            brin_codes <- trimws(unlist(strsplit(brin_input, ",")))
            
            target_institutions <- institutions %>%
                filter(brin_code %in% brin_codes)
                
        } else if (choice == "3") {
            cat("\nAll available institutions:\n")
            for (i in 1:nrow(institutions)) {
                cat(sprintf("%2d. %s (%s)\n", i, institutions$school_name[i], institutions$brin_code[i]))
            }
            
            indices_input <- readline(prompt = "Enter institution numbers (comma-separated): ")
            indices <- as.numeric(trimws(unlist(strsplit(indices_input, ","))))
            
            target_institutions <- institutions[indices, ]
            
        } else {
            stop("Invalid choice")
        }
        
    } else {
        stop("Invalid mode. Must be 'all', 'specific', or 'interactive'")
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
            comparison_mode = comparison_mode,
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

# Command line interface
if (!interactive()) {
    
    # Parse command line arguments
    args <- commandArgs(trailingOnly = TRUE)
    
    if (length(args) == 0) {
        # Default: interactive mode
        render_reports(mode = "interactive")
        
    } else if (args[1] == "all") {
        # Render all institutions
        render_reports(mode = "all")
        
    } else if (args[1] == "help") {
        cat("Usage:\n")
        cat("  Rscript render_instelling_reports.R [command] [options]\n\n")
        cat("Commands:\n")
        cat("  all                 Render reports for all institutions\n")
        cat("  help                Show this help message\n")
        cat("  <BRIN_CODE>         Render report for specific BRIN code\n")
        cat("  <no arguments>      Interactive mode\n\n")
        cat("Examples:\n")
        cat("  Rscript render_instelling_reports.R all\n")
        cat("  Rscript render_instelling_reports.R 30RR\n")
        cat("  Rscript render_instelling_reports.R\n")
        
    } else {
        # Treat first argument as BRIN code
        brin_code <- args[1]
        render_reports(mode = "specific", brin_codes = brin_code)
    }
    
} else if (exists("render_interactive") && render_interactive) {
    # For testing in interactive R session
    render_reports(mode = "interactive")
}

# Export functions for use in other scripts
cat("Functions loaded successfully. Use render_reports() to start.\n")
cat("Example usage:\n")
cat("  render_reports(mode = 'all')\n")
cat("  render_reports(mode = 'specific', brin_codes = c('30RR', '27EB'))\n")
cat("  render_reports(mode = 'interactive')\n")