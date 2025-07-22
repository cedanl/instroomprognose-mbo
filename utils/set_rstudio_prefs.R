system_username <- tolower(Sys.getenv("USERNAME"))
rstudio_prefs_path <- NULL
prefs_changed <- FALSE

if (platform == "windows") {
    if (dir.exists("C:/Users/")) {
        rstudio_prefs_path <- paste0(
            "C:/Users/",
            system_username,
            "/AppData/Roaming/RStudio/rstudio-prefs.json"
        )
    } else {
        message("RStudio preferences file not found, uncheck 'Show output inline for all R code chunks' via Tools -> Global Options -> R Markdown.")
    }
} else if (platform == "macOS") {
    if (dir.exists("~/.config/rstudio")) {
        rstudio_prefs_path <- "~/.config/rstudio/rstudio-prefs.json"
    } else if (dir.exists("~/.rstudio-desktop")) {
        rstudio_prefs_path <- "~/.rstudio-desktop/rstudio-prefs.json"
    } else {
        message("RStudio preferences file not found, uncheck 'Show output inline for all R code chunks' via Tools -> Global Options -> R Markdown.")
    }
}

if (!is.null(rstudio_prefs_path)) {

    prefs_content <- jsonlite::read_json(rstudio_prefs_path)

    ## check if rmd_chunk_output_inline exists in prefs_content
    if (!is.null(prefs_content$rmd_chunk_output_inline)) {

        # Check if it's set to true and needs to be changed
        if (prefs_content$rmd_chunk_output_inline == TRUE) {
            message("The setting 'rmd_chunk_output_inline' is currently TRUE, changing to FALSE.")
            prefs_content$rmd_chunk_output_inline <- FALSE
            jsonlite::write_json(prefs_content, rstudio_prefs_path, pretty = TRUE, auto_unbox = TRUE)

            # Flag that changes were made
            prefs_changed <- TRUE
        }
    } else {
        # Add it to the file with standard value false
        message(
            "Adding 'rmd_chunk_output_inline' with the default value (false) to rstudio-prefs.json."
        )

        # Add the new setting to the JSON object
        prefs_content$rmd_chunk_output_inline <- FALSE

        # Write the updated JSON back to the file
        jsonlite::write_json(prefs_content, rstudio_prefs_path, pretty = TRUE, auto_unbox = TRUE)

        # Flag that changes were made
        prefs_changed <- TRUE
    }
}

# If preferences were changed, offer to restart the R session
if (prefs_changed == TRUE && rstudioapi::isAvailable()) {
    message("Preferences have been updated. A restart is needed for changes to take effect.")
    restart_session <- readline("Would you like to restart the R session now? (y/n): ")

    if (tolower(restart_session) == "y") {
        message("Restarting R session...")
        rstudioapi::restartSession()
    } else {
        message("Please restart RStudio manually for the changes to take effect.")
    }
}

clear_script_objects()
