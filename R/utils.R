#' Load Data File
#'
#' @description
#' Reads a raw data file based on configuration settings or direct file specifications.
#' Uses config package to retrieve file paths and names if not directly specified.
#'
#' @param config_key Character string specifying the configuration key to lookup file information
#' @param ... Additional arguments passed to readr::read_delim
#' @param filename Optional character string specifying the name of the file to read.
#'                If NULL, filename is retrieved from config using config_key.
#' @param path Optional character string specifying the path to read from.
#'            If NULL, path is retrieved from config using config_raw_data.
#' @param config_data_path Character string specifying the config key for raw data directory.
#'                        Defaults to NULL.
#'
#' @return A tibble containing the raw data from the specified file
#'
#' @details
#' The function will first try to use provided filename and path.
#' If these are NULL, it will look up values in the config file using config_key
#' and config_raw_data respectively.
#'
#' @export
load_data <- function(config_key,
                      ...,
                      filename = NULL,
                      path = NULL,
                      config_data_path = NULL) {

    if (is.null(filename) || is.null(path)) {
        requireNamespace("config", quietly = TRUE)
    }

    if (is.null(filename)) {
        filename <- get_filename_from_config(config_key)
    }

    if (is.null(path)) {

        if (!is.null(config_data_path)) {
            path <- config::get(config_data_path)
        }
    }

    data_raw <- safe_read_csv(filename, ..., path = path)
}

#' Safe CSV Reader with Flexible Arguments
#'
#' @description
#' Safely reads a delimited file with error handling, path validation, and
#' configurable reading parameters. Uses sane defaults for European CSV format.
#'
#' @param filename Required filename
#' @param ... Additional arguments passed to readr::read_delim
#' @param path Optional path to prepend to filename
#'
#' @return A tibble containing the CSV data
#' @importFrom readr read_delim cols col_guess locale
#'
#' @export
safe_read_csv <- function(filename, ..., path = NULL) {

    # Construct full file path
    filename <- if (is.null(path)) {
        filename
    } else {
        file.path(path, filename)
    }

    filename <- normalizePath(filename, mustWork = FALSE)

    # Check if file exists
    if (!file.exists(filename)) {
        stop(sprintf("File not found: %s", filename))
    }

    # Default arguments for European CSV format
    default_args <- list(
        file = filename,
        delim = ";",
        name_repair = "unique",
        escape_double = FALSE,
        locale = locale(decimal_mark = ",", grouping_mark = "."),
        trim_ws = TRUE,
        col_types = cols(.default = col_guess()),
        na = c("", "NA", "NULL")
    )

    # Override defaults with any provided arguments
    function_args <- utils::modifyList(default_args, list(...))

    # Attempt to read file
    tryCatch({
        df <- do.call(read_delim, function_args)
        if (nrow(df) == 0) warning(sprintf("File is empty: %s", filename))
        df
    }, error = function(e) {
        stop(sprintf("Error reading file %s: %s", filename, e$message))
    })
}

#' Get Filename from Config
#'
#' @description
#' Retrieves a filename from config settings. If the config key contains a list,
#' looks for the specified argument within that list.
#'
#' @param config_key Character string specifying the config key to look up
#' @param argument Character string specifying which argument to extract from list,
#'   defaults to "filename"
#'
#' @return Character string containing the filename
#'
#' @examples
#' \dontrun{
#' # For simple config entry
#' # config.yml: default: input_file: "data.csv"
#' get_filename_from_config("input_file")
#'
#' # For list config entry
#' # config.yml: default: files: {filename: "data.csv", path: "data/"}
#' get_filename_from_config("files", "filename")
#' }
#'
#' @export
get_filename_from_config <- function(config_key, argument = "filename") {
    if (!requireNamespace("config", quietly = TRUE)) {
        stop("The 'config' package is not available. Either define the filename directly or install the package.")
    }

    config_value <- try(config::get(config_key), silent = TRUE)

    if (inherits(config_value, "try-error")) {
        stop(sprintf("The config key '%s' was'nt found in the config file", config_key))
    }

    # If config_value is a list, look for the specified argument
    if (!is.list(config_value)) {
        filename <- config_value
    }


    if (argument %in% names(config_value)) {
        filename <- (config_value[[argument]])
    } else {
        available_args <- paste(names(config_value), collapse = ", ")
        stop(sprintf(
            "Argument '%s' not found in config key '%s'. Available arguments are: %s",
            argument, config_key, available_args
        ))
    }

    # If not a list, return the value directly
    return(filename)
}

#' Get School Year from Date
#'
#' @description
#' Convert a date into a school year format (e.g., "2023/2024")
#'
#' @param date A Date or POSIXct object
#'
#' @returns
#' A character string in the format "YYYY/YYYY+1". The school year is considered
#' to start in August, so dates before August are assigned to the previous year.
#' Will error if input is not a Date or POSIXct object.
#'
#' @importFrom lubridate month year
#'
#' @export
get_school_year <- function(date) {

    if (!inherits(date, "Date") & !inherits(date, "POSIXct")) {
        stop("Input must be a Date object")
    }

    # TODO This doesn't work when a vector is passed
    # if (is.na(month(date))) {
    #     return(NA)
    # }

    adjusted_year <- ifelse(month(date) < 8,
                            year(date) - 1,
                            year(date))

    school_year_name <- paste0(adjusted_year, "/", adjusted_year + 1)


    return(school_year_name)
}

#' Save Combined Data to RDS
#'
#' @description
#' Save a data object to an RDS file in a specified directory
#'
#' @param data The data object to save.
#' @param ... Additional arguments passed to saveRDS().
#' @param filename Optional. A string for the output filename. If NULL, uses the data object's name.
#' @param path Optional. A string specifying the save location.
#' @param config_data_path Optional. A string specifying the config key for the save location.
#'
#' @returns
#' Invisibly returns the full path to the saved file. Creates directory if it
#' doesn't exist.
#'
#' @importFrom config get
#'
#' @export
save_combined <- function(data, ..., filename = NULL, path = NULL, config_data_path = "data_combined_dir") {

    ## Set the object name to the file name if not given
    if (is.null(filename)) {
        filename <- deparse(substitute(data))
    }

    save_config(data,
                filename,
                ...,
                path = path,
                config_data_path = config_data_path)
}


#' Save Ingested Data
#'
#' @description
#' Save processed data as an RDS file in a specified directory
#'
#' @param data A data object to save.
#' @param filename Optional. Name of the file to save. If NULL, uses the comment attribute of data.
#' @param path Optional. Path where to save the file.
#' @param config_data_path Optional. Config key for the save directory. Defaults to "data_ingested_dir".
#' @param ... Additional arguments passed to saveRDS().
#'
#' @returns
#' No return value. Saves the data as an RDS file.
#'
#'
#' @keywords internal
save_ingested <- function(data, ..., filename = NULL, path = NULL, config_data_path = "data_ingested_dir") {

    ## Set the comment to the file name if not given
    if (is.null(filename)) {
        filename <- comment(data)
    }

    save_config(data,
                filename,
                ...,
                path = path,
                config_data_path = config_data_path)

}

#' Save Prepared Data to RDS File
#'
#' @description
#' Save a data object to an RDS file in a specified directory
#'
#' @param data A data object to save.
#' @param ... Additional arguments passed to saveRDS().
#' @param filename Optional. Name for the output file (without extension).
#' @param path Optional. Directory path for saving.
#' @param config_data_path Optional. Config key for the data directory (default: "data_prepared_dir").
#'
#' @returns
#' No return value, called for side effects. Creates an RDS file in the specified location.
#' Will create the directory if it doesn't exist.
#'
#'
#' @export
save_prepared <- function(data, ..., filename = NULL, path = NULL, config_data_path = "data_prepared_dir") {

    ## Set the comment to the file name if not given
    if (is.null(filename)) {
        filename <- comment(data)
    }

    save_config(data,
                filename,
                ...,
                path = path,
                config_data_path = config_data_path)

}

#' Save Prepared Data and Return It
#'
#' @description
#' Save data to a prepared data directory and return the same data
#'
#' @param data A data frame or tibble to save.
#' @param filename Optional. A string specifying the output filename.
#' @param path Optional. A string specifying the output path.
#' @param config_data_path Optional. A string specifying the config path for prepared data.
#' @param ... Additional arguments passed to save_prepared().
#'
#' @returns
#' Returns the input `data` unchanged after saving it to disk.
#'
#' @export
save_prepared_and_return <- function(data, ..., filename = NULL, path = NULL, config_data_path = "data_prepared_dir") {

    save_prepared(data, ..., filename = filename, path = path, config_data_path = config_data_path)

    return(data)
}

#' Save transformed data
#'
#' @description
#' Save a data object as an RDS file in a specified directory
#'
#' @param data A data object to save.
#' @param filename Optional. A string for the output filename. If NULL, uses the object name.
#' @param ... Additional arguments passed to saveRDS().
#' @param path Optional. A string specifying the output directory path.
#' @param config_data_path Optional. A string specifying the config key for the output directory (default: "data_prepared_dir").
#'
#' @returns
#' Invisibly returns the path to the saved file. Creates the output directory if it doesn't exist.
#' Will error if the config package isn't available and no filename is provided.
#'
#'
#' @export
save_transformed <- function(data, filename = NULL, ..., path = NULL, config_data_path = "data_prepared_dir") {

    ## Set the object name to the file name if not given
    if (is.null(filename)) {
        filename <- deparse(substitute(data))
    }

    save_config(data,
                filename,
                ...,
                path = path,
                config_data_path = config_data_path)

}

#' Save Transformed Data with Optional Comments
#'
#' @description
#' Save a transformed dataset while optionally adding its filename to its comments
#'
#' @param data A data frame or tibble to save.
#' @param ... Currently unused; must be empty.
#' @param filename Optional. A string specifying the filename. If NULL, uses the data argument name.
#' @param add_comment Optional. A logical indicating whether to add the filename to comments.
#'
#' @returns
#' The data, invisibly, after saving it to disk.
#'
#' @importFrom rlang check_dots_empty
#'
#' @export
save_transformed_and_comment <- function(data, ..., filename = NULL, add_comment = TRUE) {

    if (is.null(filename)) {
        filename <- deparse(substitute(data))
    }

    if (add_comment == TRUE) {
        comment(data) <- paste0(comment(data),
                                filename,
                                collapse = ", ")
    }

    save_transformed(data, filename)

}

#' Load and Left Join Prepared Data
#'
#' @description
#' Load a prepared dataset from disk and left join it with an existing dataset
#'
#' @param data_main A data frame to join data to.
#' @param data_to_join_name A string specifying the name of the RDS file (without extension).
#' @param ... Arguments passed to dplyr::left_join.
#' @param .path Optional. A string specifying the path to load data from.
#' @param .config_data_path Optional. A string specifying the config key for the data path.
#'
#' @returns
#' A data frame containing the left join result of `data_main` with the loaded data.
#' Will error if the main data is NULL or if the file to join cannot be found.
#'
#' @importFrom dplyr left_join
#'
#' @export
left_join_load <- function(data_main, data_to_join_name, ..., .path = NULL, .config_data_path = "data_prepared_dir") {

    if (is.null(data_main)) {
        stop("The main data object is NULL")
    }

    if (is.null(.path)) {

        if (!requireNamespace("config", quietly = TRUE)) {
            stop("The 'config' package is not available. Either define the filename directly or install the package.")
        }

        path <- config::get(.config_data_path)

    } else {
        path <- .path
    }

    filename <- paste0(data_to_join_name, ".rds")
    file_path <- file.path(path, filename)

    if (!file.exists(file_path)) {
        stop(sprintf("File not found: %s", file_path))
    }

    # Load the data from the prepared folder
    data_to_join <- readRDS(file_path)

    # Perform the left join
    data_main_joined <- left_join(data_main, data_to_join, ...)

    return(data_main_joined)
}

#' Save Analyzed Data to RDS File
#'
#' @description
#' Save data to an RDS file in the analyzed data directory
#'
#' @param data An R object to save.
#' @param ... Additional arguments passed to saveRDS().
#' @param filename Optional. A string for the output filename. If NULL, uses the input object name.
#' @param path Optional. A string specifying the output path.
#' @param config_data_path Optional. A string specifying the config key for the data directory.
#'
#' @returns
#' Invisibly returns the path where the file was saved. Creates directory if it
#' doesn't exist. Will error if the config package isn't available and no filename
#' is provided.
#'
#' @export
save_analysed <- function(data, ..., filename = NULL, path = NULL, config_data_path = "data_analysed_dir") {

    ## Set the object name to the file name if not given
    if (is.null(filename)) {
        filename <- deparse(substitute(data))
    }

    save_config(data,
                filename,
                ...,
                path = path,
                config_data_path = config_data_path)

}

#' Save Data to RDS File
#'
#' @description
#' Save a data object to an RDS file at a specified location
#'
#' @param data An R object to save.
#' @param filename A string specifying the name of the file (without extension).
#' @param ... Additional arguments passed to saveRDS().
#' @param path Optional. A string specifying the directory path.
#' @param config_data_path Optional. A string specifying the config key for the directory path.
#'
#' @returns
#' Invisibly returns NULL. Creates an RDS file at the specified location.
#' Will create directories if they don't exist.
#'
#' @export
save_config <- function(data, filename, ..., path = NULL, config_data_path = NULL) {

    if (!requireNamespace("config", quietly = TRUE)) {
        stop("The 'config' package is not available. Either define the filename directly or install the package.")
    }

    if (is.null(path)) {

        if (!is.null(config_data_path)) {
            path <- config::get(config_data_path)
        }
        else {
            stop("No path provided and no config_data_path specified. Provide one.")
        }
    }

    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
    }

    file_full_path <- file.path(path, filename)

    saveRDS(data,
            paste0(file_full_path,".rds"),
            version = 3,
            ...)
}
