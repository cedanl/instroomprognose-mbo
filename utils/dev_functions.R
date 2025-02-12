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

#' Clears the global environment except for objects specified in the DEFAULT_KEEP_LIST system variable.
#'
#' This function removes all objects from the global environment except for those specified
#' in the DEFAULT_KEEP_LIST system variable. The variable is expected to be a space-separated
#' list of object names. If the variable is not set, the function will not run.
#'
#' @note This function should be used with caution as it modifies the global environment.
#'
#' @seealso \code{\link[base]{rm}} for the underlying mechanism used to remove objects.
#'
#' @keywords internal
#' @export
clear_global_proj <- function() {
    # Retrieve the default keep list from the environment variable
    env_default_keep_list <- Sys.getenv("DEFAULT_KEEP_LIST")

    # Check if the environment variable is set and convert it to a list of object names
    if (!env_default_keep_list == "") {
        default_keep_list <- strsplit(env_default_keep_list, " ")[[1]]
    } else {
        # Handle the case where the environment variable is not set
        # For example, you could throw an error or use a fallback list
        stop("The 'DEFAULT_KEEP_LIST' environment variable is not set.")
    }

    # Get the names of all objects in the global environment
    all_objects <- ls(envir = .GlobalEnv)

    # Determine which objects to remove by finding the difference between all objects and the default keep list
    objects_to_remove <- setdiff(all_objects, default_keep_list)

    # Remove the objects from the global environment
    # rm(list = objects_to_remove, envir = .GlobalEnv)
    rm(list = objects_to_remove, envir = parent.frame())
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

    dots <- match.call(expand.dots = FALSE)$...
    if (length(dots) &&
        !all(vapply(dots, function(x) is.symbol(x) || is.character(x), NA, USE.NAMES = FALSE))) {
        stop("... must contain names or character strings")
    }
    names <- vapply(dots, as.character, "")
    if (length(names) == 0L) names <- character()
    list <- .Primitive("c")(list, names)

    Teststring_assignment <- "^[a-zA-Z_0-9]*(?=(\\s<-))"
    Regels <-
        stringr::str_extract(
            readr::read_lines(filepath, skip = line_start, n_max = line_end),
            Teststring_assignment
        )
    Regels <- base::unique(Regels[!base::is.na(Regels)])

    Regels2 <- setdiff(Regels, list)
    rm(list = Regels2, pos = ".GlobalEnv")
    if (!silent) {
        base::cat(cli::style_bold(cli::col_red("De volgende variabelen worden verwijderd: \n")))
        base::cat(cli::style_bold(cli::col_red(paste(Regels2,
                                                     collapse = ", \n"
        ))))
        base::cat(paste("\n"))
    }
}



#' Source script and clear its objects
#'
#' @param ... Objects to keep in the global environment which are created in the sourced scripts
#' @param script The script to source
#'
#' @export
source_index <- function(..., script) {
    source(script)
    dots <- match.call(expand.dots = FALSE)$...
    if (length(dots) && !all(vapply(dots, function(x) {
        is.symbol(x) ||
            is.character(x)
    }, NA, USE.NAMES = FALSE))) {
        stop("... must contain names or character strings")
    }
    names <- vapply(dots, as.character, "")
    if (length(names) == 0L) {
        names <- character()
    }
    list <- .Primitive("c")(list, names)
    Teststring_assignment <- "^[a-zA-Z_0-9]*(?=(\\s<-))"
    Regels <- stringr::str_extract(readr::read_lines(script), Teststring_assignment)
    Regels <- base::unique(Regels[!base::is.na(Regels)])
    Regels2 <- setdiff(Regels, list)
    rm(list = Regels2, pos = ".GlobalEnv")
}

