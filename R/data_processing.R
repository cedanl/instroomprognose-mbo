

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


#' Shuffle Group Combinations While Preserving Structure
#'
#' @description
#' Randomizes group variable combinations for each ID while maintaining the original
#' frequency structure of the groupings.
#'
#' @param df A data frame containing the data to shuffle
#' @param id_col A string specifying the ID column name
#' @param group_vars A character vector of column names to shuffle as groups
#'
#' @returns
#' A data frame with the same structure as the input, but with group combinations
#' randomly shuffled while preserving the original frequency structure for each ID.
#'
#' @importFrom dplyr group_by ungroup mutate summarise select distinct left_join rename
#'   filter pull arrange row_number n cur_group_id across all_of
#' @importFrom rlang sym syms !! !!! .data
#'
#' @export
# Function to shuffle group combinations while preserving structure
shuffle_group_combinations <- function(df, id_col, group_vars) {
    # Step 1: Create a group ID for each unique id + variable combination
    grouped_data <- df |>
        # Group by id and the specified variables
        group_by(across(c(all_of(id_col), all_of(group_vars)))) |>
        # Create a group ID unique for each combination
        mutate(group_id = cur_group_id()) |>
        ungroup()

    # Step 2: Get the unique id values and their frequency structure
    id_structure <- grouped_data |>
        # Count how many rows each group_id has
        group_by(across(c(all_of(id_col), group_id))) |>
        summarise(count = n(), .groups = "drop") |>
        # This gives us the structure to preserve
        arrange(across(c(all_of(id_col))), group_id)

    # Step 3: Get unique variable combinations
    var_combos <- grouped_data |>
        select(all_of(group_vars)) |>
        distinct() |>
        mutate(combo_id = row_number())

    # Step 4: For each id, randomly shuffle which group_id gets which combination
    unique_ids <- unique(grouped_data[[id_col]])
    assignments <- data.frame()

    # For each id, randomly assign new combinations
    for (id_value in unique_ids) {
        # Get the groups for this id
        id_groups <- id_structure |>
            filter(.data[[id_col]] == id_value) |>
            pull(group_id)

        # Randomly assign combo_ids to these groups
        if (length(id_groups) <= nrow(var_combos)) {
            # If we have enough combinations, sample without replacement
            new_combos <- sample(var_combos$combo_id, length(id_groups))
        } else {
            # If we need more, sample with replacement
            new_combos <- sample(var_combos$combo_id, length(id_groups), replace = TRUE)
        }

        # Create assignment dataframe
        id_assignments <- data.frame(
            temp_id = id_value,
            group_id = id_groups,
            new_combo_id = new_combos
        )
        names(id_assignments)[1] <- id_col

        # Append to overall assignments
        assignments <- rbind(assignments, id_assignments)
    }

    # Step 5: Join everything back together
    # Create a join condition
    join_cols <- c(id_col, "group_id")

    randomized_df <- grouped_data |>
        # Join to get the new combo_id
        left_join(assignments, by = join_cols) |>
        # Remove the original variables
        select(-all_of(group_vars)) |>
        # Join to get the new variable values
        left_join(var_combos |> rename(new_combo_id = combo_id),
                  by = "new_combo_id") |>
        # Clean up by removing the temporary IDs
        select(-group_id, -new_combo_id)

    return(randomized_df)
}
