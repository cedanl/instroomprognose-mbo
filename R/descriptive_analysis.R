#' Filter Applications Data
#'
#' @description
#' Filters applications data based on specified criteria like school years and BRIN codes.
#'
#' @param data Applications dataset (output from load_and_enrich_applications)
#' @param brin_codes Optional vector of BRIN codes to filter on
#' @return Filtered tibble
#'
#' @importFrom dplyr filter
#'
#' @export
filter_applications_on_brin <- function(data,
                                brin_codes = NULL) {

  filtered_data <- data

  # Filter on BRIN codes if specified
  if (!is.null(brin_codes)) {
    filtered_data <- filtered_data |>
      filter(instellingserkenningscode %in% brin_codes)
  }

  return(filtered_data)
}


#' Calculate Summary Statistics
#'
#' @description
#' Calculates summary statistics for comparison purposes (averages, medians, etc.)
#'
#' @param data Applications dataset
#' @param grouping_level String: level of analysis
#' @return List with summary statistics
#'
#' @importFrom dplyr summarise group_by
#'
#' @export
calculate_summary_stats <- function(data, grouping_level = "full") {

  # Group data first
  grouped_data <- group_applications(data, grouping_level)

  # Calculate overall statistics for comparison
  overall_stats <- list(
    total_applications = nrow(grouped_data),
    total_students = length(unique(data$bsnhash)),
    average_conversion_rate = mean(grouped_data$is_enrolled, na.rm = TRUE),
    applications_per_student = nrow(grouped_data) / length(unique(data$bsnhash))
  )

  # Calculate per-institution statistics if relevant
  if (grouping_level %in% c("full", "institution")) {
    institution_stats <- grouped_data |>
      group_by(instellingserkenningscode, school) |>
      summarise(
        applications = n(),
        conversion_rate = mean(is_enrolled, na.rm = TRUE),
        students = length(unique(bsnhash)),
        .groups = "drop"
      )

    overall_stats$institution_stats <- institution_stats
  }

  return(overall_stats)
}


#' Calculate postcode distribution for a specific institution
#'
#' @param data Applications dataset with enrolled students
#' @param institution_name Name of the institution to analyze
#' @param years Vector of years to include (default: c(2023, 2024))
#' @param min_institution_students Minimum institution students per postcode to include (default: 2)
#' @return Data frame with postcode distribution analysis
#'
#' @importFrom dplyr filter mutate group_by summarise left_join distinct pull
#' @importFrom tidyr replace_na
#' @importFrom stats na.omit
#' @importFrom utils head
#'
#' @export
calculate_market_share_by_postcode <- function(data, institution_name, years = c(2023, 2024), min_institution_students = 2) {

    # Validate input data first
    required_cols <- c("status", "school", "schooljaar", "postcodecijfers", "bsnhash")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
    }

    # Check if institution exists in data
    available_institutions <- data |>
        filter(status == "ENROLLED") |>
        distinct(school) |>
        pull(school) |>
        na.omit()

    if (!institution_name %in% available_institutions) {
        warning("Institution '", institution_name, "' not found in enrolled students. Available institutions: ",
                paste(head(available_institutions, 5), collapse = ", "))
        return(data.frame())
    }

    # Filter enrolled students for the specific institution
    institution_students <- data |>
        filter(
            status == "ENROLLED",
            school == institution_name,
            schooljaar %in% years,
            !is.na(postcodecijfers)
        ) |>
        mutate(postcode_4 = sprintf("%04d", as.numeric(postcodecijfers))) |>
        filter(nchar(postcode_4) == 4)

    # Validate we have data after filtering
    if (nrow(institution_students) == 0) {
        warning("No enrolled students found for ", institution_name, " in years ", paste(years, collapse = ", "), " with valid postal codes")
        return(data.frame())
    }

    # Count unique students per postcode per year for the institution
    institution_by_postcode <- institution_students |>
        group_by(schooljaar, postcode_4) |>
        summarise(
            institution_students = n_distinct(bsnhash),
            .groups = "drop"
        )

    # Get total institution students per year
    institution_totals <- institution_by_postcode |>
        group_by(schooljaar) |>
        summarise(total_institution_students = sum(institution_students), .groups = "drop")

    # Calculate postcode distribution within institution
    postcode_distribution_analysis <- institution_by_postcode |>
        left_join(institution_totals, by = "schooljaar") |>
        mutate(
            postcode_percentage = (institution_students / total_institution_students) * 100
        ) |>
        filter(institution_students >= min_institution_students)

    # Validate final output
    if (nrow(postcode_distribution_analysis) == 0) {
        warning("No postal codes found with minimum ", min_institution_students, " students")
        return(data.frame())
    }

    # Log summary of results
    cat("Postcode distribution analysis completed for", institution_name, "\n")
    cat("- Postal codes analyzed:", nrow(postcode_distribution_analysis), "\n")
    cat("- Total", institution_name, "students:", sum(unique(institution_totals$total_institution_students)), "\n")
    cat("- Years:", paste(unique(postcode_distribution_analysis$schooljaar), collapse = ", "), "\n")

    return(postcode_distribution_analysis)
}

#' Create geographic postcode distribution visualization
#'
#' @param postcode_data Postcode distribution data from calculate_market_share_by_postcode
#' @param geo_data_path Path to geographic data file (GeoPackage format)
#' @param target_year Year to visualize
#' @param institution_name Name of institution for labels
#' @return Leaflet map object
#'
#' @importFrom sf st_read st_transform
#' @importFrom leaflet leaflet addProviderTiles addPolygons addLegend setView
#' @importFrom leaflet colorNumeric providers highlightOptions
#' @importFrom dplyr filter left_join mutate
#' @importFrom tidyr replace_na
#'
#' @export
create_market_share_map <- function(postcode_data, geo_data_path, target_year, institution_name) {

    # Load and transform geographic data
    postal_code_geo <- st_read(geo_data_path, quiet = TRUE)
    postal_code_geo_wgs84 <- st_transform(postal_code_geo, crs = 4326)

    # Filter postcode data for target year
    postcode_year <- postcode_data |>
        filter(schooljaar == target_year) |>
        mutate(postcode = as.numeric(postcode_4)) |>
        select(-postcode_4)

    # Join with geographic data
    geo_distribution <- postal_code_geo_wgs84 |>
        left_join(postcode_year, by = "postcode") |>
        mutate(
            postcode_percentage = replace_na(postcode_percentage, 0),
            institution_students = replace_na(institution_students, 0)
        )

    # Create color palette with dynamic domain
    max_percentage <- max(postcode_data$postcode_percentage, na.rm = TRUE)
    color_palette <- colorNumeric(
        palette = c("#FFFFFF", "#FFFFCC", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026"),
        domain = c(0, max_percentage),
        na.color = "transparent"
    )

    # Create map
    map <- leaflet(geo_distribution) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(
            fillColor = ~color_palette(postcode_percentage),
            fillOpacity = 0.8,
            color = "white",
            weight = 0.5,
            popup = ~paste0(
                "<b>Postcode:</b> ", postcode, "<br>",
                "<b>", institution_name, " studenten:</b> ", institution_students, "<br>",
                "<b>Percentage van ", institution_name, ":</b> ", round(postcode_percentage, 1), "%"
            ),
            label = ~paste0(postcode, ": ", round(postcode_percentage, 1), "% van ", institution_name),
            highlightOptions = highlightOptions(
                weight = 2,
                color = "black",
                fillOpacity = 1,
                bringToFront = TRUE
            )
        ) |>
        addLegend(
            pal = color_palette,
            values = ~postcode_percentage,
            title = "Percentage (%)",
            position = "bottomright"
        ) |>
        setView(lng = 5.2, lat = 52.2, zoom = 8)

    return(map)
}


#' Analyze Application Timing Patterns
#'
#' @description
#' Analyzes when applications are submitted during the academic year.
#' Based on temporal analysis from instroomprognose_prototype.qmd.
#'
#' @param data Applications dataset
#' @param academic_year_start_month Integer month when academic year starts (default: 10 for October)
#' @return List with monthly statistics and grouped data for plotting
#'
#' @importFrom dplyr mutate group_by summarise filter
#'
#' @export
analyze_application_timing <- function(data, academic_year_start_month = 10) {

    # Prepare temporal data with academic year calculations
    applications_temporal <- data |>
        filter(!is.na(begindatum))

    # Calculate monthly statistics
    monthly_stats <- applications_temporal |>
        group_by(schooljaar, academic_month) |>
        summarise(
            applications = n(),
            unique_students = length(unique(bsnhash)),
            conversions = sum(status == "ENROLLED", na.rm = TRUE),
            conversion_rate = mean(status == "ENROLLED", na.rm = TRUE) * 100,
            .groups = "drop"
        )

    # Create month labels for visualization
    month_labels <- c("Okt", "Nov", "Dec", "Jan", "Feb", "Mrt",
                     "Apr", "Mei", "Jun", "Jul", "Aug", "Sep")

    return(list(
        monthly_stats = monthly_stats,
        month_labels = month_labels,
        temporal_data = applications_temporal
    ))
}


#' Analyze Status Transitions
#'
#' @description
#' Analyzes how application statuses change over time during the academic year.
#' Based on complex status transition analysis from instroomprognose_prototype.qmd.
#'
#' @param data Raw applications dataset (not grouped)
#' @param target_year Integer year to analyze (default: 2024)
#' @return List with status transition analysis results
#'
#' @importFrom dplyr arrange group_by mutate filter lag summarise ungroup
#'
#' @export
analyze_status_transitions <- function(data, target_year = 2024) {
    # Step 1: Prepare the data
    status_data <- data |>
      filter(schooljaar_afgeleid == target_year)

    # Step 2: Count new statuses by week
    new_status_by_week <- count_new_status_by_week(status_data)

    # Step 3: Calculate transitions
    status_transitions <- calculate_status_transitions(status_data)

    # Step 4: Build running counts
    result <- build_running_status_counts(new_status_by_week, status_transitions)

    # Step 5: Create month mappings
    month_labels <- c("Okt", "Nov", "Dec", "Jan", "Feb", "Mrt",
                     "Apr", "Mei", "Jun", "Jul", "Aug", "Sep")

    month_start_weeks <- status_data |>
        filter(!is.na(academic_month)) |>
        group_by(academic_month) |>
        summarise(start_week = min(academic_week, na.rm = TRUE), .groups = "drop") |>
        filter(!is.na(start_week))

    return(list(
        result = result,
        new_status_by_week = new_status_by_week,
        status_transitions = status_transitions,
        month_labels = month_labels,
        month_start_weeks = month_start_weeks
    ))
}


#' Count new status entries by week
#'
#' @param status_data Processed status data from prepare_status_data
#' @return Data frame with new status counts by week
#'
#' @importFrom dplyr group_by summarise
#'
#' @export
count_new_status_by_week <- function(status_data) {
    status_data |>
        group_by(
            bsnhash, schooljaar, opleidingcode, instellingserkenningscode,
            academic_week, academic_month, status
        ) |>
        summarise(
            count = n_distinct(bsnhash, opleidingcode, instellingserkenningscode),
            .groups = "drop"
        )
}

#' Calculate status transitions between weeks
#'
#' @param status_data Processed status data from prepare_status_data
#' @return Data frame with status transitions
#'
#' @importFrom dplyr arrange group_by mutate filter lag summarise
#'
#' @export
calculate_status_transitions <- function(status_data) {
    status_data |>
        arrange(bsnhash, schooljaar, opleidingcode, instellingserkenningscode, begindatum) |>
        group_by(bsnhash, schooljaar, opleidingcode, instellingserkenningscode) |>
        mutate(
            prev_status = lag(status_proper_case),
            prev_week = lag(academic_week)
        ) |>
        filter(!is.na(prev_status) & status_proper_case != prev_status) |>
        group_by(academic_week, academic_month, prev_status, status_proper_case) |>
        summarise(
            transition_count = n(),
            .groups = "drop"
        )
}

#' Build running status counts over time
#'
#' @param new_status_by_week New status counts by week
#' @param status_transitions Status transitions data
#' @return Data frame with running status counts and percentages
#'
#' @importFrom dplyr left_join group_by summarise mutate
#' @importFrom tibble tibble
#' @importFrom tidyr complete
#'
#' @export
build_running_status_counts <- function(new_status_by_week, status_transitions) {
    # Get all unique weeks and statuses
    all_weeks <- sort(unique(new_status_by_week$academic_week))
    all_statuses <- unique(new_status_by_week$status_proper_case)

    # Initialize results
    result <- data.frame()

    # Build running counts matrix
    status_counts <- matrix(0, nrow = length(all_weeks), ncol = length(all_statuses))
    colnames(status_counts) <- all_statuses
    rownames(status_counts) <- all_weeks

    # For each week, build running totals
    for (i in 1:length(all_weeks)) {
        week <- all_weeks[i]

        # Start with previous week's counts
        if (i > 1) {
            status_counts[i,] <- status_counts[i-1,]
        }

        # Add new entries
        week_new <- new_status_by_week |>
            filter(academic_week == week)

        for (j in 1:nrow(week_new)) {
            status_temp <- week_new$status_proper_case[j]
            if (!is.na(status_temp) && length(status_temp) > 0 && status_temp %in% colnames(status_counts)) {
                status_counts[i, status_temp] <- status_counts[i, status_temp] + week_new$count[j]
            }
        }

        # Apply transitions
        week_transitions <- status_transitions |>
            filter(academic_week == week)

        for (j in 1:nrow(week_transitions)) {
            from_status <- week_transitions$prev_status[j]
            trans_count <- week_transitions$transition_count[j]

            # Check if from_status is not empty and exists in matrix
            if (!is.na(from_status) && length(from_status) > 0 && from_status %in% colnames(status_counts)) {
                status_counts[i, from_status] <- status_counts[i, from_status] - trans_count
            }
        }

        # Add to results
        for (status_temp in all_statuses) {
            result <- rbind(result, data.frame(
                academic_week = week,
                status_proper_case = status_temp,
                count = status_counts[i, status_temp]
            ))
        }
    }

    # Add month information and percentages
    result <- result |>
        left_join(
            new_status_by_week |>
                select(academic_week, academic_month) |>
                distinct(),
            by = "academic_week"
        )

    # Calculate percentages
    week_totals <- result |>
        group_by(academic_week) |>
        summarise(total = sum(count), .groups = "drop")

    result |>
        left_join(week_totals, by = "academic_week") |>
        mutate(percentage = count / total * 100)
}

