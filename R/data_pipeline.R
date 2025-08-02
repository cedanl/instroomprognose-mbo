#' Load and Enrich Applications Data
#'
#' @description
#' Loads applications data from config and enriches with reference data (BRIN and programmes).
#' This function combines the data loading pattern used in both QMD files.
#'
#' @param config_env String specifying config environment ("default" or "cambo")
#' @return A tibble with enriched applications data
#'
#' @importFrom config get
#' @importFrom readxl read_excel
#' @importFrom dplyr left_join mutate as.integer
#' @importFrom readr cols col_guess
#'
#' @export
load_and_enrich_applications <- function(config_env = "default") {

  # Load applications data using config
  applications_filepath <- file.path(
    config::get("data_base_dir", config = config_env),
    config::get("applications_filename", config = config_env)
  )

  applications <- safe_read_csv(
    applications_filepath,
    delim = ",",
    col_types = cols(
      postcodecijfers = "c",
      .default = col_guess()
    )
  )

  # Load reference data
  brin <- read_excel("data/reference/BRIN.xlsx")
  programmes <- read_excel("data/reference/Opleiding_dimensie.xlsx")

  # Enrich applications with reference data
  applications_enriched <- applications %>%
    # Convert opleidingcode to integer for joining (drops small % of non-numeric values)
    mutate(opleidingcode = as.integer(opleidingcode)) %>%
    left_join(programmes, by = c("opleidingcode" = "Opleidingcode")) %>%
    left_join(brin, by = c("instellingserkenningscode" = "BRIN"))

  return(applications_enriched)
}


#' Filter Applications Data
#'
#' @description
#' Filters applications data based on specified criteria like school years and BRIN codes.
#'
#' @param data Applications dataset (output from load_and_enrich_applications)
#' @param years Vector of school years to keep (default: c(2023, 2024))
#' @param brin_codes Optional vector of BRIN codes to filter on
#' @param remove_invalid_years Boolean, remove rows with schooljaar == 0 (default: TRUE)
#' @return Filtered tibble
#'
#' @importFrom dplyr filter
#'
#' @export
filter_applications <- function(data,
                                years = c(2023, 2024),
                                brin_codes = NULL,
                                remove_invalid_years = TRUE) {

  filtered_data <- data

  # Filter on school years if specified
  if (!is.null(years)) {
    filtered_data <- filtered_data %>%
      filter(schooljaar %in% years)
  }

  # Filter on BRIN codes if specified
  if (!is.null(brin_codes)) {
    filtered_data <- filtered_data %>%
      filter(instellingserkenningscode %in% brin_codes)
  }

  # Remove invalid school years if requested
  if (remove_invalid_years) {
    filtered_data <- filtered_data %>%
      filter(schooljaar != 0 | is.na(schooljaar))
  }

  return(filtered_data)
}


#' Group Applications Data
#'
#' @description
#' Groups applications data at different levels and calculates aggregated metrics.
#' Based on the grouping logic from instroomprognose_prototype.qmd.
#'
#' @param data Applications dataset
#' @param grouping_level String: "full" (student+programme+institution),
#'                              "institution" (student+institution only),
#'                              "student" (student level only)
#' @return Grouped tibble with aggregated metrics
#'
#' @importFrom dplyr group_by summarise mutate ungroup add_tally
#' @importFrom lubridate as.Date
#'
#' @export
group_applications <- function(data, grouping_level = "full") {

  # Ensure date format
  data <- data %>%
    mutate(begindatum = as.Date(begindatum))

  # Define grouping variables based on level
  grouping_vars <- switch(
    grouping_level,
    "full" = c("bsnhash", "schooljaar", "opleidingcode", "Opleidingsnaam",
               "instellingserkenningscode", "school"),
    "institution" = c("bsnhash", "schooljaar", "instellingserkenningscode", "school"),
    "student" = c("bsnhash", "schooljaar"),
    stop("Invalid grouping_level. Use 'full', 'institution', or 'student'")
  )

  # Perform grouping and aggregation
  grouped_data <- data %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      application_duration_days = as.numeric(max(begindatum) - min(begindatum)),
      statusses = paste(status, collapse = ", "),
      is_enrolled = any(status == "ENROLLED"),
      leertrajectmbo = paste(unique(leertrajectmbo), collapse = ", "),
      statussource = paste(unique(statussource), collapse = ", "),
      .groups = "drop"
    )

  # Add multiple application flags if grouping at student level
  if (grouping_level %in% c("full", "institution")) {
    grouped_data <- grouped_data %>%
      group_by(bsnhash, schooljaar) %>%
      add_tally(name = "applications_total_number") %>%
      ungroup()

    if (grouping_level == "full") {
      grouped_data <- grouped_data %>%
        group_by(bsnhash, schooljaar, instellingserkenningscode) %>%
        add_tally(name = "applications_total_number_within_institution") %>%
        ungroup() %>%
        group_by(bsnhash, schooljaar) %>%
        mutate(applications_total_number_of_institutions = length(unique(instellingserkenningscode))) %>%
        ungroup() %>%
        mutate(
          applications_is_multiple = applications_total_number > 1,
          applications_is_multiple_within_institution = applications_total_number_within_institution > 1,
          applications_is_multiple_across_institutions = applications_total_number_of_institutions > 1
        )
    }
  }

  return(grouped_data)
}


#' Get All BRIN Codes from Data
#'
#' @description
#' Extracts all unique BRIN codes from the applications data for iteration.
#'
#' @param data Applications dataset (optional, will load if not provided)
#' @param min_applications Minimum number of applications required (default: 50)
#' @return Vector of BRIN codes
#'
#' @importFrom dplyr distinct pull filter count
#'
#' @export
get_all_brin_codes <- function(data = NULL, min_applications = 50) {

  if (is.null(data)) {
    data <- load_and_enrich_applications()
  }

  # Get BRIN codes with sufficient applications
  brin_codes <- data %>%
    count(instellingserkenningscode, sort = TRUE) %>%
    filter(n >= min_applications) %>%
    pull(instellingserkenningscode)

  return(brin_codes)
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
    institution_stats <- grouped_data %>%
      group_by(instellingserkenningscode, school) %>%
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
#' @importFrom lubridate floor_date month year as.Date
#'
#' @export
analyze_application_timing <- function(data, academic_year_start_month = 10) {

    # Prepare temporal data with academic year calculations
    applications_temporal <- data %>%
        mutate(
            begindatum = as.Date(begindatum),
            month_number = month(begindatum),
            year_number = year(begindatum),
            # Calculate academic year and month
            academic_year = if_else(month_number >= academic_year_start_month,
                                  schooljaar, schooljaar - 1),
            academic_month = if_else(month_number >= academic_year_start_month,
                                   month_number - (academic_year_start_month - 1),
                                   month_number + (12 - academic_year_start_month + 1))
        ) %>%
        filter(!is.na(begindatum))

    # Calculate monthly statistics
    monthly_stats <- applications_temporal %>%
        group_by(schooljaar, academic_month) %>%
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


#' Analyze Multiple Application Patterns
#'
#' @description
#' Analyzes students with multiple applications and their conversion patterns.
#' Based on analysis from instroomprognose_prototype.qmd section 3.
#'
#' @param data Applications dataset (should be grouped data from group_applications)
#' @return List with multiple application analysis results
#'
#' @importFrom dplyr group_by summarise mutate case_when select distinct
#'
#' @export
analyze_multiple_applications <- function(data) {

    # Ensure we have the multiple application flags
    if (!"applications_is_multiple" %in% names(data)) {
        stop("Data must be grouped with group_applications(grouping_level = 'full') first")
    }

    # Analyze conversion by application pattern
    conversion_by_pattern <- data %>%
        group_by(
            applications_is_multiple,
            applications_is_multiple_within_institution,
            applications_is_multiple_across_institutions
        ) %>%
        summarise(
            total = n(),
            enrolled = sum(is_enrolled, na.rm = TRUE),
            conversion_rate = mean(is_enrolled, na.rm = TRUE) * 100,
            .groups = "drop"
        ) %>%
        mutate(
            pattern_type = case_when(
                applications_is_multiple_within_institution & applications_is_multiple_across_institutions ~
                    "Binnen én tussen instellingen",
                applications_is_multiple_within_institution ~ "Binnen dezelfde instelling",
                applications_is_multiple_across_institutions ~ "Tussen verschillende instellingen",
                TRUE ~ "Slechts één aanmelding"
            )
        )

    # Analyze distribution of application counts
    application_count_distribution <- data %>%
        select(bsnhash, schooljaar, applications_total_number) %>%
        distinct() %>%
        group_by(schooljaar, applications_total_number) %>%
        summarise(
            students = n(),
            .groups = "drop"
        ) %>%
        group_by(schooljaar) %>%
        mutate(
            students_pct = students / sum(students) * 100
        ) %>%
        ungroup() %>%
        filter(applications_total_number <= 10)  # Limit for visualization

    # Analyze conversion by application count
    student_enrollment <- data %>%
        select(bsnhash, schooljaar, applications_total_number, is_enrolled) %>%
        group_by(bsnhash, schooljaar) %>%
        summarise(
            applications_total_number = first(applications_total_number),
            student_enrolled = any(is_enrolled),
            .groups = "drop"
        )

    conversion_by_count <- student_enrollment %>%
        group_by(schooljaar, applications_total_number) %>%
        summarise(
            students = n(),
            converted_students = sum(student_enrolled),
            student_conversion_rate = mean(student_enrolled) * 100,
            .groups = "drop"
        ) %>%
        filter(applications_total_number <= 10) %>%
        mutate(
            application_conversion_rate = converted_students / (students * applications_total_number) * 100
        )

    return(list(
        conversion_by_pattern = conversion_by_pattern,
        application_count_distribution = application_count_distribution,
        conversion_by_count = conversion_by_count
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
#' @importFrom lubridate week as.Date month year
#'
#' @export
analyze_status_transitions <- function(data, target_year = 2024) {
    # Step 1: Prepare the data
    status_data <- prepare_status_data(data, target_year)

    # Step 2: Count new statuses by week
    new_status_by_week <- count_new_status_by_week(status_data)

    # Step 3: Calculate transitions
    status_transitions <- calculate_status_transitions(status_data)

    # Step 4: Build running counts
    result <- build_running_status_counts(new_status_by_week, status_transitions)

    # Step 5: Create month mappings
    month_labels <- c("Okt", "Nov", "Dec", "Jan", "Feb", "Mrt",
                     "Apr", "Mei", "Jun", "Jul", "Aug", "Sep")

    month_start_weeks <- status_data %>%
        filter(!is.na(academic_month)) %>%
        group_by(academic_month) %>%
        summarise(start_week = min(academic_week, na.rm = TRUE), .groups = "drop") %>%
        filter(!is.na(start_week))

    return(list(
        result = result,
        new_status_by_week = new_status_by_week,
        status_transitions = status_transitions,
        month_labels = month_labels,
        month_start_weeks = month_start_weeks
    ))
}


#' Prepare status data for temporal analysis
#'
#' @param data Applications dataset
#' @param target_year Integer year to analyze (default: 2024)
#' @return Data frame with processed status data
#'
#' @importFrom dplyr filter mutate
#' @importFrom lubridate week as.Date month year
#'
#' @export
prepare_status_data <- function(data, target_year = 2024) {
    data %>%
        filter(schooljaar == target_year) %>%
        mutate(
            begindatum = as.Date(begindatum),
            week_of_year = week(begindatum),
            year_of_date = year(begindatum),
            # Calculate academic week number
            academic_week = case_when(
                month(begindatum) >= 10 ~ week_of_year - week(as.Date(paste0(year_of_date, "-10-01"))) + 1,
                TRUE ~ week_of_year + (52 - week(as.Date(paste0(year_of_date-1, "-10-01")))) + 1
            ),
            academic_month = if_else(month(begindatum) >= 10,
                                   month(begindatum) - 9,
                                   month(begindatum) + 3),
            # Categorize status
            status_category = case_when(
                status == "SUBMITTED" ~ "Submitted",
                status == "RECEIVED" ~ "Received",
                status == "OFFERED" ~ "Offered",
                status == "ENROLLED" ~ "Enrolled",
                status == "REJECTED" ~ "Rejected",
                status == "WITHDRAWN" ~ "Withdrawn",
                TRUE ~ "Other"
            )
        ) %>%
        filter(!is.na(begindatum))
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
    status_data %>%
        group_by(
            bsnhash, schooljaar, opleidingcode, instellingserkenningscode,
            academic_week, academic_month, status_category
        ) %>%
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
    status_data %>%
        arrange(bsnhash, schooljaar, opleidingcode, instellingserkenningscode, begindatum) %>%
        group_by(bsnhash, schooljaar, opleidingcode, instellingserkenningscode) %>%
        mutate(
            prev_status = lag(status_category),
            prev_week = lag(academic_week)
        ) %>%
        filter(!is.na(prev_status) & status_category != prev_status) %>%
        group_by(academic_week, academic_month, prev_status, status_category) %>%
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
    all_statuses <- unique(new_status_by_week$status_category)

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
        week_new <- new_status_by_week %>%
            filter(academic_week == week)

        for (j in 1:nrow(week_new)) {
            status <- week_new$status_category[j]
            if (!is.na(status) && length(status) > 0 && status %in% colnames(status_counts)) {
                status_counts[i, status] <- status_counts[i, status] + week_new$count[j]
            }
        }

        # Apply transitions
        week_transitions <- status_transitions %>%
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
        for (status in all_statuses) {
            result <- rbind(result, data.frame(
                academic_week = week,
                status_category = status,
                count = status_counts[i, status]
            ))
        }
    }

    # Add month information and percentages
    result <- result %>%
        left_join(
            new_status_by_week %>%
                select(academic_week, academic_month) %>%
                distinct(),
            by = "academic_week"
        )

    # Calculate percentages
    week_totals <- result %>%
        group_by(academic_week) %>%
        summarise(total = sum(count), .groups = "drop")

    result %>%
        left_join(week_totals, by = "academic_week") %>%
        mutate(percentage = count / total * 100)
}


#' Calculate market share by postal code for a specific institution
#'
#' @param data Applications dataset with enrolled students
#' @param institution_name Name of the institution to analyze
#' @param years Vector of years to include (default: c(2023, 2024))
#' @param min_total_students Minimum total students per postcode to include (default: 5)
#' @return Data frame with market share analysis by postcode
#'
#' @importFrom dplyr filter mutate group_by summarise left_join
#' @importFrom tidyr replace_na
#'
#' @export
calculate_market_share_by_postcode <- function(data, institution_name, years = c(2023, 2024), min_total_students = 5) {

    # Validate input data first
    required_cols <- c("status", "school", "schooljaar", "postcodecijfers", "bsnhash")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
    }

    # Check if institution exists in data
    available_institutions <- data %>%
        filter(status == "ENROLLED") %>%
        distinct(school) %>%
        pull(school) %>%
        na.omit()

    if (!institution_name %in% available_institutions) {
        warning("Institution '", institution_name, "' not found in enrolled students. Available institutions: ",
                paste(head(available_institutions, 5), collapse = ", "))
        return(data.frame())
    }

    # Filter enrolled students for the specific institution
    institution_students <- data %>%
        filter(
            status == "ENROLLED",
            school == institution_name,
            schooljaar %in% years,
            !is.na(postcodecijfers)
        ) %>%
        mutate(postcode_4 = sprintf("%04d", as.numeric(postcodecijfers))) %>%
        filter(nchar(postcode_4) == 4)

    # Validate we have data after filtering
    if (nrow(institution_students) == 0) {
        warning("No enrolled students found for ", institution_name, " in years ", paste(years, collapse = ", "), " with valid postal codes")
        return(data.frame())
    }

    # Count unique students per postcode per year for the institution
    institution_by_postcode <- institution_students %>%
        group_by(schooljaar, postcode_4) %>%
        summarise(
            institution_students = n_distinct(bsnhash),
            .groups = "drop"
        )

    # Count total unique students per postcode per year (all institutions)
    total_by_postcode <- data %>%
        filter(
            status == "ENROLLED",
            schooljaar %in% years,
            !is.na(postcodecijfers)
        ) %>%
        mutate(postcode_4 = sprintf("%04d", as.numeric(postcodecijfers))) %>%
        filter(nchar(postcode_4) == 4) %>%
        group_by(schooljaar, postcode_4) %>%
        summarise(
            total_students = n_distinct(bsnhash),
            .groups = "drop"
        )

    total_by_postcode <- geo

    # Calculate market share
    market_share_analysis <- total_by_postcode %>%
        left_join(institution_by_postcode, by = c("schooljaar", "postcode_4")) %>%
        mutate(
            institution_students = replace_na(institution_students, 0),
            market_share_pct = (institution_students / total_students) * 100
        ) %>%
        filter(total_students >= min_total_students)

    # Validate final output
    if (nrow(market_share_analysis) == 0) {
        warning("No postal codes found with minimum ", min_total_students, " students")
        return(data.frame())
    }

    # Log summary of results
    cat("Market share analysis completed for", institution_name, "\n")
    cat("- Postal codes analyzed:", nrow(market_share_analysis), "\n")
    cat("- Postal codes with", institution_name, "students:", sum(market_share_analysis$institution_students > 0), "\n")
    cat("- Years:", paste(unique(market_share_analysis$schooljaar), collapse = ", "), "\n")

    return(market_share_analysis)
}

#' Create geographic market share visualization
#'
#' @param market_share_data Market share data from calculate_market_share_by_postcode
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
create_market_share_map <- function(market_share_data, geo_data_path, target_year, institution_name) {

    # Load and transform geographic data
    postal_code_geo <- st_read(geo_data_path, quiet = TRUE)
    postal_code_geo_wgs84 <- st_transform(postal_code_geo, crs = 4326)

    # Filter market share data for target year
    market_share_year <- market_share_data %>%
        filter(schooljaar == target_year) %>%
        mutate(postcode = as.numeric(postcode_4)) %>%
        select(-postcode_4)

    # Join with geographic data
    geo_market <- postal_code_geo_wgs84 %>%
        left_join(market_share_year, by = "postcode") %>%
        mutate(
            market_share_pct = replace_na(market_share_pct, 0),
            institution_students = replace_na(institution_students, 0),
            total_students = aantal_inwoners / 4000,
            market_share_pct = institution_students / total_students
        )

    # Create color palette
    color_palette <- colorNumeric(
        palette = c("#FFFFFF", "#FFFFCC", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026"),
        domain = c(0, 100),
        na.color = "transparent"
    )

    # Create map
    map <- leaflet(geo_market) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
            fillColor = ~color_palette(market_share_pct),
            fillOpacity = 0.8,
            color = "white",
            weight = 0.5,
            popup = ~paste0(
                "<b>Postcode:</b> ", postcode, "<br>",
                "<b>", institution_name, " studenten:</b> ", institution_students, "<br>",
                "<b>Totaal studenten:</b> ", total_students, "<br>",
                "<b>Marktaandeel:</b> ", round(market_share_pct, 1), "%"
            ),
            label = ~paste0(postcode, ": ", round(market_share_pct, 1), "% marktaandeel"),
            highlightOptions = highlightOptions(
                weight = 2,
                color = "black",
                fillOpacity = 1,
                bringToFront = TRUE
            )
        ) %>%
        addLegend(
            pal = color_palette,
            values = ~market_share_pct,
            title = "Marktaandeel (%)",
            position = "bottomright"
        ) %>%
        setView(lng = 5.2, lat = 52.2, zoom = 8)

    return(map)
}
