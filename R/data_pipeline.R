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
    
    # Filter to target year and prepare date calculations
    status_data <- data %>%
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
    
    # Count new status entries by week
    new_status_by_week <- status_data %>%
        group_by(
            bsnhash, schooljaar, opleidingcode, instellingserkenningscode,
            academic_week, academic_month, status_category
        ) %>%
        summarise(
            count = n_distinct(bsnhash, opleidingcode, instellingserkenningscode),
            .groups = "drop"
        )
    
    # Calculate status transitions
    status_transitions <- status_data %>%
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
    
    # Calculate weekly status summary 
    weekly_status_summary <- status_data %>%
        group_by(academic_week, status_category) %>%
        summarise(
            count = n(),
            .groups = "drop"
        )
    
    # Create month labels and week mapping
    month_labels <- c("Okt", "Nov", "Dec", "Jan", "Feb", "Mrt", 
                     "Apr", "Mei", "Jun", "Jul", "Aug", "Sep")
    
    month_start_weeks <- status_data %>%
        filter(!is.na(academic_month)) %>%
        group_by(academic_month) %>%
        summarise(start_week = min(academic_week, na.rm = TRUE), .groups = "drop") %>%
        filter(!is.na(start_week))
    
    return(list(
        new_status_by_week = new_status_by_week,
        status_transitions = status_transitions,
        weekly_status_summary = weekly_status_summary,
        month_labels = month_labels,
        month_start_weeks = month_start_weeks
    ))
}
