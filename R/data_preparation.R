#' Load Applications Data
#'
#' @description
#' Loads raw applications data from configured data source.
#'
#' @return A tibble with applications data
#'
#' @importFrom config get
#' @importFrom readr cols col_guess
#'
#' @export
load_applications <- function() {

  # Load applications data using config
  applications_filepath <- file.path(
    "data/01_raw",
    config::get("applications_filename")
  )

  applications <- safe_read_csv(
    applications_filepath,
    delim = ",",
    col_types = cols(
      postcodecijfers = "c",
      .default = col_guess()
    )
  )

  return(applications)
}


#' Load BRIN Information
#'
#' @description
#' Loads institutional information from BRIN reference data.
#'
#' @return A tibble with BRIN information
#'
#' @importFrom readxl read_excel
#'
#' @export
load_brin_info <- function() {
  read_excel("data/reference/BRIN.xlsx")
}


#' Load Programmes Information
#'
#' @description
#' Loads programme information from reference data.
#'
#' @return A tibble with programmes information
#'
#' @importFrom readxl read_excel
#'
#' @export
load_programmes_info <- function() {
  read_excel("data/reference/Opleiding_dimensie.xlsx")
}


#' Add Program information
#'
#' @description
#' Enriches applications data with reference data (BRIN and programmes).
#'
#' @param applications Applications data tibble
#' @return A tibble with enriched applications data
#'
#' @importFrom dplyr left_join mutate
#'
#' @export
add_external_program_variables <- function(applications) {

  brin <- load_brin_info()
  programmes <- load_programmes_info()

  # Enrich applications with reference data
  applications_enriched <- applications |>
    # Convert opleidingcode to integer for joining (drops small % of non-numeric values)
    mutate(opleidingcode = as.integer(opleidingcode)) |>
    left_join(programmes, by = c("opleidingcode" = "Opleidingcode")) |>
    left_join(brin, by = c("instellingserkenningscode" = "BRIN"))

  return(applications_enriched)
}


#' Load and Enrich Applications Data (Legacy Function)
#'
#' @description
#' Legacy function that combines loading and enriching for backward compatibility.
#' Consider using the separate load_applications() and add_external_program_variables() functions instead.
#'
#' @return A tibble with enriched applications data
#'
#' @export
load_and_enrich_applications <- function() {
  applications <- load_applications()
  return(add_external_program_variables(applications))
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
  brin_codes <- data |>
    count(instellingserkenningscode, sort = TRUE) |>
    filter(n >= min_applications) |>
    pull(instellingserkenningscode)

  return(brin_codes)
}


#' Add derived data variables
#'
#' @description
#' Adds derived date-related variables to the applications data for temporal analysis.
#'
#' @param data Applications dataset
#' @return Data with added date variables
#'
#' @importFrom dplyr mutate arrange case_when if_else
#' @importFrom lubridate ymd_hms week year month ymd
#'
#' @export
add_derived_date_variables <- function(data) {

    # Prepare data with dates
    data_with_dates <- data |>
        mutate(
            begindatum_parsed = as.Date(begindatum),
            created_date_parsed = ymd_hms(createdat),
            week_of_year = week(begindatum_parsed),
            year_of_date = year(begindatum_parsed),
            # Calculate academic week number
            academic_week = case_when(
              month(begindatum_parsed) >= 10 ~ week_of_year - week(ymd(paste0(year_of_date, "-10-01"))) + 1,
              TRUE ~ week_of_year + (52 - week(ymd(paste0(year_of_date-1, "-10-01")))) + 1
            ),
            academic_month = if_else(month(begindatum_parsed) >= 10,
                                     month(begindatum_parsed) - 9,
                                     month(begindatum_parsed) + 3)
        )
    return(data_with_dates)
}

#' Add derived status variables
#'
#' @description
#' Adds derived status-related variables to the applications data for analysis.
#'
#' @param data Applications dataset
#' @return Data with added status variables
#'
#' @importFrom dplyr mutate arrange case_when
#'
#' @export
add_derived_status_variables <- function(data) {

  # Define status order (lower number = higher priority)
  status_order <- c(
    "ENROLLED" = 6,
    "OFFERED" = 5,
    "RECEIVED" = 4,
    "SUBMITTED" = 3,
    "WITHDRAWN" = 2,
    "REJECTED" = 1
  )


  # Prepare data with dates
  data_prepared <- data |>
    mutate(
      status_numeric = status_order[status], status_numeric = ifelse(is.na(status_numeric), 0, status_numeric), # Categorize status
      status_proper_case = case_when(
        status == "SUBMITTED" ~ "Submitted",
        status == "RECEIVED" ~ "Received",
        status == "OFFERED" ~ "Offered",
        status == "ENROLLED" ~ "Enrolled",
        status == "REJECTED" ~ "Rejected",
        status == "WITHDRAWN" ~ "Withdrawn",
        TRUE ~ NA_character_
      )
    )

    return(data_prepared)
}

#' Add Postal Code info
#'
#' @description
#' Adds numeric variable and validation flag for correct postcode to dataset.
#'
#'
#' @param data Dataset with postcodecijfers column
#' @return Dataset with valid_postcode flag added
#'
#' @importFrom dplyr mutate
#' @importFrom readr parse_number
#'
#' @export
add_derived_postcode_variables <- function(data) {

  # Function to check if postcodecijfers is valid
  # Valid postcodecijfers consist of exactly 4 numeric characters
  is_valid_postcode_digits <- function(postcodecijfers) {
    ifelse(is.na(postcodecijfers), FALSE, grepl("^\\d{4}$", postcodecijfers))
  }

  # Add validation flag to dataset
  data_prepared <- data |>
    mutate(
      valid_postcode = is_valid_postcode_digits(postcodecijfers),
      postcode_4_numeriek = parse_number(postcodecijfers)
    )

  return(data_prepared)

}


#' Derive School Year from Dates
#'
#' @description
#' Derives correct school year from created_date and startmoment using complex logic.
#' Based on the extensive case_when logic from data_kwaliteit.qmd section 2.2.
#'
#' @param data Dataset with createdat, begindatum, startmoment columns
#' @return Dataset with schooljaar_afgeleid column added
#'
#' @importFrom dplyr mutate case_when
#' @importFrom lubridate month day year
#'
#' @export
add_derived_schoolyear <- function(data) {

  # Check for parsing issues
  no_dates <- sum(is.na(data$created_date_parsed))
  if (no_dates > 0) {
    warning(paste("Er zijn", no_dates, "datums niet correct geparsed."))
  }

  # Derive school years based on created_date and startmoment
  data_with_derived <- data |>
    mutate(
      schooljaar_afgeleid_created = case_when(
        is.na(created_date_parsed) ~ NA_integer_,
        month(created_date_parsed) >= 10 ~ year(created_date_parsed) + 1,
        month(created_date_parsed) < 10 ~ year(created_date_parsed),
        .default = NA_integer_
      ),
      schooljaar_afgeleid_startmoment = case_when(
        is.na(startmoment) ~ NA_integer_,
        month(startmoment) >= 8 ~ year(startmoment),
        month(startmoment) < 8 ~ year(startmoment) - 1,
        .default = NA_integer_
      )
    )

  # Final derivation with complex logic
  data_with_conclusion <- data_with_derived |>
    mutate(
      schooljaar = as.integer(as.character(schooljaar)),
      schooljaar_afgeleid = case_when(
        # Everything matches
        schooljaar == schooljaar_afgeleid_startmoment &
          schooljaar == schooljaar_afgeleid_created ~ schooljaar,

        # If schooljaar is 0, use created_date derivation
        schooljaar == 0 ~ schooljaar_afgeleid_created,

        # Early registration in august or september
        schooljaar == schooljaar_afgeleid_startmoment &
          schooljaar_afgeleid_created == schooljaar - 1 &
          month(created_date_parsed) %in% c(8,9) ~ schooljaar,

        # Late registration on October 1st
        schooljaar == schooljaar_afgeleid_startmoment &
          schooljaar_afgeleid_created == schooljaar + 1 &
          month(created_date_parsed) == 10 &
          day(created_date_parsed) == 1 ~ schooljaar,

        # Late enrollment
        schooljaar == schooljaar_afgeleid_startmoment &
          schooljaar_afgeleid_created == schooljaar + 1 &
          created_date_parsed < startmoment ~ schooljaar,

        # Registration on Oct 1 year later where created counts 2 years
        schooljaar == schooljaar_afgeleid_startmoment &
          schooljaar_afgeleid_created > schooljaar + 1 &
          month(created_date_parsed) == 10 &
          day(created_date_parsed) == 1 ~ schooljaar_afgeleid_created - 1,

        # For other cases, use created_date derivation
        schooljaar_afgeleid_created > schooljaar ~ schooljaar_afgeleid_created,

        .default = schooljaar
      )
    )

  return(data_with_conclusion)
}


#' Add Student Context Per Row
#'
#' @description
#' Adds contextual information per row about student's other applications and status progression.
#' Unlike group_applications, this function keeps all rows and adds group_id and context columns.
#'
#' @param data Applications dataset
#' @return Data with added context columns per row
#'
#' @importFrom dplyr mutate group_by ungroup row_number case_when arrange lag lead cur_group_id n_distinct select first
#' @importFrom purrr pmap_dbl
#'
#' @export
add_student_context_per_row <- function(data) {

  # Add group_id (unique identifier for each student-year-program-institution combination)
  data_with_group_id <- data |>
    group_by(bsnhash, schooljaar, opleidingcode, instellingserkenningscode) |>
    mutate(group_id = cur_group_id()) |>
    ungroup()

  # For each row, determine student's other applications at that point in time
  data_with_context <- data_with_group_id |>
    group_by(group_id) |>
    arrange(begindatum_parsed) |>
    mutate(
      # Track status progression within this application
      prev_status = lag(status),
      is_status_upgrade = case_when(
        is.na(prev_status) ~ TRUE,  # First status is always considered an upgrade
        status_numeric > lag(status_numeric) ~ TRUE,
        TRUE ~ FALSE
      ),
      # Calculate days since first application in this group
      days_since_first_application = as.numeric(begindatum_parsed - min(begindatum_parsed, na.rm = TRUE))
    ) |>
    ungroup()

  # Now add context about other applications at the time of each row
  data_final <- data_with_context |>
    group_by(bsnhash, schooljaar) |>
    arrange(begindatum_parsed) |>
    mutate(
      # Count total applications for this student-year at this point
      applications_so_far = row_number(),

      # For each row, find what other applications exist at this time
      student_other_applications_exist = n_distinct(group_id) > 1,

      # Find current maximum status across all applications for this student-year
      student_max_status_numeric = {
        # For each row, we need the max status up to that point in time
        pmap_dbl(list(begindatum_parsed), function(current_date) {
          same_student <- bsnhash == first(bsnhash) & schooljaar == first(schooljaar)
          up_to_date <- begindatum_parsed <= current_date
          eligible_statuses <- status_numeric[same_student & up_to_date]
          if (length(eligible_statuses) > 0) max(eligible_statuses) else 0
        })
      },

      # Check if this row's status equals the student's max status at this time
      student_is_max_status = status_numeric == student_max_status_numeric
    ) |>
    ungroup() |>
    # Clean up temporary columns
    select(-status_numeric, -prev_status, -is_status_upgrade)

  return(data_final)
}

