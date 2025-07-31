#' Validate Postal Codes
#'
#' @description
#' Validates Dutch postal codes (postcodecijfers) and adds validation flag to dataset.
#' Based on logic from data_kwaliteit.qmd section 2.1.
#'
#' @param data Dataset with postcodecijfers column
#' @return Dataset with valid_postcode flag added
#'
#' @importFrom dplyr mutate
#'
#' @export
validate_postcodes <- function(data) {

  # Function to check if postcodecijfers is valid
  # Valid postcodecijfers consist of exactly 4 numeric characters
  is_valid_postcode_digits <- function(postcodecijfers) {
    ifelse(is.na(postcodecijfers), FALSE, grepl("^\\d{4}$", postcodecijfers))
  }

  # Add validation flag to dataset
  data %>%
    mutate(
      valid_postcode = is_valid_postcode_digits(postcodecijfers),
      schooljaar = factor(schooljaar)
    )
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
#' @importFrom lubridate as.Date month day year
#'
#' @export
derive_schooljaar <- function(data) {

  # Ensure date columns are properly formatted
  data_with_dates <- data %>%
    mutate(
      created_date_parsed = as.Date(createdat),
      begindatum_parsed = as.Date(begindatum)
    )

  # Check for parsing issues
  no_dates <- sum(is.na(data_with_dates$created_date_parsed))
  if (no_dates > 0) {
    warning(paste("Er zijn", no_dates, "datums niet correct geparsed."))
  }

  # Derive school years based on created_date and startmoment
  data_with_derived <- data_with_dates %>%
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
  data_with_conclusion <- data_with_derived %>%
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


#' Analyze Data Quality
#'
#' @description
#' Comprehensive data quality analysis including postcode validation,
#' school year derivation, and missing data analysis.
#'
#' @param data Applications dataset
#' @param by_group String: grouping variable for analysis ("schooljaar", "instellingserkenningscode", NULL)
#' @return List with data quality metrics and summaries
#'
#' @importFrom dplyr group_by summarise n distinct
#'
#' @export
analyze_data_quality <- function(data, by_group = "schooljaar") {

  # Apply validations
  data_validated <- data %>%
    validate_postcodes() %>%
    derive_schooljaar()

  # Postcode quality analysis
  if (is.null(by_group)) {
    postcode_analysis <- data_validated %>%
      summarise(
        totaal_aanmeldingen = n(),
        geldige_postcodes = sum(valid_postcode, na.rm = TRUE),
        ongeldige_postcodes = sum(!valid_postcode, na.rm = TRUE),
        percentage_ongeldig = round(ongeldige_postcodes / totaal_aanmeldingen * 100, 2)
      )
  } else {
    postcode_analysis <- data_validated %>%
      group_by(across(all_of(by_group))) %>%
      summarise(
        totaal_aanmeldingen = n(),
        geldige_postcodes = sum(valid_postcode, na.rm = TRUE),
        ongeldige_postcodes = sum(!valid_postcode, na.rm = TRUE),
        percentage_ongeldig = round(ongeldige_postcodes / totaal_aanmeldingen * 100, 2),
        .groups = "drop"
      )
  }

  # School year quality analysis
  schooljaar_comparison <- data_validated %>%
    group_by(schooljaar, schooljaar_afgeleid) %>%
    summarise(aantal = n(), .groups = "drop") %>%
    arrange(schooljaar, schooljaar_afgeleid)

  # Changes made by derivation
  schooljaar_changes <- data_validated %>%
    mutate(veranderd = schooljaar != schooljaar_afgeleid) %>%
    group_by(schooljaar_afgeleid) %>%
    summarise(
      n = n(),
      veranderd = sum(veranderd, na.rm = TRUE),
      percentage_veranderd = sum(veranderd, na.rm = TRUE) / n() * 100,
      .groups = "drop"
    )

  # Top invalid postcodes
  top_invalid_postcodes <- data_validated %>%
    filter(!valid_postcode) %>%
    count(postcodecijfers, sort = TRUE) %>%
    filter(n > 1)

  # Missing data analysis
  missing_analysis <- data_validated %>%
    summarise(
      missing_postcode = sum(is.na(postcodecijfers)),
      missing_schooljaar = sum(is.na(schooljaar) | schooljaar == 0),
      missing_opleidingcode = sum(is.na(opleidingcode)),
      missing_createdat = sum(is.na(createdat)),
      total_rows = n()
    ) %>%
    mutate(across(starts_with("missing"), ~ round(.x / total_rows * 100, 2), .names = "pct_{.col}"))

  # Return comprehensive results
  list(
    postcode_analysis = postcode_analysis,
    schooljaar_comparison = schooljaar_comparison,
    schooljaar_changes = schooljaar_changes,
    top_invalid_postcodes = top_invalid_postcodes,
    missing_analysis = missing_analysis,
    data_with_quality_flags = data_validated
  )
}


#' Analyze Institution Data Quality
#'
#' @description
#' Analyzes data quality patterns per institution, particularly looking
#' for institutions that seem to reuse applications (always 1.00 applications per student).
#'
#' @param data Applications dataset (should be enriched)
#' @return Data frame with applications per student ratio by institution
#'
#' @importFrom dplyr group_by summarise count full_join mutate arrange desc
#'
#' @export
analyze_institution_data_quality <- function(data) {

  # Group applications by institution (student level)
  applications_by_institution <- data %>%
    mutate(begindatum = as.Date(begindatum)) %>%
    group_by(bsnhash, schooljaar, instellingserkenningscode, school) %>%
    summarise(.groups = "drop") %>%
    count(school, name = "students")

  # Group applications by full combination
  applications_full <- data %>%
    mutate(begindatum = as.Date(begindatum)) %>%
    group_by(bsnhash, schooljaar, opleidingcode, Opleidingsnaam,
             instellingserkenningscode, school) %>%
    summarise(.groups = "drop") %>%
    count(school, name = "applications")

  # Calculate applications per student ratio
  institution_quality <- full_join(
    applications_by_institution,
    applications_full,
    by = "school"
  ) %>%
    filter(!is.na(school)) %>%
    mutate(
      aanmeldingen_per_student = applications / students
    ) %>%
    arrange(desc(aanmeldingen_per_student))

  return(institution_quality)
}


#' Get Data Quality Summary
#'
#' @description
#' Provides a high-level summary of data quality issues for reporting.
#'
#' @param quality_results Output from analyze_data_quality()
#' @return Named list with key quality indicators
#'
#' @export
get_data_quality_summary <- function(quality_results) {

  postcode_summary <- quality_results$postcode_analysis
  missing_summary <- quality_results$missing_analysis
  schooljaar_summary <- quality_results$schooljaar_changes

  # Calculate overall quality score (simple example)
  quality_indicators <- c(
    postcode_quality = 100 - mean(postcode_summary$percentage_ongeldig, na.rm = TRUE),
    completeness = 100 - mean(as.numeric(missing_summary[,grepl("pct_", names(missing_summary))]), na.rm = TRUE),
    schooljaar_consistency = 100 - mean(schooljaar_summary$percentage_veranderd, na.rm = TRUE)
  )

  overall_quality <- mean(quality_indicators, na.rm = TRUE)

  list(
    overall_quality_score = round(overall_quality, 1),
    quality_indicators = round(quality_indicators, 1),
    key_issues = list(
      invalid_postcodes = sum(postcode_summary$ongeldige_postcodes, na.rm = TRUE),
      schooljaar_changes = sum(schooljaar_summary$veranderd, na.rm = TRUE),
      total_missing = sum(missing_summary$missing_postcode, missing_summary$missing_schooljaar, na.rm = TRUE)
    )
  )
}
