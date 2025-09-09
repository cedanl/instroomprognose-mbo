

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
#' @importFrom dplyr group_by summarise n distinct filter count mutate arrange across all_of starts_with
#'
#' @export
analyze_data_quality <- function(data, by_group = "schooljaar") {

  # Postcode quality analysis
  if (is.null(by_group)) {
    postcode_analysis <- data |>
      summarise(
        totaal_aanmeldingen = n(),
        geldige_postcodes = sum(valid_postcode, na.rm = TRUE),
        ongeldige_postcodes = sum(!valid_postcode, na.rm = TRUE),
        percentage_ongeldig = round(ongeldige_postcodes / totaal_aanmeldingen * 100, 2)
      )
  } else {
    postcode_analysis <- data |>
      group_by(across(all_of(by_group))) |>
      summarise(
        totaal_aanmeldingen = n(),
        geldige_postcodes = sum(valid_postcode, na.rm = TRUE),
        ongeldige_postcodes = sum(!valid_postcode, na.rm = TRUE),
        percentage_ongeldig = round(ongeldige_postcodes / totaal_aanmeldingen * 100, 2),
        .groups = "drop"
      )
  }

  # School year quality analysis
  schooljaar_comparison <- data |>
    group_by(schooljaar, schooljaar_afgeleid) |>
    summarise(aantal = n(), .groups = "drop") |>
    arrange(schooljaar, schooljaar_afgeleid)

  # Changes made by derivation
  schooljaar_changes <- data |>
    mutate(veranderd = schooljaar != schooljaar_afgeleid) |>
    group_by(schooljaar_afgeleid) |>
    summarise(
      n = n(),
      veranderd = sum(veranderd, na.rm = TRUE),
      percentage_veranderd = sum(veranderd, na.rm = TRUE) / n() * 100,
      .groups = "drop"
    )

  # Top invalid postcodes
  top_invalid_postcodes <- data |>
    filter(!valid_postcode) |>
    count(postcodecijfers, sort = TRUE) |>
    filter(n > 1)

  # Missing data analysis
  missing_analysis <- data |>
    summarise(
      missing_postcode = sum(is.na(postcodecijfers)),
      missing_schooljaar = sum(is.na(schooljaar) | schooljaar == 0),
      missing_opleidingcode = sum(is.na(opleidingcode)),
      missing_createdat = sum(is.na(createdat)),
      total_rows = n()
    ) |>
    mutate(across(starts_with("missing"), ~ round(.x / total_rows * 100, 2), .names = "pct_{.col}"))

  # Return comprehensive results
  list(
    postcode_analysis = postcode_analysis,
    schooljaar_comparison = schooljaar_comparison,
    schooljaar_changes = schooljaar_changes,
    top_invalid_postcodes = top_invalid_postcodes,
    missing_analysis = missing_analysis,
    data_with_quality_flags = data
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
  applications_by_institution <- data |>
    mutate(begindatum = as.Date(begindatum)) |>
    group_by(bsnhash, schooljaar, instellingserkenningscode, school) |>
    summarise(.groups = "drop") |>
    count(school, name = "students")

  # Group applications by full combination
  applications_full <- data |>
    mutate(begindatum = as.Date(begindatum)) |>
    group_by(bsnhash, schooljaar, opleidingcode, Opleidingsnaam,
             instellingserkenningscode, school) |>
    summarise(.groups = "drop") |>
    count(school, name = "applications")

  # Calculate applications per student ratio
  institution_quality <- full_join(
    applications_by_institution,
    applications_full,
    by = "school"
  ) |>
    filter(!is.na(school)) |>
    mutate(
      aanmeldingen_per_student = applications / students
    ) |>
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
