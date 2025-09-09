
#' Analyze Multiple Application Patterns
#'
#' @description
#' Analyzes students with multiple applications and their conversion patterns.
#' Based on analysis from instroomprognose_prototype.qmd section 3.
#'
#' @param data Applications dataset (should be grouped data from group_applications)
#' @return List with multiple application analysis results
#'
#' @importFrom dplyr group_by summarise mutate case_when select distinct first
#'
#' @export
analyze_multiple_applications <- function(data) {

  # Ensure we have the multiple application flags
  if (!"applications_is_multiple" %in% names(data)) {
    stop("Data must be grouped with group_applications(grouping_level = 'full') first")
  }

  # Analyze conversion by application pattern
  conversion_by_pattern <- data |>
    group_by(
      applications_is_multiple,
      applications_is_multiple_within_institution,
      applications_is_multiple_across_institutions
    ) |>
    summarise(
      total = n(),
      enrolled = sum(is_enrolled, na.rm = TRUE),
      conversion_rate = mean(is_enrolled, na.rm = TRUE) * 100,
      .groups = "drop"
    ) |>
    mutate(
      pattern_type = case_when(
        applications_is_multiple_within_institution & applications_is_multiple_across_institutions ~
          "Binnen en tussen instellingen",
        applications_is_multiple_within_institution ~ "Binnen dezelfde instelling",
        applications_is_multiple_across_institutions ~ "Tussen verschillende instellingen",
        TRUE ~ "Slechts 1 aanmelding"
      )
    )

  # Analyze distribution of application counts
  application_count_distribution <- data |>
    select(bsnhash, schooljaar, applications_total_number) |>
    distinct() |>
    group_by(schooljaar, applications_total_number) |>
    summarise(
      students = n(),
      .groups = "drop"
    ) |>
    group_by(schooljaar) |>
    mutate(
      students_pct = students / sum(students) * 100
    ) |>
    ungroup() |>
    filter(applications_total_number <= 10)  # Limit for visualization

  # Analyze conversion by application count
  student_enrollment <- data |>
    select(bsnhash, schooljaar, applications_total_number, is_enrolled) |>
    group_by(bsnhash, schooljaar) |>
    summarise(
      applications_total_number = first(applications_total_number),
      student_enrolled = any(is_enrolled),
      .groups = "drop"
    )

  conversion_by_count <- student_enrollment |>
    group_by(schooljaar, applications_total_number) |>
    summarise(
      students = n(),
      converted_students = sum(student_enrolled),
      student_conversion_rate = mean(student_enrolled) * 100,
      .groups = "drop"
    ) |>
    filter(applications_total_number <= 10) |>
    mutate(
      application_conversion_rate = converted_students / (students * applications_total_number) * 100
    )

  return(list(
    conversion_by_pattern = conversion_by_pattern,
    application_count_distribution = application_count_distribution,
    conversion_by_count = conversion_by_count
  ))
}


#' Group Applications Data
#'
#' @description
#' Groups applications data at different levels and calculates aggregated metrics.
#' Based on the grouping logic from instroomprognose_prototype.qmd.
#'
#' @param data Applications dataset
#' @param grouping_level String: "full" (student,programme,institution),
#'                              "institution" (student,institution only),
#'                              "student" (student level only)
#' @return Grouped tibble with aggregated metrics
#'
#' @importFrom dplyr group_by summarise mutate ungroup add_tally across all_of
#'
#' @export
group_applications <- function(data, grouping_level = "full") {

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
  grouped_data <- data |>
    group_by(across(all_of(grouping_vars))) |>
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
    grouped_data <- grouped_data |>
      group_by(bsnhash, schooljaar) |>
      add_tally(name = "applications_total_number") |>
      ungroup()

    if (grouping_level == "full") {
      grouped_data <- grouped_data |>
        group_by(bsnhash, schooljaar, instellingserkenningscode) |>
        add_tally(name = "applications_total_number_within_institution") |>
        ungroup() |>
        group_by(bsnhash, schooljaar) |>
        mutate(applications_total_number_of_institutions = length(unique(instellingserkenningscode))) |>
        ungroup() |>
        mutate(
          applications_is_multiple = applications_total_number > 1,
          applications_is_multiple_within_institution = applications_total_number_within_institution > 1,
          applications_is_multiple_across_institutions = applications_total_number_of_institutions > 1
        )
    }
  }

  return(grouped_data)
}
