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
filter_applications_on_brin <- function(data,
                                brin_codes = NULL) {

  filtered_data <- data

  # Filter on BRIN codes if specified
  if (!is.null(brin_codes)) {
    filtered_data <- filtered_data %>%
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


#' Calculate postcode distribution for a specific institution
#'
#' @param data Applications dataset with enrolled students
#' @param institution_name Name of the institution to analyze
#' @param years Vector of years to include (default: c(2023, 2024))
#' @param min_institution_students Minimum institution students per postcode to include (default: 2)
#' @return Data frame with postcode distribution analysis
#'
#' @importFrom dplyr filter mutate group_by summarise left_join
#' @importFrom tidyr replace_na
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

    # Get total institution students per year
    institution_totals <- institution_by_postcode %>%
        group_by(schooljaar) %>%
        summarise(total_institution_students = sum(institution_students), .groups = "drop")

    # Calculate postcode distribution within institution
    postcode_distribution_analysis <- institution_by_postcode %>%
        left_join(institution_totals, by = "schooljaar") %>%
        mutate(
            postcode_percentage = (institution_students / total_institution_students) * 100
        ) %>%
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
    postcode_year <- postcode_data %>%
        filter(schooljaar == target_year) %>%
        mutate(postcode = as.numeric(postcode_4)) %>%
        select(-postcode_4)

    # Join with geographic data
    geo_distribution <- postal_code_geo_wgs84 %>%
        left_join(postcode_year, by = "postcode") %>%
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
    map <- leaflet(geo_distribution) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
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
        ) %>%
        addLegend(
            pal = color_palette,
            values = ~postcode_percentage,
            title = "Percentage (%)",
            position = "bottomright"
        ) %>%
        setView(lng = 5.2, lat = 52.2, zoom = 8)

    return(map)
}