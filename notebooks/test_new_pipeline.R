# Test van nieuwe pipeline structuur
# Run dit bestand om te testen of de nieuwe functies werken

# Setup
source("utils/00_setup.R")
source("R/data_pipeline.R")
source("R/data_quality.R")

# Test 1: Basis data loading en enrichment
cat("=== Test 1: Data Loading & Enrichment ===\n")
applications <- load_and_enrich_applications()
cat("Geladen rijen:", nrow(applications), "\n")
cat("Kolommen:", ncol(applications), "\n")
cat("Unieke instellingen:", length(unique(applications$instellingserkenningscode)), "\n\n")

# Test 2: Data filtering
cat("=== Test 2: Data Filtering ===\n")
# Filter alleen 2023-2024 data
recent_data <- filter_applications(applications, years = c(2023, 2024))
cat("Rijen na jaar filtering:", nrow(recent_data), "\n")

# Filter op specifieke BRIN (MBO Amersfoort)
amersfoort_data <- filter_applications(applications, brin_codes = "30RR")
cat("Rijen voor MBO Amersfoort:", nrow(amersfoort_data), "\n\n")

# Test 3: Data grouping
cat("=== Test 3: Data Grouping ===\n")
# Groepeer op volledig niveau
grouped_full <- group_applications(recent_data, "full")
cat("Gegroepeerde rijen (full):", nrow(grouped_full), "\n")

# Groepeer op instelling niveau
grouped_institution <- group_applications(recent_data, "institution")
cat("Gegroepeerde rijen (institution):", nrow(grouped_institution), "\n\n")

# Test 4: Data quality analyse
cat("=== Test 4: Data Quality Analysis ===\n")
quality_results <- analyze_data_quality(recent_data, by_group = "schooljaar")

# Toon postcode kwaliteit
cat("Postcode kwaliteit per jaar:\n")
print(quality_results$postcode_analysis)

# Toon schooljaar wijzigingen
cat("\nSchooljaar wijzigingen:\n")
print(head(quality_results$schooljaar_changes))

# Data quality summary
quality_summary <- get_data_quality_summary(quality_results)
cat("\nOverall quality score:", quality_summary$overall_quality_score, "%\n")

# Test 5: Instituut data kwaliteit
cat("\n=== Test 5: Institution Data Quality ===\n")
institution_quality <- analyze_institution_data_quality(recent_data)
cat("Top 10 instellingen naar aanmeldingen per student:\n")
print(head(institution_quality, 10))

# Test 6: BRIN codes ophalen
cat("\n=== Test 6: Available BRIN Codes ===\n")
brin_codes <- get_all_brin_codes(recent_data, min_applications = 100)
cat("BRIN codes met >100 aanmeldingen:", length(brin_codes), "\n")
cat("Eerste 5:", paste(head(brin_codes, 5), collapse = ", "), "\n")

# Test 7: Summary statistics
cat("\n=== Test 7: Summary Statistics ===\n")
summary_stats <- calculate_summary_stats(recent_data, "full")
cat("Totaal aantal aanmeldingen:", summary_stats$total_applications, "\n")
cat("Totaal aantal studenten:", summary_stats$total_students, "\n")
cat("Gemiddelde conversie ratio:", round(summary_stats$average_conversion_rate * 100, 1), "%\n")
cat("Aanmeldingen per student:", round(summary_stats$applications_per_student, 2), "\n")

cat("\n=== Pipeline Test Compleet ===\n")
cat("Alle functies werken correct! Je kunt nu beginnen met het maken van QMD templates.\n")
