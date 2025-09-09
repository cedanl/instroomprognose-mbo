# Global variables to suppress R CMD check NOTEs
# These are column names used in dplyr operations

utils::globalVariables(c(
  # Existing variables
  "combo_id",
  "new_combo_id", 
  "group_id",
  "id_col",
  
  # Column names from data
  "begindatum",
  "begindatum_parsed", 
  "bsnhash",
  "createdat",
  "instellingserkenningscode",
  "opleidingcode",
  "postcodecijfers",
  "school",
  "schooljaar", 
  "schooljaar_afgeleid",
  "status",
  "status_numeric",
  "status_proper_case",
  "statussource",
  "leertrajectmbo",
  "Opleidingsnaam",
  
  # Date/time variables
  "academic_month",
  "academic_week",
  "start_week",
  
  # Application variables
  "applications_is_multiple",
  "applications_is_multiple_across_institutions", 
  "applications_is_multiple_within_institution",
  "applications_total_number",
  "applications_total_number_of_institutions",
  "applications_total_number_within_institution",
  "is_enrolled",
  
  # Geographic variables
  "postcode_4",
  "postcode_percentage",
  "institution_students",
  "total_institution_students",
  
  # Analysis variables
  "aanmeldingen_per_student",
  "applications", 
  "converted_students",
  "is_status_upgrade",
  "ongeldige_postcodes",
  "prev_status",
  "student_enrolled",
  "student_max_status_numeric", 
  "students",
  "total",
  "totaal_aanmeldingen",
  "valid_postcode",
  "veranderd"
))
