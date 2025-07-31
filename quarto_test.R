
# Load setup
source("utils/00_setup.R")

brin_code <- "30RR"
school_name <- "MBO Amersfoort"
output_filename_basic <- paste(school_name, brin_code, sep = "_")
output_filename_formatted <- str_replace_all(output_filename_basic, " ", "_")
output_filename_full <- paste0(output_filename_formatted, ".html")
comparison_mode <- "relative_to_average"
years <- c(2023, 2024)

quarto::quarto_render(
  input = "instelling_analysis.qmd",
  output_file = output_filename_full,
  execute_params = list(
    brin_code = brin_code,
    school_name = school_name,
    comparison_mode = comparison_mode,
    years = years
  )
)
