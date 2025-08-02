# Claude Agent Guide for instroomprognose-mbo-test

## Project Overview
Test version of the instroomprognose-mbo R package for MBO enrollment forecasting analysis. This package processes synthetic enrollment data and provides forecasting capabilities for educational institutions.

## Key Source Files
- `R/data_pipeline.R`: Core data processing functions (ingest_*, prepare_*, transform_*, analyze_*)
- `R/data_quality.R`: Validation and quality assessment functions  
- `utils/manage_packages.R`: Package loading and dependency management
- `instroomprognose_prototype.qmd`: Main analysis workflow
- `data_kwaliteit.qmd`: Data quality assessment report
- `instelling_analysis.qmd`: Institution-specific analysis template
- `utils/00_setup.R`: Environment initialization and configuration
- `config.yml`: Environment-specific settings

## Commands
- **Initialize Environment**: `source("utils/00_setup.R")`
- **Load Packages**: `source("utils/manage_packages.R")`
- **Load Development Functions**: `devtools::load_all()`
- **Generate Documentation**: `devtools::document()`
- **Run Analysis**: `quarto::quarto_render("instroomprognose_prototype.qmd")`
- **Data Quality Check**: `quarto::quarto_render("data_kwaliteit.qmd")`
- **Development Tools**: See `utils/dev_functions.R` for development helpers

## Code Style
- Use 4-space indentation
- Use snake_case for function and variable names
- Document all functions with roxygen2 comments using markdown
- Group imports at top with `@importFrom` tags
- Use `|>` pipeline operator (not `%>%`)
- Place function parameters on separate lines for clarity
- Prefer the happy path principle with guard clauses above nested if's
- Try to avoid nested for loops as well
- Use tidyverse style guide when in doubt
- Never use library calls, check if package is loaded in `utils/manage_packages.R`

## Naming Conventions
- Function names should be verbs (get_*, save_*, load_*, process_*, transform_*)
- Variable names should be descriptive and self-documenting
- Use consistent prefixes for related functions
- Have a preference for packages already mentioned in `utils/manage_packages.R`
- Secondly properly supported packages, firstly from Posit (like tidyverse and tidymodels)

## Error Handling
- Use validation checks before operations
- Employ `tryCatch` blocks for file operations and external data reads
- Format error and warnings with messages from cli package
- Use `rlang::abort()` instead of `stop()`
- Handle NULL values and missing data defensively
- Validate data integrity before proceeding with analysis

## Package Development
- Follow R package structure with proper DESCRIPTION and NAMESPACE
- Use `devtools::load_all()` for testing during development
- Document functions with roxygen2 (`#'`) including examples
- Export only necessary functions
- Use `renv` for dependency management
- Test functions with synthetic data before real data

## Critical Dependencies
- `config`: Environment-specific settings via config.yml
- `renv`: Reproducible package management
- `devtools`: Package development workflow
- `quarto`: Report generation and documentation
- Key data packages: dplyr, readr, lubridate, ggplot2, cli, rlang
- Visualization: ggplot2, plotly (for interactive plots)
- Statistical modeling: tidymodels ecosystem

## Data Configuration
- Use `config.yml` for environment-specific settings
- Default configuration uses synthetic data for testing
- CAMBO configuration for production data (when available)
- Always validate data paths and file existence before processing

## Quarto Style
- All code blocks should start with `#| label:` and then a descriptive and unique label
- Think of code blocks as a first iteration towards a stand-alone function
- Preparation and visualization should often have different code blocks
- Use Dutch language in between code blocks to explain the rationale, not the code
- Include execution options like `#| warning: false` and `#| message: false` where appropriate

## Data Processing Patterns
- Follow the data pipeline: ingest → prepare → transform → combine → analyze
- Use defensive programming for data validation
- Handle missing values explicitly
- Document data assumptions and transformations
- Create reusable functions for common data operations

## Git Workflow
- Always pull before committing: `git pull`
- Use short, concise commit messages with the format:
  - `fix: short description` (for bug fixes)
  - `feat: short description` (for new features)
  - `docs: short description` (for documentation changes)
  - `chore: short description` (for maintenance tasks)
  - `test: short description` (for test-related changes)
- Example: `git commit -m "fix: handle missing enrollment data in forecast model"`

## Project-Specific Gotchas
- Always use synthetic data for testing; real CAMBO data requires config switch
- Run `devtools::load_all()` after function changes before Quarto rendering
- Data validation functions expect specific column names - check schema first
- Quarto blocks need unique `#| label:` tags to avoid rendering conflicts
- Institution analysis requires enrollment data from previous 3 years minimum
- Status transitions analysis can be memory-intensive with large datasets
- Academic year calculations assume October start (month 10) - configurable via parameters
- Multiple application analysis requires data grouped with `group_applications(grouping_level = 'full')`

## Testing Guidelines
- Use synthetic data for development and testing
- Validate results against known patterns
- Test edge cases (missing data, empty datasets, invalid dates)
- Document expected behavior and assumptions
- Use `testthat` framework if formal tests are needed

## Claude Read-Up Strategy
<!-- ALWAYS READ: Start by reading all files in R/ directory to understand current codebase. Also read notebooks/. The notebooks often have the ground truth I tried to move to R/ to work on scale. -->