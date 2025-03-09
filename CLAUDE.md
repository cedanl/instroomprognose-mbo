# Claude Agent Guide for instroomprognose-mbo

## Commands
- **Initialize Environment**: `source("00_setup.R")`
- **Load Packages**: `source("utils/manage_packages.R")`
- **Run Analysis**: `quarto::quarto_render("instroomprognose_prototype.qmd")`
- **Development Tools**: See utils/dev_functions.R for development helpers

## Code Style
- Use 4-space indentation
- Use snake_case for function and variable names
- Document all functions with roxygen2 comments
- Group imports at top with @importFrom tags
- Use |> pipeline operator (not %>%)
- Place function parameters on separate lines for clarity
- Prefer the happy path principle with guard clauses above nested if's
- Try to avoid nested for loops as well
- Use tidyverse styleguide when in doubt

## Naming Conventions
- Function names should be verbs (get_*, save_*, load_*)
- Variable names should be descriptive and self-documenting
- Use consistent prefixes for related functions
- Have a preference for pacakges already mentioned in utils/manage_packages.R
- Secondly properply supported packages, firstly from posit (like tidyverse and tidymodels)

## Error Handling
- Use validation checks before operations
- Employ tryCatch blocks for file operations
- Format error and warnings with messages from cli-package
- Use rlang::abort instead of stop
- Handle NULL values defensively

## Quarto style
- All codeblocks should start with `#| label:` and then a descriptive and unique label
- Think of code blocks as a first iteration towards a stand-alone function
- Preparation and visualisation should often have different codeblocks
- Use Dutch language in between code blocks to explain the rationale, not the code

## Git Workflow
- Always pull before committing: `git pull`
- Use short, concise commit messages with the format:
  - `fix: short description` (for bug fixes)
  - `feat: short description` (for new features)
  - `docs: short description` (for documentation changes)
  - `chore: short description` (for maintenance tasks)
- Example: `git commit -m "fix: add user to group to solve permission issue"`
