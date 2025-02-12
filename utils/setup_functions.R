init_project_files <- function() {
    
    # Get current directory name as default new name
    current_dir <- basename(getwd())
    # Find the .Rproj file
    rproj_files <- list.files(pattern = "\\.Rproj$")
    
    description_file <- "DESCRIPTION"
    
    if (length(rproj_files) != 1) {
        warning("Expected exactly one .Rproj file, found ", length(rproj_files))
        return()
    }
    
    current_name <- rproj_files[1]
    
    if (current_name != "rP3Template.Rproj") {
        # Name is already updated
        return()
    }
    
    # Ask user if they want to rename
    new_name <- readline(prompt = sprintf("Enter new project name or press Enter to use '%s': ", current_dir))
    
    # Use directory name if user just pressed Enter
    if (new_name == "") {
        new_name <- current_dir
    }
    
    # Add .Rproj extension if not provided
    if (!grepl("\\.Rproj$", new_name)) {
        new_name <- paste0(new_name, ".Rproj")
    }
    
    # Rename the file
    if (current_name != new_name) {
        file.rename(current_name, new_name)
        message("Renamed project file to: ", new_name)
        
        # Update ProjectId
        proj_content <- readLines(new_name)
        proj_content[2] <- paste0("ProjectId: ", generate_project_id())
        writeLines(proj_content, new_name)
        
    }
    
    
    
    if (!file.exists(description_file)) {
        warning("no file DESCRIPTION found")
        return()
    }
    desciption_content <- readLines(description_file)
    if (desciption_content[1] == "Package: rP3Template") {
        # Remove .Rproj extension if present
        new_name <- sub("\\.Rproj$", "", new_name)
        
        # Create valid package name
        suggested_name <- make_valid_package_name(new_name)
        
        if (suggested_name != new_name) {
            message("Package name must follow R conventions:")
            message("- Can only contain letters, numbers, and periods")
            message("- Must start with a letter")
            message("- Cannot end with a period")
            package_name <- readline(
                sprintf("Enter package name or press Enter to use '%s': ", suggested_name)
            )
            if (package_name == "") {
                package_name <- suggested_name
            }
        } else {
            package_name <- suggested_name
        }
        
        desciption_content[1] <- paste0("Package: ", package_name)
        writeLines(desciption_content, description_file)
        message("Updated package name in DESCRIPTION to: ", package_name)
    }
}

# Generate a simple project ID without dependencies
generate_project_id <- function() {
    random_hex <- function(n) {
        paste0(sample(c(0:9, letters[1:6]), n, replace = TRUE), collapse = "")
    }
    paste(
        random_hex(8),
        random_hex(4),
        random_hex(4),
        random_hex(4),
        random_hex(12),
        sep = "-"
    )
}

# Make valid R package name
make_valid_package_name <- function(name) {
    
    # Remove non-alphanumeric characters from start until we find a letter
    name <- sub("^[^a-zA-Z]+", "", name)
    
    # Convert hyphens and underscores to camelCase
    while (grepl("[-_]", name)) {
        name <- sub("[-_]([a-zA-Z])", "\\U\\1", name, perl = TRUE)
    }
    
    # Remove any remaining invalid characters
    name <- gsub("[^a-zA-Z0-9.]", "", name)
    
    # Remove trailing periods
    name <- sub("\\.*$", "", name)
    
    return(name)
}

