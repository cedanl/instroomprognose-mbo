# Step 1: Install Homebrew (if not present)
install_homebrew()

# TODO: splitting of packages in cask (only quartz) and regular
# Step 2: Define required brew packages (ggplot2, graphics)
brew_deps <- c(
    # downloads / ssh / network stuff
    "curl",
    "openssl@3",
    "libgit2",
    "libxml2",
    # markdown, knitr
    "pandoc",
    # compile packages
    "pkg-config",
    # unicode
    # "icu4c",
    # plots
    "xquartz", # cask, must be via terminal
    "cairo",
    "pango",
    "libpng",
    "jpeg",
    "freetype"
)

# Step 3: Install missing dependencies
install_brew_packages(brew_deps)

clear_script_objects()

