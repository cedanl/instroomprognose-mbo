# Step 1: Check if R tools is installed

rtools_present <- pkgbuild::find_rtools()

# Step 2: If not installed, download and install Rtools
if (rtools_present == FALSE) {
    install_rtools()
} else {
    message("✅ Rtools is already installed.")
}


