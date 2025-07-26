# Lede Project: Pix versus bank agencies
# Dataset: Data base of bank physical agencies address
# Source: Brazil Central Bank (BCB)
# Deprecated link (incomplete time series - missing some months and discontinued on November/2024): https://www.bcb.gov.br/acessoinformacao/legado?url=https:%2F%2Fwww.bcb.gov.br%2Ffis%2Finfo%2Fagencias.asp
# Official link as of July/2025: https://www.bcb.gov.br/estabilidadefinanceira/agenciasconsorcio
# Credits: this project is an adaptation of code by Lucas Warwar: https://github.com/lucaswarwar/agencias)

# STEPS IN R
# 1- Collect datasets to get a time series
# 2- Clean XLS files
# 3- Geocode locations
# 4- Get time series in a spatial data format

# FURTHER STEPS
# 5- Plot data on Mapbox
# 6- Interactive presentation using HTML, CSS and Javascript

# Load packages
library(tidyverse)
library(tidylog)

###################
# 1- COLLECT DATA
###################

# There is one dataset per month
# The page has a dropdown menu to search for each month/dataset
# An inspection of the page returned a direct link to download each ZIP file
# It's a pretty standard name, changing only the YYYYMM information
# So we can write a loop to download our time series

# 1.1- Data download
# Set destination directory
zip_dir <- "raw/zipped_files"

if (!dir.exists(zip_dir)) {
  dir.create(zip_dir)
}

# Set the years and months
years <- 2016:2025
months <- 1:12

# Set the first part of the URL
initial_path <- "https://www.bcb.gov.br/content/estabilidadefinanceira/agenciasconsorcio/agencias/"

# Loop to download each file by constructing each URL
for (year in years) {
  for (month in months) {
    month_str <- sprintf("%02d", month) # Start one digit numbers with a zero

    file_name <- paste0(year, month_str, "AGENCIAS.zip")
    file_url <- paste0(initial_path, file_name)
    destination <- file.path(zip_dir, file_name)

    # Add a warning if the file already exists
    if (file.exists(destination)) {
      message("Already exists: ", file_name)
      next
    }

    # Function to download each file
    tryCatch({
      download.file(
        url = file_url,
        destfile = destination,
        mode = "wb",  # Important for binary files
        method = "auto",  # Automatic method selection
        quiet = FALSE  # Show the download progress
      )
      message("Download success: ", file_name)
    }, error = function(e) {
      message("Download FAIL: ", file_name, " (", e$message, ")")
    })
  }
}

# Observation: the loop showed us the failed downloads:
# 202507 to 202512 (understandable considering the script ran on July/20225)
# 202108 and 202111 (since there were only two, I downloaded them manually, they had a different name)

# So now we have files for each month in the past 10 and a half years
# The problem is, they're all (unnecessarily) zipped

# 1.2- Unzip files

# Set a new folder just for the unzipped files
unzip_dir <- "raw/unzipped_files/"

if (!dir.exists(unzip_dir)) {
  dir.create(unzip_dir)
}


# Unzip all downloaded files
for (zip_file in list.files("raw/zipped_files", pattern = "\\.zip$", full.names = TRUE)) {
  tryCatch({
    unzip(zip_file, exdir = unzip_dir)
    message("Extraction successful: ", basename(zip_file))
  }, error = function(e) {
    message("Extraction FAIL: ", basename(zip_file), " (", e$message, ")")
  })
}

