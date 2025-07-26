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
library(geocodebr)
library(sf)
library(geobr)
library(tidylog)
library(readxl)

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
dir <- "raw/zipped_files"

if (!dir.exists(dir)) {
  dir.create(dir)
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
    destination <- file.path(dir, file_name)

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

# 1.2- Unzip files

# Set a new folder just for the unzipped files
unzip_dir <- "raw/unzipped_files/"
if (!dir.exists(unzip_dir)) dir.create(unzip_dir)

# Unzip all downloaded files
for (zip_file in list.files("raw", pattern = "\\.zip$", full.names = TRUE)) {
  tryCatch({
    unzip(zip_file, exdir = unzip_dir)
    message("Extraction successful: ", basename(zip_file))
  }, error = function(e) {
    message("Extraction FAIL: ", basename(zip_file), " (", e$message, ")")
  })
}

# 1.3 Load files from May and November of each year (there should be 20 files)

# We already have the years in an object, we just need the specific months



# Set directory containing files
dir_path <- "raw/unzipped_files"  # Replace with your actual path

# Get all .xls files matching the pattern
file_list <- list.files(
  path = dir_path,
  pattern = "^\\d{6}.+\\.xls$",  # Matches YYYYMM followed by text and .xls
  full.names = TRUE
)

library(purrr)

# Process each file
walk(file_list, ~ {
  # Extract components from filename
  selected_years <- 2015:2018
  selected_months <- c(5,11)
  file_name <- basename(.x)

  # Get YYYY, MM, and name using regex
  matches <- str_match(file_name, "^(\\d{4})(\\d{2})(.+?)\\.xls$")

  # Construct object name: name_YYYY_MM
  obj_name1 <- paste0(year, month_str, "AGENCIAS.zip")
  obj_name2 <- str_glue("ag_", )#  |>
    #str_remove_all(" ")#  |>   # Remove spaces if any
    #make.names()  # Ensure valid R object name

  # Safely read and assign
  df <- possibly(readxl::read_xls, otherwise = NULL)(.x)

  if (!is.null(df)) {
    assign(obj_name, df, envir = .GlobalEnv)
    message(glue::glue("Successfully loaded: {obj_name}"))
  } else {
    warning(glue::glue("Failed to load: {file_name}"))
  }
})



for (year in selected_years) {
  for (month in selected_months) {
    month_str <- sprintf("%02d", month) # Start one digit numbers with a zero

    file_name_to_open <- paste0(year, month_str, "AGENCIAS.xls")
    file_url_to_open <- paste0("raw/unzipped_files/", file_name_to_open)
    object_name <- paste0("ag_", year, month_str)

    # Function to open each file
    tryCatch({
      object_name <- readxl::read_xls(file_url_to_open, skip = 8)
    })
  }
}



Load the most recent month to clean
ag_2024_11 <- readxl::read_xlsx("raw/unzipped_files/202411AGENCIAS.xlsx", skip = 8) # this might not work for every file



ag_set20 <- data.table::as.data.table(sapply(ag_set20, as.character)) # transforma todas as colunas em character
