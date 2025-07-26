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
# 4- Analyze time series in a spatial data format

# FURTHER STEPS
# 5- Plot data on Mapbox
# 6- Interactive presentation using HTML, CSS and Javascript

# Load packages
library(tidyverse)
library(geocodebr)
library(sf)
library(tidylog)
library(readxl)
library(data.table)
library(fs)

# Don't show results as scientific notations
options(scipen = 999)


###########################
# 2- IMPORT AND CLEAN DATA
###########################

# We have our datasets, but they are either in XLS and XLSX format
# A manual inspection shows they are not in a tidy format
# There are several lines of header and information that we don't need
# We'll begin by importing only the files from June and December and:
# Select specific columns common to all datasets
# Add a column to identify the year and month
# Compile all these datasets into one
# Clean this one big raw dataset to prepare it for the analysis

### 2.1- Batch load datasets for June and December

# Set your directory path
dir_path <- "raw/unzipped_files"  # Replace with your actual path

# Define column names to keep (in some years the column names were different)
cols_to_keep <- c(
  "CNPJ", "SEQUENCIAL DO CNPJ", "DV DO CNPJ", "NOME INSTITUIÇÃO", "SEGMENTO", "CÓD COMPE AG", "NOME AGÊNCIA", "NOME DA AGÊNCIA",
  "ENDEREÇO", "NÚMERO", "COMPLEMENTO", "BAIRRO", "CEP", "MUNICíPIO", "MUNICÍPIO",
  "UF", "DATA INÍCIO"
)

# Import files from June (06) and December (12) months
walk(dir_ls(dir_path, regexp = "\\d{4}(06|12).*\\.xls[x]?$"), ~ {
  tryCatch({
    # Extract YYYYMM from filename
    period <- str_extract(basename(.x), "\\d{6}")

    # Determine reader function based on extension
    reader <- if (str_detect(.x, "\\.xlsx$")) read_excel else read_xls

    # Read and process data
    df <- reader(.x, skip = 8) |>
      select(any_of(cols_to_keep)) |>
      mutate(
        across(everything(), ~ as.character(.)),
        across(where(is.character), ~ str_squish(.))
      )

    # Create object name
    obj_name <- paste0("ag_", period)

    # Assign to global environment
    assign(obj_name, df, envir = .GlobalEnv)

    message("Successfully processed: ", basename(.x))

  }, error = function(e) {
    warning("Failed to process ", basename(.x), ": ", e$message)
  })
})


### 2.2- Rename all columns from each object so they all match

# Define current possible column names and their standardized versions
column_mapping <- list(
  "CNPJ" = "cnpj_agency",
  "SEQUENCIAL DO CNPJ" = "cnpj_seq",
  "DV DO CNPJ" = "cnpj_dv",
  "NOME INSTITUIÇÃO" = "bank",
  "SEGMENTO" = "segment",
  "CÓD COMPE AG" = "cod_agency_compensation",
  "NOME AGÊNCIA" = "agency_name",
  "NOME DA AGÊNCIA" = "agency_name",  # Alternative name for same column
  "ENDEREÇO" = "address",
  "NÚMERO" = "street_number",
  "COMPLEMENTO" = "complement",
  "BAIRRO" = "neighborhood",
  "CEP" = "cep",
  "MUNICíPIO" = "city",
  "MUNICÍPIO" = "city",  # Alternative name for same column
  "UF" = "state",
  "DATA INÍCIO" = "opening_date"
)

# Define the desired column order
new_col_order <- c('cnpj_agency', 'cnpj_seq', 'cnpj_dv', 'bank', 'segment', 'cod_agency_compensation', 'agency_name',
                   'address', 'street_number', 'complement', 'neighborhood',
                   'cep', 'city', 'state', 'opening_date')

# Get all objects matching the pattern
agency_objects <- ls(pattern = "^ag_\\d{4}(06|12)$")

# Process each object
walk(agency_objects, ~ {
  df <- get(.x, envir = .GlobalEnv)

  # 1. Rename columns (handling alternative names)
  df <- df %>%
    rename_with(~ {
      # Find matching standardized name for each column
      map_chr(.x, function(col) {
        found <- names(column_mapping)[map_lgl(column_mapping, ~ any(.x == col))]
        if (length(found) > 0) column_mapping[[found[1]]] else col
      })
    })

  # 2. Select and reorder columns (add period if missing)
  df <- df  |>
    select(any_of(new_col_order), everything())  |>
    mutate(period = if ("period" %in% names(.)) period else str_extract(.x, "\\d{6}"))

  # 3. Ensure all expected columns exist (fill NA if missing)
  #missing_cols <- setdiff(new_col_order, names(df))
  #if (length(missing_cols) > 0) {
  #  df[missing_cols] <- NA
 # }

  # 4. Apply final column order
 # df <- df  |>
   # select(all_of(new_col_order))

  # Save back to original object name
  assign(.x, df, envir = .GlobalEnv)

  message("Processed: ", .x)
})

# Verification message
  message("\nSuccessfully standardized ", length(agency_objects), " objects")


### 2.3- Create a single dataset for all months and years (June and December)

# Get all objects matching the pattern
agency_objects <- ls(pattern = "^ag_\\d{4}(06|12)$")

# Loop to bind all objects into a single table
agencies <- map_dfr(
  .x = mget(agency_objects, envir = .GlobalEnv),
  .f = ~ .x,
  .id = "source_object")

# CHECKPOINT FOR CODING ERRORS
# On a glimpse, we can see that a few lines (52 on a total of 373,003, so 0,014%) have weird addresses
# This means their values for columns like that city and state are incorrect
# In some cases, all values are NA
# Using filtering and the janitor::tabyl function, we can see there are:
# Two messy rows in every dataset
# 14 messy rows on the 201806 dataset
# 50 rows are actually just left over rows from the Excel file, and not part of the table
# And 2 rows are from the same bank agency treated with wrong inputs in columns
# Solution: we'll just remove these rows because they are statistically insignificant to our universe

glimpse(ag_201606)
glimpse(ag_201912)
glimpse(ag_202112)

check <- agencies |>
  filter(is.na(state) | state == "79117-010")

check_distinct <- check |>
  janitor::tabyl(source_object)

# !!! From this point on, our dataset will not have 52 rows that had value problems.
# We'll go ahead and start cleaning the names of columns.
raw_agencies <- agencies |>
  filter(!is.na(UF) & UF != "79117-010") |>
  mutate(city = case_when(
    is.na(`MUNICÍPIO`) ~ `MUNICíPIO`,
    T ~ `MUNICÍPIO`),
    agency_name = case_when(
      is.na(`NOME AGÊNCIA`) ~ `NOME DA AGÊNCIA`,
      T ~ `NOME AGÊNCIA`)) |>
  select("source_object",
         "CNPJ",
         "SEQUENCIAL DO CNPJ",
         "DV DO CNPJ",
         "NOME INSTITUIÇÃO",
         "SEGMENTO",
         "CÓD COMPE AG",
         "agency_name",
         "ENDEREÇO",
         "NÚMERO",
         "COMPLEMENTO",
         "BAIRRO",
         "CEP",
         "city",
         "UF",
         "DATA INÍCIO") |>
  rename('cnpj_agency' = "CNPJ",
         'cnpj_seq' = "SEQUENCIAL DO CNPJ",
         'cnpj_dv' = "DV DO CNPJ",
         'bank' = "NOME INSTITUIÇÃO",
         'segment' = "SEGMENTO",
         'cod_agency_compensation' = "CÓD COMPE AG",
         'address' = "ENDEREÇO",
         'street_number' = "NÚMERO",
         'complement' = "COMPLEMENTO",
         'neighborhood' = "BAIRRO",
         'cep' = "CEP",
         'state' = "UF",
         'opening_date' = "DATA INÍCIO")



## SAVING OUR WORK SO FAR
write_rds(raw_agencies, "raw/raw_agencies_june_december.rds")

### 2.5- Removing from our environment all the unnecessary objects
if (length(agency_objects) > 0) {
  rm(list = agency_objects, envir = .GlobalEnv)
  message("Removed ", length(agency_objects), " objects: ",
          paste(agency_objects, collapse = ", "))
} else {
  message("No matching objects found to remove")
}

rm(agencies)
rm(check)
rm(check_distinct)
rm(states)
rm(column_mapping)

### 2.4- Clean each variable

# If starting a new session from here
raw_agencies <- readRDS("raw/raw_agencies_june_december.rds")
glimpse(raw_agencies)

# 2.4.1- Clean data for CNPJ, address and city
df_agencies <- raw_agencies |>
  mutate(city = str_to_lower(city), # Preparing column for future join
         cep = str_remove(cep, "-"),
         cep = as.integer(cep),
         cep = case_when(
           cep == 110600002 ~ 11060002, # Fixing zip code problems for future geocoding (see more at the end)
           cep == 256201001 ~ 25620001, # Fixing zip code problems for future geocoding (see more at the end)
           T ~ cep),
         full_agency_name = str_c(bank, " (AGÊNCIA ", agency_name, ")"), # New column with good agency name
         cnpj_seq = data.table::fcase(stringr::str_length(cnpj_seq) == 1, paste0('000',cnpj_seq),
                                      stringr::str_length(cnpj_seq) == 2, paste0('00',cnpj_seq),
                                      stringr::str_length(cnpj_seq) == 3, paste0('0', cnpj_seq),
                                      stringr::str_length(cnpj_seq) == 4, cnpj_seq),
         cnpj_dv = data.table::fcase(stringr::str_length(cnpj_dv) == 1, paste0('0',cnpj_dv),
                                     stringr::str_length(cnpj_dv) == 2, cnpj_dv),
         cnpj = str_c(cnpj_agency, "\\", cnpj_seq, "-", cnpj_dv)) |> # New column with official CNPJ
  select('source_object',
         'cnpj',
         'bank',
         'segment',
         'cod_agency_compensation',
         'full_agency_name',
         'address',
         'street_number',
         'complement',
         'neighborhood',
         'cep',
         'city',
         'state',
         'opening_date') |>
  mutate(address_street = case_when(
      str_detect(address, "\\s\\d") ~ str_extract(address, "^.*?(?=\\s\\d)") |> str_remove(",$"),
      str_detect(address, ",\\s*\\d") ~ str_extract(address, "^.*?(?=,\\s*\\d)") |> str_remove(",$"),
      str_detect(address, ",S/N") ~ str_extract(address, "^.*?(?=,S/N)") |> str_remove(",$"),
      str_detect(address, ", S/N") ~ str_extract(address, "^.*?(?=, S/N)") |> str_remove(",$"),
      str_detect(address, " S/N") ~ str_extract(address, "^.*?(?= S/N)") |> str_remove(",$"),
      str_detect(address, ", S/ N") ~ str_extract(address, "^.*?(?=, S/ N)") |> str_remove(",$"),
      str_detect(address, ", Nº") ~ str_extract(address, "^.*?(?=, Nº)") |> str_remove(",$"),
      str_detect(address, ",Nº") ~ str_extract(address, "^.*?(?=,Nº)") |> str_remove(",$"),
      T ~ address),
    address_number = case_when(
      str_detect(address, "\\s\\d") ~ str_extract(address, "(?<=\\s)\\d+"),
      str_detect(address, ",\\d") ~ str_extract(address, ",\\s*(\\d+)", group = 1),
      str_detect(address, ",S/N") ~ str_extract(address, "(?<=,S/N)"),
      str_detect(address, ", S/N") ~ str_extract(address, "(?<=, S/N)"),
      str_detect(address, " S/N") ~ str_extract(address, "(?<= S/N)"),
      str_detect(address, ", S/ N") ~ str_extract(address, "(?<=, S/ N)"),
      str_detect(address, ", Nº") ~ str_extract(address, "(?<=, Nº)\\d+"),
      str_detect(address, ",Nº") ~ str_extract(address, "(?<=,Nº)\\d+"),
      str_detect(address, ", Nº ") ~ str_extract(address, "(?<=, Nº )\\d+"),
      str_detect(address, ",Nº ") ~ str_extract(address, "(?<=,Nº )\\d+"),
      !is.na(street_number) ~ street_number,
      T ~ NA_character_),
    address_street = str_trim(address_street),
    address_number = sub("\\D.*", "", address_number),
    address_street = str_remove(address_street, ",$"))

# CHECKPOINT FOR CODING ERRORS
# Checking to see if there are still many incomplete addresses that could prevent adequate geocoding
street_number_check <- df_agencies |>
  filter(is.na(address_number))

# Message: removed 365,583 rows (98%), 7,368 rows remaining
# Good news, only 2% of addresses are in this situation now

# Save this cleaned file
write_rds(df_agencies, "raw/df_agencies_june_december.rds")

# If starting a new session from here
df_agencies <- readRDS("raw/df_agencies_june_december.rds")
glimpse(df_agencies)

# 2.4.2- Join with municipal code dataset

# Use geobr package to get a list of cities and their respective code
# Transform dataset to prepare for join
city <- geobr::lookup_muni(name_muni = 'all') |>
  mutate(city = abjutils::rm_accent(stringr::str_to_lower(name_muni)),
         city_state = str_c(name_muni, "-", abbrev_state)) |>
  rename(state = abbrev_state) |>
  select(city,
         state,
         code_muni,
         name_muni,
         city_state)

# Join datasets

agencies_joined <- left_join(
  x = df_agencies,
  y = city,
  by = c("city", "state"))


# 2.4.3- Geocoding

# First we'll flatten the dataset to get only the unique address
agencies_to_geocode <- agencies_joined |>
  distinct(full_agency_name,
         cod_agency_compensation,
         code_muni,
         city_state,
         address_street,
         address_number,
         cep,
         neighborhood,
         name_muni,
         state)

# Great, we got rid of 79% duplicated values and have to geocode 79,175 unique rows
geocoding_fields <- geocodebr::definir_campos(
  logradouro = "address_street",
  numero = "address_number",
  cep = "cep",
  localidade = "neighborhood",
  municipio = "name_muni",
  estado = "state")

geocoded_addresses <- geocodebr::geocode(
  enderecos = agencies_to_geocode,
  campos_endereco = geocoding_fields,
  resultado_completo = FALSE,
  resolver_empates = TRUE,
  resultado_sf = FALSE,
  verboso = FALSE,
  cache = TRUE,
  n_cores = 1)
beep::beep()

# It worked, let's see how many rows don't have lat/lon:
na_check <- geocoded_addresses |>
  filter(is.na(lat))

# Only 517 (0,9% of the total). So we can just ignore these
geocoded_addresses_narm <- geocoded_addresses |>
  filter(!is.na(lat))

# And join back to our df_agencies

agencies_full <- left_join(
  x = agencies_joined,
  y = geocoded_addresses_narm,
  by = c("full_agency_name",
         "cod_agency_compensation",
         "code_muni",
         "city_state",
         "address_street",
         "address_number",
         "cep",
         "neighborhood",
         "name_muni",
         "state"))

# Okay, we're finished cleaning and are left with 372,951 agencies every June and December.
# We will save this files to use it on our data analysis

write_rds(agencies_full, "data/agencies_2016_2025_jun_dec.rds")

# For data visualization purposes, there is no need to keep all of these points and months
# It will also leave us with a file unnecesarily heavy to load
# So we'll filter out columns


agencies <- left_join(
  x = agencies_joined,
  y = geocoded_addresses_narm,
  by = c("full_agency_name",
         "cod_agency_compensation",
         "code_muni",
         "city_state",
         "address_street",
         "address_number",
         "cep",
         "neighborhood",
         "name_muni",
         "state")) |>
  filter(!is.na(lat)) |> # Remove the rows without geolocation
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  rename(geom = geometry,
         full_address = endereco_encontrado) |>
  mutate(year = str_sub(source_object, start = 4, end = 7),
         month = str_sub(source_object, start = -2),
         month_year = str_c(month, "-", year)) |>
  select(year, # Select fewer columns to get a file not too big
         month_year,
         cnpj,
         full_agency_name,
         bank,
         code_muni,
         city_state,
         full_address,
         geom)

# Okay, we're finished cleaning and are left with 369,337 points showing bank agencies over time.
# We will use this can try to get a smaller dataset, for datavisualization purposes.

# Now we can save this as a geoJSON file and explore a visualization on Mapbox.
# And we can analyze the data to gather more insights for our story.

st_write(agencies,
         dsn = "data/agencies_2016_2025_jun_dec.geojson",
         driver = "GeoJSON",
         append=FALSE)



round_geoloc <- geocoded_addresses_narm |>
  mutate(lat_round = round(lat,4),
         lon_round = round(lon,4)) |>
  select(full_agency_name,
         cod_agency_compensation,
         code_muni,
         city_state,
         address_street,
         address_number,
         cep,
         neighborhood,
         name_muni,
         state,
         endereco_encontrado,
         lat,
         lon,
         lat_round,
         lon_round)

lat_check <- round_geoloc |>
  distinct(lat,lon)

lat_round_check <- round_geoloc |>
  distinct(lat_round,lon_round)

agencies_round <- left_join(
  x = agencies_joined,
  y = round_geoloc,
  by = c("full_agency_name",
         "cod_agency_compensation",
         "code_muni",
         "city_state",
         "address_street",
         "address_number",
         "cep",
         "neighborhood",
         "name_muni",
         "state")) |>
  select(source_object,
         full_agency_name,
         city_state,
         endereco_encontrado,
         lat,
         lon) |>
  filter(!is.na(lat)) |> # Remove the rows without geolocation
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  rename(geom = geometry,
         full_address = endereco_encontrado) |>
  mutate(year = str_sub(source_object, start = 4, end = 7),
         month = str_sub(source_object, start = -2),
         month_year = str_c(month, "-", year)) |>
  select(year, # Select fewer columns to get a file not too big
         month,
         month_year,
         full_agency_name,
         city_state,
         full_address,
         geom)

agencies_jun <- agencies_round |>
  filter(month == "06") |>
  select(year,
         full_agency_name,
         city_state,
         full_address,
         geom) |>
  distinct(year, geom)

st_write(agencies_jun,
         dsn = "data/agencies_2016_2025_jun_v3.geojson",
         driver = "GeoJSON",
         append=FALSE)

agencies_jun_2016_2025 <- agencies_jun |>
  filter(year == 2016 | year == 2025)

st_write(agencies_jun_2016_2025,
         dsn = "data/agencies_2016_or_2025.geojson",
         driver = "GeoJSON",
         append=FALSE)

distinct_agencies <- agencies_jun_2016_2025 |>
  janitor::tabyl(year)

install.packages("geojsonio")

geojsonio::topojson_write(agencies_dec, file = "data/agencies_2016_2025_dec.topojson")

# CHECKPOINT FOR ZIP CODE ERROR
# On "raw_agencies" an error telling you the CEP column should have no more than 8 digits
# It shows you the index position of places with that problem: 11144 and 18125

# Let's see them:
agencies_to_geocode[11144,]
agencies_to_geocode[18125,]

# These are two agencies from different banks in different cities
cep_check <- agencies_joined |>
  filter((str_detect(full_agency_name, "BANCO BRADESCO") & code_muni == 3303906) |
           (str_detect(full_agency_name, "BANIF") & code_muni == 3548500))

# There are 118 results, not all of them with more than 8 digits on the "cep" column.
# Since it's an integer, we can filter the column for any "number" larger than 99.999.999 (which would have 9 or more digits)
cep_digits_check <- agencies_joined |>
  filter(cep > 99999999)

# There are 6 rows with problems, but only two values that repeat themselves:
cep_digits_check_distinct <- cep_digits_check |>
  distinct(cep)

# Checking the "cep_check" object will show us the correct value for these two zip codes
# So a simple case_when function will fix the 6 problematic rows:
agencies_joined <- agencies_joined |>
  mutate(cep = case_when(
    cep == 110600002 ~ 11060002,
    cep == 256201001 ~ 25620001,
    T ~ cep))





glimpse(df_agencies)
#### CONTINUAR DAQUI

agency_code_check <- agencies_cleaned |>
  janitor::tabyl(cod_agency_compensation)

number_check <- agencies_cleaned |>
  filter(is.na(number))

##
unique_agencies <- agencies_cleaned |>
  distinct(full_agency_name)

unique_banks <- agencies_cleaned |>
  distinct(bank)

df_check <- agencies_cleaned |>
  filter(state == "DF")

check <- raw_agencies |>
  distinct(state)






##### CONTINUAR DAQUI




?lookup_muni







