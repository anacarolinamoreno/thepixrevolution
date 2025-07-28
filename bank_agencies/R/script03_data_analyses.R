# Lede Project: Visualizating bank agencies in Brazil over time
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
library(geobr)
library(sf)
library(tidylog)
library(readxl)
library(data.table)
library(fs)

# Don't show results as scientific notations
options(scipen = 999)


###########################
# 4- ANALYZE DATA
###########################

# Load dataset from script_02:
agencies_raw <- readRDS("data/agencies_2016_2025_jun_dec.rds")

# Get information on the control type of each agency, to find the PUBLIC banks:
# Source: Brazil Central Bank's IF.Data: https://www3.bcb.gov.br/ifdata/#
control_type <- read.csv2("raw/ifdata_control_type.csv") |>
  select(`Instituição`, TC) |>
  rename(bank = `Instituição`) |>
  filter(!is.na(TC)) |>
  mutate(control_type = case_when(
    TC == 1 ~ "public_bank",
    TC == 2 ~ "private",
    TC == 3 ~ "private",
    T ~ "private"))

# Join the two datasets:
agencies_df <- left_join(
  x = agencies_raw,
  y = control_type,
  by = "bank") |>
  mutate(control_type = case_when(
    is.na(control_type) ~ "private",
    T ~ control_type))

# Create period columns and drop private columns:
agencies_df <- agencies_df |>
  mutate(year = str_sub(source_object, start = 4, end = 7),
         month = str_sub(source_object, start = -2),
         year_month = str_c(year, "-", month)) |>
  rename(full_address = endereco_encontrado) |>
  select(year,
         month,
         year_month,
         cnpj,
         full_agency_name,
         bank,
         segment,
         control_type,
         cod_agency_compensation,
         opening_date,
         code_muni,
         name_muni,
         state,
         city_state,
         full_address,
         lat,
         lon)


### QUESTION 1: How many bank agencies closed every year between June 2016 and June 2025?
agencies_jun <- agencies_df |>
  filter(month == "06") |>
  group_by(year) |>
  summarise(total = n())

agencies_jun_wide <- agencies_jun |>
  mutate(control_type = "all") |>
  pivot_wider(values_from = total, names_from = year)

agencies_control_type <- agencies_df |>
  filter(month == "06") |>
  group_by(year, control_type) |>
  summarise(total = n())

agencies_control_type_wide <- agencies_control_type |>
  pivot_wider(values_from = total, names_from = year)

agencies_over_time <- bind_rows(agencies_control_type_wide, agencies_jun_wide) |>
  mutate(pct_var = (( `2025` - `2016` ) * 100 ) / `2016`)

### ANSWER:
# The total number fell 28.8%, from 22,701 to 16,165.
# The drop was less intense among public banks (-15.9%) than national or foreign private banks (-39.3%).

### QUESTION 2: Has this drop changed the proportion of public/private banks significantly?

public_private <- agencies_control_type |>
  filter(year == "2016" | year == "2025") |>
  pivot_wider(values_from = total, names_from = control_type) |>
  mutate(total = private + public_bank,
         pct_public = round((( public_bank * 100 ) / total),1),
         pct_private = round((( private * 100 ) / total),1))

### ANSWER:
# Yes. In June 2016, agencies from public banks represented 45.1% of the total.
# In June 2025, they became the majority of agencies: 53.2%.

### QUESTION 3: How many municipalities now have zero bank agencies?

# Count the unique number of towns from each year
banks_in_muni <- agencies_df |>
  distinct(year, code_muni) |>
  group_by(year) |>
  summarise(towns = n())

# Create a dataframe with the total number of towns in each year (it varies)
years <- 2016:2025
total_muni <- c(5568,5568,5568,5568,5568,5568,5568,5568,5569,5569)
muni_data <- data.frame(
  year = as.character(years),
  total_muni = total_muni)

# Join both datasets and calculate the proportion
muni_with_banks <- left_join(x = banks_in_muni,
                             y = muni_data,
                             by = "year") |>
  mutate(pct_with_bank = round((( towns * 100 ) / total_muni),1))

### ANSWER:
# In 2025 Brazil had 2,957 municipalities with at least one bank agency.
# That's a 17.2% drop from 2016, when it had 3,571 towns with a bank agency.
# Back in 2016, 64.1% of towns had a bank agency. Now, it's just over half (53.1%).

##################
#DATAVIZ PREP
##################

# I want to use this insight (fewer municipalities with brick-and-mortar banks) in a dataviz.
# I chose an animated SVG using d3 on my HTML
# So I need a dataset with 100 rows and 3 columns:
# Column 1 (pct): listing numbers from 1 to 100 as the percentiles
# Column 2 (agencies_2016): 64 rows showing the value 1 (because they have an agency) and 36 with value 0
# Column 3 (agencies_2025): 64 rows showing the value 1 (because they have an agency) and 36 with value 0

# Create the three vectors
pct <- 1:100  # Percentiles from 1 to 100
agencies_2016 <- c(rep(0, 36), rep(1, 64))  # 64% of towns with agencies
agencies_2025 <- c(rep(0, 47), rep(2, 53))  # 53% of towns with agencies

# Combine into a dataframe
d3_df <- data.frame(pct, agencies_2016, agencies_2025)

# View the first few rows
head(d3_df)

write.csv(d3_df, "data/munis_with_agencies.csv", row.names = F)
write.csv(d3_df, "~/Documents/Lede_2025/thepixrevolution/page_data/munis_with_agencies.csv", row.names = F)

### QUESTION: How many people live in places without a bank agency?

# Let's use the "sidrar" package to fetch the population estimates for each town and year:
install.packages("sidrar")
pop <- sidrar::get_sidra(api = "/t/6579/n6/all/v/all/p/last%207")

# The API endpoint was given to us by the Sidra website after searching for the correct table and parameters:
# https://sidra.ibge.gov.br/tabela/6579

glimpse(pop)

pop_cleaned <- pop |>
  select(`Município (Código)`,
         Ano,
         Valor) |>
  rename(code_muni = `Município (Código)`,
         year = Ano,
         pop = Valor)

# There are no estimates for years 2022, 2023 and 2025, that means we need to use the previous one
# We'll create these rows repeating the same value for the year before those for each location

pop_2022 <- pop_cleaned |>
  filter(year == "2021") |>
  mutate(year = "2022")

pop_2023 <- pop_cleaned |>
  filter(year == "2021") |>
  mutate(year = "2023")

pop_2025 <- pop_cleaned |>
  filter(year == "2024") |>
  mutate(year = "2025")

# Now we add these rows to our previous dataset
pop_complete <- bind_rows(pop_cleaned,
                          pop_2022,
                          pop_2023,
                          pop_2025) |>
  arrange(code_muni, year)

# Filter our dataset to get the municipalities with bank agencies on each year
muni_with_banks_per_year <- agencies_df |>
  distinct(year, code_muni) |>
  mutate(code_muni = as.character(code_muni))

# Join both datasets
muni_with_banks_and_pop <- left_join(x = muni_with_banks_per_year,
                                     y = pop_complete,
                                     by = c("year", "code_muni")) |>
  mutate(pop = as.integer(pop))

# Sum the population in places with agencies on each year
muni_banks_total_pop <- muni_with_banks_and_pop |>
  group_by(year) |>
  summarise(pop_in_muni_with_banks = sum(pop, na.rm = TRUE))

# Sum the total population in Brazil on each year
pop_brazil <- pop_complete |>
  group_by(year) |>
  summarise(total_pop = sum(pop, na.rm = TRUE))

# Join datasets and calculate the proportion
pop_and_banks <- left_join(x = muni_banks_total_pop,
                           y = pop_brazil,
                           by = "year") |>
  mutate(pct_pop_in_muni_with_banks = round((( pop_in_muni_with_banks * 100 ) / total_pop),1))

### ANSWER:
# According to the official population estimates, in 2016, 93% of the Brazilian population lived in a municipality with at least one bank agency.
# That proportion fell to 90,4% in 2025.
