

df <- tibble(id = 1:3, x = c("m-123", "f-455", "f-123"))
# There are three basic ways to split up a string into pieces:
# 1. with a delimiter

df_sep <- df  |>
  separate_wider_delim(x, delim = "-", names = c("gender", "unit"))

library(stringi)
stri_split_fixed('a_b_c_d', '_', n = 2)
stri_split_fixed('a_b_c__d', '_')
stri_split_fixed('a_b_c__d', '_', omit_empty=TRUE)
stri_split_fixed('a_b_c__d', '_', n=2, tokens_only=FALSE) # 'a' & remainder
stri_split_fixed('a_b_c__d', '_', n=2, tokens_only=TRUE) # 'a' & 'b' only
stri_split_fixed('a_b_c__d', '_', n=4, omit_empty=TRUE, tokens_only=TRUE)
stri_split_fixed('a_b_c__d', '_', n=4, omit_empty=FALSE, tokens_only=TRUE)
stri_split_fixed('a_b_c__d', '_', omit_empty=NA)
stri_split_fixed(c('ab_c', 'd_ef_g', 'h', ''), '_', n=1, tokens_only=TRUE, omit_empty=TRUE)
stri_split_fixed(c('ab_c', 'd_ef_g', 'h', ''), '_', n=2, tokens_only=TRUE, omit_empty=TRUE)
stri_split_fixed(c('ab_c', 'd_ef_g', 'h', ''), '_', n=3, tokens_only=TRUE, omit_empty=TRUE)



library(stringr)
library(dplyr)

# Sample data including both space and comma separators
address_data <- tibble(
  full_address = c(
    "AV. VER. JOSE GOMES DUDA 1338",       # Space separator
    "RUA DAS FLORES, 123",                 # Comma separator
    "PRAÇA DA SÉ 100",                     # Space separator
    "AVENIDA PAULISTA, 900",               # Comma separator
    "RODOVIA DOS IMIGRANTES 1000, BLOCK A",# Space first, then comma
    "ALAMEDA SANTOS, 2000, SUITE 101",     # Comma first, then space
    "RUA SEM NUMERO",                      # No number
    "AV. DOUBLE 123, 456"                  # Multiple numbers (should take first)
  )
)

# Split addresses at first occurrence of either space+digit or comma+digit
split_addresses <- address_data  |>
  mutate(
    # Extract street name (everything before first space/comma+digit)
    street_name = case_when(
      str_detect(full_address, "\\s\\d") ~ str_extract(full_address, "^.*?(?=\\s\\d)"),
      str_detect(full_address, ",\\s*\\d") ~ str_extract(full_address, "^.*?(?=,\\s*\\d)"),
      TRUE ~ full_address
    ),

    # Extract street number (first digit sequence after space/comma)
    street_number = case_when(
      str_detect(full_address, "\\s\\d") ~ str_extract(full_address, "(?<=\\s)\\d+"),
      str_detect(full_address, ",\\s*\\d") ~ str_extract(full_address, "(?<=,\\s*)\\d+"),
      TRUE ~ NA_character_
    ),

    # Clean up results
    street_name = str_trim(street_name),
    street_number = str_trim(street_number)
  )  |>
  select(-full_address)

# View results
split_addresses


# Sample data
my_strings <- c("123abc123DEF", "456_ghi", "789-jkl", "000XYZ")

# Use sub() to remove everything after the first non-digit
# sub() replaces only the first match
cleaned_strings <- sub("\\D.*", "", my_strings)

# Print the result
print(cleaned_strings)
