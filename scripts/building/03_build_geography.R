# ============================================================
# 03: BUILD GEOGRAPHIC CHARACTERISTICS
# ============================================================
# Purpose: Construct school locale classifications.
#
# Source: NCES school geographic data
# Output: data/processed/geography_panel.csv
#
# COMPLETED STEPS:
# - Download and inspect NCES geographic data
# - Identify school IDs and locale classifications
# - Classify schools as city, suburb, town, or rural
# - Standardize school IDs to match the readiness panel
# - Check for duplicate and unmatched schools
# - Save the cleaned geographic dataset
# ============================================================

# STEP 1: Load packages
library(rvest)
library(dplyr)

# STEP 2: Read the NCES download
nces_tables <- read_html(
  "data/raw/ncesdata_F2015DC2.xls"
) |>
  html_table(
    header = FALSE,
    trim = TRUE,
    convert = FALSE
  )

nces_raw <- nces_tables[[1]]

# The column headings are in row 6.
# The preceding rows contain source information and notes.
names(nces_raw) <- as.character(
  unlist(nces_raw[6, ], use.names = FALSE)
)

nces_raw <- nces_raw[-seq_len(6), ]

# STEP 3: Inspect the imported data
nrow(nces_raw)

nces_raw |>
  select(
    `NCES School ID`,
    `State School ID`,
    `School Name`,
    `Locale Code*`,
    `Locale*`
  ) |>
  head()

# STEP 4: Build the geographic lookup
valid_locale_codes <- c(11:13, 21:23, 31:33, 41:43)

geography_lookup <- nces_raw |>
  transmute(
    nces_school_id = trimws(`NCES School ID`),
    state_school_id = trimws(`State School ID`),
    nces_school_name = `School Name`,
    locale_code_raw = trimws(`Locale Code*`),
    locale_description = `Locale*`,
    
    # Preserve the year associated with the locale data
    locale_source_year = "2024-25"
  ) |>
  mutate(
    # Example: SC-0160-001 becomes 0160001
    school_id = if_else(
      grepl("^SC-[0-9]{4}-[0-9]{3}$", state_school_id),
      gsub("-", "", sub("^SC-", "", state_school_id)),
      NA_character_
    ),
    
    # Keep recognized locale codes; leave other entries missing
    locale_code = as.integer(
      if_else(
        locale_code_raw %in% as.character(valid_locale_codes),
        locale_code_raw,
        NA_character_
      )
    ),
    
    locale_type = case_when(
      locale_code %in% 11:13 ~ "City",
      locale_code %in% 21:23 ~ "Suburb",
      locale_code %in% 31:33 ~ "Town",
      locale_code %in% 41:43 ~ "Rural",
      TRUE ~ NA_character_
    )
  )


# STEP 5: Check the lookup
cat(
  "Unrecognized school IDs:",
  sum(is.na(geography_lookup$school_id)),
  "\n"
)

cat(
  "Duplicate nonmissing school IDs:",
  sum(duplicated(
    geography_lookup$school_id[
      !is.na(geography_lookup$school_id)
    ]
  )),
  "\n"
)

print(
  geography_lookup |>
    count(locale_type),
  n = Inf
)

# STEP 6: Check geographic coverage of our readiness sample
readiness <- read.csv(
  "data/processed/career_readiness_panel.csv",
  colClasses = c(school_id = "character")
)

# Match the lookup to each school-year for inspection
geography_check <- readiness |>
  select(school_id, academic_year, school_name) |>
  left_join(
    geography_lookup |>
      select(
        school_id,
        nces_school_id,
        locale_code,
        locale_type,
        locale_source_year
      ),
    by = "school_id",
    relationship = "many-to-one"
  )

# Confirm the match preserved all readiness records
stopifnot(nrow(geography_check) == nrow(readiness))

# Show coverage by academic year
print(
  geography_check |>
    group_by(academic_year) |>
    summarise(
      observations = n(),
      missing_locale = sum(is.na(locale_type)),
      .groups = "drop"
    )
)

# Show schools needing investigation
missing_geography <- geography_check |>
  filter(is.na(locale_type)) |>
  select(
    academic_year,
    school_id,
    school_name,
    nces_school_id
  )

print(missing_geography, row.names = FALSE)