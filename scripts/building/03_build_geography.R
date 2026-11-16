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
library(readxl)
library(dplyr)


# STEP 2: Identify the historical source files
sources <- data.frame(
  academic_year = c("2021-22", "2022-23", "2023-24"),
  
  source_year = c("2021-2022", "2022-2023", "2023-2024"),
  
  directory_zip = c(
    "ccd_sch_029_2122_w_1a_071722.zip",
    "ccd_sch_029_2223_w_1a_083023.zip",
    "ccd_sch_029_2324_w_1a_073124.zip"
  ),
  
  geography_zip = c(
    "EDGE_GEOCODE_PUBLICSCH_2122.zip",
    "EDGE_GEOCODE_PUBLICSCH_2223.zip",
    "EDGE_GEOCODE_PUBLICSCH_2324.zip"
  )
)


# STEP 3: Read the readiness dataset
readiness <- read.csv(
  "data/processed/career_readiness_panel.csv",
  colClasses = c(
    school_id = "character",
    academic_year = "character"
  )
)

stopifnot(
  !anyNA(readiness[c("school_id", "academic_year")]),
  !anyDuplicated(
    readiness[c("school_id", "academic_year")]
  ),
  all(readiness$academic_year %in% sources$academic_year)
)


# STEP 4: Create a function to extract one file from a ZIP
extract_file <- function(zip_name, file_pattern) {
  
  available_zips <- list.files(
    "data/raw",
    pattern = "\\.zip$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  zip_path <- available_zips[
    tolower(basename(available_zips)) == tolower(zip_name)
  ]
  
  if (length(zip_path) != 1) {
    stop("Expected exactly one ZIP named: ", zip_name)
  }
  
  contents <- unzip(zip_path, list = TRUE)$Name
  
  selected_file <- contents[
    grepl(file_pattern, contents, ignore.case = TRUE)
  ]
  
  if (length(selected_file) != 1) {
    stop(
      "Expected exactly one matching file inside ",
      zip_name,
      ". Contents: ",
      paste(contents, collapse = ", ")
    )
  }
  
  # Extract into a temporary folder; keep the original ZIP unchanged
  extraction_folder <- tempfile("nces_")
  dir.create(extraction_folder)
  
  unzip(
    zip_path,
    files = selected_file,
    exdir = extraction_folder
  )
  
  return(file.path(extraction_folder, selected_file))
}


# STEP 5: Create a function to build one historical year
build_geography <- function(i) {
  
  year <- sources$academic_year[i]
  
  # Read the school directory CSV
  directory_file <- extract_file(
    sources$directory_zip[i],
    "\\.csv$"
  )
  
  directory <- read.csv(
    directory_file,
    colClasses = "character",
    check.names = FALSE
  )
  
  stopifnot(
    all(c(
      "ST", "ST_SCHID", "NCESSCH",
      "SCH_NAME", "SCHOOL_YEAR"
    ) %in% names(directory))
  )
  
  directory <- directory |>
    filter(ST == "SC")
  
  stopifnot(
    nrow(directory) > 0,
    all(directory$SCHOOL_YEAR == sources$source_year[i]),
    all(grepl(
      "^SC-[0-9]{4}-[0-9]{3}$",
      trimws(directory$ST_SCHID)
    ))
  )
  
  directory <- directory |>
    transmute(
      school_id = gsub(
        "-", "",
        sub("^SC-", "", trimws(ST_SCHID))
      ),
      nces_school_id = trimws(NCESSCH),
      nces_school_name = SCH_NAME
    )
  
  stopifnot(
    !anyNA(directory$school_id),
    !anyDuplicated(directory$school_id)
  )
  
  
  # Read the geographic Excel workbook
  geography_file <- extract_file(
    sources$geography_zip[i],
    "\\.xlsx$"
  )
  
  geography <- read_excel(
    geography_file,
    sheet = 1,
    col_types = "text"
  )
  
  stopifnot(
    all(c(
      "NCESSCH", "STATE", "LOCALE", "SCHOOLYEAR"
    ) %in% names(geography))
  )
  
  geography <- geography |>
    filter(STATE == "SC")
  
  stopifnot(
    nrow(geography) > 0,
    all(geography$SCHOOLYEAR == sources$source_year[i])
  )
  
  valid_codes <- as.character(
    c(11:13, 21:23, 31:33, 41:43)
  )
  
  geography <- geography |>
    transmute(
      nces_school_id = trimws(NCESSCH),
      locale_code_raw = trimws(LOCALE)
    ) |>
    mutate(
      locale_code = as.integer(
        if_else(
          locale_code_raw %in% valid_codes,
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
  
  stopifnot(
    !anyNA(geography$nces_school_id),
    !anyDuplicated(geography$nces_school_id)
  )
  
  
  # Match geographic information to this year's readiness sample
  result <- readiness |>
    filter(academic_year == year) |>
    select(school_id, academic_year, school_name) |>
    left_join(
      directory,
      by = "school_id",
      relationship = "many-to-one"
    ) |>
    left_join(
      geography,
      by = "nces_school_id",
      relationship = "many-to-one"
    ) |>
    mutate(
      locale_source_year = year,
      directory_source_file = sources$directory_zip[i],
      locale_source_file = sources$geography_zip[i]
    )
  
  cat(
    year, ":",
    nrow(result), "records;",
    sum(is.na(result$locale_type)), "missing locales\n"
  )
  
  return(result)
}


# STEP 6: Combine all three historical years
geography_panel <- lapply(
  seq_len(nrow(sources)),
  build_geography
) |>
  bind_rows() |>
  arrange(academic_year, school_id)


# STEP 7: Validate the combined panel
stopifnot(
  nrow(geography_panel) == nrow(readiness),
  
  !anyNA(
    geography_panel[c("school_id", "academic_year")]
  ),
  
  !anyDuplicated(
    geography_panel[c("school_id", "academic_year")]
  )
)

print(
  geography_panel |>
    group_by(academic_year) |>
    summarise(
      observations = n(),
      missing_locale = sum(is.na(locale_type)),
      .groups = "drop"
    )
)


# STEP 8: Save the panel and missing-locale report
dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  geography_panel,
  "data/processed/geography_panel.csv",
  row.names = FALSE,
  na = ""
)

missing_geography <- geography_panel |>
  filter(is.na(locale_type))

write.csv(
  missing_geography,
  "data/processed/geography_missing_records.csv",
  row.names = FALSE,
  na = ""
)

print(
  missing_geography |>
    select(
      academic_year,
      school_id,
      school_name,
      nces_school_id
    ),
  row.names = FALSE
)

cat("\nHistorical geography panel saved successfully!\n")
cat("School-year observations:", nrow(geography_panel), "\n")