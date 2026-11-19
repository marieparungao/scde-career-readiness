# ============================================================
# 03: BUILD GEOGRAPHIC CHARACTERISTICS PANEL
# ============================================================
# Purpose: Match SC high schools to historical NCES
# geographic locale classifications.
#
# Input: NCES CCD school directories and EDGE locale files
# Output: data/processed/geography_panel.csv
#
# Academic years: 2022-23 through 2025-26
# NOTE: 2025-26 uses its preliminary directory with 2024-25
# locales for matched schools. Available carried-forward locales
# are flagged; unmatched schools remain missing for review.
# ============================================================


# STEP 1: Load packages and identify NCES sources

library(readxl)
library(dplyr)

years <- c("2022-23", "2023-24", "2024-25", "2025-26")
nces_years <- years
locale_years <- c(years[1:3], "2024-25")

# Directory archives have version-specific filenames
nces_sources <- data.frame(
  academic_year = nces_years,
  source_year = paste0(
    substr(nces_years, 1, 4), "-",
    as.integer(substr(nces_years, 1, 4)) + 1L
  ),
  directory_zip = c(
    "ccd_sch_029_2223_w_1a_083023.zip",
    "ccd_sch_029_2324_w_1a_073124.zip",
    "ccd_sch_029_2425_w_1a_073025.zip",
    "ccd_sch_029_2526_w_0a_050626.zip"
  ),
  locale_source_year = locale_years,
  geography_source_year = paste0(
    substr(locale_years, 1, 4), "-",
    as.integer(substr(locale_years, 1, 4)) + 1L
  ),
  locale_zip = paste0(
    "EDGE_GEOCODE_PUBLICSCH_",
    substr(locale_years, 3, 4),
    substr(locale_years, 6, 7),
    ".zip"
  )
)

options(timeout = max(600, getOption("timeout")))

# STEP 2: Locate or download the required ZIP archives

archives <- unique(data.frame(
  filename = c(
    nces_sources$directory_zip,
    nces_sources$locale_zip
  ),
  url = c(
    paste0(
      "https://nces.ed.gov/ccd/Data/zip/",
      nces_sources$directory_zip
    ),
    paste0(
      "https://nces.ed.gov/programs/edge/data/",
      nces_sources$locale_zip
    )
  )
))

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

for (i in seq_len(nrow(archives))) {
  available <- list.files(
    "data/raw", pattern = "\\.zip$", recursive = TRUE,
    full.names = TRUE, ignore.case = TRUE
  )
  matching <- available[
    tolower(basename(available)) == tolower(archives$filename[i])
  ]
  if (length(matching) > 1) {
    stop("Multiple archives found for ", archives$filename[i])
  }
  file <- if (length(matching) == 1) matching else
    file.path("data/raw", archives$filename[i])
  
  if (!file.exists(file)) {
    cat("Downloading:", archives$filename[i], "\n")
    
    tryCatch({
      status <- download.file(
        archives$url[i], file,
        mode = "wb", quiet = TRUE
      )
      if (status != 0) stop("Download unsuccessful")
    }, error = function(e) {
      if (file.exists(file)) file.remove(file)
      stop(
        "Download failed. Download manually:\n",
        archives$url[i], "\nSave as: ", file,
        "\nReason: ", conditionMessage(e)
      )
    })
  }
  # Check that the archive is readable
  stopifnot(nrow(unzip(file, list = TRUE)) > 0)
}


# STEP 3: Create functions to extract and clean NCES data

# Extract one matching file without modifying the ZIP
extract_file <- function(zip_name, pattern) {
  
  available <- list.files(
    "data/raw", pattern = "\\.zip$", recursive = TRUE,
    full.names = TRUE, ignore.case = TRUE
  )
  zip_path <- available[
    tolower(basename(available)) == tolower(zip_name)
  ]
  if (length(zip_path) != 1) {
    stop("Expected one ZIP named ", zip_name)
  }
  
  contents <- unzip(zip_path, list = TRUE)$Name
  selected <- contents[grepl(pattern, contents, ignore.case = TRUE)]
  
  if (length(selected) != 1) {
    stop("Expected one matching file inside ", zip_name)
  }
  
  folder <- tempfile("nces_")
  dir.create(folder)
  
  unzip(zip_path, files = selected, exdir = folder)
  
  file.path(folder, selected)
}
# Build geographic classifications for one historical year
clean_geography <- function(i) {
  
  year <- nces_sources$academic_year[i]
  source_year <- nces_sources$source_year[i]
  
  # Read the CCD school directory
  directory <- read.csv(
    extract_file(nces_sources$directory_zip[i], "\\.csv$"),
    colClasses = "character",
    check.names = FALSE
  )
  
  stopifnot(all(c(
    "ST", "ST_SCHID", "NCESSCH", "SCH_NAME", "SCHOOL_YEAR"
  ) %in% names(directory)))
  
  directory <- directory |>
    filter(ST == "SC")
  
  # Validate against the actual directory year
  stopifnot(
    nrow(directory) > 0,
    !anyNA(directory[c("SCHOOL_YEAR", "ST_SCHID", "NCESSCH")]),
    all(directory$SCHOOL_YEAR == source_year),
    all(grepl(
      "^SC-[0-9]{4}-[0-9]{3}$",
      trimws(directory$ST_SCHID)
    ))
  )
  
  directory <- directory |>
    transmute(
      school_id = gsub(
        "-", "", sub("^SC-", "", trimws(ST_SCHID))
      ),
      nces_school_id = trimws(NCESSCH)
    )
  # Read the matching EDGE geographic workbook
  geography <- read_excel(
    extract_file(nces_sources$locale_zip[i], "\\.xlsx$"),
    col_types = "text"
  )
  
  stopifnot(all(c(
    "STATE", "SCHOOLYEAR", "NCESSCH", "LOCALE"
  ) %in% names(geography)))
  
  geography <- geography |>
    filter(STATE == "SC")
  
  stopifnot(
    nrow(geography) > 0,
    !anyNA(geography[c("SCHOOLYEAR", "NCESSCH")]),
    all(geography$SCHOOLYEAR == nces_sources$geography_source_year[i])
  )
  
  geography <- geography |>
    transmute(
      nces_school_id = trimws(NCESSCH),
      locale_code = suppressWarnings(as.integer(LOCALE))
    ) |>
    mutate(
      locale_type = case_when(
        locale_code %in% 11:13 ~ "City",
        locale_code %in% 21:23 ~ "Suburb",
        locale_code %in% 31:33 ~ "Town",
        locale_code %in% 41:43 ~ "Rural",
        TRUE ~ NA_character_
      )
    )
  
  # Confirm unique identifiers before matching
  stopifnot(
    !anyDuplicated(directory$school_id),
    !anyDuplicated(geography$nces_school_id)
  )
  # Match geographic information to the readiness sample
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
      directory_source_year = source_year,
      locale_source_year = nces_sources$locale_source_year[i],
      locale_carried_forward = !is.na(locale_type) &
        (nces_sources$locale_source_year[i] != year),
      directory_source_file = nces_sources$directory_zip[i],
      locale_source_file = nces_sources$locale_zip[i]
    )
  
  cat(year, ":", nrow(result), "schools;",
      sum(is.na(result$locale_type)), "missing locales\n")
  
  result
}
# STEP 4: Build the four-year geography panel

readiness <- read.csv(
  "data/processed/career_readiness_panel.csv",
  colClasses = c(
    school_id = "character",
    academic_year = "character"
  )
)

stopifnot(
  !anyNA(readiness[c("school_id", "academic_year")]),
  setequal(unique(readiness$academic_year), years),
  !anyDuplicated(readiness[c("school_id", "academic_year")])
)

# Match each year through its own directory. The 2025-26
# preliminary directory is linked to the prior year's EDGE locales.
geography_panel <- lapply(
  seq_len(nrow(nces_sources)),
  clean_geography
) |>
  bind_rows() |>
  arrange(academic_year, school_id)


# STEP 5: Validate and save the dataset

stopifnot(
  nrow(geography_panel) == nrow(readiness),
  !anyNA(geography_panel[c("school_id", "academic_year")]),
  !anyDuplicated(
    geography_panel[c("school_id", "academic_year")]
  )
)
# Check geographic coverage by academic year
print(
  geography_panel |>
    group_by(academic_year) |>
    summarise(
      schools = n(),
      missing_locale = sum(is.na(locale_type)),
      carried_forward = sum(locale_carried_forward),
      .groups = "drop"
    )
)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

write.csv(
  geography_panel,
  "data/processed/geography_panel.csv",
  row.names = FALSE
)

# Save unmatched records for further investigation
write.csv(
  filter(geography_panel, is.na(locale_type)),
  "data/processed/geography_missing_records.csv",
  row.names = FALSE
)

cat("\nGeographic characteristics panel saved successfully!\n")
cat("Observations:", nrow(geography_panel), "\n")
cat("Variables:", ncol(geography_panel), "\n")
