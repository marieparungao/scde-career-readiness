# ============================================================
# 05: BUILD CHRONIC ABSENTEEISM PANEL
# ============================================================
# Purpose: Construct school-level chronic absenteeism rates.
#
# Input: Validated SCDE AdditionalInfo workbooks (Script 00)
# Output: data/processed/absenteeism_panel.csv
#
# Academic years: 2022-23 through 2025-26
# Note: Absenteeism data are from the PRIOR school year.
# ============================================================


# STEP 1: Load packages and identify source files

library(readxl)
library(dplyr)

years <- c("2022-23", "2023-24", "2024-25", "2025-26")

sources <- read.csv("data/raw/scde_source_manifest.csv")

additional_sources <- sources |>
  filter(type == "additional", academic_year %in% years)

stopifnot(
  nrow(additional_sources) == length(years),
  !anyDuplicated(additional_sources$academic_year),
  setequal(additional_sources$academic_year, years)
)


# STEP 2: Create a function to clean one academic year

clean_absenteeism <- function(year) {
  
  # Locate the validated AdditionalInfo workbook
  file <- additional_sources$file_path[
    additional_sources$academic_year == year
  ]
  
  if (length(file) != 1 || !file.exists(file)) {
    stop("Missing AdditionalInfo workbook for ", year)
  }
  
  # Import chronic absenteeism data for high schools
  raw <- read_excel(
    file,
    sheet = "5b.SchoolClimate_ChronicAbs",
    col_types = "text"
  ) |>
    filter(SCHOOLTYPECD == "H")
  
  # Verify school identifiers before conversion
  stopifnot(
    nrow(raw) > 0,
    !anyNA(raw$SCHOOLID),
    all(grepl("^[0-9]{6,7}$", trimws(raw$SCHOOLID)))
  )
  
  # Select and standardize variables
  clean <- raw |>
    transmute(
      academic_year = year,
      report_card_year = as.integer(ReportCardYear),
      
      # Report cards contain prior-year absenteeism data
      absenteeism_data_year = paste0(
        as.integer(substr(year, 1, 4)) - 1L,
        "-", substr(year, 3, 4)
      ),
      
      school_id = sprintf("%07d", as.integer(SCHOOLID)),
      district_name = trimws(gsub("[[:space:]]+", " ", DistrictNm)),
      school_name = trimws(gsub("[[:space:]]+", " ", SchoolNm)),
      chronic_absenteeism_pct = PctChronic_ALL
    )
  
  # Verify the report-card year and unique school IDs
  expected_year <- as.integer(substr(year, 1, 4)) + 1L
  
  stopifnot(
    !anyNA(clean$report_card_year),
    all(clean$report_card_year == expected_year),
    !anyDuplicated(clean$school_id)
  )
  
  # Convert percentages; suppressed values remain NA
  values <- trimws(clean$chronic_absenteeism_pct)
  values[values %in% c("*", "")] <- NA_character_
  
  if (any(!is.na(values) &
          is.na(suppressWarnings(as.numeric(values))))) {
    stop("Unexpected absenteeism values in ", year)
  }
  
  clean$chronic_absenteeism_pct <- as.numeric(values)
  
  cat(year, ":", nrow(clean), "high schools\n")
  
  return(clean)
}


# STEP 3: Build the four-year panel

absenteeism_panel <- years |>
  lapply(clean_absenteeism) |>
  bind_rows()


# STEP 4: Validate the completed panel

stopifnot(
  !anyDuplicated(
    absenteeism_panel[c("school_id", "academic_year")]
  ),
  all(is.na(absenteeism_panel$chronic_absenteeism_pct) |
        between(absenteeism_panel$chronic_absenteeism_pct,
                0, 100))
)

# Summarize school coverage and missing rates
print(
  absenteeism_panel |>
    group_by(academic_year, absenteeism_data_year) |>
    summarise(
      schools = n(),
      missing_rates = sum(is.na(chronic_absenteeism_pct)),
      .groups = "drop"
    )
)

# Check coverage against the career-readiness panel
readiness <- read.csv(
  "data/processed/career_readiness_panel.csv",
  colClasses = c(school_id = "character")
)

unmatched <- anti_join(
  readiness,
  absenteeism_panel,
  by = c("school_id", "academic_year")
)

cat("Readiness observations without absenteeism records:",
    nrow(unmatched), "\n")


# STEP 5: Save the dataset

dir.create("data/processed", showWarnings = FALSE)

write.csv(
  absenteeism_panel,
  "data/processed/absenteeism_panel.csv",
  row.names = FALSE
)

cat("\nChronic absenteeism panel saved successfully!\n")
cat("Observations:", nrow(absenteeism_panel), "\n")
cat("Variables:", ncol(absenteeism_panel), "\n")