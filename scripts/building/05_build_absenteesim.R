# ============================================================
# 05: BUILD CHRONIC ABSENTEEISM CHARACTERISTICS
# ============================================================
# Purpose: Construct school-level chronic absenteeism
# rates across five academic years.
#
# Source: SC School Report Card workbooks
# Output: data/processed/absenteeism_panel.csv
#
# COMPLETED STEPS:
# - Locate chronic absenteeism data in report cards
# - Definition comparability across years remains under review
# - Extract school-level chronic absenteeism rates
# - Standardize school IDs and academic years
# - Check missing values and school coverage
# - Save the cleaned absenteeism dataset

# STEP 1: Load packages
library(readxl)
library(dplyr)


# STEP 2: Find the source workbooks
additional_files <- list.files(
  "data/raw",
  pattern = "AdditionalInfo",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)


# STEP 3: Create a function to import and clean one year
build_absenteeism <- function(filename, year_label) {
  
  # Find the requested workbook
  file_path <- additional_files[
    tolower(basename(additional_files)) == tolower(filename)
  ]
  
  # Confirm exactly one matching file exists
  stopifnot(length(file_path) == 1)
  
  # Import the chronic absenteeism sheet
  raw_data <- read_excel(
    file_path,
    sheet = "5b.SchoolClimate_ChronicAbs",
    col_types = "text"
  )
  
  # Keep high schools and the columns we need
  clean_data <- raw_data |>
    filter(SCHOOLTYPECD == "H") |>
    transmute(
      school_id = sprintf("%07d", as.integer(SCHOOLID)),
      academic_year = year_label,
      district_name = gsub("[[:space:]]+", " ", DistrictNm),
      school_name = gsub("[[:space:]]+", " ", SchoolNm),
      chronic_absenteeism_pct = as.numeric(
        na_if(trimws(PctChronic_ALL), "*")
      )
    )
  
  return(clean_data)
}


# STEP 4: Build each year
# The workbook year is the END of the academic year.

absenteeism_2022 <- build_absenteeism(
  "ReportCardData_AdditionalInfo2022.XLSx",
  "2021-22"
)

absenteeism_2023 <- build_absenteeism(
  "ReportCardData_AdditionalInfo2023_3.XLSx",
  "2022-23"
)

absenteeism_2024 <- build_absenteeism(
  "ReportCardData_AdditionalInfo2024_6.XLSx",
  "2023-24"
)

absenteeism_2025 <- build_absenteeism(
  "ReportCardData_AdditionalInfo2025 5.xlsx",
  "2024-25"
)

absenteeism_2026 <- build_absenteeism(
  "ReportCardData_AdditionalInfo2026.XLSx",
  "2025-26"
)


# STEP 5: Combine the five years
absenteeism_panel <- bind_rows(
  absenteeism_2022,
  absenteeism_2023,
  absenteeism_2024,
  absenteeism_2025,
  absenteeism_2026
) |>
  arrange(academic_year, school_id)


# STEP 6: Check identifiers and percentage ranges
stopifnot(
  !anyNA(absenteeism_panel[c("school_id", "academic_year")]),
  
  all(grepl("^[0-9]{7}$", absenteeism_panel$school_id)),
  
  !anyDuplicated(
    absenteeism_panel[c("school_id", "academic_year")]
  ),
  
  all(
    is.na(absenteeism_panel$chronic_absenteeism_pct) |
      between(absenteeism_panel$chronic_absenteeism_pct, 0, 100)
  )
)

# Display observations and missing rates by year
print(
  absenteeism_panel |>
    group_by(academic_year) |>
    summarise(
      observations = n(),
      missing_rates = sum(is.na(chronic_absenteeism_pct)),
      .groups = "drop"
    )
)


# STEP 7: Save the cleaned dataset
dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  absenteeism_panel,
  "data/processed/absenteeism_panel.csv",
  row.names = FALSE,
  na = ""
)

cat("\nAbsenteeism dataset saved successfully!\n")
cat("School-year observations:", nrow(absenteeism_panel), "\n")