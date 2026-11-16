# ============================================================
# 06: BUILD DISABILITY STATUS CHARACTERISTICS
# ============================================================
# Purpose: Construct school-level disability composition
# as an additional independent variable.
#
# Source: SCDE school-level data (to be identified)
# Output: data/processed/disability_panel.csv
#
# COMPLETED STEPS:
# - Identify a reliable school-level disability measure
# - Determine whether data exist for all four years
# - Verify definitions and appropriate denominators
# - Calculate the percentage of students with disabilities
# - Standardize school IDs and academic years
# - Check missing values and school coverage
# - Save the cleaned disability dataset
# ============================================================

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


# STEP 3: Import and clean one year
build_disability <- function(filename, year_label) {
  
  file_path <- additional_files[
    tolower(basename(additional_files)) == tolower(filename)
  ]
  
  stopifnot(length(file_path) == 1)
  
  raw_data <- read_excel(
    file_path,
    sheet = "5b.SchoolClimate_ChronicAbs",
    col_types = "text"
  )
  
  clean_data <- raw_data |>
    filter(SCHOOLTYPECD == "H") |>
    transmute(
      school_id = sprintf("%07d", as.integer(SCHOOLID)),
      academic_year = year_label,
      district_name = gsub("[[:space:]]+", " ", DistrictNm),
      school_name = gsub("[[:space:]]+", " ", SchoolNm),
      
      # Preserve original entries, including suppression symbols
      disability_count_raw = NTotal_DI,
      attendance_population_raw = NTotal_ALL,
      
      disability_count = as.numeric(
        na_if(trimws(NTotal_DI), "*")
      ),
      
      attendance_population = as.numeric(
        na_if(trimws(NTotal_ALL), "*")
      )
    ) |>
    mutate(
      disability_share_attendance_pct = if_else(
        !is.na(attendance_population) &
          attendance_population > 0,
        100 * disability_count / attendance_population,
        NA_real_
      )
    )
  
  return(clean_data)
}


# STEP 4: Build each year
disability_2022 <- build_disability(
  "ReportCardData_AdditionalInfo2022.XLSx",
  "2021-22"
)

disability_2023 <- build_disability(
  "ReportCardData_AdditionalInfo2023_3.XLSx",
  "2022-23"
)

disability_2024 <- build_disability(
  "ReportCardData_AdditionalInfo2024_6.XLSx",
  "2023-24"
)


# STEP 5: Combine the years
disability_panel <- bind_rows(
  disability_2022,
  disability_2023,
  disability_2024
) |>
  arrange(academic_year, school_id)


# STEP 6: Check the results
stopifnot(
  !anyNA(disability_panel[c("school_id", "academic_year")]),
  
  all(grepl("^[0-9]{7}$", disability_panel$school_id)),
  
  !anyDuplicated(
    disability_panel[c("school_id", "academic_year")]
  ),
  
  all(
    is.na(disability_panel$disability_share_attendance_pct) |
      between(
        disability_panel$disability_share_attendance_pct,
        0, 100
      )
  )
)

print(
  disability_panel |>
    group_by(academic_year) |>
    summarise(
      observations = n(),
      missing_disability = sum(
        is.na(disability_share_attendance_pct)
      ),
      .groups = "drop"
    )
)


# STEP 7: Save the dataset
dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  disability_panel,
  "data/processed/disability_panel.csv",
  row.names = FALSE,
  na = ""
)

cat("\nDisability dataset saved successfully!\n")
cat("School-year observations:", nrow(disability_panel), "\n")