# ============================================================
# 06: BUILD DISABILITY STATUS CHARACTERISTICS
# ============================================================
# Purpose: Construct a school-level disability composition
# proxy across five academic years: 2021-22 through 2025-26.
#
# Source: SC School Report Card AdditionalInfo workbooks
# Sheet: 5b.SchoolClimate_ChronicAbs
# Output: data/processed/disability_panel.csv
#
# MEASURE:
# 100 * NTotal_DI / NTotal_ALL
#
# This measures the disability share of the population used
# for chronic absenteeism reporting. It is a proxy for
# school-wide disability composition.
#
# REVIEW STILL NEEDED:
# - Verify disability subgroup definitions across years
# - Verify absenteeism reporting population eligibility
# - Determine whether this proxy is comparable across years
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
  
  # Confirm exactly one matching file exists
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
      district_name = trimws(
        gsub("[[:space:]]+", " ", DistrictNm)
      ),
      school_name = trimws(
        gsub("[[:space:]]+", " ", SchoolNm)
      ),
      
      # Preserve original entries, including suppression symbols
      disability_count_raw = NTotal_DI,
      attendance_population_raw = NTotal_ALL,
      
      # Suppressed counts remain missing, rather than zero
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
# The workbook year is the END of the academic year.

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

disability_2025 <- build_disability(
  "ReportCardData_AdditionalInfo2025 5.xlsx",
  "2024-25"
)

disability_2026 <- build_disability(
  "ReportCardData_AdditionalInfo2026.XLSx",
  "2025-26"
)


# STEP 5: Combine the five years
disability_panel <- bind_rows(
  disability_2022,
  disability_2023,
  disability_2024,
  disability_2025,
  disability_2026
) |>
  arrange(academic_year, school_id)


# STEP 6: Check identifiers, counts, and percentage ranges
stopifnot(
  !anyNA(disability_panel[c("school_id", "academic_year")]),
  
  all(grepl("^[0-9]{7}$", disability_panel$school_id)),
  
  !anyDuplicated(
    disability_panel[c("school_id", "academic_year")]
  ),
  
  all(
    is.na(disability_panel$disability_count) |
      disability_panel$disability_count >= 0
  ),
  
  all(
    is.na(disability_panel$attendance_population) |
      disability_panel$attendance_population >= 0
  ),
  
  all(
    is.na(disability_panel$disability_count) |
      is.na(disability_panel$attendance_population) |
      disability_panel$disability_count <=
      disability_panel$attendance_population
  ),
  
  all(
    is.na(disability_panel$disability_share_attendance_pct) |
      between(
        disability_panel$disability_share_attendance_pct,
        0, 100
      )
  )
)

# Display school coverage and missing values by year
print(
  disability_panel |>
    group_by(academic_year) |>
    summarise(
      observations = n(),
      missing_disability_count = sum(
        is.na(disability_count)
      ),
      missing_attendance_population = sum(
        is.na(attendance_population)
      ),
      zero_attendance_population = sum(
        attendance_population == 0,
        na.rm = TRUE
      ),
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
cat(
  "Academic years:",
  paste(sort(unique(disability_panel$academic_year)),
        collapse = ", "),
  "\n"
)