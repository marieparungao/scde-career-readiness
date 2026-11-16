# ============================================================
# 04: BUILD ACADEMIC ACHIEVEMENT CHARACTERISTICS
# ============================================================
# Purpose: Construct a school-level academic achievement
# measure across five academic years: 2021-22 to 2025-26.
#
# Source: SC School Report Card researcher workbooks
# Output: data/processed/academic_panel.csv
#
# MEASURE:
# 100 * NUM_ACHIEVE / DEN_ACHIEVE
# Achievement points earned as a percentage of available points.
# This is not the percentage of students meeting proficiency.
#
# REVIEW STILL NEEDED:
# - Verify achievement scoring rules across years
# - Determine whether the measure is comparable across years
# ============================================================


# STEP 1: Load packages
library(readxl)
library(dplyr)


# STEP 2: Find the researcher workbooks
researcher_files <- list.files(
  "data/raw",
  pattern = "forResearchers",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)


# STEP 3: Create a function to import and clean one year
build_academic <- function(filename, year_label,
                           sheet_name, rows_to_skip) {
  
  file_path <- researcher_files[
    tolower(basename(researcher_files)) == tolower(filename)
  ]
  
  # Confirm exactly one matching file exists
  stopifnot(length(file_path) == 1)
  
  read_excel(
    file_path,
    sheet = sheet_name,
    skip = rows_to_skip,
    col_types = "text"
  ) |>
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
      
      achievement_rating = trimws(RATE_ACHIEVE),
      
      achievement_points = as.numeric(
        na_if(trimws(NUM_ACHIEVE), "*")
      ),
      
      achievement_available = as.numeric(
        na_if(trimws(DEN_ACHIEVE), "*")
      )
    ) |>
    mutate(
      academic_achievement_points_pct = if_else(
        !is.na(achievement_rating) &
          achievement_rating != "Not Rated" &
          !is.na(achievement_available) &
          achievement_available > 0,
        100 * achievement_points / achievement_available,
        NA_real_
      )
    )
}


# STEP 4: Build each year
# The workbook year is the END of the academic year.
# Ratings-sheet names and header positions vary by year.

academic_2022 <- build_academic(
  filename = "ReportCardData_forResearchers2022.XLSx",
  year_label = "2021-22",
  sheet_name = "Ratings",
  rows_to_skip = 2
)

academic_2023 <- build_academic(
  filename = "ReportCardData_forResearchers2023_3.XLSx",
  year_label = "2022-23",
  sheet_name = "Ratings",
  rows_to_skip = 2
)

academic_2024 <- build_academic(
  filename = "ReportCardData_forResearchers2024_11.xlsx",
  year_label = "2023-24",
  sheet_name = "REPORT_CARD_RATINGS",
  rows_to_skip = 0
)

academic_2025 <- build_academic(
  filename = "ReportCardData_forResearchers2025 5.xlsx",
  year_label = "2024-25",
  sheet_name = "REPORT_CARD_RATINGS",
  rows_to_skip = 0
)

academic_2026 <- build_academic(
  filename = "ReportCardData_forResearchers2026.XLSx",
  year_label = "2025-26",
  sheet_name = "REPORT_CARD_RATINGS",
  rows_to_skip = 0
)


# STEP 5: Combine the five years
academic_panel <- bind_rows(
  academic_2022,
  academic_2023,
  academic_2024,
  academic_2025,
  academic_2026
) |>
  arrange(academic_year, school_id)


# STEP 6: Check identifiers and percentage ranges
stopifnot(
  !anyNA(academic_panel[c("school_id", "academic_year")]),
  
  all(grepl("^[0-9]{7}$", academic_panel$school_id)),
  
  !anyDuplicated(
    academic_panel[c("school_id", "academic_year")]
  ),
  
  all(
    is.na(academic_panel$academic_achievement_points_pct) |
      between(
        academic_panel$academic_achievement_points_pct,
        0, 100
      )
  )
)

# Display school coverage and missing achievement by year
print(
  academic_panel |>
    group_by(academic_year) |>
    summarise(
      observations = n(),
      not_rated = sum(
        achievement_rating == "Not Rated",
        na.rm = TRUE
      ),
      missing_achievement = sum(
        is.na(academic_achievement_points_pct)
      ),
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
  academic_panel,
  "data/processed/academic_panel.csv",
  row.names = FALSE,
  na = ""
)

cat("\nAcademic dataset saved successfully!\n")
cat("School-year observations:", nrow(academic_panel), "\n")