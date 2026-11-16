# ============================================================
# 04: BUILD ACADEMIC ACHIEVEMENT CHARACTERISTICS
# ============================================================
# Purpose: Construct a comparable school-level measure
# of academic achievement across four academic years.
#
# Source: SC School Report Card workbooks
# Output: data/processed/academic_panel.csv
#
# COMPLETED STEPS:
# - Identify available academic achievement measures
# - Select a consistent measure across all four years
# - Verify variable definitions and reporting years
# - Extract school-level academic achievement data
# - Standardize school IDs and academic years
# - Check missing values and school coverage
# - Save the cleaned academic dataset
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

print(researcher_files)

# STEP 3: Import the 2022 ratings sheet
file_2022 <- researcher_files[
  tolower(basename(researcher_files)) ==
    tolower("ReportCardData_forResearchers2022.XLSx")
]

stopifnot(length(file_2022) == 1)

academic_2022_raw <- read_excel(
  file_2022,
  sheet = "Ratings",
  skip = 2,
  col_types = "text"
)

# Display achievement information for high schools
academic_2022_raw |>
  filter(SCHOOLTYPECD == "H") |>
  select(
    SCHOOLID,
    SchoolNm,
    NUM_ACHIEVE,
    DEN_ACHIEVE,
    RATE_ACHIEVE
  ) |>
  head()

# STEP 4: Clean the 2022 achievement data
academic_2022 <- academic_2022_raw |>
  filter(SCHOOLTYPECD == "H") |>
  transmute(
    school_id = sprintf("%07d", as.integer(SCHOOLID)),
    academic_year = "2021-22",
    district_name = gsub("[[:space:]]+", " ", DistrictNm),
    school_name = gsub("[[:space:]]+", " ", SchoolNm),
    
    achievement_rating = RATE_ACHIEVE,
    
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

# Check the results
nrow(academic_2022)

sum(duplicated(academic_2022$school_id))

summary(academic_2022$academic_achievement_points_pct)

# STEP 5: Create a function to build the remaining years
build_academic <- function(filename, year_label,
                           sheet_name, rows_to_skip) {
  
  file_path <- researcher_files[
    tolower(basename(researcher_files)) == tolower(filename)
  ]
  
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
      district_name = gsub("[[:space:]]+", " ", DistrictNm),
      school_name = gsub("[[:space:]]+", " ", SchoolNm),
      achievement_rating = RATE_ACHIEVE,
      
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


# STEP 6: Build 2023 and 2024
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


# STEP 7: Combine all three years
academic_panel <- bind_rows(
  academic_2022,
  academic_2023,
  academic_2024
) |>
  arrange(academic_year, school_id)


# STEP 8: Check identifiers and percentage ranges
stopifnot(
  !anyNA(academic_panel[c("school_id", "academic_year")]),
  
  all(grepl("^[0-9]{7}$", academic_panel$school_id)),
  
  !anyDuplicated(
    academic_panel[c("school_id", "academic_year")]
  ),
  
  all(
    is.na(academic_panel$academic_achievement_points_pct) |
      between(academic_panel$academic_achievement_points_pct, 0, 100)
  )
)

print(
  academic_panel |>
    group_by(academic_year) |>
    summarise(
      observations = n(),
      missing_achievement = sum(
        is.na(academic_achievement_points_pct)
      ),
      .groups = "drop"
    )
)


# STEP 9: Save the cleaned dataset
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