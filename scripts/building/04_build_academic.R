# ============================================================
# 04: BUILD ACADEMIC ACHIEVEMENT PANEL
# ============================================================
# Purpose: Construct school-level academic achievement
# measures using SCDE accountability ratings.
#
# Input: Validated SCDE researcher workbooks (Script 00)
# Output: data/processed/academic_panel.csv
#
# Academic years: 2022-23 through 2025-26
# Measure: Achievement points earned / available points * 100
# Note: This is not a student proficiency rate.
# ============================================================


# STEP 1: Load packages and identify source files

library(readxl)
library(dplyr)

years <- c("2022-23", "2023-24", "2024-25", "2025-26")

sources <- read.csv("data/raw/scde_source_manifest.csv")

researcher_sources <- sources |>
  filter(type == "researcher", academic_year %in% years)

stopifnot(
  nrow(researcher_sources) == length(years),
  !anyDuplicated(researcher_sources$academic_year),
  setequal(researcher_sources$academic_year, years)
)


# STEP 2: Create a function to clean one academic year

clean_academic <- function(year) {
  
  # Locate the validated researcher workbook
  file <- researcher_sources$file_path[
    researcher_sources$academic_year == year
  ]
  
  if (!file.exists(file)) {
    stop("Missing researcher workbook for ", year)
  }
  
  # Ratings worksheet names and header rows vary by year
  sheets <- excel_sheets(file)
  
  if ("REPORT_CARD_RATINGS" %in% sheets) {
    sheet <- "REPORT_CARD_RATINGS"
    skip_rows <- 0
  } else if ("Ratings" %in% sheets) {
    sheet <- "Ratings"
    skip_rows <- 2
  } else {
    stop("Ratings worksheet not found for ", year)
  }
  
  # Import school-level academic achievement data
  raw <- read_excel(
    file,
    sheet = sheet,
    skip = skip_rows,
    col_types = "text"
  ) |>
    filter(SCHOOLTYPECD == "H")
  
  # Verify required columns exist
  required <- c(
    "SCHOOLID", "DistrictNm", "SchoolNm",
    "RATE_ACHIEVE", "NUM_ACHIEVE", "DEN_ACHIEVE"
  )
  
  if (!all(required %in% names(raw))) {
    stop("Missing achievement columns for ", year)
  }
  
  # Verify school identifiers
  stopifnot(
    nrow(raw) > 0,
    !anyNA(raw$SCHOOLID),
    all(grepl("^[0-9]{6,7}$", trimws(raw$SCHOOLID)))
  )
  
  # Select and standardize variables
  clean <- raw |>
    transmute(
      academic_year = year,
      school_id = sprintf("%07d", as.integer(SCHOOLID)),
      district_name = trimws(gsub("[[:space:]]+", " ", DistrictNm)),
      school_name = trimws(gsub("[[:space:]]+", " ", SchoolNm)),
      achievement_rating = trimws(RATE_ACHIEVE),
      achievement_points = NUM_ACHIEVE,
      achievement_available = DEN_ACHIEVE
    )
  
  # Convert numeric values; suppressed values remain NA
  for (variable in c("achievement_points",
                     "achievement_available")) {
    
    values <- trimws(clean[[variable]])
    values[values %in% c("*", "")] <- NA_character_
    
    if (any(!is.na(values) &
            is.na(suppressWarnings(as.numeric(values))))) {
      stop("Unexpected values in ", year, ": ", variable)
    }
    
    clean[[variable]] <- as.numeric(values)
  }
  
  # Calculate percentage of available achievement points earned
  clean <- clean |>
    mutate(
      academic_achievement_points_pct = if_else(
        !is.na(achievement_rating) &
          achievement_rating != "Not Rated" &
          !is.na(achievement_points) &
          !is.na(achievement_available) &
          achievement_available > 0,
        100 * achievement_points / achievement_available,
        NA_real_
      )
    )
  
  # Confirm unique school identifiers
  stopifnot(!anyDuplicated(clean$school_id))
  
  cat(year, ":", nrow(clean), "high schools\n")
  
  return(clean)
}


# STEP 3: Build the four-year panel

academic_panel <- years |>
  lapply(clean_academic) |>
  bind_rows()


# STEP 4: Validate the completed panel

stopifnot(
  !anyDuplicated(
    academic_panel[c("school_id", "academic_year")]
  ),
  all(is.na(academic_panel$academic_achievement_points_pct) |
        between(academic_panel$academic_achievement_points_pct,
                0, 100))
)

# Summarize school coverage and missing achievement
print(
  academic_panel |>
    group_by(academic_year) |>
    summarise(
      schools = n(),
      not_rated = sum(achievement_rating == "Not Rated",
                      na.rm = TRUE),
      missing_achievement = sum(
        is.na(academic_achievement_points_pct)
      ),
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
  academic_panel,
  by = c("school_id", "academic_year")
)

cat("Readiness observations without academic records:",
    nrow(unmatched), "\n")


# STEP 5: Save the dataset

dir.create("data/processed", showWarnings = FALSE)

write.csv(
  academic_panel,
  "data/processed/academic_panel.csv",
  row.names = FALSE
)

cat("\nAcademic achievement panel saved successfully!\n")
cat("Observations:", nrow(academic_panel), "\n")
cat("Variables:", ncol(academic_panel), "\n")