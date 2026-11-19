# ============================================================
# 06: BUILD DISABILITY STATUS PANEL
# ============================================================
# Purpose: Construct a school-level disability composition
# proxy using chronic absenteeism reporting populations.
#
# Input: Validated SCDE AdditionalInfo workbooks (Script 00)
# Output: data/processed/disability_panel.csv
#
# Academic years: 2022-23 through 2025-26
# Measure: NTotal_DI / NTotal_ALL * 100
# Note: Based on prior-year absenteeism reporting populations,
# not necessarily total school enrollment.
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

clean_disability <- function(year) {
  
  # Locate the validated AdditionalInfo workbook
  file <- additional_sources$file_path[
    additional_sources$academic_year == year
  ]
  
  if (length(file) != 1 || !file.exists(file)) {
    stop("Missing AdditionalInfo workbook for ", year)
  }
  
  # Import chronic absenteeism population data for high schools
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
      
      # Same prior-year reporting period as chronic absenteeism
      disability_data_year = paste0(
        as.integer(substr(year, 1, 4)) - 1L,
        "-", substr(year, 3, 4)
      ),
      
      school_id = sprintf("%07d", as.integer(SCHOOLID)),
      district_name = trimws(gsub("[[:space:]]+", " ", DistrictNm)),
      school_name = trimws(gsub("[[:space:]]+", " ", SchoolNm)),
      
      # Preserve original counts, including suppressed values
      disability_count_raw = NTotal_DI,
      attendance_population_raw = NTotal_ALL,
      
      disability_count = NTotal_DI,
      attendance_population = NTotal_ALL
    )
  
  # Verify report-card year and unique school identifiers
  expected_year <- as.integer(substr(year, 1, 4)) + 1L
  
  stopifnot(
    !anyNA(clean$report_card_year),
    all(clean$report_card_year == expected_year),
    !anyDuplicated(clean$school_id)
  )
  
  # Convert counts; suppressed values remain NA
  for (variable in c("disability_count",
                     "attendance_population")) {
    
    values <- gsub(",", "", trimws(clean[[variable]]))
    values[values %in% c("*", "")] <- NA_character_
    
    if (any(!is.na(values) &
            is.na(suppressWarnings(as.numeric(values))))) {
      stop("Unexpected values in ", year, ": ", variable)
    }
    
    clean[[variable]] <- as.numeric(values)
  }
  
  # Calculate disability share of the reporting population
  clean <- clean |>
    mutate(
      disability_share_attendance_pct = if_else(
        !is.na(disability_count) &
          !is.na(attendance_population) &
          attendance_population > 0,
        100 * disability_count / attendance_population,
        NA_real_
      )
    )
  
  cat(year, ":", nrow(clean), "high schools\n")
  
  return(clean)
}


# STEP 3: Build the four-year panel

disability_panel <- years |>
  lapply(clean_disability) |>
  bind_rows()


# STEP 4: Validate the completed panel

stopifnot(
  !anyDuplicated(
    disability_panel[c("school_id", "academic_year")]
  ),
  all(is.na(disability_panel$disability_count) |
        disability_panel$disability_count >= 0),
  all(is.na(disability_panel$attendance_population) |
        disability_panel$attendance_population >= 0),
  all(is.na(disability_panel$disability_count) |
        is.na(disability_panel$attendance_population) |
        disability_panel$disability_count <=
        disability_panel$attendance_population),
  all(is.na(disability_panel$disability_share_attendance_pct) |
        between(disability_panel$disability_share_attendance_pct,
                0, 100))
)

# Summarize school coverage and missing disability information
print(
  disability_panel |>
    group_by(academic_year, disability_data_year) |>
    summarise(
      schools = n(),
      missing_disability = sum(
        is.na(disability_share_attendance_pct)
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
  disability_panel,
  by = c("school_id", "academic_year")
)

cat("Readiness observations without disability records:",
    nrow(unmatched), "\n")


# STEP 5: Save the dataset

dir.create("data/processed", showWarnings = FALSE)

write.csv(
  disability_panel,
  "data/processed/disability_panel.csv",
  row.names = FALSE
)

cat("\nDisability status panel saved successfully!\n")
cat("Observations:", nrow(disability_panel), "\n")
cat("Variables:", ncol(disability_panel), "\n")