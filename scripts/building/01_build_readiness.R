# ============================================================
# 01: BUILD CAREER READINESS PANEL
# ============================================================
# Purpose: Combine four years of high-school career-readiness
# outcomes and graduation cohort sizes into one panel.
#
# Input: Validated SCDE researcher workbooks (Script 00)
# Output: data/processed/career_readiness_panel.csv
#
# Academic years: 2022-23 through 2025-26
# ============================================================


# STEP 1: Load packages and identify source files

library(readxl)
library(dplyr)

years <- c("2022-23", "2023-24", "2024-25", "2025-26")

# Read the source manifest created by Script 00
sources <- read.csv("data/raw/scde_source_manifest.csv")

# Keep only researcher workbooks for our study period
researcher_sources <- sources |>
  filter(type == "researcher", academic_year %in% years)

# Confirm exactly one source for each academic year
stopifnot(
  nrow(researcher_sources) == length(years),
  !anyDuplicated(researcher_sources$academic_year),
  setequal(researcher_sources$academic_year, years)
)


# STEP 2: Create a function to clean one academic year

clean_readiness <- function(year) {
  
  # Locate the appropriate workbook using the manifest
  file <- researcher_sources$file_path[
    researcher_sources$academic_year == year
  ]
  
  if (!file.exists(file)) {
    stop("Missing researcher workbook for ", year)
  }
  
  # Import career-readiness data for high schools only
  raw <- read_excel(
    file,
    sheet = "4.CollegeCareer",
    col_types = "text"
  ) |>
    filter(SCHOOLTYPECD == "H")
  
  # Select and standardize variables
  clean <- raw |>
    transmute(
      academic_year = year,
      report_card_year = as.integer(ReportCardYear),
      district_name = trimws(gsub("[[:space:]]+", " ", DistrictNm)),
      school_name = trimws(gsub("[[:space:]]+", " ", SchoolNm)),
      school_id = sprintf("%07d", as.integer(SCHOOLID)),
      cohort_size = NumInCohort,
      overall_readiness = PctCareer,
      cte_readiness = PctCTE,
      wbl_readiness = PctWBL,
      assessment_readiness = PctCarRdy,
      asvab_readiness = PctASVAB,
      credential_readiness = PctCred
    )
  
  # Verify report-card year matches the academic year
  expected_year <- as.integer(substr(year, 1, 4)) + 1L
  
  stopifnot(
    nrow(clean) > 0,
    !anyNA(clean$report_card_year),
    all(clean$report_card_year == expected_year),
    !anyNA(clean$school_id),
    all(grepl("^[0-9]{7}$", clean$school_id)),
    !anyDuplicated(clean$school_id)
  )
  
  # Convert numeric variables; suppressed values remain NA
  numeric_variables <- c(
    "cohort_size",
    "overall_readiness", "cte_readiness", "wbl_readiness",
    "assessment_readiness", "asvab_readiness",
    "credential_readiness"
  )
  
  for (variable in numeric_variables) {
    
    values <- trimws(clean[[variable]])
    values[values %in% c("*", "")] <- NA_character_
    
    # Stop if any unexpected nonnumeric values appear
    if (any(!is.na(values) &
            is.na(suppressWarnings(as.numeric(values))))) {
      stop("Unexpected values in ", year, ": ", variable)
    }
    
    clean[[variable]] <- as.numeric(values)
  }
  
  cat(year, ":", nrow(clean), "high schools\n")
  
  return(clean)
}


# STEP 3: Build the four-year panel

career_readiness_panel <- years |>
  lapply(clean_readiness) |>
  bind_rows()


# STEP 4: Validate the completed panel

outcomes <- c(
  "overall_readiness", "cte_readiness", "wbl_readiness",
  "assessment_readiness", "asvab_readiness",
  "credential_readiness"
)

stopifnot(
  !anyDuplicated(
    career_readiness_panel[c("school_id", "academic_year")]
  ),
  all(sapply(
    career_readiness_panel[outcomes],
    function(x) all(is.na(x) | (x >= 0 & x <= 100))
  )),
  all(is.na(career_readiness_panel$cohort_size) |
        career_readiness_panel$cohort_size > 0)
)

# Summarize school coverage and missing readiness by year
print(
  career_readiness_panel |>
    group_by(academic_year) |>
    summarise(
      schools = n(),
      missing_readiness = sum(is.na(overall_readiness)),
      .groups = "drop"
    )
)


# STEP 5: Save the dataset

dir.create("data/processed", showWarnings = FALSE)

write.csv(
  career_readiness_panel,
  "data/processed/career_readiness_panel.csv",
  row.names = FALSE
)

cat("\nCareer-readiness panel saved successfully!\n")
cat("Observations:", nrow(career_readiness_panel), "\n")
cat("Variables:", ncol(career_readiness_panel), "\n")