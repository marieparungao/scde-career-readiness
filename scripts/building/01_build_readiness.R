# ============================================================
# 01: BUILD CAREER READINESS PANEL
# ============================================================
# Purpose: Combine four years of school-level career-readiness
# outcomes and graduation cohort sizes into one panel.
#
# Input: ReportCardData_forResearchersYYYY-YY.xlsx
# Output: data/processed/career_readiness_panel.csv
#
# Academic years: 2022-23 through 2025-26
# ============================================================


# STEP 1: Define academic years and outcomes

years <- c("2022-23", "2023-24", "2024-25", "2025-26")

outcomes <- c(
  "overall_readiness",
  "cte_readiness",
  "wbl_readiness",
  "assessment_readiness",
  "asvab_readiness",
  "credential_readiness"
)


# STEP 2: Create a function to clean one year

clean_readiness <- function(year) {
  
  # Locate the standardized workbook from Script 00
  file <- file.path(
    "data/raw",
    paste0("ReportCardData_forResearchers", year, ".xlsx")
  )
  
  if (!file.exists(file)) {
    stop("Missing file: ", file, ". Run Script 00 first.")
  }
  
  raw <- readxl::read_excel(
    file,
    sheet = "4.CollegeCareer",
    col_types = "text"
  )
  
  # Keep only high-school records
  raw <- raw |>
    dplyr::filter(SCHOOLTYPECD == "H")
  
  # Verify school IDs before converting them
  if (anyNA(raw$SCHOOLID) ||
      any(!grepl("^[0-9]{6,7}$", trimws(raw$SCHOOLID)))) {
    stop("Invalid school IDs in ", year)
  }
  
  # Select and standardize the variables
  clean <- raw |>
    dplyr::transmute(
      academic_year = year,
      report_card_year = as.integer(ReportCardYear),
      district_name = gsub("[[:space:]]+", " ", DistrictNm),
      school_name = gsub("[[:space:]]+", " ", SchoolNm),
      school_id = sprintf("%07d", as.integer(SCHOOLID)),
      cohort_size = NumInCohort,
      overall_readiness = PctCareer,
      cte_readiness = PctCTE,
      wbl_readiness = PctWBL,
      assessment_readiness = PctCarRdy,
      asvab_readiness = PctASVAB,
      credential_readiness = PctCred
    )
  
  # Check that the report-card year is correct
  expected_year <- as.integer(substr(year, 1, 4)) + 1L
  
  if (anyNA(clean$report_card_year) ||
      any(clean$report_card_year != expected_year)) {
    stop("Incorrect report-card year in ", year)
  }
  
  # Convert numeric variables; preserve suppressed values as NA
  numeric_variables <- c("cohort_size", outcomes)
  
  for (variable in numeric_variables) {
    
    values <- trimws(clean[[variable]])
    values[values %in% c("*", "")] <- NA_character_
    
    # Stop if unexpected nonnumeric values appear
    invalid <- unique(values[
      !is.na(values) &
        is.na(suppressWarnings(as.numeric(values)))
    ])
    
    if (length(invalid) > 0) {
      stop("Unexpected values in ", year, " / ", variable)
    }
    
    clean[[variable]] <- as.numeric(values)
  }
  
  # Verify one record per school per year
  stopifnot(
    !anyNA(clean$school_id),
    !anyDuplicated(clean$school_id)
  )
  
  cat(year, ":", nrow(clean), "high schools\n")
  
  return(clean)
}


# STEP 3: Build the four-year panel

career_readiness_panel <- years |>
  lapply(clean_readiness) |>
  dplyr::bind_rows()


# STEP 4: Validate the completed panel

# Confirm unique school-year observations
stopifnot(
  !anyDuplicated(
    career_readiness_panel[c("school_id", "academic_year")]
  )
)

# Check for invalid percentages and cohort sizes
invalid_percentages <- sapply(
  career_readiness_panel[outcomes],
  function(x) sum(x < 0 | x > 100, na.rm = TRUE)
)

stopifnot(
  all(invalid_percentages == 0),
  sum(career_readiness_panel$cohort_size <= 0,
      na.rm = TRUE) == 0
)

# Summarize observations and missing outcomes by year
print(
  career_readiness_panel |>
    dplyr::group_by(academic_year) |>
    dplyr::summarise(
      schools = dplyr::n(),
      missing_readiness = sum(is.na(overall_readiness)),
      .groups = "drop"
    )
)

cat("\nMissing values by variable:\n")
print(sapply(
  career_readiness_panel[c("cohort_size", outcomes)],
  function(x) sum(is.na(x))
))


# STEP 5: Save the panel

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  career_readiness_panel,
  "data/processed/career_readiness_panel.csv",
  row.names = FALSE
)

cat("\nCareer-readiness panel saved successfully!\n")
cat("Observations:", nrow(career_readiness_panel), "\n")
cat("Variables:", ncol(career_readiness_panel), "\n")