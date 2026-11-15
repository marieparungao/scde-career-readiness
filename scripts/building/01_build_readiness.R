# ============================================================
# 01: BUILD CAREER READINESS PANEL
# ============================================================
# Purpose: Combine career-readiness data from four academic
# years into one school-level panel dataset.
#
# Input: Report Card researcher workbooks (2022-23 to 2025-26)
# Output: data/processed/career_readiness_panel.csv
# ============================================================


# STEP 1: Identify our academic years

years <- c("2022-23", "2023-24", "2024-25", "2025-26")


# STEP 2: Create a function to clean one year of data

clean_readiness <- function(year) {
  
  # Locate and import the year's Excel workbook
  file <- paste0(
    "data/raw/ReportCardData_forResearchers",
    year, ".xlsx"
  )
  
  raw <- readxl::read_excel(
    file,
    sheet = "4.CollegeCareer",
    col_types = "text"
  )
  
  # Keep high schools and select the variables we need
  clean <- raw |>
    dplyr::filter(SCHOOLTYPECD == "H") |>
    dplyr::transmute(
      academic_year = year,
      report_card_year = as.integer(ReportCardYear),
      district_name = DistrictNm,
      school_name = SchoolNm,
      school_id = SCHOOLID,
      cohort_size = NumInCohort,
      overall_readiness = PctCareer,
      cte_readiness = PctCTE,
      wbl_readiness = PctWBL,
      assessment_readiness = PctCarRdy,
      asvab_readiness = PctASVAB,
      credential_readiness = PctCred
    )
  
  # Identify variables that should contain numbers
  numeric_variables <- c(
    "cohort_size",
    "overall_readiness",
    "cte_readiness",
    "wbl_readiness",
    "assessment_readiness",
    "asvab_readiness",
    "credential_readiness"
  )
  
  # Convert numerical variables and preserve "*" as missing
  for (variable in numeric_variables) {
    
    values <- trimws(clean[[variable]])
    values[values %in% c("*", "")] <- NA_character_
    
    # Stop if we encounter any other unexpected text codes
    unexpected <- unique(values[
      !is.na(values) &
        is.na(suppressWarnings(as.numeric(values)))
    ])
    
    if (length(unexpected) > 0) {
      stop("Unexpected values in ", year, " / ",
           variable, ": ", paste(unexpected, collapse = ", "))
    }
    
    clean[[variable]] <- as.numeric(values)
  }
  
  # Check that each school appears only once in this year
  if (anyNA(clean$school_id) ||
      anyDuplicated(clean$school_id) > 0) {
    stop("Missing or duplicate school IDs in ", year)
  }
  
  # Display how many high schools were retained
  cat(year, ":", nrow(clean), "high schools\n")
  
  return(clean)
}


# STEP 3: Apply the function to all four years

career_readiness_panel <- years |>
  lapply(clean_readiness) |>
  dplyr::bind_rows()


# STEP 4: Check and save the combined dataset

# Display total rows and columns
print(dim(career_readiness_panel))

# Confirm each school-year combination is unique
stopifnot(!anyDuplicated(
  career_readiness_panel[c("school_id", "academic_year")]
))

# Save the cleaned panel
write.csv(
  career_readiness_panel,
  "data/processed/career_readiness_panel.csv",
  row.names = FALSE
)

cat("Career-readiness panel saved successfully!\n")

# STEP 5: Validate the combined dataset

# Identify the six career-readiness outcomes
outcomes <- c(
  "overall_readiness",
  "cte_readiness",
  "wbl_readiness",
  "assessment_readiness",
  "asvab_readiness",
  "credential_readiness"
)

# Count missing values in each numerical variable
cat("\nMissing values:\n")
print(sapply(
  career_readiness_panel[c("cohort_size", outcomes)],
  function(x) sum(is.na(x))
))

# Check for percentages outside the valid 0-100 range
cat("\nInvalid percentages:\n")
print(sapply(
  career_readiness_panel[outcomes],
  function(x) sum(x < 0 | x > 100, na.rm = TRUE)
))

# Check for invalid graduation-cohort sizes
cat("\nNonpositive cohort sizes:\n")
print(sum(
  career_readiness_panel$cohort_size <= 0,
  na.rm = TRUE
))

# DATA NOTES:
# 14 school-year observations have missing cohort and
# career-readiness information across all seven measures.
# These observations are retained with NA values.
# The meaning of SCDE's "*" code still requires verification.
