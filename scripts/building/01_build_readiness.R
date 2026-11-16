# ============================================================
# 01: BUILD CAREER READINESS PANEL
# ============================================================
# Purpose: Combine career-readiness data from four academic
# years into one school-level panel dataset.
#
# Input: Report Card researcher workbooks (2022-23 to 2025-26)
# Output: data/processed/career_readiness_panel.csv
# ============================================================


# STEP 1: Match academic years to the actual workbooks

input_files <- data.frame(
  academic_year = c("2021-22", "2022-23", "2023-24"),
  
  filename = c(
    "ReportCardData_forResearchers2022.XLSx",
    "ReportCardData_forResearchers2023_3.XLSx",
    "ReportCardData_forResearchers2024_11.xlsx"
  )
)

years <- input_files$academic_year


# STEP 2: Create a function to clean one year of data

clean_readiness <- function(year) {
  
  # Find the actual workbook assigned to this academic year
  filename <- input_files$filename[
    input_files$academic_year == year
  ]
  
  available_files <- list.files(
    "data/raw",
    recursive = TRUE,
    full.names = TRUE
  )
  
  file <- available_files[
    tolower(basename(available_files)) == tolower(filename)
  ]
  
  # Confirm exactly one matching workbook exists
  stopifnot(length(file) == 1)
  
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
      school_id = sprintf("%07d", as.integer(SCHOOLID)),
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
# In the currently uploaded workbooks, 16 school-year records
# have "*" in cohort size and all six readiness outcomes.
# These records are retained with NA values.
# The interpretation of "*" should be confirmed in the
# source documentation; it is not treated as zero.