# ============================================================
# BUILD MASTER CAREER READINESS DATASET
# ============================================================
# Purpose: Merge all school-level datasets into one
# analysis-ready panel covering four academic years.
#
# Inputs: Cleaned datasets in data/processed/
# Output: data/processed/career_readiness_master.csv
#
# ============================================================

# STEP 1: Import the six completed datasets

# Read the career-readiness panel (our base dataset)
readiness <- read.csv(
  "data/processed/career_readiness_panel.csv",
  colClasses = c(school_id = "character")
)

# Read the school-characteristics panel
characteristics <- read.csv(
  "data/processed/school_characteristics_panel.csv",
  colClasses = c(school_id = "character")
)

# Read the academic achievement panel
academic <- read.csv(
  "data/processed/academic_panel.csv",
  colClasses = c(school_id = "character")
)

# Read the absenteeism panel
absenteeism <- read.csv(
  "data/processed/absenteeism_panel.csv",
  colClasses = c(school_id = "character")
)

# Read the disability panel
disability <- read.csv(
  "data/processed/disability_panel.csv",
  colClasses = c(school_id = "character")
)

geography <- read.csv(
  "data/processed/geography_panel.csv",
  colClasses = c(
    school_id = "character",
    academic_year = "character",
    nces_school_id = "character"
  ),
  na.strings = c("", "NA")
)

# Restrict every panel to the four-year study period
study_years <- c("2022-23", "2023-24", "2024-25", "2025-26")

readiness <- dplyr::filter(readiness, academic_year %in% study_years)
characteristics <- dplyr::filter(characteristics, academic_year %in% study_years)
academic <- dplyr::filter(academic, academic_year %in% study_years)
absenteeism <- dplyr::filter(absenteeism, academic_year %in% study_years)
disability <- dplyr::filter(disability, academic_year %in% study_years)
geography <- dplyr::filter(geography, academic_year %in% study_years)

# Confirm all four years are present in the readiness base
stopifnot(setequal(unique(readiness$academic_year), study_years))

print(dplyr::count(readiness, academic_year))

# Check that all panels have valid, unique school-year identifiers
panels <- list(
  readiness = readiness,
  characteristics = characteristics,
  academic = academic,
  absenteeism = absenteeism,
  disability = disability,
  geography = geography
)

for (panel_name in names(panels)) {
  panel <- panels[[panel_name]]
  
  stopifnot(
    !anyNA(panel[c("school_id", "academic_year")]),
    all(trimws(as.character(panel$school_id)) != ""),
    all(trimws(as.character(panel$academic_year)) != ""),
    !anyDuplicated(panel[c("school_id", "academic_year")])
  )
}

# STEP 2: Check how many school-year observations match

# Find readiness observations with a matching characteristics record
matched <- dplyr::inner_join(
  readiness,
  characteristics,
  by = c("school_id", "academic_year")
)

# Find readiness observations without a matching record
unmatched <- dplyr::anti_join(
  readiness,
  characteristics,
  by = c("school_id", "academic_year")
)

# Display the results
cat("Readiness observations:", nrow(readiness), "\n")
cat("Matched observations:", nrow(matched), "\n")
cat("Unmatched observations:", nrow(unmatched), "\n")

# Display unmatched schools, if any
print(
  unmatched[, c("academic_year", "school_id", "school_name")],
  row.names = FALSE
)

# STEP 3: Merge career readiness and school characteristics

# Confirm school-year identifiers are unique in both datasets
stopifnot(
  !anyDuplicated(readiness[c("school_id", "academic_year")]),
  !anyDuplicated(characteristics[c("school_id", "academic_year")])
)

# Merge school characteristics into our readiness panel
master_dataset <- readiness |>
  dplyr::left_join(
    characteristics |>
      dplyr::select(-district_name, -school_name),
    by = c("school_id", "academic_year"),
    relationship = "one-to-one"
  )

# Confirm that the new panels have unique, nonmissing keys
for (panel in list(academic, absenteeism, disability)) {
  
  stopifnot(
    !anyNA(panel[c("school_id", "academic_year")]),
    
    !anyDuplicated(
      panel[c("school_id", "academic_year")]
    )
  )
}

# Add academic achievement
master_dataset <- master_dataset |>
  dplyr::left_join(
    academic |>
      dplyr::select(
        school_id,
        academic_year,
        academic_achievement_points_pct
      ),
    by = c("school_id", "academic_year"),
    relationship = "one-to-one"
  )

# Add chronic absenteeism
master_dataset <- master_dataset |>
  dplyr::left_join(
    absenteeism |>
      dplyr::select(
        school_id,
        academic_year,
        chronic_absenteeism_pct
      ),
    by = c("school_id", "academic_year"),
    relationship = "one-to-one"
  )

# Add the disability-composition proxy
master_dataset <- master_dataset |>
  dplyr::left_join(
    disability |>
      dplyr::select(
        school_id,
        academic_year,
        disability_share_attendance_pct
      ),
    by = c("school_id", "academic_year"),
    relationship = "one-to-one"
  )

# Check geographic merge keys
stopifnot(
  !anyNA(geography[c("school_id", "academic_year")]),
  
  !anyDuplicated(
    geography[c("school_id", "academic_year")]
  )
)

# Add historical geographic characteristics
master_dataset <- master_dataset |>
  dplyr::left_join(
    geography |>
      dplyr::select(
        school_id,
        academic_year,
        nces_school_id,
        locale_code,
        locale_type,
        locale_source_year
      ),
    by = c("school_id", "academic_year"),
    relationship = "one-to-one"
  )

# Verify that the merge preserved our readiness sample
cat("Master dataset observations:", nrow(master_dataset), "\n")
cat("Master dataset variables:", ncol(master_dataset), "\n")

# Count observations with missing enrollment information
cat("Missing enrollment values:",
    sum(is.na(master_dataset$enrollment)), "\n")

# Confirm that no observations were added or lost
stopifnot(nrow(master_dataset) == nrow(readiness))

# STEP 4: Check missing values in the master dataset

# Count missing values in our key variables
key_variables <- c(
  "cohort_size",
  "overall_readiness",
  "cte_readiness",
  "wbl_readiness",
  "assessment_readiness",
  "asvab_readiness",
  "credential_readiness",
  "enrollment",
  "poverty_rate",
  "female_share",
  "black_share",
  "hispanic_share",
  "academic_achievement_points_pct",
  "chronic_absenteeism_pct",
  "disability_share_attendance_pct",
  "locale_type"
)

cat("\nMissing values in key variables:\n")

print(sapply(
  master_dataset[key_variables],
  function(x) sum(is.na(x))
))

# Check whether missing readiness and characteristics
# information occur in the same school-year observations
cat("\nMissing both readiness and poverty information:",
    sum(
      is.na(master_dataset$overall_readiness) &
        is.na(master_dataset$poverty_rate)
    ), "\n")

# Confirm the master contains exactly the four study years
stopifnot(
  setequal(unique(master_dataset$academic_year), study_years)
)

# Show observations and schools in each year
cat("\nSample coverage by year:\n")

coverage_by_year <- master_dataset |>
  dplyr::group_by(academic_year) |>
  dplyr::summarise(
    observations = dplyr::n(),
    schools = dplyr::n_distinct(school_id),
    .groups = "drop"
  )

print(coverage_by_year)

# Show missing values separately for each year
cat("\nMissing values by year:\n")

missing_by_year <- master_dataset |>
  dplyr::group_by(academic_year) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(key_variables),
      ~ sum(is.na(.x))
    ),
    .groups = "drop"
  )

print(missing_by_year, width = Inf)

# Show how many years each school contributes
cat("\nNumber of years observed per school:\n")

master_dataset |>
  dplyr::count(school_id, name = "years_observed") |>
  dplyr::count(years_observed, name = "number_of_schools") |>
  print()

# Confirm expected numeric variables were imported as numeric
expected_numeric <- setdiff(key_variables, "locale_type")

nonnumeric_variables <- expected_numeric[
  !vapply(
    master_dataset[expected_numeric],
    is.numeric,
    logical(1)
  )
]

if (length(nonnumeric_variables) > 0) {
  stop(
    "Review these columns before analysis; they are not numeric: ",
    paste(nonnumeric_variables, collapse = ", ")
  )
}
# Inspect numeric ranges for unexpected values or scales
cat("\nNumeric variable checks:\n")

numeric_variables <- key_variables[
  vapply(
    master_dataset[key_variables],
    is.numeric,
    logical(1)
  )
]

print(summary(master_dataset[numeric_variables]))

# STEP 5: Save the preliminary master dataset

# Confirm that school-year observations remain unique
stopifnot(
  !anyDuplicated(
    master_dataset[c("school_id", "academic_year")]
  )
)

# Save the combined dataset
write.csv(
  master_dataset,
  "data/processed/career_readiness_master.csv",
  row.names = FALSE
)

# Confirm successful completion
cat("\nMaster dataset saved successfully!\n")
cat("Observations:", nrow(master_dataset), "\n")
cat("Variables:", ncol(master_dataset), "\n")

# ============================================================
# MASTER DATASET NOTES
# ============================================================
# Study period: 2022-23 through 2025-26.
# Unit of observation: one school in one academic year.
#
# Included data:
# - Six career-readiness outcomes and graduation cohort size
# - Enrollment, poverty, gender, and race/ethnicity
# - Geographic locale and locale source year
# - Academic achievement points percentage
# - Chronic absenteeism percentage
# - Disability-composition proxy
#
# Measurement notes:
# - Disability share uses the absenteeism reporting population,
#   not the graduation cohort.
# - Career-readiness pathways overlap; their percentages
#   should not be added together.
#
# Checks to review before analysis:
# - School coverage and missing values by academic year
# - Numeric ranges and percentage scales
# - Academic achievement comparability across years
# - Geographic source years, including carried-forward locales
#
# Use the current script output for missing-data counts.
# ============================================================