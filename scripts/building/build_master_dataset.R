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

# STEP 1: Import the two completed datasets

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
    by = c("school_id", "academic_year")
  )

# Verify that the merge preserved our readiness sample
cat("Master dataset observations:", nrow(master_dataset), "\n")
cat("Master dataset variables:", ncol(master_dataset), "\n")

# Count observations without matching headcount information
cat("Missing headcount matches:",
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
  "hispanic_share"
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
# NOTES FOR FUTURE DATA CONSTRUCTION
# ============================================================
# This is a preliminary master dataset.
#
# Current data:
# - Six career-readiness outcomes and graduation cohort size
# - School enrollment, poverty, gender, and race/ethnicity
#
# Missing data:
# - 14 observations have missing career-readiness information
# - 8 observations have no matching headcount information
# - These groups do not overlap
#
# TODO:
# - Merge geographic characteristics (Script 03)
# - Merge academic achievement (Script 04)
# - Merge chronic absenteeism (Script 05)
# - Merge disability status (Script 06)
# - Recheck missing values and school coverage
# ============================================================