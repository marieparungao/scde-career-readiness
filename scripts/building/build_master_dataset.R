# ============================================================
# BUILD MASTER CAREER READINESS DATASET (RUN ALL)
# ============================================================
# Run from the scde-career-readiness project root.
# Rebuilds source manifest, six panels, and the master dataset.
# Output: data/processed/career_readiness_master.csv
# ============================================================

library(readxl)
library(dplyr)
options(timeout = max(600, getOption("timeout")))


# STEP 1: Run source validation and all six construction scripts

script_dir <- "scripts/building"

scripts <- c(
  "00_download_data.R",
  "01_build_readiness.R",
  "02_build_characteristics.R",
  "03_build_geography.R",
  "04_build_academic.R",
  "05_build_absenteeism.R",
  "06_build_disability.R"
)

paths <- file.path(script_dir, scripts)

if (!all(file.exists(paths))) {
  stop(
    "Missing construction scripts: ",
    paste(paths[!file.exists(paths)], collapse = ", "),
    ". Run this file from the project root."
  )
}

for (script in paths) {
  cat("\n=== Running", basename(script), "===\n")
  source(script, local = new.env(parent = globalenv()))
}

# STEP 2: Import the completed panels

read_panel <- function(filename) {
  read.csv(
    file.path("data/processed", filename),
    colClasses = c(school_id = "character")
  )
}

readiness <- read_panel("career_readiness_panel.csv")
characteristics <- read_panel("school_characteristics_panel.csv")
academic <- read_panel("academic_panel.csv")
absenteeism <- read_panel("absenteeism_panel.csv")
disability <- read_panel("disability_panel.csv")

geography <- read.csv(
  "data/processed/geography_panel.csv",
  colClasses = c(
    school_id = "character",
    nces_school_id = "character"
  )
)

panels <- list(
  readiness = readiness,
  characteristics = characteristics,
  academic = academic,
  absenteeism = absenteeism,
  disability = disability,
  geography = geography
)

keys <- c("school_id", "academic_year")
expected_years <- c("2022-23", "2023-24", "2024-25", "2025-26")

# Validate school-year identifiers in every panel
for (name in names(panels)) {
  panel <- panels[[name]]
  
  stopifnot(
    !anyNA(panel[keys]),
    !anyDuplicated(panel[keys]),
    all(grepl("^[0-9]{7}$", panel$school_id)),
    all(panel$academic_year %in% expected_years)
  )
}

stopifnot(
  setequal(unique(readiness$academic_year), expected_years)
)


# STEP 3: Merge all panels, preserving the readiness sample

master_dataset <- readiness |>
  left_join(
    select(characteristics, -district_name, -school_name),
    by = keys,
    relationship = "one-to-one"
  ) |>
  left_join(
    select(
      academic, all_of(keys),
      academic_achievement_points_pct
    ),
    by = keys,
    relationship = "one-to-one"
  ) |>
  left_join(
    select(
      absenteeism, all_of(keys),
      absenteeism_data_year,
      chronic_absenteeism_pct
    ),
    by = keys,
    relationship = "one-to-one"
  ) |>
  left_join(
    select(
      disability, all_of(keys),
      disability_data_year,
      disability_share_attendance_pct
    ),
    by = keys,
    relationship = "one-to-one"
  ) |>
  left_join(
    select(
      geography, all_of(keys),
      nces_school_id,
      locale_code,
      locale_type,
      locale_source_year,
      locale_carried_forward
    ),
    by = keys,
    relationship = "one-to-one"
  )

# Confirm no schools were added, removed, or duplicated
stopifnot(
  nrow(master_dataset) == nrow(readiness),
  identical(master_dataset[keys], readiness[keys]),
  !anyDuplicated(master_dataset[keys])
)


# STEP 4: Report school coverage and missing values

cat("\n=== Master dataset checks ===\n")
cat("Readiness observations retained:", nrow(master_dataset), "\n")
cat("Master variables:", ncol(master_dataset), "\n")

# Count readiness observations without matching source records
cat("\nReadiness observations without source-panel records:\n")

for (name in setdiff(names(panels), "readiness")) {
  missing_matches <- nrow(
    anti_join(readiness, panels[[name]], by = keys)
  )
  
  cat(name, ": ", missing_matches, "\n", sep = "")
}

# Check missing values in key analysis variables
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

cat("\nMissing values in analysis variables:\n")

print(sapply(
  master_dataset[key_variables],
  function(x) sum(is.na(x))
))


# STEP 5: Save the completed master dataset

write.csv(
  master_dataset,
  "data/processed/career_readiness_master.csv",
  row.names = FALSE
)

cat("\nMaster dataset saved successfully!\n")
cat("Output: data/processed/career_readiness_master.csv\n")