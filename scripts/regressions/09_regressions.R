# ============================================================
# CAREER-READINESS REGRESSIONS
# Academic years: 2022-23 through 2025-26
# ============================================================

library(fixest)

# STEP 1: Import the master dataset
master <- read.csv(
  "data/processed/career_readiness_master.csv",
  colClasses = c(
    school_id = "character",
    academic_year = "character"
  )
)

# Verify the study period and identifiers
study_years <- c(
  "2022-23", "2023-24", "2024-25", "2025-26"
)

stopifnot(
  setequal(unique(master$academic_year), study_years),
  !anyNA(master[c("school_id", "academic_year")]),
  !anyDuplicated(master[c("school_id", "academic_year")])
)

# Treat geographic locale as a category
master$locale_type <- factor(master$locale_type)

# The first category is the reference category
cat("\nLocale categories; first is the reference:\n")
print(levels(master$locale_type))


# STEP 2: Identify the outcomes
outcomes <- c(
  "overall_readiness",
  "cte_readiness",
  "wbl_readiness",
  "assessment_readiness",
  "asvab_readiness",
  "credential_readiness"
)

# Identify the explanatory variables
controls <- c(
  "poverty_rate",
  "female_share",
  "black_share",
  "hispanic_share",
  "locale_type",
  "academic_achievement_points_pct",
  "chronic_absenteeism_pct",
  "disability_share_attendance_pct",
  "cohort_size"
)


# STEP 3: Estimate the six regressions
models <- list()

for (outcome in outcomes) {
  
  # Example structure:
  # readiness ~ school characteristics | academic_year
  formula <- as.formula(
    paste(
      outcome,
      "~",
      paste(controls, collapse = " + "),
      "| academic_year"
    )
  )
  
  models[[outcome]] <- feols(
    formula,
    data = master,
    vcov = ~ school_id
  )
}


# STEP 4: Display results

# Detailed results for overall readiness
summary(models[["overall_readiness"]])

# 95% confidence intervals for overall readiness
confint(models[["overall_readiness"]])

# Compare all six models
etable(models)

# Report sample sizes
cat("\nObservations used in each model:\n")
print(sapply(models, nobs))


# STEP 5: Save the results
dir.create(
  "outputs/regressions",
  recursive = TRUE,
  showWarnings = FALSE
)

# Save a readable table
writeLines(
  capture.output(etable(models)),
  "outputs/regressions/year_effects_results.txt"
)

# Save fitted models for later analysis
saveRDS(
  models,
  "outputs/regressions/year_effects_models.rds"
)

cat("\nRegression results saved in outputs/regressions/\n")
# ============================================================
# SCHOOL AND YEAR FIXED-EFFECTS MODELS
# ============================================================

# Remove locale: a characteristic that stays constant within
# a school cannot be estimated separately from school effects.
within_controls <- setdiff(controls, "locale_type")

models_school_fe <- list()

for (outcome in outcomes) {
  
  # Start with the same complete-data sample as the
  # corresponding original regression.
  required_variables <- c(
    outcome, controls, "school_id", "academic_year"
  )
  
  model_data <- master[
    complete.cases(master[required_variables]),
  ]
  
  model_formula <- as.formula(
    paste(
      outcome,
      "~",
      paste(within_controls, collapse = " + "),
      "| school_id + academic_year"
    )
  )
  
  models_school_fe[[outcome]] <- feols(
    model_formula,
    data = model_data,
    vcov = ~ school_id
  )
}

# Display all six school fixed-effects models
etable(models_school_fe)

# Compare the two specifications for overall readiness
etable(
  models[["overall_readiness"]],
  models_school_fe[["overall_readiness"]],
  headers = c("Year effects", "School + year effects")
)

# Display detailed overall-readiness results
summary(models_school_fe[["overall_readiness"]])

# Report observations used
sapply(models_school_fe, nobs)

# Save the new results
writeLines(
  capture.output(etable(models_school_fe)),
  "outputs/regressions/school_year_effects_results.txt"
)

saveRDS(
  models_school_fe,
  "outputs/regressions/school_year_effects_models.rds"
)
# ============================================================
# SENSITIVITY CHECK: EXCLUDE COHORTS BELOW 30
# ============================================================

models_cohort30 <- list()

for (outcome in outcomes) {
  
  required_variables <- c(
    outcome, controls, "school_id", "academic_year"
  )
  
  model_data <- master[
    complete.cases(master[required_variables]),
  ]
  
  model_data <- model_data[
    model_data$cohort_size >= 30,
  ]
  
  model_formula <- as.formula(
    paste(
      outcome,
      "~",
      paste(within_controls, collapse = " + "),
      "| school_id + academic_year"
    )
  )
  
  models_cohort30[[outcome]] <- feols(
    model_formula,
    data = model_data,
    vcov = ~ school_id
  )
}

# Print the overall-readiness comparison
print(etable(
  models_school_fe[["overall_readiness"]],
  models_cohort30[["overall_readiness"]],
  headers = c("Original sample", "Cohorts >= 30")
))

# Print the CTE-readiness comparison
print(etable(
  models_school_fe[["cte_readiness"]],
  models_cohort30[["cte_readiness"]],
  headers = c("Original sample", "Cohorts >= 30")
))

# Save the sensitivity results
writeLines(
  capture.output(etable(models_cohort30)),
  "outputs/regressions/cohort30_sensitivity.txt"
)
# ============================================================
# COMPARE INCLUDED AND EXCLUDED SCHOOL-YEAR OBSERVATIONS
# Based on the overall-readiness model with year effects
# ============================================================

required_variables <- c(
  "overall_readiness",
  controls,
  "school_id",
  "academic_year"
)

sample_check <- master

sample_check$regression_sample <- ifelse(
  complete.cases(sample_check[required_variables]),
  "Included",
  "Excluded: missing data"
)

# Count included and excluded observations by year
sample_by_year <- sample_check |>
  dplyr::count(academic_year, regression_sample)

print(sample_by_year)

# Compare available school characteristics
comparison_variables <- c(
  "overall_readiness",
  "cohort_size",
  "enrollment",
  "poverty_rate",
  "academic_achievement_points_pct",
  "chronic_absenteeism_pct",
  "disability_share_attendance_pct"
)

sample_comparison <- sample_check |>
  dplyr::group_by(regression_sample) |>
  dplyr::summarise(
    observations = dplyr::n(),
    dplyr::across(
      dplyr::all_of(comparison_variables),
      list(
        available = ~ sum(!is.na(.x)),
        mean = ~ if (all(is.na(.x))) NA_real_
        else mean(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

print(sample_comparison, width = Inf)

# Compare geographic composition
locale_comparison <- sample_check |>
  dplyr::count(regression_sample, locale_type) |>
  dplyr::group_by(regression_sample) |>
  dplyr::mutate(percent = 100 * n / sum(n)) |>
  dplyr::ungroup()

print(locale_comparison)

# Save the checks
write.csv(
  sample_by_year,
  "outputs/regressions/sample_inclusion_by_year.csv",
  row.names = FALSE
)

write.csv(
  sample_comparison,
  "outputs/regressions/included_excluded_comparison.csv",
  row.names = FALSE
)

write.csv(
  locale_comparison,
  "outputs/regressions/included_excluded_locales.csv",
  row.names = FALSE
)