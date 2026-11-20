# ============================================================
# 09: CAREER-READINESS REGRESSION ANALYSIS
# ============================================================
# Project: SCDE Career Readiness
# Academic years: 2022-23 through 2025-26
#
# Purpose:
# Examine associations between school characteristics and
# six career-readiness outcomes across SC public high schools.
#
# Main models: Year fixed effects only
# Standard errors: Clustered by school
#
# Outputs:
# - Regression tables
# - Small-cohort sensitivity checks
# - Missing-data diagnostics
# - Coefficient tables and plots by characteristic
# - Regression significance heatmap
#
# Run from the scde-career-readiness project root.
# ============================================================


# ============================================================
# STEP 1: LOAD PACKAGES AND DATA
# ============================================================

library(fixest)
library(dplyr)
library(ggplot2)
library(tidyr)

# Create output folders
dir.create(
  "outputs/regressions",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "outputs/regressions/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

# Import master dataset
master <- read.csv(
  "data/processed/career_readiness_master.csv",
  colClasses = c(
    school_id = "character",
    academic_year = "character"
  )
)

# Verify study period and unique identifiers
study_years <- c(
  "2022-23",
  "2023-24",
  "2024-25",
  "2025-26"
)

stopifnot(
  setequal(unique(master$academic_year), study_years),
  !anyNA(master[c("school_id", "academic_year")]),
  !anyDuplicated(master[c("school_id", "academic_year")])
)

# Set City as the reference category
master$locale_type <- factor(
  master$locale_type,
  levels = c("City", "Rural", "Suburb", "Town")
)

cat("\nGeographic locale reference category: City\n")


# ============================================================
# STEP 2: DEFINE OUTCOMES AND SCHOOL CHARACTERISTICS
# ============================================================

# Dependent variables
outcomes <- c(
  "overall_readiness",
  "cte_readiness",
  "wbl_readiness",
  "assessment_readiness",
  "asvab_readiness",
  "credential_readiness"
)

# Independent variables
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


# ============================================================
# STEP 3: ESTIMATE SIX YEAR FIXED-EFFECTS REGRESSIONS
# ============================================================

models <- list()

for (outcome in outcomes) {
  
  model_formula <- as.formula(
    paste(
      outcome,
      "~",
      paste(controls, collapse = " + "),
      "| academic_year"
    )
  )
  
  models[[outcome]] <- feols(
    model_formula,
    data = master,
    vcov = ~ school_id
  )
}

cat("\n=== YEAR FIXED-EFFECTS REGRESSIONS ===\n")

# Display all six models
print(etable(models))

# Display detailed overall-readiness model
print(summary(models[["overall_readiness"]]))

# Show sample sizes
cat("\nObservations used in each model:\n")
print(sapply(models, nobs))

# Save regression table
writeLines(
  capture.output(etable(models)),
  "outputs/regressions/year_effects_results.txt"
)

# Save fitted models
saveRDS(
  models,
  "outputs/regressions/year_effects_models.rds"
)

# Save HTML table
etable(
  models,
  tex = FALSE,
  file = "outputs/regressions/year_effects_table.html"
)


# ============================================================
# STEP 4: SENSITIVITY CHECK - EXCLUDE COHORTS BELOW 30
# ============================================================

# This checks whether results change when excluding
# schools with very small graduation cohorts.

models_cohort30 <- list()

for (outcome in outcomes) {
  
  required_variables <- c(
    outcome,
    controls,
    "school_id",
    "academic_year"
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
      paste(controls, collapse = " + "),
      "| academic_year"
    )
  )
  
  models_cohort30[[outcome]] <- feols(
    model_formula,
    data = model_data,
    vcov = ~ school_id
  )
}

cat("\n=== COHORT-SIZE SENSITIVITY CHECK ===\n")

# Compare overall readiness
print(etable(
  models[["overall_readiness"]],
  models_cohort30[["overall_readiness"]],
  headers = c("Full sample", "Cohorts >= 30")
))

# Compare CTE readiness
print(etable(
  models[["cte_readiness"]],
  models_cohort30[["cte_readiness"]],
  headers = c("Full sample", "Cohorts >= 30")
))

# Save sensitivity results
writeLines(
  capture.output(etable(models_cohort30)),
  "outputs/regressions/cohort30_sensitivity.txt"
)

saveRDS(
  models_cohort30,
  "outputs/regressions/cohort30_models.rds"
)


# ============================================================
# STEP 5: EXAMINE MISSING DATA AND SAMPLE EXCLUSIONS
# ============================================================

# Define variables needed for the overall-readiness model
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
  count(academic_year, regression_sample)

cat("\n=== REGRESSION SAMPLE BY YEAR ===\n")
print(sample_by_year)

# Compare characteristics of included and excluded observations
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
  group_by(regression_sample) |>
  summarise(
    observations = n(),
    across(
      all_of(comparison_variables),
      list(
        available = ~ sum(!is.na(.x)),
        mean = ~ if (all(is.na(.x))) NA_real_
        else mean(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

cat("\n=== INCLUDED VS. EXCLUDED OBSERVATIONS ===\n")
print(sample_comparison, width = Inf)

# Compare geographic composition
locale_comparison <- sample_check |>
  count(regression_sample, locale_type) |>
  group_by(regression_sample) |>
  mutate(percent = 100 * n / sum(n)) |>
  ungroup()

print(locale_comparison)

# Save sample diagnostics
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


# ============================================================
# STEP 6: EXTRACT REGRESSION COEFFICIENTS
# ============================================================

# Convert regression output into a dataset for visualization

extract_results <- function(model_list) {
  
  bind_rows(lapply(names(model_list), function(outcome) {
    
    model <- model_list[[outcome]]
    ct <- as.data.frame(coeftable(model))
    
    # Obtain model-based confidence intervals
    ci <- confint(model)
    
    data.frame(
      outcome = outcome,
      characteristic = rownames(ct),
      estimate = ct[[1]],
      std_error = ct[[2]],
      p_value = ct[[4]],
      ci_low = ci[rownames(ct), 1],
      ci_high = ci[rownames(ct), 2],
      row.names = NULL
    )
  }))
}

results <- extract_results(models)


# ============================================================
# STEP 7: ADD READABLE VARIABLE LABELS
# ============================================================

outcome_labels <- c(
  overall_readiness = "Overall Readiness",
  cte_readiness = "CTE",
  wbl_readiness = "Work-Based Learning",
  assessment_readiness = "Assessment",
  asvab_readiness = "ASVAB",
  credential_readiness = "Employability Credential"
)

characteristic_labels <- c(
  poverty_rate = "Poverty Rate",
  female_share = "Female Student Share",
  black_share = "Black Student Share",
  hispanic_share = "Hispanic Student Share",
  locale_typeRural = "Rural vs. City",
  locale_typeSuburb = "Suburb vs. City",
  locale_typeTown = "Town vs. City",
  academic_achievement_points_pct = "Academic Achievement",
  chronic_absenteeism_pct = "Chronic Absenteeism",
  disability_share_attendance_pct = "Disability Composition",
  cohort_size = "Graduation Cohort Size"
)

results <- results |>
  mutate(
    outcome_label = unname(outcome_labels[outcome]),
    characteristic_label =
      unname(characteristic_labels[characteristic]),
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.10 ~ ".",
      TRUE ~ ""
    ),
    significant_5pct = p_value < 0.05
  )

# Check that all coefficients received labels
stopifnot(
  !anyNA(results$outcome_label),
  !anyNA(results$characteristic_label)
)

# Save complete coefficient results
write.csv(
  results,
  "outputs/regressions/all_coefficients.csv",
  row.names = FALSE
)


# ============================================================
# STEP 8: CREATE A TABLE AND FIGURE FOR EACH CHARACTERISTIC
# ============================================================

# Consistent ordering of readiness outcomes
outcome_order <- c(
  "Overall Readiness",
  "CTE",
  "Work-Based Learning",
  "Assessment",
  "ASVAB",
  "Employability Credential"
)

for (variable in unique(results$characteristic)) {
  
  subset_results <- results |>
    filter(characteristic == variable) |>
    mutate(
      outcome_label = factor(
        outcome_label,
        levels = rev(outcome_order)
      )
    )
  
  # ----------------------------------------------------------
  # A. Export a readable coefficient table
  # ----------------------------------------------------------
  
  characteristic_table <- subset_results |>
    transmute(
      Outcome = as.character(outcome_label),
      Coefficient = round(estimate, 4),
      `Standard Error` = round(std_error, 4),
      `p-value` = round(p_value, 4),
      Significance = significance
    )
  
  write.csv(
    characteristic_table,
    paste0(
      "outputs/regressions/figures/",
      variable,
      "_table.csv"
    ),
    row.names = FALSE
  )
  
  # ----------------------------------------------------------
  # B. Generate coefficient plot
  # ----------------------------------------------------------
  
  p <- ggplot(
    subset_results,
    aes(
      x = estimate,
      y = outcome_label
    )
  ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "gray50"
    ) +
    geom_segment(
      aes(
        x = ci_low,
        xend = ci_high,
        y = outcome_label,
        yend = outcome_label
      ),
      color = "#234B70",
      linewidth = 0.8
    ) +
    geom_point(
      aes(color = significant_5pct),
      size = 3
    ) +
    scale_color_manual(
      values = c(
        "FALSE" = "#8C96A0",
        "TRUE" = "#234B70"
      ),
      labels = c(
        "FALSE" = "Not significant",
        "TRUE" = "Significant (p < 0.05)"
      ),
      name = NULL
    ) +
    labs(
      title = characteristic_labels[[variable]],
      subtitle = "Year fixed-effects regression estimates",
      x = "Regression coefficient (95% CI)",
      y = NULL,
      caption = paste(
        "Year fixed effects included.",
        "Standard errors clustered by school."
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom"
    )
  
  ggsave(
    paste0(
      "outputs/regressions/figures/",
      variable,
      "_coefficients.png"
    ),
    plot = p,
    width = 9,
    height = 5,
    dpi = 300
  )
}

cat("\nCharacteristic tables and figures saved.\n")

# ============================================================
# STEP 9: CREATE IMPROVED REGRESSION HEATMAP
# ============================================================

# Instead of coloring by coefficient magnitude,
# classify each relationship by direction and significance.
#
# This avoids misleading comparisons between coefficients
# measured in different units.

heatmap_data <- results |>
  mutate(
    relationship = case_when(
      p_value < 0.05 & estimate > 0 ~
        "Significant positive",
      p_value < 0.05 & estimate < 0 ~
        "Significant negative",
      TRUE ~ "Not significant"
    ),
    relationship = factor(
      relationship,
      levels = c(
        "Significant negative",
        "Not significant",
        "Significant positive"
      )
    ),
    outcome_label = factor(
      outcome_label,
      levels = outcome_order
    ),
    characteristic_label = factor(
      characteristic_label,
      levels = rev(unname(characteristic_labels))
    ),
    result_label = ifelse(
      p_value < 0.05,
      significance,
      ""
    )
  )

p_heatmap <- ggplot(
  heatmap_data,
  aes(
    x = outcome_label,
    y = characteristic_label,
    fill = relationship
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.7
  ) +
  geom_text(
    aes(label = result_label),
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Significant negative" = "#C96A65",
      "Not significant" = "#E8EAED",
      "Significant positive" = "#4A7FA5"
    ),
    name = "Association"
  ) +
  labs(
    title = "Career-Readiness Regression Results",
    subtitle = "Year fixed-effects models",
    x = NULL,
    y = NULL,
    caption = paste(
      "Colored cells indicate associations significant",
      "at p < 0.05. Standard errors clustered by school.",
      "*, **, *** indicate p < 0.05, 0.01, and 0.001."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 35,
      hjust = 1
    ),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

print(p_heatmap)

ggsave(
  "outputs/regressions/figures/regression_heatmap.png",
  plot = p_heatmap,
  width = 12,
  height = 7,
  dpi = 300
)


# ============================================================
# STEP 10: FINAL OUTPUT MESSAGE
# ============================================================

cat("\n============================================\n")
cat("REGRESSION ANALYSIS COMPLETED SUCCESSFULLY\n")
cat("============================================\n")

cat("\nMain specification: Year fixed effects only\n")
cat("Standard errors: Clustered by school\n")
cat("Outcomes analyzed:", length(outcomes), "\n")

cat("\nResults saved to:\n")
cat("outputs/regressions/\n")
cat("outputs/regressions/figures/\n")

# ============================================================
# STEP 11: COMBINED GEOGRAPHIC REGRESSION COMPARISON
# ============================================================
# Compare all four locales across six readiness outcomes.
# City is the reference category (coefficient = 0).
# All other coefficients are relative to City.
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# Create geographic coefficient dataset
geo_results <- results |>
  filter(grepl("^locale_type", characteristic)) |>
  mutate(
    locale = sub("locale_type", "", characteristic)
  ) |>
  select(outcome_label, locale, estimate, p_value)

# Add City as the reference category
city_reference <- data.frame(
  outcome_label = unique(geo_results$outcome_label),
  locale = "City",
  estimate = 0,
  p_value = NA_real_
)

geo_results <- bind_rows(
  geo_results,
  city_reference
) |>
  mutate(
    locale = factor(
      locale,
      levels = c("City", "Rural", "Suburb", "Town")
    ),
    outcome_label = factor(
      outcome_label,
      levels = outcome_order
    )
  )

# ============================================================
# A. CREATE COMPARISON TABLE
# ============================================================

geo_table <- geo_results |>
  mutate(
    coefficient = ifelse(
      locale == "City",
      "Reference (0)",
      sprintf("%+.2f", estimate)
    ),
    coefficient = ifelse(
      !is.na(p_value) & p_value < 0.05,
      paste0(coefficient, "*"),
      coefficient
    )
  ) |>
  select(outcome_label, locale, coefficient) |>
  pivot_wider(
    names_from = locale,
    values_from = coefficient
  )

print(geo_table)

write.csv(
  geo_table,
  "outputs/regressions/figures/geography_comparison_table.csv",
  row.names = FALSE
)

# ============================================================
# B. CREATE GROUPED COEFFICIENT CHART
# ============================================================

p_geo <- ggplot(
  geo_results,
  aes(
    x = outcome_label,
    y = estimate,
    fill = locale
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    color = "gray40"
  ) +
  scale_fill_manual(
    values = c(
      "City" = "#9CA3AF",
      "Rural" = "#234B70",
      "Suburb" = "#00A6A6",
      "Town" = "#D87A5C"
    )
  ) +
  labs(
    title = "Geographic Differences in Career Readiness",
    subtitle = "Year FE regression coefficients relative to City",
    x = NULL,
    y = "Adjusted difference from City (percentage points)",
    fill = "Locale",
    caption = paste(
      "City is the reference category (coefficient = 0).",
      "Other school characteristics and year effects controlled for.",
      "Standard errors clustered by school."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    ),
    legend.position = "bottom"
  )

print(p_geo)

ggsave(
  "outputs/regressions/figures/geography_comparison.png",
  plot = p_geo,
  width = 11,
  height = 6,
  dpi = 300
)

cat("\nCombined geographic comparison saved!\n")