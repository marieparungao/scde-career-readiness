# ============================================================
# 07: DESCRIPTIVE STATISTICS AND TIME TRENDS
# ============================================================
# Project: SCDE Career Readiness
# Academic years: 2022-23 through 2025-26
#
# Purpose:
# Examine the prevalence, variation, and changes in
# career-readiness qualification rates across SC high schools.
#
# Run from the scde-career-readiness project root.
# ============================================================


# ============================================================
# STEP 1: LOAD PACKAGES AND MASTER DATASET
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

master <- read.csv(
  "data/processed/career_readiness_master.csv",
  colClasses = c(
    school_id = "character",
    academic_year = "character"
  )
)

# Identify the six dependent variables
outcomes <- c(
  "overall_readiness",
  "assessment_readiness",
  "cte_readiness",
  "asvab_readiness",
  "wbl_readiness",
  "credential_readiness"
)

# Readable labels for tables and figures
labels <- c(
  overall_readiness = "Overall Readiness",
  assessment_readiness = "Assessment",
  cte_readiness = "CTE",
  asvab_readiness = "ASVAB",
  wbl_readiness = "Work-Based Learning",
  credential_readiness = "Employability Credential"
)

years <- c(
  "2022-23",
  "2023-24",
  "2024-25",
  "2025-26"
)

# Validate dataset
stopifnot(
  all(c("school_id", "academic_year", outcomes) %in% names(master)),
  !anyDuplicated(master[c("school_id", "academic_year")]),
  setequal(unique(master$academic_year), years)
)

# Create output folder
outdir <- "outputs/descriptives"

dir.create(
  outdir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Reshape the six outcomes into long format
long <- master |>
  select(school_id, academic_year, all_of(outcomes)) |>
  pivot_longer(
    all_of(outcomes),
    names_to = "outcome",
    values_to = "rate"
  ) |>
  mutate(
    outcome_label = unname(labels[outcome]),
    academic_year = factor(academic_year, levels = years)
  )


# ============================================================
# STEP 2: TABLE 1 - SUMMARY STATISTICS
# ============================================================

# Calculate overall descriptive statistics for each pathway
summary_stats <- long |>
  group_by(outcome, outcome_label) |>
  summarise(
    n = sum(!is.na(rate)),
    mean = mean(rate, na.rm = TRUE),
    median = median(rate, na.rm = TRUE),
    sd = sd(rate, na.rm = TRUE),
    min = min(rate, na.rm = TRUE),
    max = max(rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(match(outcome, outcomes))

cat("\n=== TABLE 1: SUMMARY STATISTICS ===\n")

print(summary_stats, n = Inf)

write.csv(
  summary_stats,
  file.path(outdir, "01_summary_statistics.csv"),
  row.names = FALSE
)


# ============================================================
# STEP 3: FIGURE 1 - AVERAGE QUALIFICATION RATES
# ============================================================

# Compare the prevalence of all six readiness outcomes

p_means <- summary_stats |>
  mutate(
    outcome_label = factor(
      outcome_label,
      levels = rev(outcome_label)
    )
  ) |>
  ggplot(aes(x = outcome_label, y = mean)) +
  geom_col(
    fill = "#234B70",
    width = 0.7
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", mean)),
    hjust = -0.12,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 85),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Average Career-Readiness Qualification Rates",
    subtitle = "SC high schools, 2022-23 through 2025-26",
    x = NULL,
    y = "Average school-year qualification rate (%)",
    caption = "Unweighted school-year means; pathways overlap."
  ) +
  theme_minimal(base_size = 12)

print(p_means)

ggsave(
  file.path(outdir, "01_pathway_means.png"),
  plot = p_means,
  width = 9,
  height = 5,
  dpi = 300
)


# ============================================================
# STEP 4: TABLE 2 - FOUR-YEAR TRENDS
# ============================================================

# Calculate yearly average and median qualification rates
trends <- long |>
  group_by(academic_year, outcome, outcome_label) |>
  summarise(
    n = sum(!is.na(rate)),
    mean = mean(rate, na.rm = TRUE),
    median = median(rate, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== TABLE 2: YEARLY QUALIFICATION RATES ===\n")

print(trends, n = Inf)

write.csv(
  trends,
  file.path(outdir, "02_yearly_trends.csv"),
  row.names = FALSE
)


# ============================================================
# STEP 5: FIGURE 2 - FOUR-YEAR TREND GRAPH
# ============================================================

# Display five outcomes together.
# Employability Credential is graphed separately because
# its average rates are much smaller.

p_trends <- trends |>
  filter(outcome != "credential_readiness") |>
  ggplot(
    aes(
      x = academic_year,
      y = mean,
      color = outcome_label,
      group = outcome_label
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 85)) +
  labs(
    title = "Career-Readiness Trends Across SC High Schools",
    subtitle = "Unweighted school averages, 2022-23 to 2025-26",
    x = "Academic year",
    y = "Average qualification rate (%)",
    color = "Outcome",
    caption = paste(
      "Pathway rates overlap.",
      "Employability Credential shown separately."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

print(p_trends)

ggsave(
  file.path(outdir, "02_readiness_trends.png"),
  plot = p_trends,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# STEP 6: FIGURE 3 - EMPLOYABILITY CREDENTIAL TREND
# ============================================================

# Use a separate scale so small changes remain visible.

p_credential <- trends |>
  filter(outcome == "credential_readiness") |>
  ggplot(
    aes(
      x = academic_year,
      y = mean,
      group = 1
    )
  ) +
  geom_line(
    color = "#234B70",
    linewidth = 1
  ) +
  geom_point(
    color = "#234B70",
    size = 2.5
  ) +
  scale_y_continuous(limits = c(0, 1.5)) +
  labs(
    title = "Employability Credential Qualification Over Time",
    subtitle = "SC high schools, 2022-23 through 2025-26",
    x = "Academic year",
    y = "Average qualification rate (%)",
    caption = "Unweighted school means; different vertical scale."
  ) +
  theme_minimal(base_size = 12)

print(p_credential)

ggsave(
  file.path(outdir, "03_credential_trend.png"),
  plot = p_credential,
  width = 8,
  height = 5,
  dpi = 300
)


# ============================================================
# STEP 7: TABLE 3 - CHANGES WITHIN THE SAME SCHOOLS
# ============================================================

# Identify schools that appear in all four academic years.

four_year_ids <- master |>
  count(school_id, name = "years_observed") |>
  filter(years_observed == 4) |>
  pull(school_id)

cat("\nSchools observed in all four years:",
    length(four_year_ids), "\n")

# For these schools, compare first and last years.
# A school must have nonmissing rates in both years
# to contribute to a particular outcome's comparison.

matched <- long |>
  filter(
    school_id %in% four_year_ids,
    as.character(academic_year) %in% c("2022-23", "2025-26")
  ) |>
  select(
    school_id,
    outcome,
    outcome_label,
    academic_year,
    rate
  ) |>
  pivot_wider(
    names_from = academic_year,
    values_from = rate
  ) |>
  mutate(
    change_pp = `2025-26` - `2022-23`
  )

# Summarize changes across schools
change_summary <- matched |>
  group_by(outcome, outcome_label) |>
  summarise(
    schools = sum(!is.na(change_pp)),
    mean_change_pp = mean(change_pp, na.rm = TRUE),
    median_change_pp = median(change_pp, na.rm = TRUE),
    pct_increased = 100 * mean(change_pp > 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(match(outcome, outcomes))

cat("\n=== TABLE 3: MATCHED-SCHOOL CHANGES ===\n")

print(change_summary, n = Inf)

write.csv(
  change_summary,
  file.path(outdir, "03_matched_school_changes.csv"),
  row.names = FALSE
)


# ============================================================
# STEP 8: FIGURE 4 - MATCHED-SCHOOL CHANGES
# ============================================================

# Show average percentage-point changes within the same schools

p_change <- change_summary |>
  mutate(
    outcome_label = factor(
      outcome_label,
      levels = rev(outcome_label)
    )
  ) |>
  ggplot(
    aes(
      x = outcome_label,
      y = mean_change_pp
    )
  ) +
  geom_col(
    fill = "#234B70",
    width = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4
  ) +
  coord_flip() +
  labs(
    title = "Change in Qualification Rates Within the Same Schools",
    subtitle = "2022-23 to 2025-26; schools observed in all four years",
    x = NULL,
    y = "Mean within-school change (percentage points)",
    caption = paste(
      "Uses schools with nonmissing values",
      "in both endpoint years for each outcome."
    )
  ) +
  theme_minimal(base_size = 12)

print(p_change)

ggsave(
  file.path(outdir, "04_matched_school_changes.png"),
  plot = p_change,
  width = 9,
  height = 5,
  dpi = 300
)


# ============================================================
# STEP 9: FINAL OUTPUT MESSAGE
# ============================================================

cat("\n============================================\n")
cat("DESCRIPTIVE ANALYSIS COMPLETED SUCCESSFULLY\n")
cat("============================================\n")

cat("\nTables and figures saved to:\n")
cat(outdir, "\n")