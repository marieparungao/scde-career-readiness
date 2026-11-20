# ============================================================
# 08: GEOGRAPHY AND POVERTY DESCRIPTIVE ANALYSIS
# ============================================================
# Project: SCDE Career Readiness
# Years: 2022-23 through 2025-26
#
# Purpose:
# Compare career-readiness qualification pathways across
# school poverty levels and geographic locale classifications.
#
# These are unadjusted descriptive comparisons.
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

outcomes <- c(
  "overall_readiness",
  "assessment_readiness",
  "cte_readiness",
  "asvab_readiness",
  "wbl_readiness",
  "credential_readiness"
)

outcome_labels <- c(
  overall_readiness = "Overall Readiness",
  assessment_readiness = "Assessment",
  cte_readiness = "CTE",
  asvab_readiness = "ASVAB",
  wbl_readiness = "Work-Based Learning",
  credential_readiness = "Employability Credential"
)

years <- c(
  "2022-23", "2023-24", "2024-25", "2025-26"
)

locale_levels <- c("City", "Suburb", "Town", "Rural")

stopifnot(
  all(c("school_id", "academic_year", "poverty_rate",
        "locale_type", outcomes) %in% names(master)),
  !anyDuplicated(master[c("school_id", "academic_year")])
)

outdir <- "outputs/descriptives/geography_poverty"

dir.create(
  outdir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Readable long-format outcome data
long <- master |>
  select(
    school_id, academic_year, poverty_rate,
    locale_type, all_of(outcomes)
  ) |>
  pivot_longer(
    cols = all_of(outcomes),
    names_to = "outcome",
    values_to = "rate"
  ) |>
  mutate(
    outcome_label = unname(outcome_labels[outcome]),
    outcome_label = factor(
      outcome_label,
      levels = unname(outcome_labels)
    ),
    academic_year = factor(academic_year, levels = years),
    locale_type = factor(locale_type, levels = locale_levels)
  )


# ============================================================
# STEP 2: CREATE POVERTY QUARTILES
# ============================================================

# Divide school-year observations into four groups based
# on their reported poverty rates across the pooled sample.
#
# Q1 = lowest poverty
# Q4 = highest poverty
#
# Schools can change quartiles between years.
# Equal rates at quartile boundaries may be split by ntile().

poverty_data <- master |>
  filter(!is.na(poverty_rate)) |>
  mutate(
    poverty_quartile = ntile(poverty_rate, 4),
    poverty_group = factor(
      paste0("Q", poverty_quartile),
      levels = c("Q1", "Q2", "Q3", "Q4"),
      labels = c(
        "Q1: Lowest",
        "Q2",
        "Q3",
        "Q4: Highest"
      )
    )
  )

# Document poverty ranges and school-year counts
poverty_ranges <- poverty_data |>
  group_by(poverty_group) |>
  summarise(
    observations = n(),
    schools = n_distinct(school_id),
    min_poverty = min(poverty_rate),
    mean_poverty = mean(poverty_rate),
    max_poverty = max(poverty_rate),
    .groups = "drop"
  )

cat("\n=== POVERTY QUARTILE DEFINITIONS ===\n")
print(poverty_ranges)

write.csv(
  poverty_ranges,
  file.path(outdir, "01_poverty_quartile_ranges.csv"),
  row.names = FALSE
)


# ============================================================
# STEP 3: POVERTY QUARTILE SUMMARY TABLE
# ============================================================

poverty_long <- poverty_data |>
  select(
    school_id, academic_year, poverty_group,
    all_of(outcomes)
  ) |>
  pivot_longer(
    cols = all_of(outcomes),
    names_to = "outcome",
    values_to = "rate"
  ) |>
  mutate(
    outcome_label = unname(outcome_labels[outcome])
  )

poverty_summary <- poverty_long |>
  group_by(poverty_group, outcome, outcome_label) |>
  summarise(
    observations = sum(!is.na(rate)),
    mean = mean(rate, na.rm = TRUE),
    median = median(rate, na.rm = TRUE),
    sd = sd(rate, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== POVERTY QUARTILE READINESS SUMMARY ===\n")
print(poverty_summary, n = Inf)

write.csv(
  poverty_summary,
  file.path(outdir, "02_poverty_pathway_summary.csv"),
  row.names = FALSE
)


# ============================================================
# STEP 4: FIGURE 1 - POVERTY QUARTILE COMPARISON
# ============================================================

# Compare the main career-readiness outcomes across quartiles.
# Credential is omitted from this graph because its rates
# are substantially smaller; it remains in the tables.

p_poverty <- poverty_summary |>
  filter(outcome != "credential_readiness") |>
  ggplot(
    aes(
      x = poverty_group,
      y = mean,
      fill = outcome_label
    )
  ) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.75
  ) +
  labs(
    title = "Career Readiness by School Poverty Quartile",
    subtitle = "SC high schools, 2022-23 through 2025-26",
    x = "School poverty quartile",
    y = "Average qualification rate (%)",
    fill = "Outcome",
    caption = paste(
      "Unweighted school-year averages.",
      "Q1 = lowest poverty; Q4 = highest poverty.",
      "Pathways overlap."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

print(p_poverty)

ggsave(
  file.path(outdir, "01_poverty_quartile_comparison.png"),
  p_poverty,
  width = 11,
  height = 6,
  dpi = 300
)


# ============================================================
# STEP 5: FIGURE 2 - POVERTY PATHWAY HEATMAP
# ============================================================

p_poverty_heatmap <- poverty_summary |>
  mutate(
    outcome_label = factor(
      outcome_label,
      levels = unname(outcome_labels)
    )
  ) |>
  ggplot(
    aes(
      x = poverty_group,
      y = outcome_label,
      fill = mean
    )
  ) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = sprintf("%.1f%%", mean)),
    size = 3.5
  ) +
  scale_fill_gradient(
    low = "#E8F0F5",
    high = "#234B70"
  ) +
  labs(
    title = "Qualification Pathways Across Poverty Quartiles",
    subtitle = "Average school-year qualification rates",
    x = "School poverty quartile",
    y = NULL,
    fill = "Rate (%)",
    caption = "Unweighted averages; pathways overlap."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank()
  )

print(p_poverty_heatmap)

ggsave(
  file.path(outdir, "02_poverty_heatmap.png"),
  p_poverty_heatmap,
  width = 9,
  height = 5,
  dpi = 300
)


# ============================================================
# STEP 6: FIGURE 3 - POVERTY VS. CTE SCATTERPLOT
# ============================================================

# Each point represents one school-year observation.
# The fitted line is descriptive and does not control for
# other school characteristics or year fixed effects.

p_poverty_cte <- master |>
  filter(
    !is.na(poverty_rate),
    !is.na(cte_readiness)
  ) |>
  ggplot(
    aes(
      x = poverty_rate,
      y = cte_readiness
    )
  ) +
  geom_point(
    alpha = 0.25,
    color = "#234B70",
    size = 1.5
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "#C26A65"
  ) +
  labs(
    title = "School Poverty and CTE Qualification",
    subtitle = "Each observation represents one school-year",
    x = "School poverty rate (%)",
    y = "CTE qualification rate (%)",
    caption = paste(
      "Unadjusted linear relationship.",
      "Observations may include the same school in multiple years."
    )
  ) +
  theme_minimal(base_size = 12)

print(p_poverty_cte)

ggsave(
  file.path(outdir, "03_poverty_cte_scatter.png"),
  p_poverty_cte,
  width = 9,
  height = 6,
  dpi = 300
)


# ============================================================
# STEP 7: GEOGRAPHIC SUMMARY STATISTICS
# ============================================================

# Count schools and school-year observations by locale
geography_coverage <- master |>
  group_by(locale_type) |>
  summarise(
    observations = n(),
    schools = n_distinct(school_id),
    missing_overall = sum(is.na(overall_readiness)),
    .groups = "drop"
  )

cat("\n=== GEOGRAPHIC SAMPLE COVERAGE ===\n")
print(geography_coverage)

write.csv(
  geography_coverage,
  file.path(outdir, "04_geography_coverage.csv"),
  row.names = FALSE
)

# Calculate pathway means by locale
geography_summary <- long |>
  filter(!is.na(locale_type)) |>
  group_by(locale_type, outcome, outcome_label) |>
  summarise(
    observations = sum(!is.na(rate)),
    mean = mean(rate, na.rm = TRUE),
    median = median(rate, na.rm = TRUE),
    sd = sd(rate, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== GEOGRAPHIC PATHWAY SUMMARY ===\n")
print(geography_summary, n = Inf)

write.csv(
  geography_summary,
  file.path(outdir, "05_geography_pathway_summary.csv"),
  row.names = FALSE
)


# ============================================================
# STEP 8: FIGURE 4 - GEOGRAPHIC PATHWAY COMPARISON
# ============================================================

# Compare five main outcomes across locale categories

p_geography <- geography_summary |>
  filter(outcome != "credential_readiness") |>
  ggplot(
    aes(
      x = locale_type,
      y = mean,
      fill = outcome_label
    )
  ) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.75
  ) +
  labs(
    title = "Career Readiness by Geographic Locale",
    subtitle = "City, suburb, town, and rural high schools",
    x = "Geographic locale",
    y = "Average qualification rate (%)",
    fill = "Outcome",
    caption = paste(
      "Unweighted school-year averages.",
      "Geographic categories from NCES."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

print(p_geography)

ggsave(
  file.path(outdir, "04_geography_pathway_comparison.png"),
  p_geography,
  width = 11,
  height = 6,
  dpi = 300
)


# ============================================================
# STEP 9: FIGURE 5 - GEOGRAPHIC PATHWAY HEATMAP
# ============================================================

p_geo_heatmap <- geography_summary |>
  ggplot(
    aes(
      x = locale_type,
      y = outcome_label,
      fill = mean
    )
  ) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = sprintf("%.1f%%", mean)),
    size = 3.5
  ) +
  scale_fill_gradient(
    low = "#E8F0F5",
    high = "#234B70"
  ) +
  labs(
    title = "Qualification Pathways by Geographic Locale",
    subtitle = "Average school-year qualification rates",
    x = "Geographic locale",
    y = NULL,
    fill = "Rate (%)",
    caption = "Unweighted school-year averages; pathways overlap."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank()
  )

print(p_geo_heatmap)

ggsave(
  file.path(outdir, "05_geography_heatmap.png"),
  p_geo_heatmap,
  width = 9,
  height = 5,
  dpi = 300
)


# ============================================================
# STEP 10: FIGURE 6 - READINESS TRENDS BY GEOGRAPHY
# ============================================================

# Show changes in overall readiness across academic years
# separately for each geographic category.

geography_trends <- master |>
  filter(!is.na(locale_type)) |>
  group_by(academic_year, locale_type) |>
  summarise(
    observations = sum(!is.na(overall_readiness)),
    mean_overall = mean(overall_readiness, na.rm = TRUE),
    mean_cte = mean(cte_readiness, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    academic_year = factor(
      academic_year,
      levels = years
    ),
    locale_type = factor(
      locale_type,
      levels = locale_levels
    )
  )

cat("\n=== GEOGRAPHIC TRENDS ===\n")
print(geography_trends, n = Inf)

write.csv(
  geography_trends,
  file.path(outdir, "06_geography_trends.csv"),
  row.names = FALSE
)

p_geo_trends <- geography_trends |>
  ggplot(
    aes(
      x = academic_year,
      y = mean_overall,
      color = locale_type,
      group = locale_type
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "Overall Career Readiness Over Time by Geography",
    subtitle = "SC high schools, 2022-23 through 2025-26",
    x = "Academic year",
    y = "Average overall readiness rate (%)",
    color = "Locale",
    caption = "Unweighted school averages; school composition can vary by year."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

print(p_geo_trends)

ggsave(
  file.path(outdir, "06_geography_trends.png"),
  p_geo_trends,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# STEP 11: FINAL OUTPUT MESSAGE
# ============================================================

cat("\n============================================\n")
cat("GEOGRAPHY AND POVERTY ANALYSIS COMPLETED\n")
cat("============================================\n")

cat("\nTables and figures saved to:\n")
cat(outdir, "\n")