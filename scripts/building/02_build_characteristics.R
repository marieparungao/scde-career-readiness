# ============================================================
# 02: BUILD SCHOOL CHARACTERISTICS PANEL
# ============================================================
# Purpose: Combine four years of SCDE 135-day headcount
# data to construct school-level socioeconomic and
# demographic characteristics.
#
# Academic years: 2022-23 through 2025-26
#
# Variables constructed:
# - School enrollment (student count)
# - Poverty rate (% of enrolled students in poverty)
# - Gender composition (% female, male, missing)
# - Racial/ethnic composition (% in each reported category)
#
# Original student counts are retained alongside percentages.
#
# Input: data/raw/Headcount135_YYYY-YY.xlsx
# Output: data/processed/school_characteristics_panel.csv
#
# Note: Disability status is not available in these headcount
# files and will be investigated separately.
# ============================================================
# ============================================================


# STEP 1: Identify academic years

years <- c("2022-23", "2023-24", "2024-25", "2025-26")


# STEP 2: Create a function to clean one year of headcounts

clean_headcounts <- function(year) {
  
  # Locate and import the Excel workbook
  file <- paste0("data/raw/Headcount135_", year, ".xlsx")
  
  raw <- readxl::read_excel(
    file,
    sheet = "school by gender race pip",
    skip = 7,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )
  
  # Confirm that the expected 16 columns are present
  if (ncol(raw) != 16) {
    stop("Unexpected column count in ", year)
  }
  
  # Assign readable names to the 16 columns
  names(raw) <- c(
    "school_id", "district_name", "school_name",
    "enrollment", "female", "male", "gender_missing",
    "black", "american_indian", "asian", "hispanic",
    "pacific_islander", "multiracial", "white",
    "race_missing", "poverty_count"
  )
  
  # Keep school records and standardize school IDs
  raw <- raw |>
    dplyr::filter(
      grepl("^[0-9]{6,9}$", trimws(school_id))
    ) |>
    dplyr::mutate(
      school_id = sub("^0+", "", trimws(school_id)),
      academic_year = year
    )
  
  # Identify columns containing numerical counts
  count_variables <- c(
    "enrollment", "female", "male", "gender_missing",
    "black", "american_indian", "asian", "hispanic",
    "pacific_islander", "multiracial", "white",
    "race_missing", "poverty_count"
  )
  
  # Convert counts to numbers, preserving missing values
  for (variable in count_variables) {
    
    values <- gsub(",", "", trimws(raw[[variable]]))
    values[values %in% c("*", "")] <- NA_character_
    
    # Stop if unexpected text appears in a numeric column
    unexpected <- unique(values[
      !is.na(values) &
        is.na(suppressWarnings(as.numeric(values)))
    ])
    
    if (length(unexpected) > 0) {
      stop("Unexpected values in ", year, " / ",
           variable, ": ", paste(unexpected, collapse = ", "))
    }
    
    raw[[variable]] <- as.numeric(values)
  }
  
  # Check that school IDs are unique within each year
  if (anyDuplicated(raw$school_id) > 0) {
    stop("Duplicate school IDs in ", year)
  }
  
  # Display the number of schools imported
  cat(year, ":", nrow(raw), "schools\n")
  
  return(raw)
}


# STEP 3: Combine all four years

headcount_panel <- years |>
  lapply(clean_headcounts) |>
  dplyr::bind_rows()


# STEP 4: Inspect the combined dataset

# Display total rows and columns
print(dim(headcount_panel))

# Preview the first three schools
print(head(headcount_panel, 3), width = Inf)

# Check for duplicate school-year observations
print(anyDuplicated(
  headcount_panel[c("school_id", "academic_year")]
))

# STEP 5: Validate school enrollment counts

# Calculate the total students across gender categories
gender_total <- rowSums(
  headcount_panel[c("female", "male", "gender_missing")]
)

# Calculate the total students across racial/ethnic categories
race_total <- rowSums(
  headcount_panel[c(
    "black", "american_indian", "asian", "hispanic",
    "pacific_islander", "multiracial", "white",
    "race_missing"
  )]
)

# Check for missing or invalid enrollment
cat("Missing enrollment:",
    sum(is.na(headcount_panel$enrollment)), "\n")

cat("Zero or negative enrollment:",
    sum(headcount_panel$enrollment <= 0, na.rm = TRUE), "\n")

# Check whether demographic totals equal enrollment
cat("Gender totals not matching enrollment:",
    sum(gender_total != headcount_panel$enrollment,
        na.rm = TRUE), "\n")

cat("Race totals not matching enrollment:",
    sum(race_total != headcount_panel$enrollment,
        na.rm = TRUE), "\n")

# Poverty counts should not exceed enrollment
cat("Poverty counts exceeding enrollment:",
    sum(headcount_panel$poverty_count >
          headcount_panel$enrollment, na.rm = TRUE), "\n")

# Count records with missing demographic or poverty values
cat("Rows with missing numerical data:",
    sum(!complete.cases(
      headcount_panel[c(
        "enrollment", "female", "male", "gender_missing",
        "black", "american_indian", "asian", "hispanic",
        "pacific_islander", "multiracial", "white",
        "race_missing", "poverty_count"
      )]
    )), "\n")

# STEP 6: Calculate school characteristics

# Convert student counts into percentages of enrollment
school_characteristics_panel <- headcount_panel |>
  dplyr::mutate(
    
    # Socioeconomic composition
    poverty_rate = poverty_count / enrollment * 100,
    
    # Gender composition
    female_share = female / enrollment * 100,
    male_share = male / enrollment * 100,
    gender_missing_share = gender_missing / enrollment * 100,
    
    # Racial/ethnic composition
    black_share = black / enrollment * 100,
    american_indian_share = american_indian / enrollment * 100,
    asian_share = asian / enrollment * 100,
    hispanic_share = hispanic / enrollment * 100,
    pacific_islander_share = pacific_islander / enrollment * 100,
    multiracial_share = multiracial / enrollment * 100,
    white_share = white / enrollment * 100,
    race_missing_share = race_missing / enrollment * 100
  )

# Preview our calculated percentages
print(
  head(
    school_characteristics_panel |>
      dplyr::select(
        academic_year,
        school_name,
        enrollment,
        poverty_rate,
        female_share,
        black_share,
        hispanic_share,
        white_share
      ),
    5
  ),
  width = Inf
)
# STEP 7: Validate percentages and save the dataset

# Identify all calculated percentage variables
percentage_variables <- c(
  "poverty_rate",
  grep("_share$", names(school_characteristics_panel),
       value = TRUE)
)

# Count missing or invalid percentages in each variable
invalid_percentages <- sapply(
  school_characteristics_panel[percentage_variables],
  function(x) sum(is.na(x) | x < 0 | x > 100)
)

print(invalid_percentages)

# Stop if any percentage is invalid
stopifnot(all(invalid_percentages == 0))

# Save the completed school characteristics dataset
write.csv(
  school_characteristics_panel,
  "data/processed/school_characteristics_panel.csv",
  row.names = FALSE
)

cat("School characteristics panel saved successfully!\n")
