# ============================================================
# 02: BUILD SCHOOL CHARACTERISTICS PANEL
# ============================================================
# Purpose: Construct school-level enrollment, poverty,
# gender, and racial/ethnic characteristics.
#
# Input: Validated SCDE 135-day headcounts (Script 00)
# Output: data/processed/school_characteristics_panel.csv
#
# Academic years: 2022-23 through 2025-26
# Includes all schools; the master dataset selects high schools.
# ============================================================


# STEP 1: Load packages and identify source files

library(readxl)
library(dplyr)

years <- c("2022-23", "2023-24", "2024-25", "2025-26")

sources <- read.csv("data/raw/scde_source_manifest.csv")

headcount_sources <- sources |>
  filter(type == "headcount", academic_year %in% years)

stopifnot(
  nrow(headcount_sources) == length(years),
  !anyDuplicated(headcount_sources$academic_year),
  setequal(headcount_sources$academic_year, years)
)

count_variables <- c(
  "enrollment", "female", "male", "gender_missing",
  "black", "american_indian", "asian", "hispanic",
  "pacific_islander", "multiracial", "white",
  "race_missing", "poverty_count"
)


# STEP 2: Create a function to clean one academic year

clean_headcounts <- function(year) {
  
  # Locate the validated 135-day headcount workbook
  file <- headcount_sources$file_path[
    headcount_sources$academic_year == year
  ]
  
  if (!file.exists(file)) {
    stop("Missing headcount workbook for ", year)
  }
  
  # Read school records after the seven header rows
  raw <- read_excel(
    file,
    sheet = "school by gender race pip",
    skip = 7,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )
  
  stopifnot(ncol(raw) == 16)
  
  # Assign descriptive column names
  names(raw) <- c(
    "school_id", "district_name", "school_name",
    count_variables
  )
  
  # Keep school records and standardize identifiers
  clean <- raw |>
    filter(grepl("^[0-9]{6,7}$", trimws(school_id))) |>
    mutate(
      academic_year = year,
      school_id = sprintf("%07d", as.integer(school_id)),
      district_name = trimws(gsub("[[:space:]]+", " ", district_name)),
      school_name = trimws(gsub("[[:space:]]+", " ", school_name))
    )
  
  # Convert student counts to numeric values
  for (variable in count_variables) {
    
    values <- gsub(",", "", trimws(clean[[variable]]))
    values[values %in% c("*", "")] <- NA_character_
    
    # Stop if unexpected nonnumeric values appear
    if (any(!is.na(values) &
            is.na(suppressWarnings(as.numeric(values))))) {
      stop("Unexpected values in ", year, ": ", variable)
    }
    
    clean[[variable]] <- as.numeric(values)
  }
  
  # Confirm unique school IDs and complete counts
  stopifnot(
    !anyNA(clean$school_id),
    !anyDuplicated(clean$school_id),
    all(complete.cases(clean[count_variables]))
  )
  
  cat(year, ":", nrow(clean), "schools\n")
  
  return(clean)
}


# STEP 3: Build the four-year panel

headcount_panel <- years |>
  lapply(clean_headcounts) |>
  bind_rows()


# STEP 4: Validate counts and calculate characteristics

stopifnot(
  !anyDuplicated(
    headcount_panel[c("school_id", "academic_year")]
  )
)

# Verify that demographic totals equal school enrollment
gender_total <- rowSums(
  headcount_panel[c("female", "male", "gender_missing")]
)

race_total <- rowSums(
  headcount_panel[c(
    "black", "american_indian", "asian", "hispanic",
    "pacific_islander", "multiracial", "white",
    "race_missing"
  )]
)

stopifnot(
  all(headcount_panel[count_variables] >= 0),
  all(headcount_panel$enrollment > 0),
  all(gender_total == headcount_panel$enrollment),
  all(race_total == headcount_panel$enrollment),
  all(headcount_panel$poverty_count <= headcount_panel$enrollment)
)

# Calculate percentages of total school enrollment
school_characteristics_panel <- headcount_panel |>
  mutate(
    poverty_rate = poverty_count / enrollment * 100,
    
    female_share = female / enrollment * 100,
    male_share = male / enrollment * 100,
    gender_missing_share = gender_missing / enrollment * 100,
    
    black_share = black / enrollment * 100,
    american_indian_share = american_indian / enrollment * 100,
    asian_share = asian / enrollment * 100,
    hispanic_share = hispanic / enrollment * 100,
    pacific_islander_share = pacific_islander / enrollment * 100,
    multiracial_share = multiracial / enrollment * 100,
    white_share = white / enrollment * 100,
    race_missing_share = race_missing / enrollment * 100
  )

# Verify all calculated percentages are valid
percentage_variables <- c(
  "poverty_rate",
  grep("_share$", names(school_characteristics_panel), value = TRUE)
)

stopifnot(
  all(sapply(
    school_characteristics_panel[percentage_variables],
    function(x) all(!is.na(x) & x >= 0 & x <= 100)
  ))
)


# STEP 5: Save the dataset

dir.create("data/processed", showWarnings = FALSE)

write.csv(
  school_characteristics_panel,
  "data/processed/school_characteristics_panel.csv",
  row.names = FALSE
)

cat("\nSchool characteristics panel saved successfully!\n")
cat("Observations:", nrow(school_characteristics_panel), "\n")
cat("Variables:", ncol(school_characteristics_panel), "\n")