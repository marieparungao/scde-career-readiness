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
# Original counts and headcount source metadata are retained.
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




# STEP 2: Create a function to clean one year
clean_headcounts <- function(year) {
  
  # Locate the validated 135-day headcount workbook
  file <- headcount_sources$file_path[
    headcount_sources$academic_year == year
  ]
  
  if (!file.exists(file)) {
    stop("Missing headcount workbook for ", year)
  }
  
  # Verify the workbook title identifies the requested year and snapshot.
  header <- read_excel(
    file,
    sheet = 1,
    range = "A2",
    col_names = FALSE,
    col_types = "text"
  )
  
  title <- as.character(header[[1]][1])
  title_digits <- gsub("[^0-9]", "", title)
  start_year <- as.integer(substr(year, 1, 4))
  expected_year_digits <- paste0(start_year, start_year + 1L)
  
  if (
    is.na(title) ||
    !grepl(expected_year_digits, title_digits, fixed = TRUE) ||
    !grepl("135-Day", title, fixed = TRUE)
  ) {
    stop("Unexpected year or snapshot in ", basename(file), ": ", title)
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
      headcount_day = 135L,
      headcount_source_file = basename(file),
      school_id = sprintf("%07d", as.integer(school_id)),
      district_name = trimws(gsub("[[:space:]]+", " ", district_name)),
      school_name = trimws(gsub("[[:space:]]+", " ", school_name))
    )
  
  # Convert student counts to numeric values
  for (variable in count_variables) {
    
    values <- gsub(",", "", trimws(clean[[variable]]))
    values[values %in% c("*", "")] <- NA_character_
    
    unexpected <- unique(values[
      !is.na(values) &
        is.na(suppressWarnings(as.numeric(values)))
    ])
    
    if (length(unexpected) > 0) {
      stop(
        "Unexpected values in ", year, " / ", variable,
        ": ", paste(unexpected, collapse = ", ")
      )
    }
    
    clean[[variable]] <- as.numeric(values)
  }
  
  # Confirm school records exist and identifiers are unique
  stopifnot(
    nrow(clean) > 0,
    !anyNA(clean$school_id),
    !anyDuplicated(clean$school_id)
  )
  
  cat(year, ":", nrow(clean), "schools\n")
  
  return(clean)
}


# STEP 3: Build the four-year panel

headcount_panel <- years |>
  lapply(clean_headcounts) |>
  bind_rows()

stopifnot(
  setequal(headcount_panel$academic_year, years),
  
  all(grepl("^[0-9]{7}$", headcount_panel$school_id)),
  
  !anyDuplicated(
    headcount_panel[c("school_id", "academic_year")]
  ),
  
  all(headcount_panel$headcount_day == 135L)
)

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

count_checks <- c(
  rows_with_missing_counts = sum(
    !complete.cases(headcount_panel[count_variables])
  ),
  
  nonpositive_enrollment = sum(
    headcount_panel$enrollment <= 0,
    na.rm = TRUE
  ),
  
  negative_counts = sum(
    as.matrix(headcount_panel[count_variables]) < 0,
    na.rm = TRUE
  ),
  
  gender_total_mismatch = sum(
    gender_total != headcount_panel$enrollment,
    na.rm = TRUE
  ),
  
  race_total_mismatch = sum(
    race_total != headcount_panel$enrollment,
    na.rm = TRUE
  ),
  
  poverty_exceeds_enrollment = sum(
    headcount_panel$poverty_count >
      headcount_panel$enrollment,
    na.rm = TRUE
  )
)

print(count_checks)

# These source files are expected to have complete, valid counts.
# Stop for review if any check fails.
stopifnot(all(count_checks == 0))


# STEP 5: Calculate school characteristics
school_characteristics_panel <- headcount_panel |>
  dplyr::mutate(
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


# STEP 6: Validate percentages
percentage_variables <- c(
  "poverty_rate",
  grep(
    "_share$",
    names(school_characteristics_panel),
    value = TRUE
  )
)

invalid_percentages <- sapply(
  school_characteristics_panel[percentage_variables],
  function(x) {
    sum(!is.finite(x) | x < 0 | x > 100)
  }
)


stopifnot(all(invalid_percentages == 0))

# Display school counts and snapshot by year
print(
  school_characteristics_panel |>
    dplyr::group_by(academic_year, headcount_day) |>
    dplyr::summarise(
      observations = dplyr::n(),
      .groups = "drop"
    )
)


# STEP 7: Save the dataset
dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  school_characteristics_panel,
  "data/processed/school_characteristics_panel.csv",
  row.names = FALSE,
  na = ""
)

cat("\nSchool characteristics panel saved successfully!\n")
cat("Observations:", nrow(school_characteristics_panel), "\n")
cat("Variables:", ncol(school_characteristics_panel), "\n")
    