# ============================================================
# 02: BUILD SCHOOL CHARACTERISTICS PANEL
# ============================================================
# Purpose: Combine five years of SCDE headcount data.
#
# Academic years: 2021-22 through 2025-26
# Snapshot: 135-day for every year
#
# Variables:
# - Active, funded enrollment
# - Poverty rate
# - Gender composition
# - Racial/ethnic composition
#
# Original counts are retained alongside percentages.
# Imports all school levels; the readiness panel determines
# the high-school sample when datasets are merged.
#
# Output: data/processed/school_characteristics_panel.csv
#
# NOTE:
# The enrollment eligibility wording changes in 2025-26.
# Its implications for comparability remain under review.
# ============================================================


# STEP 1: Identify the exact source files
input_files <- data.frame(
  academic_year = c(
    "2021-22",
    "2022-23",
    "2023-24",
    "2024-25",
    "2025-26"
  ),
  
  source_year = c(
    "2021-2022",
    "2022-2023",
    "2023-2024",
    "2024-2025",
    "2025-2026"
  ),
  
  filename = c(
    paste0(
      "135 Day - School Headcount by Gender, Ethnicity ",
      "and Pupils in Poverty - 2021-22.xlsx"
    ),
    paste0(
      "School Headcount by Gender, Ethnicity ",
      "and Pupils in Poverty - 2022-2023.xlsx"
    ),
    "School Headcount by Gender, Ethnicity and Pupils in Poverty.xlsx",
    "School Headcount by Gender, Ethnicity and Pupils in Poverty (1).xlsx",
    "School Headcount by Gender, Ethnicity and Pupils in Poverty (2).xlsx"
  ),
  
  headcount_day = rep(135L, 5)
)

years <- input_files$academic_year

headcount_files <- list.files(
  "data/raw",
  pattern = "\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

count_variables <- c(
  "enrollment", "female", "male", "gender_missing",
  "black", "american_indian", "asian", "hispanic",
  "pacific_islander", "multiracial", "white",
  "race_missing", "poverty_count"
)


# STEP 2: Create a function to clean one year
clean_headcounts <- function(year) {
  
  source_info <- input_files[
    input_files$academic_year == year,
  ]
  
  stopifnot(nrow(source_info) == 1)
  
  file <- headcount_files[
    tolower(basename(headcount_files)) ==
      tolower(source_info$filename)
  ]
  
  if (length(file) != 1) {
    stop(
      "Expected exactly one workbook named: ",
      source_info$filename,
      ". Found: ",
      length(file)
    )
  }
  
  # Check the year and snapshot in cell A2
  header <- readxl::read_excel(
    file,
    sheet = 1,
    range = "A2",
    col_names = FALSE,
    col_types = "text"
  )
  
  title <- as.character(header[[1]][1])
  
  # Remove punctuation so hyphens and en dashes match
  title_digits <- gsub("[^0-9]", "", title)
  expected_year <- gsub("[^0-9]", "", source_info$source_year)
  
  if (
    is.na(title) ||
    !grepl(expected_year, title_digits, fixed = TRUE) ||
    !grepl("135-Day", title, fixed = TRUE)
  ) {
    stop(
      "Unexpected year or snapshot in ",
      basename(file),
      ": ",
      title
    )
  }
  
  # Read data after the seven header rows
  raw <- readxl::read_excel(
    file,
    sheet = 1,
    skip = 7,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )
  
  if (ncol(raw) != 16) {
    stop("Unexpected column count in ", year)
  }
  
  names(raw) <- c(
    "school_id", "district_name", "school_name",
    "enrollment", "female", "male", "gender_missing",
    "black", "american_indian", "asian", "hispanic",
    "pacific_islander", "multiracial", "white",
    "race_missing", "poverty_count"
  )
  
  # Keep school records and standardize identifiers
  raw <- raw |>
    dplyr::filter(
      grepl("^[0-9]{6,7}$", trimws(school_id))
    ) |>
    dplyr::mutate(
      school_id = sprintf(
        "%07d",
        as.integer(trimws(school_id))
      ),
      district_name = trimws(
        gsub("[[:space:]]+", " ", district_name)
      ),
      school_name = trimws(
        gsub("[[:space:]]+", " ", school_name)
      ),
      academic_year = year,
      headcount_day = source_info$headcount_day,
      headcount_source_file = basename(file)
    )
  
  # Convert counts without treating missing entries as zero
  for (variable in count_variables) {
    
    values <- gsub(",", "", trimws(raw[[variable]]))
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
    
    raw[[variable]] <- as.numeric(values)
  }
  
  stopifnot(
    nrow(raw) > 0,
    !anyNA(raw$school_id),
    !anyDuplicated(raw$school_id)
  )
  
  cat(year, ":", nrow(raw), "schools\n")
  
  return(raw)
}


# STEP 3: Combine all five years
headcount_panel <- years |>
  lapply(clean_headcounts) |>
  dplyr::bind_rows()

stopifnot(
  setequal(headcount_panel$academic_year, years),
  
  all(grepl("^[0-9]{7}$", headcount_panel$school_id)),
  
  !anyDuplicated(
    headcount_panel[c("school_id", "academic_year")]
  ),
  
  all(headcount_panel$headcount_day == 135L)
)


# STEP 4: Validate student counts
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

print(invalid_percentages)

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
cat(
  "School-year observations:",
  nrow(school_characteristics_panel),
  "\n"
)