# ============================================================
# 02: BUILD SCHOOL CHARACTERISTICS PANEL
# ============================================================
# Purpose: Construct school-level poverty, gender, and
# racial/ethnic composition from 135-day headcounts.
#
# Input: Headcount135_YYYY-YY.xlsx
# Output: data/processed/school_characteristics_panel.csv
#
# Academic years: 2022-23 through 2025-26
# Includes all school types; high schools are selected
# through the readiness panel when building the master.
# ============================================================


# STEP 1: Define academic years

years <- c("2022-23", "2023-24", "2024-25", "2025-26")

count_variables <- c(
  "enrollment", "female", "male", "gender_missing",
  "black", "american_indian", "asian", "hispanic",
  "pacific_islander", "multiracial", "white",
  "race_missing", "poverty_count"
)


# STEP 2: Create a function to clean one year

clean_headcounts <- function(year) {
  
  file <- file.path(
    "data/raw",
    paste0("Headcount135_", year, ".xlsx")
  )
  
  if (!file.exists(file)) {
    stop("Missing file: ", file, ". Run Script 00 first.")
  }
  
  # Read school records after the seven header rows
  raw <- readxl::read_excel(
    file,
    sheet = "school by gender race pip",
    skip = 7,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )
  
  stopifnot(ncol(raw) == 16)
  
  # Assign names matching the source column order
  names(raw) <- c(
    "school_id", "district_name", "school_name",
    count_variables
  )
  
  # Keep school records and standardize seven-digit IDs
  clean <- raw |>
    dplyr::filter(
      grepl("^[0-9]{6,7}$", trimws(school_id))
    ) |>
    dplyr::mutate(
      academic_year = year,
      school_id = sprintf(
        "%07d", as.integer(trimws(school_id))
      ),
      district_name = gsub(
        "[[:space:]]+", " ", district_name
      ),
      school_name = gsub(
        "[[:space:]]+", " ", school_name
      )
    )
  
  # Convert student counts to numeric values
  for (variable in count_variables) {
    
    values <- gsub(",", "", trimws(clean[[variable]]))
    values[values %in% c("*", "")] <- NA_character_
    
    invalid <- unique(values[
      !is.na(values) &
        is.na(suppressWarnings(as.numeric(values)))
    ])
    
    if (length(invalid) > 0) {
      stop("Unexpected values in ", year, " / ", variable)
    }
    
    clean[[variable]] <- as.numeric(values)
  }
  
  # Check school identifiers and numerical completeness
  stopifnot(
    !anyNA(clean$school_id),
    !anyDuplicated(clean$school_id),
    all(complete.cases(clean[count_variables]))
  )
  
  cat(year, ":", nrow(clean), "schools\n")
  
  return(clean)
}


# STEP 3: Combine all four years

headcount_panel <- years |>
  lapply(clean_headcounts) |>
  dplyr::bind_rows()


# STEP 4: Validate enrollment and subgroup counts

# Confirm unique school-year records
stopifnot(
  !anyDuplicated(
    headcount_panel[c("school_id", "academic_year")]
  )
)

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

# All counts must be consistent with enrollment
stopifnot(
  all(headcount_panel$enrollment > 0),
  all(gender_total == headcount_panel$enrollment),
  all(race_total == headcount_panel$enrollment),
  all(headcount_panel$poverty_count <= headcount_panel$enrollment)
)

cat("\nEnrollment and demographic counts validated.\n")


# STEP 5: Calculate school characteristics

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


# STEP 6: Validate percentages and save

percentage_variables <- c(
  "poverty_rate",
  grep("_share$", names(school_characteristics_panel),
       value = TRUE)
)

# All calculated percentages must be between 0 and 100
stopifnot(
  all(sapply(
    school_characteristics_panel[percentage_variables],
    function(x) all(!is.na(x) & x >= 0 & x <= 100)
  ))
)

# Save the completed characteristics panel
dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  school_characteristics_panel,
  "data/processed/school_characteristics_panel.csv",
  row.names = FALSE
)

cat("\nSchool characteristics panel saved successfully!\n")
cat("Observations:", nrow(school_characteristics_panel), "\n")
cat("Variables:", ncol(school_characteristics_panel), "\n")