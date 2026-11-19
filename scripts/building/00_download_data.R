
# ============================================================
# 00: DOWNLOAD AND VALIDATE PROJECT DATA
# ============================================================
# Purpose: Download and verify SCDE source data for
# academic years 2022-23 through 2025-26.
#
# All files are saved in data/raw/ with academic-year names.
# Existing files are validated rather than downloaded again.
#
# Run this script before building any datasets.
# ============================================================


# STEP 1: Define academic years and source URLs

years <- c("2022-23", "2023-24", "2024-25", "2025-26")

# Report-card file directories use the END of the academic year
report_base <- paste0(
  "https://screportcards.ed.sc.gov/files/",
  as.integer(substr(years, 1, 4)) + 1,
  "/data-files/"
)

# Report-card download links
researcher_urls <- paste0(
  report_base,
  ifelse(
    years == "2025-26",
    "report-card-data-for-researchers-",
    "report-cards-data-for-researchers-"
  ),
  years, "/"
)

additional_urls <- paste0(
  report_base,
  ifelse(
    years == "2025-26",
    "report-card-data-additional-info-for-",
    "report-cards-data-additional-info-for-"
  ),
  years, "/"
)

# Headcount download links vary slightly across years
headcount_base <- paste0(
  "https://www.ed.sc.gov/data/other/student-counts/",
  "active-student-headcounts/"
)

headcount_paths <- c(
  "135-day-school-headcount-by-gender-ethnicity-and-pupils-in-poverty-2022-23/",
  "school-headcount-by-gender-ethnicity-and-pupils-in-poverty/",
  "135-day-school-headcount-by-gender-ethnicity-and-pupils-in-poverty/",
  "135-day-school-headcount-by-gender-ethnicity-and-pupils-in-poverty/"
)

headcount_urls <- paste0(
  headcount_base,
  years, "-active-student-headcounts/",
  headcount_paths
)


# STEP 2: Create one list of required files

downloads <- data.frame(
  year = rep(years, 3),
  type = rep(
    c("researcher", "additional", "headcount"),
    each = length(years)
  ),
  filename = c(
    paste0("ReportCardData_forResearchers", years, ".xlsx"),
    paste0("ReportCardData_AdditionalInfo", years, ".xlsx"),
    paste0("Headcount135_", years, ".xlsx")
  ),
  url = c(researcher_urls, additional_urls, headcount_urls)
)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)


# STEP 3: Function to validate each workbook

validate_file <- function(file, type, year) {
  
  # Confirm the file is a readable Excel workbook
  readxl::excel_sheets(file)
  
  start_year <- as.integer(substr(year, 1, 4))
  report_year <- start_year + 1L
  
  if (type == "headcount") {
    
    # Verify the internal year and 135-day snapshot
    title <- readxl::read_excel(
      file,
      sheet = "school by gender race pip",
      range = "A2",
      col_names = FALSE,
      col_types = "text"
    )[[1]][1]
    
    if (is.na(title) ||
        !grepl(as.character(start_year), title) ||
        !grepl(as.character(report_year), title) ||
        !grepl("135.Day", title, ignore.case = TRUE)) {
      stop("Incorrect headcount year or snapshot")
    }
    
  } else {
    
    # Verify the report-card year inside the workbook
    sheet <- if (type == "researcher") {
      "4.CollegeCareer"
    } else {
      "5b.SchoolClimate_ChronicAbs"
    }
    
    sample <- readxl::read_excel(
      file,
      sheet = sheet,
      n_max = 1,
      col_types = "text"
    )
    
    if (!"ReportCardYear" %in% names(sample) ||
        is.na(sample$ReportCardYear[1]) ||
        as.integer(sample$ReportCardYear[1]) != report_year) {
      stop("Incorrect report-card year")
    }
  }
  
  TRUE
}


# STEP 4: Download missing files and validate everything

failed <- character()

for (i in seq_len(nrow(downloads))) {
  
  destination <- file.path(
    "data/raw", downloads$filename[i]
  )
  
  # Download only if the file does not already exist
  new_file <- !file.exists(destination)
  
  if (new_file) {
    
    tryCatch({
      
      status <- download.file(
        downloads$url[i],
        destfile = destination,
        mode = "wb",
        quiet = TRUE
      )
      
      if (status != 0) stop("Download unsuccessful")
      
    }, error = function(e) {
      cat("Download error:", conditionMessage(e), "\n")
    })
  }
  
  # Validate both downloaded and existing files
  valid <- tryCatch(
    validate_file(
      destination,
      downloads$type[i],
      downloads$year[i]
    ),
    error = function(e) {
      cat("Validation error:", conditionMessage(e), "\n")
      FALSE
    }
  )
  
  if (valid) {
    
    cat("Verified:", downloads$filename[i], "\n")
    
  } else {
    
    failed <- c(failed, downloads$filename[i])
    
    # Remove only invalid files downloaded during this run
    if (new_file && file.exists(destination)) {
      file.remove(destination)
    }
  }
}


# STEP 5: Report any problems

if (length(failed) > 0) {
  
  cat("\nFILES REQUIRING ATTENTION:\n")
  
  for (name in failed) {
    
    i <- match(name, downloads$filename)
    
    cat("\nFile:", name)
    cat("\nDownload:", downloads$url[i])
    cat("\nSave as: data/raw/", name, "\n", sep = "")
  }
  
  stop("\nResolve the files above and rerun Script 00.")
}

cat("\nAll 12 SCDE files downloaded and validated!\n")