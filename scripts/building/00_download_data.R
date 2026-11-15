# ============================================================
# DOWNLOAD PROJECT DATA
# ============================================================
# Purpose: Download public SCDE datasets needed for the
# career-readiness project.
#
# Run this script before the data-building scripts.
# Files already downloaded will be skipped.
# ============================================================


# STEP 1: Identify academic years and source locations

years <- c("2022-23", "2023-24", "2024-25", "2025-26")

# Official report-card download directories
report_base <- c(
  "https://screportcards.ed.sc.gov/files/2023/data-files/",
  "https://screportcards.ed.sc.gov/files/2024/data-files/",
  "https://screportcards.ed.sc.gov/files/2025/data-files/",
  "https://screportcards.ed.sc.gov/files/2026/data-files/"
)


# STEP 2: Identify researcher workbooks

researcher_files <- data.frame(
  filename = paste0(
    "ReportCardData_forResearchers", years, ".xlsx"
  ),
  url = paste0(report_base, c(
    "report-cards-data-for-researchers-2022-23/",
    "report-cards-data-for-researchers-2023-24/",
    "report-cards-data-for-researchers-2024-25/",
    "report-card-data-for-researchers-2025-26/"
  ))
)


# NOTE: The 2025-26 Additional Information workbook may
# encounter an SSL error during automatic downloading.
# If this occurs, follow the manual instructions in Step 7.

# STEP 3: Identify additional-information workbooks

additional_files <- data.frame(
  filename = paste0(
    "ReportCardData_AdditionalInfo", years, ".xlsx"
  ),
  url = paste0(report_base, c(
    "report-cards-data-additional-info-for-2022-23/",
    "report-cards-data-additional-info-for-2023-24/",
    "report-cards-data-additional-info-for-2024-25/",
    "report-card-data-additional-info-for-2025-26/"
  ))
)


# STEP 4: Identify 135-day school headcount workbooks

headcount_base <- paste0(
  "https://www.ed.sc.gov/data/other/student-counts/",
  "active-student-headcounts/"
)

headcount_files <- data.frame(
  filename = paste0("Headcount135_", years, ".xlsx"),
  url = paste0(headcount_base, c(
    paste0(
      "2022-23-active-student-headcounts/",
      "135-day-school-headcount-by-gender-ethnicity-",
      "and-pupils-in-poverty-2022-23/"
    ),
    paste0(
      "2023-24-active-student-headcounts/",
      "school-headcount-by-gender-ethnicity-",
      "and-pupils-in-poverty/"
    ),
    paste0(
      "2024-25-active-student-headcounts/",
      "135-day-school-headcount-by-gender-ethnicity-",
      "and-pupils-in-poverty/"
    ),
    paste0(
      "2025-26-active-student-headcounts/",
      "135-day-school-headcount-by-gender-ethnicity-",
      "and-pupils-in-poverty/"
    )
  ))
)


# STEP 5: Download any files not already saved

downloads <- rbind(
  researcher_files,
  additional_files,
  headcount_files
)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

for (i in seq_len(nrow(downloads))) {
  
  destination <- file.path("data/raw", downloads$filename[i])
  
  if (file.exists(destination)) {
    cat("Already downloaded:", downloads$filename[i], "\n")
    next
  }
  
  tryCatch({
    
    download.file(
      downloads$url[i],
      destfile = destination,
      mode = "wb",
      quiet = TRUE
    )
    
    # Confirm that the downloaded file is a readable Excel workbook
    readxl::excel_sheets(destination)
    
    cat("Downloaded:", downloads$filename[i], "\n")
    
  }, error = function(e) {
    
    # Delete incomplete or invalid downloads
    if (file.exists(destination)) file.remove(destination)
    
    cat("DOWNLOAD FAILED:", downloads$filename[i], "\n")
    cat("Reason:", conditionMessage(e), "\n")
  })
}


# STEP 6: Verify that all expected files exist

missing_files <- setdiff(
  downloads$filename,
  list.files("data/raw")
)

cat("\nFiles still missing:", length(missing_files), "\n")
print(missing_files)

# STEP 7: Provide manual download instructions if needed

# If any files could not be downloaded automatically,
# display their source links and required filenames.

if (length(missing_files) > 0) {
  
  cat("\nMANUAL DOWNLOAD INSTRUCTIONS:\n")
  
  for (name in missing_files) {
    
    cat("\nFile:", name, "\n")
    cat("Source:", downloads$url[downloads$filename == name], "\n")
    cat("Save to: data/raw/", name, "\n", sep = "")
  }
}