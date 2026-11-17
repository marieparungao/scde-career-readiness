# ============================================================
# 00: DOWNLOAD AND INVENTORY SCDE SOURCE FILES
# ============================================================
# Purpose: Locate or download 12 SCDE workbooks for academic
# years 2022-23 through 2025-26, and verify their contents.
#
# Keep original/raw filenames. Only remove a browser-added
# " (1)", " (2)", etc. suffix when it is safe to do so.
#
# A 2023 report card belongs to academic year 2022-23;
# 2024 -> 2023-24, 2025 -> 2024-25, 2026 -> 2025-26.
#
# Output: data/raw/scde_source_manifest.csv
# Subsequent scripts should read file paths from this manifest.
# ============================================================

# STEP 1: Academic years and official download links

years <- c("2022-23", "2023-24", "2024-25", "2025-26")
end_years <- as.integer(substr(years, 1, 4)) + 1L

report_base <- paste0(
  "https://screportcards.ed.sc.gov/files/", end_years, "/data-files/"
)

researcher_urls <- paste0(
  report_base,
  ifelse(years == "2025-26",
         "report-card-data-for-researchers-",
         "report-cards-data-for-researchers-"),
  years, "/"
)

additional_urls <- paste0(
  report_base,
  ifelse(years == "2025-26",
         "report-card-data-additional-info-for-",
         "report-cards-data-additional-info-for-"),
  years, "/"
)

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
  headcount_base, years, "-active-student-headcounts/", headcount_paths
)

# These report-card filenames are from the team member's downloads.
# Browser-added suffixes and capitalization can vary by computer.
# The 135-day headcount fallback filenames are project labels;
# existing headcount filenames are preserved when already present.

downloads <- data.frame(
  academic_year = rep(years, 3),
  type = rep(c("researcher", "additional", "headcount"), each = length(years)),
  fallback_filename = c(
    "ReportCardData_forResearchers2023_3.XLSx",
    "ReportCardData_forResearchers2024_11.xlsx",
    "ReportCardData_forResearchers2025 5.xlsx",
    "ReportCardData_forResearchers2026.XLSx",
    "ReportCardData_AdditionalInfo2023_3.XLSx",
    "ReportCardData_AdditionalInfo2024_6.XLSx",
    "ReportCardData_AdditionalInfo2025 5.xlsx",
    "ReportCardData_AdditionalInfo2026.XLSx",
    paste0("Headcount135_", years, ".xlsx")
  ),
  url = c(researcher_urls, additional_urls, headcount_urls),
  stringsAsFactors = FALSE
)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# STEP 2: Validate workbook type, academic year, and headcount day

validate_file <- function(file, type, year) {
  sheets <- readxl::excel_sheets(file)
  end_year <- as.integer(substr(year, 1, 4)) + 1L
  start_year <- end_year - 1L
  
  if (type == "headcount") {
    if (!"school by gender race pip" %in% sheets) return(FALSE)
    title <- readxl::read_excel(
      file, sheet = "school by gender race pip", range = "A2",
      col_names = FALSE, col_types = "text"
    )[[1]][1]
    return(!is.na(title) &&
             grepl(as.character(start_year), title, fixed = TRUE) &&
             grepl(as.character(end_year), title, fixed = TRUE) &&
             grepl("135.Day", title, ignore.case = TRUE))
  }
  
  sheet <- if (type == "researcher") "4.CollegeCareer" else
    "5b.SchoolClimate_ChronicAbs"
  if (!sheet %in% sheets) return(FALSE)
  
  sample <- readxl::read_excel(
    file, sheet = sheet, n_max = 1, col_types = "text"
  )
  "ReportCardYear" %in% names(sample) &&
    !is.na(sample$ReportCardYear[1]) &&
    suppressWarnings(as.integer(sample$ReportCardYear[1])) == end_year
}

# STEP 3: Find the correct existing file or download it

manifest <- downloads[c("academic_year", "type", "url")]
manifest$file_path <- NA_character_
failed <- character()

for (i in seq_len(nrow(downloads))) {
  year <- downloads$academic_year[i]
  type <- downloads$type[i]
  
  existing <- list.files(
    "data/raw", pattern = "\\.xlsx$", recursive = TRUE,
    full.names = TRUE, ignore.case = TRUE
  )
  matches <- existing[vapply(existing, function(f) {
    tryCatch(isTRUE(validate_file(f, type, year)),
             error = function(e) FALSE)
  }, logical(1))]
  
  if (!length(matches)) {
    # download.file() requires a destination filename because the
    # public links end with '/'; the fallback preserves the source
    # names documented above where available.
    destination <- file.path("data/raw", downloads$fallback_filename[i])
    if (file.exists(destination)) {
      cat("Invalid file already exists:", destination, "\n")
      failed <- c(failed, paste(type, year))
      next
    }
    
    ok <- tryCatch({
      status <- download.file(downloads$url[i], destfile = destination,
                              mode = "wb", quiet = TRUE)
      if (status != 0) stop("Download unsuccessful")
      if (!isTRUE(validate_file(destination, type, year))) {
        stop("Downloaded workbook did not pass validation")
      }
      TRUE
    }, error = function(e) {
      cat("Download/validation error:", conditionMessage(e), "\n")
      FALSE
    })
    
    if (!ok) {
      if (file.exists(destination)) file.remove(destination)
      failed <- c(failed, paste(type, year))
      cat("Manual download:", downloads$url[i], "\n")
      next
    }
    matches <- destination
  }
  
  # If multiple matching files exist, only treat them as interchangeable
  # if they are byte-for-byte identical. Do not delete duplicates.
  if (length(matches) > 1) {
    if (length(unique(unname(tools::md5sum(matches)))) != 1) {
      cat("Different valid copies found for:", type, year, "\n")
      print(matches)
      failed <- c(failed, paste(type, year))
      next
    }
    cat("Identical extra copies found for:", type, year, "\n")
  }
  
  # Prefer the documented source filename when an identical copy exists.
  preferred <- matches[
    tolower(basename(matches)) ==
      tolower(downloads$fallback_filename[i])
  ]
  chosen <- if (length(preferred)) preferred[1] else matches[1]
  
  # Only normalize a browser duplicate suffix, e.g. "file (1).xlsx".
  # Do not rename other original files or overwrite an existing file.
  base_name <- sub(" \\([0-9]+\\)(?=\\.xlsx$)", "", basename(chosen),
                   perl = TRUE, ignore.case = TRUE)
  if (base_name != basename(chosen)) {
    clean_path <- file.path(dirname(chosen), base_name)
    if (!file.exists(clean_path) && file.rename(chosen, clean_path)) {
      chosen <- clean_path
      cat("Removed browser duplicate suffix:", basename(chosen), "\n")
    }
  }
  
  manifest$file_path[i] <- chosen
  cat("Verified:", year, type, "->", basename(chosen), "\n")
}

# STEP 4: Save mapping for downstream scripts

if (length(failed)) {
  stop("Unresolved source files: ", paste(failed, collapse = "; "),
       ". Resolve these before building the datasets.")
}

write.csv(
  manifest[c("academic_year", "type", "file_path", "url")],
  "data/raw/scde_source_manifest.csv", row.names = FALSE
)

cat("\nAll 12 SCDE workbooks verified. Source manifest saved.\n")
