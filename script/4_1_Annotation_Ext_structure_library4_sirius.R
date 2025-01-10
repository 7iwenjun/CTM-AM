library(readxl)

# 1. Define folder and file paths (using relative paths)
excel_folder_path <- "./output_data/3_2_2_Filter_Ext_MGO_MS2_SPmatch_mzRT_Filtered_Data"
mgf_path <- "./input_data/3_Ext_MZmine_data_align/E31_MZminedata.mgf"
matched_mgf_path <- "./output_data/matched_output.mgf"
unmatched_mgf_path <- "./output_data/unmatched_output.mgf"

# 2. Read all `row.ID` values from Excel files
row_id_list <- unique(unlist(lapply(
  list.files(excel_folder_path, pattern = "\\.xlsx$", full.names = TRUE),
  function(file) {
    tryCatch({
      excel_data <- read_excel(file, sheet = 1, col_types = "text")
      if ("row.ID" %in% colnames(excel_data)) {
        return(as.character(excel_data$row.ID))
      }
    }, error = function(e) {
      cat("Error reading file:", file, "\n", e, "\n")
      return(NULL)
    })
  }
)))

cat("Total number of `row.ID` retrieved from Excel files:", length(row_id_list), "\n")

# 3. Read the MGF file
mgf_lines <- readLines(mgf_path)

# 4. Initialize matched and unmatched spectra
matched_mgf_content <- c()
unmatched_mgf_content <- c()

# 5. Process spectra in the MGF file for matching and categorization
capture_spectrum <- FALSE
current_spectrum <- c()
matched_count <- 0
unmatched_count <- 0

for (line in mgf_lines) {
  if (grepl("BEGIN IONS", line)) {
    # Start capturing a spectrum
    capture_spectrum <- TRUE
    current_spectrum <- c(line)
  } else if (grepl("END IONS", line)) {
    # Finish capturing a spectrum
    current_spectrum <- c(current_spectrum, line)
    
    # Look for `FEATURE_ID`
    feature_id_line <- grep("FEATURE_ID=", current_spectrum, value = TRUE)
    if (length(feature_id_line) > 0) {
      feature_id <- trimws(sub("FEATURE_ID=", "", feature_id_line))
      
      # Determine if it matches and categorize
      if (feature_id %in% row_id_list) {
        matched_mgf_content <- c(matched_mgf_content, current_spectrum)
        matched_count <- matched_count + 1
      } else {
        unmatched_mgf_content <- c(unmatched_mgf_content, current_spectrum)
        unmatched_count <- unmatched_count + 1
      }
    } else {
      # If no `FEATURE_ID`, categorize as unmatched
      unmatched_mgf_content <- c(unmatched_mgf_content, current_spectrum)
      unmatched_count <- unmatched_count + 1
    }
    capture_spectrum <- FALSE
  } else if (capture_spectrum) {
    # Capture spectrum content
    current_spectrum <- c(current_spectrum, line)
  }
}

# 6. Write matched and unmatched spectra to new MGF files
writeLines(matched_mgf_content, matched_mgf_path)
writeLines(unmatched_mgf_content, unmatched_mgf_path)

# 7. Output information
cat("Total number of matched spectra:", matched_count, "\n")
cat("Total number of unmatched spectra:", unmatched_count, "\n")
cat("Matched MGF data has been saved to:", matched_mgf_path, "\n")
cat("Unmatched MGF data has been saved to:", unmatched_mgf_path, "\n")
