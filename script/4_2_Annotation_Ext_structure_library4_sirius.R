# Load necessary packages
library(readxl)
library(writexl)
library(dplyr)
library(readr)  # For reading CSV files
library(progress)  # For progress bar

# Set file paths
csv_file <- "./input_data/5_Sirius_processed_data/structure_identifications.csv"
target_file <- "./input_data/5_Sirius_processed_data/canopus_structure_summary.csv"
folder2 <- "./output_data/3_2_2_Filter_Ext_MGO_MS2_SPmatch_mzRT_Filtered_Data"
output_folder <- "./output_data/4_Annotation_Ext_structure_library2"

# Ensure the output folder exists
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Read the CSV file and select the required columns
data1 <- read.csv(csv_file) %>%
  select(mappingFeatureId, InChI, name, smiles, ConfidenceScoreExact, ConfidenceScoreApproximate) %>%
  mutate(mappingFeatureId = as.character(mappingFeatureId))

# Read the canopus_structure_summary.csv file and select the required columns
target_data <- read_csv(target_file) %>%
  mutate(mappingFeatureId = as.character(mappingFeatureId)) %>%
  select(mappingFeatureId, precursorFormula, adduct, `NPC#class`, `NPC#superclass`)

# Get the file names from folder2
files_folder2 <- list.files(folder2, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)

# Initialize the progress bar
pb <- progress_bar$new(
  total = length(files_folder2),
  format = "  Processing [:bar] :current/:total (:percent) ETA: :eta"
)

# Iterate through the Excel files in folder2
for (file2 in files_folder2) {
  pb$tick()  # Update progress bar
  
  # Print the name of the file being processed
  cat("Processing file: ", file2, "\n")
  
  # Read data from folder2
  data2 <- read_xlsx(file2)
  
  # Ensure row.ID and mappingFeatureId columns have consistent data types
  data2 <- data2 %>% mutate(row.ID = as.character(row.ID))
  
  # First join with data1
  matched_data <- data2 %>%
    left_join(data1, by = c("row.ID" = "mappingFeatureId")) %>%
    mutate(
      name = ifelse(is.na(name), "unnamed", name),
      ConfidenceScoreExact = ifelse(is.na(ConfidenceScoreExact), 0, ConfidenceScoreExact),
      ConfidenceScoreApproximate = ifelse(is.na(ConfidenceScoreApproximate), 0, ConfidenceScoreApproximate)
    )
  
  # Second join with target_data
  matched_data <- matched_data %>%
    left_join(target_data, by = c("row.ID" = "mappingFeatureId")) %>%
    mutate(
      precursorFormula = ifelse(is.na(precursorFormula), "unknown", precursorFormula),
      adduct = ifelse(is.na(adduct), "unknown", adduct),
      `NPC#class` = ifelse(is.na(`NPC#class`), "unknown", `NPC#class`),
      `NPC#superclass` = ifelse(is.na(`NPC#superclass`), "unknown", `NPC#superclass`)
    )
  
  # Check for unmatched row.ID values and print warning
  unmatched_ids <- setdiff(data2$row.ID, c(data1$mappingFeatureId, target_data$mappingFeatureId))
  if (length(unmatched_ids) > 0) {
    cat("The following row.ID values were not matched: \n", paste(unmatched_ids, collapse = ", "), "\n")
  }
  
  # Print a preview of the matched data (first few rows)
  cat("Preview of matched data (first few rows): \n")
  print(head(matched_data))
  
  # Construct the output file path
  output_file <- paste0("SP_", tools::file_path_sans_ext(basename(file2)), ".xlsx")
  output_path <- file.path(output_folder, output_file)
  
  # Write the results to a new file in the output folder, retaining all original data
  writexl::write_xlsx(matched_data, output_path)
  
  # Print processing progress
  cat("Processing complete: ", file2, "output saved to", output_path, "\n")
}

# Print completion message
cat("All files have been processed successfully!\n")
