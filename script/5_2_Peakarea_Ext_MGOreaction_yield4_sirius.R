# Load necessary libraries
library(readr)
library(dplyr)
library(writexl)

# Define relative file paths
input_folder <- "output_data/5_Peakarea_Ext_MGOreaction_yield4"
output_folder <- "output_data/6_Peakarea_Ext_MGOreaction_SIRIUS"

# Create output folder if it does not exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# List all Excel files in the input folder
input_files <- list.files(input_folder, pattern = "\\.xlsx$", full.names = TRUE)

# Process each Excel file in the input folder
for (file in input_files) {
  # Read the current file
  input_data <- readxl::read_excel(file)
  
  # Check if the necessary columns are present
  if (!all(c("ConfidenceScoreExact", "ConfidenceScoreApproximate", "Efficiency", "name") %in% colnames(input_data))) {
    message(paste("The file is missing necessary columns, skipping processing:", basename(file)))
    next
  }
  
  # Filter data: retain only rows where ConfidenceScoreApproximate >= 0.64
  filtered_data <- input_data %>%
    filter(ConfidenceScoreApproximate >= 0.64)
  
  
  # Skip processing if the filtered data is empty
  if (nrow(filtered_data) == 0) {
    message(paste("No data after filtering, skipping file:", basename(file)))
    next
  }
  
  # Re-rank the Rank column, sorting by Efficiency in descending order and starting from 1
  filtered_data <- filtered_data %>%
    arrange(desc(Efficiency)) %>%
    mutate(Rank = dense_rank(desc(Efficiency)))
  
  # Define the output file path
  output_file <- file.path(output_folder, paste0("processed_", basename(file)))
  
  # Write the updated data frame to a new Excel file
  write_xlsx(filtered_data, output_file)
  
  message(paste("Processed and saved:", output_file))
}

message("All files processed successfully.")
