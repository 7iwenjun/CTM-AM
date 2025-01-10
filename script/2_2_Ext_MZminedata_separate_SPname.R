library(dplyr)
library(fs)
library(purrr)
library(readr)

# Set relative paths for input and output folders
input_folder <- "output_data/2_1_Data_Ext_MZminedata_separate_SPname"
output_folder <- "output_data/2_2_Data_Ext_MZminedata_separate_SPname"

# Create output folder (create if it doesn't exist)
dir_create(output_folder)

# Helper function to calculate ppm error
calculate_ppm <- function(observed, theoretical) {
  abs(observed - theoretical) / theoretical * 1e6
}

# Data processing function: group by m/z ±10 ppm, calculate peak area difference, and annotate labels
process_grouped_data <- function(df) {
  # Identify MGO and non-MGO columns
  mgo_cols <- grep("MGO", colnames(df), value = TRUE)
  non_mgo_cols <- setdiff(grep("Ext_E", colnames(df), value = TRUE), mgo_cols)
  
  # Group by m/z ±10 ppm
  df <- df %>%
    arrange(`row.m.z`) %>%
    mutate(
      Peakgroup_id = cumsum(c(TRUE, calculate_ppm(`row.m.z`[-1], `row.m.z`[-n()]) > 10))
    )
  
  # Calculate peak area difference and annotate labels by group
  result <- df %>%
    group_by(Peakgroup_id) %>%
    mutate(
      NonMGO.peak.area = sum(across(all_of(non_mgo_cols)), na.rm = TRUE),
      MGO.peak.area = sum(across(all_of(mgo_cols)), na.rm = TRUE),
      Peak_Area_Change = abs(MGO.peak.area - NonMGO.peak.area),
      Label = case_when(
        MGO.peak.area > NonMGO.peak.area ~ "P",
        MGO.peak.area < NonMGO.peak.area ~ "S",
        TRUE ~ ""
      )
    ) %>%
    ungroup()
  
  return(result)
}

# Batch process all CSV files
csv_files <- dir_ls(input_folder, regexp = "\\.csv$")

# Iterate over each file, process the data, and save the results
map(csv_files, function(file_path) {
  # Read CSV file
  df <- read_csv(file_path)
  
  # Process data, adding Peakgroup_id, peak area difference, and label columns
  final_data <- process_grouped_data(df)
  
  # Generate new output file path
  file_name <- basename(file_path)
  output_path <- file.path(output_folder, file_name)
  
  # Save the processed data, keeping original columns and adding new ones
  write_csv(final_data, output_path)
  cat("Processed and saved file:", output_path, "\n")
})

cat("All files have been processed.\n")
