library(readxl)
library(dplyr)
library(openxlsx)
library(stringr)

# Define input and output folder paths
input_folder <- "./output_data/4_Annotation_Ext_structure_library2"
output_folder <- "./output_data/5_Peakarea_Ext_MGOreaction_yield4"

# Create the output folder if it does not exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Get all Excel file paths from the input folder
files <- list.files(input_folder, pattern = "*.xlsx", full.names = TRUE)

# Initialize a variable to store the total sum of S_Peak_Area_Sum1 across all files
all_files_total_S_Peak_Area_Sum1 <- 0

# Step 1: Iterate through each file to calculate S_Peak_Area_Sum1
for (file_path in files) {
  
  # Read the Excel file
  data <- read_excel(file_path)
  
  # Print the name of the file being processed
  print(paste("Processing file:", file_path))
  
  # Modify the grouping logic
  if (nrow(data) == 0) {
    # If the dataset is empty, skip processing
    print(paste("File is empty, skipping:", file_path))
    next
  } else if (nrow(data) == 1) {
    # If the dataset has only one row, directly assign group_id = 1
    data <- data %>%
      mutate(new_group_id = 1)
  } else if (any(data$ConfidenceScoreApproximate >= 0.64, na.rm = TRUE)) {
    # If there are values greater than or equal to 0.64 in the ConfidenceScoreApproximate column, group by the name column
    data <- data %>%
      group_by(name) %>%
      mutate(new_group_id = cur_group_id()) %>%
      ungroup() %>%
      mutate(new_group_id = as.numeric(factor(new_group_id)))
  } else {
    # If all values in the ConfidenceScoreApproximate column are less than 0.64, group by the S_value column within a 5ppm tolerance range
    ppm_threshold <- 5e-6  # 5ppm
    data <- data %>%
      arrange(S_value) %>%
      mutate(
        diff_values = c(0, diff(S_value)), # Ensure the length is consistent with the data
        lag_values = lag(S_value, default = first(S_value)), # Ensure lag behaves consistently
        new_group_id = cumsum(diff_values / lag_values > ppm_threshold)
      ) %>%
      select(-diff_values, -lag_values) # Clean up temporary columns
  }
  
  # Calculate S_Peak_Area_Sum1 (peak area before reaction) and remove duplicates
  data_for_S1_calculation <- data %>%
    group_by(new_group_id, S_retention_time) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    group_by(new_group_id) %>%
    distinct(S_Peak.area1, .keep_all = TRUE) %>%  # Remove duplicate S_Peak.area1 data
    summarize(S_Peak_Area_Sum1 = sum(S_Peak.area1, na.rm = TRUE))
  
  # Calculate the total sum of unique S_Peak_Area_Sum1 for the current file
  file_S_Peak_Area_Sum1_total <- sum(unique(data_for_S1_calculation$S_Peak_Area_Sum1), na.rm = TRUE)
  
  # Print the total sum of unique S_Peak_Area_Sum1 for the current file
  print(paste("Total unique S_Peak_Area_Sum1 for the file:", file_S_Peak_Area_Sum1_total))
  
  # Add the file's total sum to the overall total across all files
  all_files_total_S_Peak_Area_Sum1 <- all_files_total_S_Peak_Area_Sum1 + file_S_Peak_Area_Sum1_total
}

# Print the overall total of unique S_Peak_Area_Sum1 across all files
print(paste("Overall total unique S_Peak_Area_Sum1 across all files:", all_files_total_S_Peak_Area_Sum1))

# Step 2: Reprocess each file using the overall total and calculate weighted efficiency
for (file_path in files) {
  
  # Read the Excel file
  data <- read_excel(file_path)
  
  # Modify grouping logic
  if (nrow(data) == 0) {
    # Skip processing if the file is empty
    print(paste("File is empty, skipping:", file_path))
    next
  } else if (nrow(data) == 1) {
    # If the file contains only one row, assign group_id = 1 directly
    data <- data %>%
      mutate(new_group_id = 1)
  } else if (any(data$ConfidenceScoreExact >= 0.5, na.rm = TRUE) | any(data$ConfidenceScoreApproximate >= 0.5, na.rm = TRUE)) {
    # If either ConfidenceScoreExact or ConfidenceScoreApproximate has values >= 0.5, group by the name column
    data <- data %>%
      group_by(name) %>%
      mutate(new_group_id = cur_group_id()) %>%
      ungroup() %>%
      mutate(new_group_id = as.numeric(factor(new_group_id)))
  } else {
    # If both columns have values < 0.5, group by S_value within a 5 ppm error range
    ppm_threshold <- 5e-6  # 5 ppm
    data <- data %>%
      arrange(S_value) %>%
      mutate(
        diff_values = c(0, diff(S_value)),  # Ensure the length matches the data
        lag_values = lag(S_value, default = ifelse(length(S_value) > 0, S_value[1], 0)),  # Handle empty or missing values
        new_group_id = cumsum(diff_values / lag_values > ppm_threshold)
      ) %>%
      select(-diff_values, -lag_values)  # Remove temporary columns
  }
  
  # Calculate S_Peak_Area_Sum1 (peak area before reaction) and remove duplicates
  data_for_S1_calculation <- data %>%
    group_by(new_group_id, S_retention_time) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    group_by(new_group_id) %>%
    distinct(S_Peak.area1, .keep_all = TRUE) %>%  # Remove duplicate S_Peak.area1 data
    summarize(S_Peak_Area_Sum1 = sum(S_Peak.area1, na.rm = TRUE))
  
  # Calculate S_Peak_Area_Sum2 (peak area after reaction) and remove duplicates
  data_for_S2_calculation <- data %>%
    group_by(new_group_id) %>%
    distinct(S_Peak.area2, .keep_all = TRUE) %>%  # Remove duplicate S_Peak.area2 data
    summarize(S_Peak_Area_Sum2 = sum(S_Peak.area2, na.rm = TRUE))
  
  # Merge S_Peak_Area_Sum1 and S_Peak_Area_Sum2 calculations back into the original data
  data <- data %>%
    left_join(data_for_S1_calculation, by = "new_group_id") %>%
    left_join(data_for_S2_calculation, by = "new_group_id")
  
  # Assign weights to each S_Peak_Area_Sum1 using the overall total
  data <- data %>%
    mutate(weight = S_Peak_Area_Sum1 / all_files_total_S_Peak_Area_Sum1)
  
  # Calculate weighted reaction efficiency
  data <- data %>%
    mutate(Efficiency = ((S_Peak_Area_Sum1 - S_Peak_Area_Sum2) * weight) / S_Peak_Area_Sum1)
  
  # Sort by Efficiency in descending order and assign ranks
  data <- data %>%
    arrange(desc(Efficiency)) %>%  # Sort Efficiency in descending order
    mutate(Rank = dense_rank(desc(Efficiency)))  # Use dense_rank for ranking
  
  # Generate the output file path with the same name as the original file
  output_file_path <- file.path(output_folder, basename(file_path))
  
  # Save the processed data
  write.xlsx(data, file = output_file_path)
  
  print(paste("File saved:", output_file_path))
}

print("All files have been processed and saved.")
