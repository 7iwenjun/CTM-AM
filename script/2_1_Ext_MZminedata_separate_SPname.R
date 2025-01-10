# Load necessary packages
library(dplyr)
library(purrr)  # For list operations
library(fs)     # For file path operations

# Set relative paths for input and output folders
input_folder <- "input_data/3_Ext_MZmine_data_align"
output_folder <- "output_data/2_1_Data_Ext_MZminedata_separate_SPname"

# Create output folder (create if it doesn't exist)
dir_create(output_folder)

# Data processing function: extract data by column name and generate CSV files
process_data <- function(df) {
  # Get column name information
  col_names <- colnames(df)
  
  # Get base columns
  base_cols <- c("row.ID", "row.m.z", "row.retention.time")
  
  # Get columns starting with "Ext_E" and identify their prefixes
  e_cols <- grep("Ext_E", col_names, value = TRUE)
  e_prefixes <- unique(gsub("_(MGO_)?24h_ddms30.mzML.Peak.area", "", e_cols))
  
  processed_data_list <- list()
  
  for (prefix in e_prefixes) {
    # Get columns related to this prefix
    e_group_cols <- grep(paste0("^", prefix, "_"), col_names, value = TRUE)
    
    if (length(e_group_cols) > 1) {
      # Ensure the "MGO" column is last
      mgo_col <- grep("MGO", e_group_cols, value = TRUE)
      non_mgo_cols <- setdiff(e_group_cols, mgo_col)
      sorted_group_cols <- c(non_mgo_cols, mgo_col)
      
      # Extract relevant columns
      combined_df <- df[, c(base_cols, sorted_group_cols)]
      
      # Remove rows where all peak area values are zero
      row_has_nonzero <- rowSums(combined_df[, -c(1:3)] != 0) > 0
      filtered_df <- combined_df[row_has_nonzero, ]
      
      # Sort by 'row.m.z'
      sorted_df <- filtered_df[order(filtered_df$`row.m.z`), ]
      
      # Save processed data
      processed_data_list[[prefix]] <- sorted_df
    } else {
      cat("The number of columns for prefix", prefix, "is less than 2, skipping.\n")
    }
  }
  
  return(processed_data_list)
}

# Batch process all CSV files in the folder
csv_files <- dir_ls(input_folder, regexp = "\\.csv$")

# Process files one by one and save the results
map(csv_files, function(file_path) {
  # Read CSV file
  df <- read.csv(file_path)
  
  # Process data
  processed_data_list <- process_data(df)
  
  # Save each processed dataset to the output folder
  for (name in names(processed_data_list)) {
    final_data <- processed_data_list[[name]]
    
    # Generate a standardized output file name
    standardized_name <- sub("Ext2", "Ext", name)  # Remove the "2" after Ext
    output_path <- file.path(output_folder, paste0(standardized_name, "_processed_Ext_Mzminedata_quant.csv"))
    
    # Save the CSV file
    write.csv(final_data, output_path, row.names = FALSE)
    cat("Processed file saved to:", output_path, "\n")
  }
})

cat("All files have been processed.\n")
