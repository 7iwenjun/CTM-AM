# Load necessary libraries
library(openxlsx)
library(dplyr)

# Specify the paths for the data folder and the database file
data_folder <- "./output_data/2_2_Data_Ext_MZminedata_separate_SPname/"
database_file <- "./input_data/2_MGO_products_library/MGO_Combination_Results.csv"

# Read the database file and ensure numerical types are correct
database <- read.csv(database_file)
database$lower_limit <- as.numeric(database$lower_limit)
database$upper_limit <- as.numeric(database$upper_limit)

# Get all CSV filenames in the data folder that match the specified pattern
data_files <- list.files(data_folder, pattern = "^X20240730_Ext_E\\d+_processed_Ext_Mzminedata_quant\\.csv$", full.names = TRUE)

# Define a function to process a single file
process_file <- function(data_file, database) {
  
  E_number <- gsub(".*_E(\\d+)_.*", "\\1", data_file)
  
  # Dynamically generate column names
  S_peak_area_col1 <- paste0("X20240730_Ext_E", E_number, "_24h_ddms30.mzML.Peak.area")
  S_peak_area_col2 <- paste0("X20240730_Ext_E", E_number, "_MGO_24h_ddms30.mzML.Peak.area")
  P_peak_area_col1 <- S_peak_area_col1
  P_peak_area_col2 <- S_peak_area_col2
  
  # Read the data file
  data <- read.csv(data_file)
  
  # Check if the file is empty
  if (nrow(data) == 0) return(NULL)
  
  # Ensure dynamically generated column names exist
  required_cols <- c(S_peak_area_col1, S_peak_area_col2, P_peak_area_col1, P_peak_area_col2)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Filter data with labels "S" and "P"
  S_data <- data[data$Label == "S", ]
  P_data <- data[data$Label == "P", ]
  
  if (nrow(S_data) == 0 | nrow(P_data) == 0) return(NULL)
  
  # Define a function to check if values fall within the database range
  is_within_range <- function(val1, val2, database) {
    difference <- val2 - val1
    within_range <- database$lower_limit <= difference & difference <= database$upper_limit
    if (any(within_range, na.rm = TRUE)) {
      matched_row <- which(within_range)[1]
      return(database[matched_row, c("n", "value_type")])
    } else {
      return(NA)
    }
  }
  
  # Initialize the result dataframe
  filtered_pairs <- data.frame(
    Peakgroup_id = integer(),
    row.ID = integer(),
    S_value = numeric(),
    S_retention_time = numeric(),
    S_Peak.area1 = numeric(),
    S_Peak.area2 = numeric(),
    P_value = numeric(),
    P_retention_time = numeric(),
    P_Peak.area1 = numeric(),
    P_Peak.area2 = numeric(),
    n = integer(),
    value_type = character(),
    stringsAsFactors = FALSE
  )
  
  # Match S and P data
  for (i in seq_along(S_data$`row.m.z`)) {
    s_val <- S_data$`row.m.z`[i]
    s_row_ID <- S_data$row.ID[i]
    s_peakgroup_id <- S_data$Peakgroup_id[i]
    
    for (p_val in P_data$`row.m.z`) {
      if (p_val > s_val) {
        match_info <- is_within_range(s_val, p_val, database)
        if (!is.na(match_info)[1]) {
          S_info <- S_data[S_data$`row.m.z` == s_val, c("row.retention.time", S_peak_area_col1, S_peak_area_col2)]
          P_info <- P_data[P_data$`row.m.z` == p_val, c("row.retention.time", P_peak_area_col1, P_peak_area_col2)]
          
          retention_time_diff <- abs(S_info[1, "row.retention.time"] - P_info[1, "row.retention.time"])
          if (retention_time_diff < 6) {
            filtered_pairs <- rbind(filtered_pairs, data.frame(
              Peakgroup_id = s_peakgroup_id,
              row.ID = s_row_ID,
              S_value = s_val,
              S_retention_time = S_info[1, "row.retention.time"],
              S_Peak.area1 = S_info[1, S_peak_area_col1],
              S_Peak.area2 = S_info[1, S_peak_area_col2],
              P_value = p_val,
              P_retention_time = P_info[1, "row.retention.time"],
              P_Peak.area1 = P_info[1, P_peak_area_col1],
              P_Peak.area2 = P_info[1, P_peak_area_col2],
              n = match_info$n,
              value_type = match_info$value_type
            ))
          }
        }
      }
    }
  }
  
  return(filtered_pairs)
}

# Process all files and save the results
for (data_file in data_files) {
  tryCatch({
    E_number <- gsub(".*_E(\\d+)_.*", "\\1", data_file)
    filtered_pairs <- process_file(data_file, database)
    
    if (!is.null(filtered_pairs) && nrow(filtered_pairs) > 0) {
      wb <- createWorkbook()
      addWorksheet(wb, "Sheet1")
      writeData(wb, "Sheet1", filtered_pairs)
      
      output_file <- paste0("./output_data/3_1_Filter_Ext_MGO_MS1_SPmatch_mzRT/SP_E", E_number, "_X20240730_Ext_combined_mgo.xlsx")
      saveWorkbook(wb, output_file, overwrite = TRUE)
    } else {
      cat("File: ", data_file, " has no matching pairs. No output file generated.\n")
    }
  }, error = function(e) {
    cat("Error processing file: ", data_file, "\nError message: ", conditionMessage(e), "\n")
  })
}
