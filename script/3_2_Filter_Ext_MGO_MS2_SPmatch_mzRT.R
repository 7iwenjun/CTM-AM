# Load required R packages
library(MSnbase)
library(readxl)
library(writexl)
library(dplyr)

# Set relative folder paths
folder_path <- "./output_data/3_1_Filter_Ext_MGO_MS1_SPmatch_mzRT"
mgf_file_paths <- c(
  "./input_data/3_Ext_MZmine_data_align/E31_MZminedata.mgf"
)
output_folder <- "./output_data/3_2_1_Filter_Ext_MGO_MS2_SPmatch_mzRT"
filtered_output_folder <- "./output_data/3_2_2_Filter_Ext_MGO_MS2_SPmatch_mzRT_Filtered_Data"

# Parameter settings
noise_threshold <- 1E3  # Noise filtering threshold
precursor_mz_tolerance_ppm <- 5  # Precursor m/z matching tolerance ±5 ppm
rt_tolerance <- 0.1 * 60  # Retention time matching tolerance ±0.1 min (6 seconds)
tolerance <- 0.01  # In-spectrum m/z matching tolerance ±0.01 Da

# Create output folders if they do not exist
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
if (!dir.exists(filtered_output_folder)) dir.create(filtered_output_folder, recursive = TRUE)

# Parse MGF files
read_mgf_file <- function(file_path) {
  spectra <- list()
  current_spectrum <- NULL
  mgf_lines <- readLines(file_path)
  
  for (line in mgf_lines) {
    if (grepl("BEGIN IONS", line, ignore.case = TRUE)) {
      current_spectrum <- list("m/z" = numeric(), "precursor_mz" = NA, "RT" = NA, "intensity" = numeric())
    } else if (grepl("END IONS", line, ignore.case = TRUE)) {
      valid_indices <- which(current_spectrum[["intensity"]] >= noise_threshold)
      current_spectrum[["m/z"]] <- current_spectrum[["m/z"]][valid_indices]
      current_spectrum[["intensity"]] <- current_spectrum[["intensity"]][valid_indices]
      spectra <- append(spectra, list(current_spectrum))
      current_spectrum <- NULL
    } else if (!is.null(current_spectrum)) {
      if (grepl("^PEPMASS=", line, ignore.case = TRUE)) {
        current_spectrum[["precursor_mz"]] <- as.numeric(strsplit(sub("^PEPMASS=", "", line), " ")[[1]][1])
      } else if (grepl("^RTINSECONDS=", line, ignore.case = TRUE)) {
        current_spectrum[["RT"]] <- round(as.numeric(sub("^RTINSECONDS=", "", line)))
      } else {
        parts <- strsplit(line, "\\s+")[[1]]
        if (length(parts) >= 2) {
          current_spectrum[["m/z"]] <- c(current_spectrum[["m/z"]], as.numeric(parts[1]))
          current_spectrum[["intensity"]] <- c(current_spectrum[["intensity"]], as.numeric(parts[2]))
        }
      }
    }
  }
  return(spectra)
}

# Load all MGF files
spectra_list <- lapply(mgf_file_paths, read_mgf_file) %>% unlist(recursive = FALSE)
cat("Successfully parsed", length(spectra_list), "spectra.\n")

# Define bidirectional matching function
count_and_return_matched_peaks <- function(s_mzs, p_mzs, tolerance) {
  matched_peaks <- list()
  
  for (mz in s_mzs) {
    matched_p_mz <- p_mzs[abs(p_mzs - mz) <= tolerance]
    if (length(matched_p_mz) > 0) {
      matched_peaks <- append(matched_peaks, list(mz))
    }
  }
  
  for (p_mz in p_mzs) {
    matched_s_mz <- s_mzs[abs(s_mzs - p_mz) <= tolerance]
    if (length(matched_s_mz) > 0) {
      matched_peaks <- append(matched_peaks, list(p_mz))
    }
  }
  
  # Ensure the result is a numeric array, remove duplicates, and keep two decimal places
  matched_peaks <- unlist(matched_peaks)
  if (length(matched_peaks) == 0) {
    return(numeric(0)) # Return a numeric empty array
  }
  return(unique(round(matched_peaks, 2))) # Remove duplicates and keep two decimal places
}

# Read and process each Excel file
excel_files <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

for (excel_file_path in excel_files) {
  # Read data using read_excel()
  excel_data <- read_excel(excel_file_path, sheet = 1)
  
  # Clean column names by removing spaces and hidden characters
  colnames(excel_data) <- trimws(colnames(excel_data))
  
  # Ensure the P_value column is numeric
  if ("P_value" %in% colnames(excel_data)) {
    excel_data$P_value <- as.numeric(excel_data$P_value)
  } else {
    stop("Error: The 'P_value' column is missing in the file.")
  }
  
  # Extract required data columns and convert units
  s_values <- excel_data$S_value
  p_values <- excel_data$P_value
  s_retention_times <- round(excel_data$S_retention_time * 60)
  p_retention_times <- round(excel_data$P_retention_time * 60)
  
  matched_peak_counts <- numeric(length(s_values))
  total_peak_counts <- numeric(length(s_values))
  jaccard_similarities <- numeric(length(s_values))
  matched_mz_list <- vector("list", length(s_values)) # Newly added matched m/z column
  
  # Iterate through each row for spectrum matching
  for (i in seq_along(s_values)) {
    s_value <- s_values[i]
    p_value <- p_values[i]
    s_ret_time <- s_retention_times[i]
    p_ret_time <- p_retention_times[i]
    
    s_spectrum <- NULL
    p_spectrum <- NULL
    
    # Find matching S and P spectra
    for (spectrum in spectra_list) {
      if (!is.na(s_value) && !is.na(spectrum[["precursor_mz"]]) &&
          abs((spectrum[["precursor_mz"]] - s_value) / s_value * 1E6) <= precursor_mz_tolerance_ppm &&
          abs(spectrum[["RT"]] - s_ret_time) <= rt_tolerance) {
        s_spectrum <- spectrum
      }
      if (!is.na(p_value) && !is.na(spectrum[["precursor_mz"]]) &&
          abs((spectrum[["precursor_mz"]] - p_value) / p_value * 1E6) <= precursor_mz_tolerance_ppm &&
          abs(spectrum[["RT"]] - p_ret_time) <= rt_tolerance) {
        p_spectrum <- spectrum
      }
      if (!is.null(s_spectrum) && !is.null(p_spectrum)) break
    }
    
    # Calculate matching results
    if (!is.null(s_spectrum) && !is.null(p_spectrum)) {
      matched_mz <- count_and_return_matched_peaks(s_spectrum[["m/z"]], p_spectrum[["m/z"]], tolerance)
      matched_peak_counts[i] <- length(matched_mz)
      total_peak_counts[i] <- length(unique(c(s_spectrum[["m/z"]], p_spectrum[["m/z"]])))
      jaccard_similarities[i] <- ifelse(total_peak_counts[i] > 0, matched_peak_counts[i] / total_peak_counts[i], 0)
      
      # Ensure proper writing of Matched_mz
      if (length(matched_mz) > 0) {
        matched_mz_list[[i]] <- paste(sprintf("%.2f", matched_mz), collapse = ", ")
      } else {
        matched_mz_list[[i]] <- NA
      }
    } else {
      matched_peak_counts[i] <- 0
      total_peak_counts[i] <- 0
      jaccard_similarities[i] <- 0
      matched_mz_list[[i]] <- NA
    }
  }
  
  # Add results to the dataframe and save
  excel_data$Matched_Peak_Counts <- matched_peak_counts
  excel_data$Total_Peak_Counts_S <- total_peak_counts
  excel_data$Jaccard_Similarity <- jaccard_similarities
  excel_data$Matched_mz <- sapply(matched_mz_list, function(x) if (is.na(x)) NA else paste(x, collapse = ", "))
  
  output_file_path <- file.path(output_folder, basename(excel_file_path))
  write_xlsx(excel_data, output_file_path)
  
  # Filter data and save
  filtered_data <- excel_data %>%
    filter(P_Peak.area1 == 0 & Matched_Peak_Counts > 3) %>%
    filter(!(n >= 4 & Jaccard_Similarity <= 0.3)) %>%
    filter(P_value <= 900) %>%
    filter(S_retention_time >= 1 & S_retention_time <= 19) %>%
    filter(S_Peak.area1 != 0)
  
  filtered_data <- filtered_data %>%
    mutate(Peakgroup_id = as.integer(as.factor(Peakgroup_id)))
  
  filtered_output_file_path <- file.path(filtered_output_folder, basename(excel_file_path))
  write_xlsx(filtered_data, filtered_output_file_path)
  
  cat("File", basename(excel_file_path), "processed successfully.\n")
}

cat("All files processed successfully.\n")
