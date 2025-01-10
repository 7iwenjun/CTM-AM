library(ggplot2)
library(readxl)
library(dplyr)
library(ggrepel)

# Set relative paths
folder1 <- "./input_data/4_Ext_S_raw_csv_active_data"
folder2 <- "./output_data/6_Peakarea_Ext_MGOreaction_SIRIUS"
output_folder <- "./figure/6_Graph_TIC_active_peaks"

# Get file lists from folders
files1 <- list.files(folder1, pattern = "\\.csv$", full.names = TRUE)
files2 <- list.files(folder2, pattern = "\\.xlsx$", full.names = TRUE)

# Define a function to extract E number
extract_e_number <- function(filename) {
  match <- regmatches(filename, regexpr("E\\d+", filename))
  return(gsub("E", "", match))
}

# Extract E number for each file
files1_e_numbers <- sapply(basename(files1), extract_e_number)
files2_e_numbers <- sapply(basename(files2), extract_e_number)

# Initialize a list for unprocessed files
unprocessed_files <- list()

# Tolerance matching function with a tolerance of 0.1
tolerance_match <- function(data_time, s_time, tolerance = 0.1) {
  return(data_time[abs(data_time - s_time) <= tolerance])
}

# Loop through the files for processing
for (i in seq_along(files1)) {
  e_number <- files1_e_numbers[i]
  matching_file2 <- files2[files2_e_numbers == e_number]
  
  message(paste("Processing file:", files1[i], "Matching Excel file:", matching_file2))
  
  if (length(matching_file2) > 0) {
    # Read the CSV file from folder1
    data <- tryCatch({
      read.csv(files1[i], skip = 3)
    }, error = function(e) {
      message(paste("Failed to read file:", files1[i]))
      return(NULL)
    })
    
    if (is.null(data) || nrow(data) == 0) {
      message(paste("Empty or invalid data in file:", files1[i]))
      unprocessed_files <- append(unprocessed_files, files1[i])
      next
    }
    
    # Read the Excel file from folder2
    file2 <- tryCatch({
      read_excel(matching_file2)
    }, error = function(e) {
      message(paste("Failed to read Excel file:", matching_file2))
      return(NULL)
    })
    
    if (is.null(file2) || nrow(file2) == 0) {
      message(paste("Empty or invalid data in Excel file:", matching_file2))
      unprocessed_files <- append(unprocessed_files, matching_file2)
      next
    }
    
    # Remove missing values from the data
    data <- data %>%
      filter(!is.na(Time.min.), !is.na(Relative.Abundance))
    
    if (nrow(data) == 0) {
      message(paste("No valid rows in data file:", files1[i]))
      unprocessed_files <- append(unprocessed_files, files1[i])
      next
    }
    
    # Extract required columns
    file2_filtered <- file2 %>%
      select(S_retention_time, Rank, S_Peak.area1) %>%
      group_by(Rank) %>%
      filter(S_Peak.area1 == max(S_Peak.area1)) %>%
      ungroup() %>%
      distinct(Rank, .keep_all = TRUE)
    
    if (nrow(file2_filtered) == 0) {
      message(paste("Filtered data is empty for file:", matching_file2))
      unprocessed_files <- append(unprocessed_files, matching_file2)
      next
    }
    
    # Process data and perform tolerance matching for time
    data$Time.min. <- round(data$Time.min., 2)
    file2_filtered$S_retention_time <- round(file2_filtered$S_retention_time, 2)
    
    message("Data Time.min. range: ", paste(range(data$Time.min., na.rm = TRUE), collapse = " - "))
    message("Filtered S_retention_time range: ", paste(range(file2_filtered$S_retention_time, na.rm = TRUE), collapse = " - "))
    
    matched_data <- tibble(Time.min. = numeric(), Relative.Abundance = numeric(), Rank = numeric())
    
    for (j in seq_along(file2_filtered$S_retention_time)) {
      closest_rows <- tolerance_match(data$Time.min., file2_filtered$S_retention_time[j], tolerance = 0.1)
      
      if (length(closest_rows) > 0) {
        closest_row <- data[abs(data$Time.min. - file2_filtered$S_retention_time[j]) <= 0.1, ]
        closest_row <- closest_row[which.max(closest_row$Relative.Abundance), ]
        closest_row$Rank <- file2_filtered$Rank[j]
        matched_data <- bind_rows(matched_data, closest_row)
      }
    }
    
    if (nrow(matched_data) == 0) {
      message(paste("No matched data for E number:", e_number))
      unprocessed_files <- append(unprocessed_files, files1[i])
      next
    }
    
    # Sort by Time.min. and aggregate labels for the same time point
    matched_data <- matched_data %>%
      arrange(Time.min.) %>%
      group_by(Time.min.) %>%
      summarise(
        Relative.Abundance = max(Relative.Abundance),  # Take the maximum abundance
        Rank = ifelse(
          n() == 1,  # If only one label
          as.character(Rank),  # Keep the label directly
          paste(Rank[Rank <= 10], collapse = ", ")  # For multiple labels, keep numbers <= 10
        ),
        Color = ifelse(any(as.numeric(strsplit(Rank, ", ")[[1]]) <= 10), "red", "blue"),  # Set color dynamically
        label_position = max(Relative.Abundance) + 0.1 * max(Relative.Abundance)  # Add label_position column
      ) %>%
      ungroup() %>%
      filter(Rank != "")  # Remove rows without valid Rank
    
    # Ensure label_position is within y_max
    y_max <- 1.2 * max(data$Relative.Abundance, na.rm = TRUE)
    matched_data <- matched_data %>%
      mutate(label_position = ifelse(is.na(label_position), 0.05 * y_max, label_position),  # Replace NA with default
             label_position = pmax(label_position, 0.05 * y_max),  # Lower bound
             label_position = pmin(label_position, y_max - 0.1 * y_max)) %>%  # Upper bound
      filter(!is.na(Relative.Abundance), !is.na(label_position))
    
    # Log removed data
    removed_data <- matched_data %>%
      filter(label_position < 0.05 * y_max | label_position > (y_max - 0.1 * y_max))
    if (nrow(removed_data) > 0) {
      message("Rows removed due to invalid label positions:")
      print(removed_data)
    }
    
    # Dynamically determine x-axis and y-axis ranges
    x_limits <- range(data$Time.min., na.rm = TRUE)
    y_limits <- range(data$Relative.Abundance, na.rm = TRUE)
    
    # Generate TIC plot and annotate with Rank numbers
    p <- ggplot(data, aes(x = Time.min., y = Relative.Abundance)) +
      geom_line(color = "black") +
      labs(x = "Time (min)", y = "Intensity") +
      geom_segment(data = matched_data, 
                   aes(x = Time.min., xend = Time.min., 
                       y = Relative.Abundance, yend = label_position, color = Color), 
                   linewidth = 0.3) + 
      geom_text_repel(data = matched_data, 
                      aes(x = Time.min., y = label_position, 
                          label = Rank, color = Color),
                      size = 3, fontface = "bold", direction = "y", 
                      nudge_y = 0.1 * y_max, segment.size = 0.3, 
                      box.padding = 0.5, point.padding = 0.3, max.overlaps = Inf) +
      scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
      scale_color_manual(values = c("red" = "#8B0000", "blue" = "#8A8A92")) +
      guides(color = "none") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5)
      )
    
    # Save TIC plot
    output_file <- file.path(output_folder, paste0("TIC_E", e_number, ".png"))
    ggsave(output_file, plot = p, width = 14, height = 4, dpi = 300)
    
  } else {
    message(paste("No matching file found:", files1[i]))
    unprocessed_files <- append(unprocessed_files, files1[i])
  }
}

# Output the list of unprocessed files
if (length(unprocessed_files) > 0) {
  message("List of unprocessed files:")
  print(unprocessed_files)
} else {
  message("All files have been processed.")
}
