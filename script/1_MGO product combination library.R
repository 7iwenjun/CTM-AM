# MGO product combination library

# Define constants
S <- 0
MGO <- 72.0210
H2_mass <- 2.0157
H2O_mass <- 18.0106

# Define a function to calculate 15 ppm error range
calculate_ppm_range <- function(value, ppm = 50) {
  error <- value * ppm / 10^6
  lower_limit <- value - error
  upper_limit <- value + error
  return(c(lower_limit, upper_limit))
}

# Create a data frame to store results
results <- data.frame(
  n = integer(),
  value_type = character(),
  value = double(),
  lower_limit = double(),
  upper_limit = double(),
  stringsAsFactors = FALSE
)

# Recursive function to generate combinations
generate_combinations <- function(n, current_n, i_count, j_count) {
  if (current_n == n) {
    value <- S + n * MGO - i_count * H2_mass - j_count * H2O_mass
    value_type <- paste0("S+", n, "*MGO-", i_count, "*H2-", j_count, "*H2O")
    range <- calculate_ppm_range(value, 50)
    results <<- rbind(results, data.frame(
      n = n,
      value_type = value_type,
      value = value,
      lower_limit = range[1],
      upper_limit = range[2],
      stringsAsFactors = FALSE
    ))
  } else {
    generate_combinations(n, current_n + 1, i_count + 1, j_count)
    generate_combinations(n, current_n + 1, i_count, j_count + 1)
    generate_combinations(n, current_n + 1, i_count, j_count)
  }
}

# Calculate values and error ranges for each column
for (n in 1:10) {
  generate_combinations(n, 0, 0, 0)
}

# Remove duplicate values
results <- results[!duplicated(results$value), ]

# Print results for n = 1
results_n1 <- results[results$n == 1, ]
print(results_n1)

# Save results to relative path
relative_path <- "output/MGO_Combination_Results.csv"
dir.create("output", showWarnings = FALSE)
write.csv(results, relative_path, row.names = FALSE)
