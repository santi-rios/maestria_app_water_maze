# Script to convert wide-format Morris Water Maze data to long format
# for use in the Shiny app
# Author: GitHub Copilot
# Date: 2025-08-31

library(dplyr)

# Function to convert wide format to long format
convert_wide_to_long <- function(file_path, group_name) {
  cat("Processing file:", file_path, "\n")
  
  # Read the wide format data using base R
  wide_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Get column names and identify x,y,t triplets
  col_names <- names(wide_data)
  
  # Find positions of x columns (every 3rd column starting from 1)
  x_positions <- seq(1, length(col_names), by = 3)
  n_animals <- length(x_positions)
  
  cat("Found", n_animals, "animals in the data\n")
  
  # Initialize list to store individual animal data
  animal_data_list <- list()
  
  # Process each animal
  for (i in 1:n_animals) {
    # Get the column indices for this animal
    x_col <- x_positions[i]
    y_col <- x_col + 1
    t_col <- x_col + 2
    
    # Extract data for this animal
    animal_data <- data.frame(
      time = wide_data[[t_col]],
      x = wide_data[[x_col]],
      y = wide_data[[y_col]],
      Individual = i - 1,  # Start from 0 to match app format
      Group = group_name,
      stringsAsFactors = FALSE
    )
    
    # Remove rows with missing data
    animal_data <- animal_data[complete.cases(animal_data), ]
    
    # Only keep animals with sufficient data points
    if (nrow(animal_data) >= 10) {
      animal_data_list[[i]] <- animal_data
    }
  }
  
  # Combine all animals into one dataframe
  long_data <- do.call(rbind, animal_data_list)
  
  # Reset row names
  rownames(long_data) <- NULL
  
  cat("Converted data has", nrow(long_data), "rows and", length(unique(long_data$Individual)), "individuals\n")
  
  return(long_data)
}

# Process the files
cat("=== Converting Cooke 2020 Data ===\n\n")

# File paths
file_31_2 <- "/home/santi/Projects/maestria_app_water_maze/data/cooke2020/31-2.csv"
file_37_3 <- "/home/santi/Projects/maestria_app_water_maze/data/cooke2020/prueba_37-3.csv"

# Convert files
data_31_2 <- convert_wide_to_long(file_31_2, "Group_31_2")
data_37_3 <- convert_wide_to_long(file_37_3, "Group_37_3")

# Combine both datasets
combined_data <- rbind(
  data_31_2,
  data_37_3
)

# Show summary
cat("\n=== Data Summary ===\n")
cat("Total rows:", nrow(combined_data), "\n")
cat("Total individuals:", length(unique(paste(combined_data$Group, combined_data$Individual))), "\n")
cat("Groups:", paste(unique(combined_data$Group), collapse = ", "), "\n")

# Show data structure
cat("\n=== Data Structure ===\n")
cat("Columns:", paste(names(combined_data), collapse = ", "), "\n")
cat("First few rows:\n")
print(head(combined_data, 10))

# Show group summary
cat("\n=== Group Summary ===\n")
group_summary <- combined_data %>%
  group_by(Group) %>%
  summarise(
    n_individuals = length(unique(Individual)),
    n_observations = n(),
    time_range = paste0(round(min(time, na.rm = TRUE), 2), " - ", round(max(time, na.rm = TRUE), 2)),
    x_range = paste0(round(min(x, na.rm = TRUE), 2), " - ", round(max(x, na.rm = TRUE), 2)),
    y_range = paste0(round(min(y, na.rm = TRUE), 2), " - ", round(max(y, na.rm = TRUE), 2)),
    .groups = 'drop'
  )
print(group_summary)

# Save the converted data
output_file <- "/home/santi/Projects/maestria_app_water_maze/data/cooke2020_converted.csv"
write.csv(combined_data, output_file, row.names = FALSE)
cat("\n=== Data saved to:", output_file, "===\n")

# Create individual files as well
write.csv(data_31_2, "/home/santi/Projects/maestria_app_water_maze/data/cooke2020_31-2_converted.csv", row.names = FALSE)
write.csv(data_37_3, "/home/santi/Projects/maestria_app_water_maze/data/cooke2020_37-3_converted.csv", row.names = FALSE)

cat("Individual files also saved:\n")
cat("- /home/santi/Projects/maestria_app_water_maze/data/cooke2020_31-2_converted.csv\n")
cat("- /home/santi/Projects/maestria_app_water_maze/data/cooke2020_37-3_converted.csv\n")

cat("\n=== Conversion Complete! ===\n")
cat("You can now load these files in your Shiny app for entropy analysis.\n")
