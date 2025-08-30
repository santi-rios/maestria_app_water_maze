# Test script for the enhanced Morris Water Maze app
# Testing individual entropy plots and auto-detection

library(dplyr)
library(ggplot2)

# Load functions
source("functions.R")

# Test data loading with individuals
cat("Testing data loading with individuals...\n")
data <- load_and_process_data(use_sample = TRUE)
cat("✓ Data loaded successfully\n")
cat("Columns:", names(data), "\n")
cat("Groups:", unique(data$Group), "\n")
if ("Individual" %in% names(data)) {
  cat("Individuals per group:\n")
  print(table(data$Group, data$Individual))
}

# Test auto-detection
cat("\nTesting auto-detection...\n")
detected_params <- auto_detect_arena(data)
cat("✓ Auto-detection completed\n")
cat("Detected parameters:\n")
str(detected_params)

# Test individual entropy plots
cat("\nTesting individual entropy plots...\n")
entropy_results <- create_individual_entropy_plots(
  data, 
  detected_params$platform_x, 
  detected_params$platform_y,
  detected_params$center_x,
  detected_params$center_y,
  detected_params$radius
)

cat("✓ Individual entropy plots created\n")
cat("Number of plots:", length(entropy_results$plots), "\n")
cat("Entropy data:\n")
print(entropy_results$entropy_data)

# Test one plot
if (length(entropy_results$plots) > 0) {
  cat("\nTesting plot generation...\n")
  first_plot <- entropy_results$plots[[1]]
  
  # Save test plot
  ggsave("test_entropy_plot.png", first_plot, width = 8, height = 6)
  cat("✓ Test plot saved as test_entropy_plot.png\n")
}

cat("\n=== All tests completed successfully! ===\n")
