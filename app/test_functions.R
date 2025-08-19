# Test script for Morris Water Maze functions
# This script tests each function individually to validate functionality

# Load required libraries
library(dplyr)
library(ggplot2)

# Load functions
source("functions.R")

# Test 1: Load and process data function
cat("=== Test 1: Load and process data ===\n")
tryCatch({
  # Test with sample data
  sample_data <- load_and_process_data(use_sample = TRUE)
  cat("✓ Sample data loaded successfully\n")
  cat("  - Number of rows:", nrow(sample_data), "\n")
  cat("  - Groups found:", unique(sample_data$Group), "\n")
  cat("  - Columns:", paste(names(sample_data), collapse = ", "), "\n")
}, error = function(e) {
  cat("✗ Error loading sample data:", e$message, "\n")
})

# Test 2: Calculate entropy for single trajectory
cat("\n=== Test 2: Calculate entropy ===\n")
tryCatch({
  # Create simple test data
  x_test <- c(10, 15, 20, 25, 30, 35, 40)
  y_test <- c(10, 12, 15, 18, 22, 25, 28)
  plat_x_test <- 117.8
  plat_y_test <- 38.4
  
  entropy_value <- calculate_entropy(x_test, y_test, plat_x_test, plat_y_test)
  cat("✓ Entropy calculation successful\n")
  cat("  - Entropy value:", entropy_value, "\n")
}, error = function(e) {
  cat("✗ Error in entropy calculation:", e$message, "\n")
})

# Test 3: Calculate group entropy
cat("\n=== Test 3: Calculate group entropy ===\n")
tryCatch({
  if (exists("sample_data")) {
    group_entropy <- calculate_group_entropy(sample_data, 117.8, 38.4)
    cat("✓ Group entropy calculation successful\n")
    print(group_entropy)
  } else {
    cat("✗ Sample data not available for group entropy test\n")
  }
}, error = function(e) {
  cat("✗ Error in group entropy calculation:", e$message, "\n")
})

# Test 4: Calculate distance
cat("\n=== Test 4: Calculate distance ===\n")
tryCatch({
  x_test <- c(0, 3, 4)  # Should give distance 3 + 1 = 4
  y_test <- c(0, 4, 4)
  
  distance <- calculate_distance(x_test, y_test)
  cat("✓ Distance calculation successful\n")
  cat("  - Distance:", distance, "\n")
  cat("  - Expected: ~5\n")
}, error = function(e) {
  cat("✗ Error in distance calculation:", e$message, "\n")
})

# Test 5: Calculate summary statistics
cat("\n=== Test 5: Calculate summary statistics ===\n")
tryCatch({
  if (exists("sample_data")) {
    summary_stats <- calculate_summary_stats(sample_data)
    cat("✓ Summary statistics calculation successful\n")
    print(summary_stats)
  } else {
    cat("✗ Sample data not available for summary statistics test\n")
  }
}, error = function(e) {
  cat("✗ Error in summary statistics calculation:", e$message, "\n")
})

# Test 6: Statistical tests
cat("\n=== Test 6: Statistical tests ===\n")
tryCatch({
  if (exists("sample_data")) {
    summary_stats <- calculate_summary_stats(sample_data)
    statistical_tests <- perform_statistical_tests(summary_stats)
    cat("✓ Statistical tests successful\n")
    cat("  - Test type:", statistical_tests$test_type, "\n")
    
    if (!is.null(statistical_tests$distance_test)) {
      cat("  - Distance test performed\n")
    }
    if (!is.null(statistical_tests$velocity_test)) {
      cat("  - Velocity test performed\n")
    }
  } else {
    cat("✗ Sample data not available for statistical tests\n")
  }
}, error = function(e) {
  cat("✗ Error in statistical tests:", e$message, "\n")
})

# Test 7: Create plots
cat("\n=== Test 7: Create plots ===\n")
tryCatch({
  if (exists("sample_data")) {
    # Test trajectory plot
    p1 <- create_trajectory_plot(sample_data, 117.8, 38.4, 90.13, 61.3, 65)
    cat("✓ Trajectory plot creation successful\n")
    
    # Test heatmap
    p2 <- create_heatmap_plot(sample_data)
    cat("✓ Heatmap creation successful\n")
    
    cat("  - Plots are ggplot objects and ready for rendering\n")
  } else {
    cat("✗ Sample data not available for plot tests\n")
  }
}, error = function(e) {
  cat("✗ Error in plot creation:", e$message, "\n")
})

cat("\n=== Test Summary ===\n")
cat("All individual function tests completed.\n")
cat("Check the results above to see which functions passed or failed.\n")
