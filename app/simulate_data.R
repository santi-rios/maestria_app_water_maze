# Script to Generate Artificial Morris Water Maze Trajectories
# Author: Shiny App for Water Maze Analysis
# Date: 2025

library(dplyr)

#' Generate a single trajectory with realistic movement patterns
#' @param n_points number of points in trajectory
#' @param start_x starting x coordinate
#' @param start_y starting y coordinate
#' @param target_x target platform x coordinate
#' @param target_y target platform y coordinate
#' @param center_x center of apparatus x coordinate
#' @param center_y center of apparatus y coordinate
#' @param radius radius of apparatus
#' @param learning_rate how quickly the animal learns (0-1)
#' @param noise_level amount of random noise in movement
#' @param max_time maximum time for trial
#' @return data frame with time, x, y coordinates
generate_single_trajectory <- function(n_points = 100, 
                                     start_x = NULL, 
                                     start_y = NULL,
                                     target_x = 117.8, 
                                     target_y = 38.4,
                                     center_x = 90.13, 
                                     center_y = 61.3,
                                     radius = 65,
                                     learning_rate = 0.1,
                                     noise_level = 5,
                                     max_time = 60) {
  
  # Generate random starting position if not provided
  if (is.null(start_x) || is.null(start_y)) {
    angle <- runif(1, 0, 2*pi)
    start_radius <- runif(1, radius*0.7, radius*0.9)
    start_x <- center_x + start_radius * cos(angle)
    start_y <- center_y + start_radius * sin(angle)
  }
  
  # Initialize trajectory
  x <- numeric(n_points)
  y <- numeric(n_points)
  time <- seq(0, max_time, length.out = n_points)
  
  x[1] <- start_x
  y[1] <- start_y
  
  # Simulate movement
  for (i in 2:n_points) {
    # Calculate direction towards target
    target_dir_x <- target_x - x[i-1]
    target_dir_y <- target_y - y[i-1]
    target_dist <- sqrt(target_dir_x^2 + target_dir_y^2)
    
    # Normalize direction
    if (target_dist > 0) {
      target_dir_x <- target_dir_x / target_dist
      target_dir_y <- target_dir_y / target_dist
    }
    
    # Add learning component (increases over time)
    learning_factor <- learning_rate * (i / n_points)
    
    # Add some random exploration
    random_angle <- runif(1, 0, 2*pi)
    random_x <- cos(random_angle)
    random_y <- sin(random_angle)
    
    # Combine directed and random movement
    move_x <- learning_factor * target_dir_x + (1 - learning_factor) * random_x
    move_y <- learning_factor * target_dir_y + (1 - learning_factor) * random_y
    
    # Add noise
    move_x <- move_x + rnorm(1, 0, noise_level / 100)
    move_y <- move_y + rnorm(1, 0, noise_level / 100)
    
    # Calculate step size (decreases as animal gets closer to target)
    step_size <- 2 + 3 * (target_dist / radius)
    
    # Update position
    x[i] <- x[i-1] + move_x * step_size
    y[i] <- y[i-1] + move_y * step_size
    
    # Keep within apparatus bounds
    dist_from_center <- sqrt((x[i] - center_x)^2 + (y[i] - center_y)^2)
    if (dist_from_center > radius) {
      # Push back to edge
      angle_to_center <- atan2(y[i] - center_y, x[i] - center_x)
      x[i] <- center_x + radius * cos(angle_to_center)
      y[i] <- center_y + radius * sin(angle_to_center)
    }
    
    # Stop if reached platform
    if (target_dist < 5) {
      # Fill remaining points with platform position
      x[i:n_points] <- target_x
      y[i:n_points] <- target_y
      break
    }
  }
  
  return(data.frame(
    Time = time,
    X = x,
    Y = y
  ))
}

#' Generate multiple trajectories for different groups
#' @param n_subjects_per_group number of subjects per group
#' @param groups vector of group names
#' @param ... parameters passed to generate_single_trajectory
#' @return data frame with all trajectories
generate_group_trajectories <- function(n_subjects_per_group = 3,
                                       groups = c("Control", "Treatment"),
                                       ...) {
  
  all_trajectories <- list()
  
  for (group in groups) {
    for (subject in 1:n_subjects_per_group) {
      
      # Vary parameters by group
      if (group == "Control") {
        learning_rate <- runif(1, 0.05, 0.15)  # Slower learning
        noise_level <- runif(1, 8, 12)         # More noise
      } else if (group == "Treatment") {
        learning_rate <- runif(1, 0.15, 0.25)  # Faster learning
        noise_level <- runif(1, 3, 7)          # Less noise
      } else {
        learning_rate <- runif(1, 0.08, 0.18)  # Default
        noise_level <- runif(1, 5, 10)         # Default
      }
      
      # Generate trajectory
      trajectory <- generate_single_trajectory(
        learning_rate = learning_rate,
        noise_level = noise_level,
        ...
      )
      
      # Add group and subject information
      trajectory$Treatment <- group
      trajectory$Subject <- paste0(group, "_", subject)
      
      all_trajectories[[paste0(group, "_", subject)]] <- trajectory
    }
  }
  
  # Combine all trajectories
  combined_data <- dplyr::bind_rows(all_trajectories)
  
  return(combined_data)
}

#' Generate sample data files for the app
#' @param output_dir directory to save files
#' @param n_subjects number of subjects per group
generate_sample_files <- function(output_dir = ".", n_subjects = 3) {
  
  # Generate control group data
  control_data <- generate_group_trajectories(
    n_subjects_per_group = n_subjects,
    groups = "Control",
    n_points = 150,
    max_time = 45
  )
  
  # Generate treatment group data  
  treatment_data <- generate_group_trajectories(
    n_subjects_per_group = n_subjects,
    groups = "Treatment",
    n_points = 120,
    max_time = 35
  )
  
  # Save files
  write.csv(control_data, file.path(output_dir, "sample_data_control_multi.csv"), row.names = FALSE)
  write.csv(treatment_data, file.path(output_dir, "sample_data_treatment_multi.csv"), row.names = FALSE)
  
  # Also create a combined file
  combined_data <- rbind(control_data, treatment_data)
  write.csv(combined_data, file.path(output_dir, "sample_data_combined.csv"), row.names = FALSE)
  
  cat("Sample files generated:\n")
  cat("- sample_data_control_multi.csv\n")
  cat("- sample_data_treatment_multi.csv\n") 
  cat("- sample_data_combined.csv\n")
  
  return(list(
    control = control_data,
    treatment = treatment_data,
    combined = combined_data
  ))
}

# Generate the sample files if this script is run directly
if (!interactive()) {
  generate_sample_files(".")
}
