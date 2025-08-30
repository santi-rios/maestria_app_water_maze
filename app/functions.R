# Functions for Morris Water Maze Analysis
# Author: Shiny App for Water Maze Analysis
# Date: 2025

# Load required libraries
library(dplyr)
library(ggplot2)

#' Calculate entropy for a single trajectory
#' @param x numeric vector of x coordinates
#' @param y numeric vector of y coordinates
#' @param plat_x platform x coordinate
#' @param plat_y platform y coordinate
#' @return entropy value
calculate_entropy <- function(x, y, plat_x, plat_y) {
  # Check if we have enough data points
  if (length(x) < 3 || length(y) < 3) {
    return(NA)
  }
  
  # Calculate differences between platform and raw data
  d_x <- x - plat_x
  d_y <- y - plat_y
  dist2 <- d_x^2 + d_y^2
  
  # Entropy calculation
  w <- rep(1, length(x))
  sw <- sum(w)
  
  if (sw == 0) return(NA)
  
  xm <- sum(w * d_x) / sw
  ym <- sum(w * d_y) / sw
  xxm <- sum(w * d_x * d_x) / sw
  yym <- sum(w * d_y * d_y) / sw
  xym <- sum(w * d_x * d_y) / sw
  
  # Covariance matrix
  Sig <- matrix(c(xxm - xm^2, xym - xm * ym,
                  xym - xm * ym, yym - ym^2), nrow=2)
  
  # Check for singular matrix (determinant near zero)
  det_Sig <- det(Sig)
  if (abs(det_Sig) < 1e-10) {
    # Handle singular matrix by adding small value to diagonal
    Sig[1,1] <- Sig[1,1] + 1e-6
    Sig[2,2] <- Sig[2,2] + 1e-6
    det_Sig <- det(Sig)
  }
  
  # Calculate eigenvalues more robustly
  eig_result <- eigen(Sig, symmetric = TRUE)
  eig_val <- eig_result$values
  
  # Ensure eigenvalues are positive
  eig_val[eig_val <= 0] <- 1e-6
  
  var_xy2 <- eig_val[1] * eig_val[2]
  
  mdist2 <- sum(w * dist2) / sw
  
  # Ensure positive values for logarithm
  if (var_xy2 <= 0) var_xy2 <- 1e-6
  if (mdist2 <= 0) mdist2 <- 1e-6
  
  entropy <- log(mdist2) + 0.5 * log(var_xy2)
  
  # Check for infinite or NaN values
  if (!is.finite(entropy)) {
    return(NA)
  }
  
  return(entropy)
}

#' Calculate entropy for grouped data
#' @param data data frame with columns: x, y, Group
#' @param plat_x platform x coordinate
#' @param plat_y platform y coordinate
#' @return data frame with Group and entropy columns
calculate_group_entropy <- function(data, plat_x, plat_y) {
  entropy_data <- data %>%
    dplyr::group_by(Group) %>% 
    dplyr::summarise(
      entropy = calculate_entropy(x, y, plat_x, plat_y),
      .groups = 'drop'
    )
  
  return(entropy_data)
}

#' Calculate trajectory distance
#' @param x numeric vector of x coordinates
#' @param y numeric vector of y coordinates
#' @return total distance traveled
calculate_distance <- function(x, y) {
  if(length(x) <= 1) return(0)
  distance <- sqrt(diff(x)^2 + diff(y)^2)
  return(sum(distance, na.rm = TRUE))
}

#' Calculate summary statistics for grouped data
#' @param data data frame with columns: x, y, time, Group
#' @return data frame with summary statistics per group
calculate_summary_stats <- function(data) {
  summary_data <- data %>% 
    dplyr::group_by(.data$Group) %>% 
    dplyr::summarise(
      total_distance = calculate_distance(.data$x, .data$y),
      total_time = max(.data$time, na.rm = TRUE) - min(.data$time, na.rm = TRUE),
      mean_velocity = .data$total_distance / .data$total_time,
      n_points = dplyr::n(),
      .groups = 'drop'
    )
  
  return(summary_data)
}

#' Perform statistical comparisons between groups
#' @param summary_data data frame with summary statistics
#' @return list with statistical test results
perform_statistical_tests <- function(summary_data) {
  n_groups <- length(unique(summary_data$Group))
  results <- list()
  
  if (n_groups == 2) {
    # T-tests for two groups
    results$distance_test <- tryCatch({
      t.test(total_distance ~ Group, data = summary_data)
    }, error = function(e) NULL)
    
    results$velocity_test <- tryCatch({
      t.test(mean_velocity ~ Group, data = summary_data)
    }, error = function(e) NULL)
    
    results$test_type <- "t-test"
    
  } else if (n_groups > 2) {
    # ANOVA for multiple groups
    results$distance_test <- tryCatch({
      aov(total_distance ~ Group, data = summary_data)
    }, error = function(e) NULL)
    
    results$velocity_test <- tryCatch({
      aov(mean_velocity ~ Group, data = summary_data)
    }, error = function(e) NULL)
    
    results$test_type <- "ANOVA"
    
  } else {
    results$test_type <- "single_group"
  }
  
  return(results)
}

#' Load and process data files
#' @param file_paths vector of file paths
#' @param use_sample logical, whether to use sample data
#' @return processed data frame
load_and_process_data <- function(file_paths = NULL, use_sample = FALSE) {
  
  if (use_sample) {
    files <- c("sample_data_control_individuals.csv", "sample_data_treatment_individuals.csv")
    data_list <- lapply(files, read.csv)
    names(data_list) <- c("Control", "Treatment")
    data <- dplyr::bind_rows(data_list, .id = "Group")
    
  } else {
    if (is.null(file_paths)) {
      stop("file_paths must be provided when use_sample is FALSE")
    }
    
    # If single directory provided, read all CSV files
    if (length(file_paths) == 1 && dir.exists(file_paths)) {
      file_paths <- list.files(path = file_paths, pattern = "\\.csv$", full.names = TRUE)
    }
    
    if (length(file_paths) == 0) {
      stop("No CSV files found in the specified directory.")
    }
    
    data_list <- lapply(file_paths, read.csv)
    
    # Check if files have treatment column
    has_treatment <- all(sapply(data_list, function(df) {
      any(tolower(names(df)) == "treatment")
    }))
    
    if (has_treatment) {
      data <- dplyr::bind_rows(data_list)
      
      treatment_col <- names(data)[tolower(names(data)) == "treatment"][1]
      data <- data %>% dplyr::rename(Group = all_of(treatment_col))
      
    } else {
      group_names <- tools::file_path_sans_ext(basename(file_paths))
      names(data_list) <- group_names
      data <- dplyr::bind_rows(data_list, .id = "Group")
    }
  }

  # Clean column names
  data <- janitor::clean_names(data)
  
  # Standardize Group column name
  if ("group" %in% names(data)) {
    data <- data %>% dplyr::rename(Group = "group")
  }
  
  # Check for individual identifier
  if ("individual" %in% names(data)) {
    data <- data %>% dplyr::rename(Individual = "individual")
  }
  
  # Check if first row is header-like (contains non-numeric values)
  first_row_bad <- any(is.na(as.numeric(as.character(data[1, c("time", "x", "y")]))))
  if (first_row_bad) {
    data <- data[-1, ]
  }
  
  # Convert to numeric
  data <- data %>%
    dplyr::mutate(
      time = as.numeric(as.character(.data$time)),
      x = as.numeric(as.character(.data$x)),
      y = as.numeric(as.character(.data$y))
    ) %>%
    dplyr::filter(!is.na(.data$time), !is.na(.data$x), !is.na(.data$y))
  
  return(data)
}



#' Create trajectory plot
#' @param data processed data frame
#' @param plat_x platform x coordinate
#' @param plat_y platform y coordinate
#' @param wm_centr_x center x coordinate
#' @param wm_centr_y center y coordinate
#' @param radio_wm radius of apparatus
#' @return ggplot object
create_trajectory_plot <- function(data, plat_x, plat_y, wm_centr_x, wm_centr_y, radio_wm) {
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$x, y = .data$y, color = .data$Group)) +
    ggplot2::geom_path(alpha = 0.7, linewidth = 0.8) +
    ggplot2::geom_point(data = data.frame(x = plat_x, y = plat_y), 
                       ggplot2::aes(.data$x, .data$y), 
                       color = "red", size = 5, inherit.aes = FALSE) +
    ggplot2::annotate("path",
                     x = wm_centr_x + radio_wm * cos(seq(0, 2*pi, length.out = 100)),
                     y = wm_centr_y + radio_wm * sin(seq(0, 2*pi, length.out = 100)),
                     color = "blue", alpha = 0.5, linewidth = 1) +
    ggplot2::labs(title = "Trayectorias por Grupo", x = "X", y = "Y") +
    ggplot2::theme_minimal() +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "bottom")
  
  return(p)
}

#' Create heatmap plot with smooth density and arena boundaries
#' @param data processed data frame
#' @param wm_centr_x center x coordinate of apparatus (optional)
#' @param wm_centr_y center y coordinate of apparatus (optional)
#' @param radio_wm radius of apparatus (optional)
#' @param plat_x platform x coordinate (optional)
#' @param plat_y platform y coordinate (optional)
#' @return ggplot object
create_heatmap_plot <- function(data, wm_centr_x = NULL, wm_centr_y = NULL, 
                               radio_wm = NULL, plat_x = NULL, plat_y = NULL) {
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_density_2d_filled(
      contour_var = "ndensity",  # Normalize density for better comparison between groups
      alpha = 0.8
    ) +
    ggplot2::facet_wrap(~ .data$Group) +
    ggplot2::labs(
      title = "Mapa de Densidad de Posición por Grupo",
      subtitle = "Colores más intensos indican mayor concentración de tiempo",
      x = "Coordenada X",
      y = "Coordenada Y",
      fill = "Densidad\nRelativa"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_fixed() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 8),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      strip.background = ggplot2::element_rect(fill = "lightgray", color = NA),
      strip.text = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorsteps(
        title = "Densidad de\nOcupación",
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        barwidth = ggplot2::unit(15, "lines"),
        barheight = ggplot2::unit(0.5, "lines")
      )
    )
  
  # Add arena boundary circle if parameters provided
  if (!is.null(wm_centr_x) && !is.null(wm_centr_y) && !is.null(radio_wm)) {
    p <- p + ggplot2::annotate(
      "path",
      x = wm_centr_x + radio_wm * cos(seq(0, 2*pi, length.out = 100)),
      y = wm_centr_y + radio_wm * sin(seq(0, 2*pi, length.out = 100)),
      color = "black", 
      linewidth = 0.8,
      alpha = 0.7
    )
  }
  
  # Add platform location if parameters provided
  if (!is.null(plat_x) && !is.null(plat_y)) {
    p <- p + ggplot2::geom_point(
      data = data.frame(x = plat_x, y = plat_y),
      ggplot2::aes(x = .data$x, y = .data$y),
      color = "red",
      size = 4,
      shape = 19,
      alpha = 0.8,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = data.frame(x = plat_x, y = plat_y),
      ggplot2::aes(x = .data$x, y = .data$y),
      color = "white",
      size = 2,
      shape = 19,
      alpha = 0.8,
      inherit.aes = FALSE
    )
  }
  
  return(p)
}

#' Create individual entropy plots with covariance ellipse visualization
#' @param data processed data frame
#' @param plat_x platform x coordinate
#' @param plat_y platform y coordinate
#' @param wm_centr_x center x coordinate (optional)
#' @param wm_centr_y center y coordinate (optional)
#' @param radio_wm radius of apparatus (optional)
#' @return list with plots and entropy values per individual
create_individual_entropy_plots <- function(data, plat_x, plat_y, 
                                           wm_centr_x = NULL, wm_centr_y = NULL, 
                                           radio_wm = NULL) {
  
  # Check if data has individual identifiers
  if (!"individual" %in% names(data) && !"Individual" %in% names(data)) {
    # If no individual column, treat each group as a single individual
    data$Individual <- data$Group
  } else {
    # Standardize column name
    if ("individual" %in% names(data)) {
      data <- data %>% dplyr::rename(Individual = individual)
    }
  }
  
  # Get unique individuals
  individuals <- unique(paste(data$Group, data$Individual, sep = "_"))
  
  plot_list <- list()
  entropy_values <- data.frame()
  
  for (ind in individuals) {
    # Extract group and individual
    parts <- strsplit(ind, "_")[[1]]
    group_name <- parts[1]
    ind_name <- if(length(parts) > 1) parts[2] else parts[1]
    
    # Filter data for this individual
    ind_data <- data %>%
      dplyr::filter(.data$Group == group_name & .data$Individual == ind_name)
    
    if (nrow(ind_data) < 3) next  # Skip if insufficient data
    
    # Calculate entropy
    entropy_val <- calculate_entropy(ind_data$x, ind_data$y, plat_x, plat_y)
    
    # Calculate covariance matrix for ellipse
    d_x <- ind_data$x - plat_x
    d_y <- ind_data$y - plat_y
    
    xm <- mean(d_x)
    ym <- mean(d_y)
    
    cov_matrix <- cov(cbind(d_x, d_y))
    
    # Create ellipse data
    ellipse_data <- get_ellipse_data(xm + plat_x, ym + plat_y, cov_matrix)
    
    # Create plot
    p <- ggplot2::ggplot(ind_data, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_path(alpha = 0.6, linewidth = 0.5, color = "gray40") +
      ggplot2::geom_point(alpha = 0.4, size = 0.8, color = "gray60") +
      # Add covariance ellipse
      ggplot2::geom_path(data = ellipse_data, 
                        ggplot2::aes(x = .data$x, y = .data$y),
                        color = "blue", linewidth = 1.2, alpha = 0.8,
                        inherit.aes = FALSE) +
      # Add mean point
      ggplot2::geom_point(x = xm + plat_x, y = ym + plat_y,
                         color = "blue", size = 3, alpha = 0.8) +
      # Add platform
      ggplot2::geom_point(x = plat_x, y = plat_y,
                         color = "red", size = 4, alpha = 0.8) +
      ggplot2::labs(
        title = paste0(group_name, " - ", ind_name, "\nEntropía: ", round(entropy_val, 3)),
        x = "Coordenada X", y = "Coordenada Y"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::coord_fixed() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
        panel.grid = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white", color = NA)
      )
    
    # Add arena boundary if provided
    if (!is.null(wm_centr_x) && !is.null(wm_centr_y) && !is.null(radio_wm)) {
      p <- p + ggplot2::annotate(
        "path",
        x = wm_centr_x + radio_wm * cos(seq(0, 2*pi, length.out = 100)),
        y = wm_centr_y + radio_wm * sin(seq(0, 2*pi, length.out = 100)),
        color = "black", linewidth = 0.8, alpha = 0.5
      )
    }
    
    plot_list[[ind]] <- p
    
    # Store entropy value
    entropy_values <- rbind(entropy_values, 
                           data.frame(
                             Group = group_name,
                             Individual = ind_name,
                             Entropy = entropy_val
                           ))
  }
  
  return(list(plots = plot_list, entropy_data = entropy_values))
}

#' Helper function to create ellipse data from covariance matrix
#' @param x_center center x coordinate
#' @param y_center center y coordinate
#' @param cov_matrix 2x2 covariance matrix
#' @param confidence confidence level for ellipse (default 0.95)
#' @return data frame with ellipse coordinates
get_ellipse_data <- function(x_center, y_center, cov_matrix, confidence = 0.95) {
  # Get eigenvalues and eigenvectors
  eigen_result <- eigen(cov_matrix)
  eigenvalues <- eigen_result$values
  eigenvectors <- eigen_result$vectors
  
  # Calculate scaling factor for confidence interval
  scale_factor <- sqrt(qchisq(confidence, df = 2))
  
  # Create ellipse points
  theta <- seq(0, 2*pi, length.out = 100)
  
  # Scale by square root of eigenvalues
  ellipse_points <- cbind(
    sqrt(eigenvalues[1]) * cos(theta),
    sqrt(eigenvalues[2]) * sin(theta)
  )
  
  # Rotate by eigenvectors and scale
  rotated_points <- (eigenvectors %*% t(ellipse_points)) * scale_factor
  
  # Translate to center
  ellipse_data <- data.frame(
    x = rotated_points[1, ] + x_center,
    y = rotated_points[2, ] + y_center
  )
  
  return(ellipse_data)
}

#' Auto-detect arena parameters from data
#' @param data processed data frame
#' @return list with estimated arena parameters
auto_detect_arena <- function(data) {
  # Calculate data bounds
  x_range <- range(data$x, na.rm = TRUE)
  y_range <- range(data$y, na.rm = TRUE)
  
  # Estimate center as midpoint of data
  center_x <- mean(x_range)
  center_y <- mean(y_range)
  
  # Estimate radius as maximum distance from center to any point
  distances <- sqrt((data$x - center_x)^2 + (data$y - center_y)^2)
  radius <- quantile(distances, 0.95, na.rm = TRUE)  # Use 95th percentile to avoid outliers
  
  # Try to detect platform as the point with minimum movement (if time data available)
  if ("time" %in% names(data)) {
    # Group by proximity and find the area with lowest velocity
    data$distance_from_prev <- c(0, sqrt(diff(data$x)^2 + diff(data$y)^2))
    
    # Create grid and find area with lowest average movement
    grid_size <- 20
    x_breaks <- seq(x_range[1], x_range[2], length.out = grid_size)
    y_breaks <- seq(y_range[1], y_range[2], length.out = grid_size)
    
    data$x_bin <- cut(data$x, breaks = x_breaks, include.lowest = TRUE)
    data$y_bin <- cut(data$y, breaks = y_breaks, include.lowest = TRUE)
    
    platform_area <- data %>%
      dplyr::group_by(.data$x_bin, .data$y_bin) %>%
      dplyr::summarise(
        avg_movement = mean(.data$distance_from_prev, na.rm = TRUE),
        count = dplyr::n(),
        avg_x = mean(.data$x, na.rm = TRUE),
        avg_y = mean(.data$y, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      dplyr::filter(.data$count >= 5) %>%  # Minimum points in area
      dplyr::arrange(.data$avg_movement)
    
    if (nrow(platform_area) > 0) {
      platform_x <- platform_area$avg_x[1]
      platform_y <- platform_area$avg_y[1]
    } else {
      # Fallback: use center
      platform_x <- center_x
      platform_y <- center_y
    }
  } else {
    # No time data available, use center as fallback
    platform_x <- center_x
    platform_y <- center_y
  }
  
  return(list(
    center_x = center_x,
    center_y = center_y,
    radius = radius,
    platform_x = platform_x,
    platform_y = platform_y,
    data_bounds = list(x_range = x_range, y_range = y_range)
  ))
}
