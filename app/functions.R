# Functions for Morris Water Maze Analysis
# Author: Shiny App for Water Maze Analysis
# Date: 2025

#' Calculate entropy for a single trajectory
#' @param x numeric vector of x coordinates
#' @param y numeric vector of y coordinates
#' @param plat_x platform x coordinate
#' @param plat_y platform y coordinate
#' @return entropy value
calculate_entropy <- function(x, y, plat_x, plat_y) {
  # Calculate differences between platform and raw data
  d_x <- x - plat_x
  d_y <- y - plat_y
  dist2 <- d_x^2 + d_y^2
  
  # Entropy calculation
  w <- 1
  sw <- sum(w)
  
  xm <- mean(w * d_x) / sw
  ym <- mean(w * d_y) / sw
  xxm <- mean(w * d_x * d_x) / sw
  yym <- mean(w * d_y * d_y) / sw
  xym <- mean(w * d_x * d_y) / sw
  
  Sig <- matrix(c(xxm - xm^2, xym - xm * ym,
                  xym - xm * ym, yym - ym^2), nrow=2)
  
  eig_val <- eigen(Sig)$values
  var_xy2 <- eig_val[1] * eig_val[2]
  
  mdist2 <- mean(w * dist2) / sw
  
  entropy <- log(mdist2) + 0.5 * log(var_xy2)
  return(entropy)
}

#' Calculate entropy for grouped data
#' @param data data frame with columns: x, y, Group
#' @param plat_x platform x coordinate
#' @param plat_y platform y coordinate
#' @return data frame with Group and entropy columns
calculate_group_entropy <- function(data, plat_x, plat_y) {
  entropy_data <- data %>%
    dplyr::group_by(.data$Group) %>% 
    dplyr::summarise(
      entropy = calculate_entropy(.data$x, .data$y, plat_x, plat_y),
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
    files <- c("sample_data_control.csv", "sample_data_treatment.csv")
    data_list <- lapply(files, read.csv)
    names(data_list) <- c("Control", "Treatment")
    data <- dplyr::bind_rows(data_list, .id = "Group")
    
  } else {
    if (is.null(file_paths)) {
      stop("file_paths must be provided when use_sample is FALSE")
    }
    
    # ✅ Nuevo comportamiento: si se pasa un solo directorio, leer todos los .csv
    if (length(file_paths) == 1 && dir.exists(file_paths)) {
      file_paths <- list.files(path = file_paths, pattern = "\\.csv$", full.names = TRUE)
    }
    
    if (length(file_paths) == 0) {
      stop("No CSV files found in the specified directory.")
    }
    
    data_list <- lapply(file_paths, read.csv)
    
    # Verificar si los archivos tienen una columna 'treatment' o 'Treatment'
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

  # Limpieza de nombres de columnas
  data <- janitor::clean_names(data)
  
  # Renombrar group si es necesario
  if ("group" %in% names(data)) {
    data <- data %>% dplyr::rename(Group = group)
  }
  
  # Verificación opcional de primera fila "basura"
  first_row_bad <- any(is.na(as.numeric(as.character(data[1, c("time", "x", "y")]))))
  if (first_row_bad) {
    data <- data[-1, ]
  }
  
  # Conversión a numérico
  data <- data %>%
    dplyr::mutate(
      time = as.numeric(as.character(time)),
      x = as.numeric(as.character(x)),
      y = as.numeric(as.character(y))
    ) %>%
    dplyr::filter(!is.na(time), !is.na(x), !is.na(y))
  
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
    ggplot2::geom_path(alpha = 0.7, size = 0.8) +
    ggplot2::geom_point(data = data.frame(x = plat_x, y = plat_y), 
                       ggplot2::aes(.data$x, .data$y), 
                       color = "red", size = 5, inherit.aes = FALSE) +
    ggplot2::annotate("path",
                     x = wm_centr_x + radio_wm * cos(seq(0, 2*pi, length.out = 100)),
                     y = wm_centr_y + radio_wm * sin(seq(0, 2*pi, length.out = 100)),
                     color = "blue", alpha = 0.5, size = 1) +
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
  
  # Create custom color palette similar to Rtrack (yellow to orange to dark red)
  heat_colors <- viridisLite::viridis(n = 256)
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$x, y = .data$y)) +
    # Use stat_density_2d_filled for smoother, continuous heatmap
    ggplot2::geom_density_2d_filled(
    #   alpha = 0.8,
    #   bins = 15,  # More bins for smoother gradation
      contour_var = "count"  # Normalize density for better comparison between groups
    ) +
    # Add contour lines for better definition
    # ggplot2::stat_density_2d(
    #   color = "white", 
    #   alpha = 0.3, 
    #   linewidth = 0.3,
    #   bins = 8
    # ) +
    # Custom color scale
    # ggplot2::scale_fill_manual(
    #   values = heat_colors,
    #   name = "Densidad",
    #   guide = ggplot2::guide_legend(
    #     override.aes = list(alpha = 1),
    #     title.position = "top",
    #     title.hjust = 0.5
    #   )
    # ) +
    ggplot2::facet_wrap(~ .data$Group) +
    ggplot2::labs(
      title = "Mapa de Densidad de Posición por Grupo",
      x = "Coordenada X",
      y = "Coordenada Y"
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
    )
  
  # Add arena boundary circle if parameters provided
  if (!is.null(wm_centr_x) && !is.null(wm_centr_y) && !is.null(radio_wm)) {
    p <- p + ggplot2::annotate(
      "path",
      x = wm_centr_x + radio_wm * cos(seq(0, 2*pi, length.out = 100)),
      y = wm_centr_y + radio_wm * sin(seq(0, 2*pi, length.out = 100)),
      color = "black", 
      linewidth = 1.2,
      alpha = 0.8
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
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = data.frame(x = plat_x, y = plat_y),
      ggplot2::aes(x = .data$x, y = .data$y),
      color = "white",
      size = 2,
      shape = 19,
      inherit.aes = FALSE
    )
  }
  
  return(p)
}

#' Create heatmap plot using Rtrack style (alternative implementation)
#' @param data processed data frame
#' @param wm_centr_x center x coordinate of apparatus
#' @param wm_centr_y center y coordinate of apparatus  
#' @param radio_wm radius of apparatus
#' @param plat_x platform x coordinate (optional)
#' @param plat_y platform y coordinate (optional)
#' @param use_rtrack logical, whether to try using Rtrack package if available
#' @return ggplot object or Rtrack plot
create_heatmap_rtrack_style <- function(data, wm_centr_x, wm_centr_y, radio_wm, 
                                       plat_x = NULL, plat_y = NULL, use_rtrack = FALSE) {
  
  # If Rtrack is requested and available, try to use it
  if (use_rtrack && requireNamespace("Rtrack", quietly = TRUE)) {
    # This would require converting data to Rtrack format
    # For now, we'll fall back to ggplot2 implementation
    warning("Rtrack implementation not yet available, using ggplot2 version")
  }
  
  # Enhanced ggplot2 version with very smooth gradients
  heat_colors <- grDevices::colorRampPalette(c("#FFFF33", "#FFA500", "#703E3E"))(20)
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$x, y = .data$y)) +
    # Use geom_density_2d_filled with more levels for smoother appearance
    ggplot2::geom_density_2d_filled(
      alpha = 0.85,
      bins = 20,
      contour_var = "ndensity",
      show.legend = TRUE
    ) +
    # Add subtle contour lines
    ggplot2::geom_density_2d(
      color = "white", 
      alpha = 0.2, 
      linewidth = 0.2,
      bins = 12
    ) +
    # Rtrack-inspired color palette
    ggplot2::scale_fill_manual(
      values = heat_colors,
      name = "Densidad\nNormalizada"
    ) +
    ggplot2::facet_wrap(~ .data$Group) +
    # Add arena boundary circle
    ggplot2::annotate(
      "path",
      x = wm_centr_x + radio_wm * cos(seq(0, 2*pi, length.out = 200)),
      y = wm_centr_y + radio_wm * sin(seq(0, 2*pi, length.out = 200)),
      color = "black", 
      linewidth = 1.5,
      alpha = 0.9
    ) +
    ggplot2::labs(
      title = "Mapa de Densidad Espacial (Estilo Rtrack)",
      x = "Coordenada X",
      y = "Coordenada Y"
    ) +
    ggplot2::theme_void() +  # Cleaner background like Rtrack
    ggplot2::coord_fixed() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
      panel.spacing = ggplot2::unit(1, "lines")
    )
  
  # Add platform location if provided
  if (!is.null(plat_x) && !is.null(plat_y)) {
    p <- p + 
      ggplot2::geom_point(
        data = data.frame(x = plat_x, y = plat_y),
        ggplot2::aes(x = .data$x, y = .data$y),
        color = "black",
        size = 5,
        shape = 21,
        fill = "red",
        stroke = 1,
        inherit.aes = FALSE
      )
  }
  
  return(p)
}
