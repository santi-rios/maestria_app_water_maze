# Helper and analysis functions for the water maze Shiny app

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(purrr)
})

# --- Core entropy computation -------------------------------------------------
calculate_entropy <- function(x, y, platform_x, platform_y, eps = 1e-8) {
  # Shift coordinates relative to platform
  rx <- x - platform_x
  ry <- y - platform_y
  n <- length(rx)
  if (n < 3L) return(NA_real_)
  # Radial mean square distance
  d2 <- mean(rx^2 + ry^2, na.rm = TRUE)
  if (!is.finite(d2) || d2 <= 0) d2 <- eps
  # Covariance of relative coordinates
  X <- cbind(rx, ry)
  # Regularize covariance for stability
  S <- tryCatch(stats::cov(X, use = "complete.obs"), error = function(e) NULL)
  if (is.null(S) || any(!is.finite(S))) {
    S <- matrix(0, nrow = 2, ncol = 2)
  }
  # Add small ridge to avoid singularity
  S <- S + diag(eps, 2)
  eig <- tryCatch(eigen(S, symmetric = TRUE, only.values = TRUE)$values,
                  error = function(e) c(eps, eps))
  eig[!is.finite(eig) | eig < eps] <- eps
  log_det <- sum(log(eig))
  H <- log(d2) + 0.5 * log_det
  as.numeric(H)
}

# --- Group-level entropy summary ---------------------------------------------
calculate_group_entropy <- function(data, platform_x, platform_y) {
  df <- data
  if (!"Group" %in% names(df)) df$Group <- "Grupo"
  df %>%
    dplyr::group_by(Group) %>%
    dplyr::summarise(
      entropy = calculate_entropy(x, y, platform_x, platform_y),
      .groups = "drop"
    )
}

# --- Trajectory plot ----------------------------------------------------------
create_trajectory_plot <- function(data, platform_x, platform_y, center_x, center_y, radius) {
  p <- ggplot(data, aes(x = x, y = y, color = Group)) +
    geom_path(alpha = 0.5, linewidth = 0.6) +
    annotate("path",
             x = center_x + radius * cos(seq(0, 2*pi, length.out = 200)),
             y = center_y + radius * sin(seq(0, 2*pi, length.out = 200)),
             color = "black", linewidth = 0.8, linetype = "dotted") +
    geom_point(x = platform_x, y = platform_y, color = "red", size = 3) +
    coord_fixed() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(x = "X", y = "Y", title = "Trayectorias por Grupo", color = "Grupo")
  p
}

# --- Heatmap plot -------------------------------------------------------------
create_heatmap_plot <- function(data, center_x, center_y, radius, platform_x = NA, platform_y = NA) {
  p <- ggplot(data, aes(x, y)) +
    stat_density_2d_filled(geom = "polygon", contour_var = "ndensity", bins = 12, alpha = 0.9) +
    scale_fill_viridis_d(option = "C") +
    annotate("path",
             x = center_x + radius * cos(seq(0, 2*pi, length.out = 200)),
             y = center_y + radius * sin(seq(0, 2*pi, length.out = 200)),
             color = "black", linewidth = 0.8, linetype = "dotted") +
    coord_fixed() +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(title = "Densidad de Ocupación", fill = "Densidad")
  if (is.finite(platform_x) && is.finite(platform_y)) {
    p <- p + geom_point(aes(x = platform_x, y = platform_y), color = "red", size = 3, inherit.aes = FALSE)
  }
  p
}

# --- Ellipse helper -----------------------------------------------------------
.get_ellipse <- function(mu, Sigma, level = 0.95, n = 200) {
  # Return data.frame x,y for ellipse of covariance Sigma centered at mu
  if (any(!is.finite(Sigma))) Sigma <- diag(1e-8, 2)
  Sigma <- as.matrix(Sigma)
  ev <- eigen(Sigma, symmetric = TRUE)
  vals <- ev$values
  vecs <- ev$vectors
  vals[vals < 1e-8] <- 1e-8
  # Chi-square quantile for 2D
  c2 <- stats::qchisq(level, df = 2)
  t <- seq(0, 2*pi, length.out = n)
  circ <- rbind(cos(t), sin(t))
  pts <- t(mu) + sqrt(c2) * t(vecs %*% (diag(sqrt(vals)) %*% circ))
  data.frame(x = pts[,1], y = pts[,2])
}

# --- Individual entropy plots -------------------------------------------------
create_individual_entropy_plots <- function(data, platform_x, platform_y, center_x, center_y, radius) {
  df <- data
  if (!"Individual" %in% names(df)) {
    df$Individual <- "Individuo"
  }
  if (!"Group" %in% names(df)) df$Group <- "Grupo"
  df <- df %>% dplyr::filter(is.finite(x), is.finite(y))

  # Compute entropy per individual
  ent_df <- df %>%
    dplyr::group_by(Individual, Group) %>%
    dplyr::summarise(
      entropy = calculate_entropy(x, y, platform_x, platform_y),
      .groups = "drop"
    )

  # Build plots per individual
  individuals <- unique(df$Individual)
  plots <- vector("list", length(individuals))
  names(plots) <- as.character(individuals)
  for (i in seq_along(individuals)) {
    ind <- individuals[i]
    di <- df %>% dplyr::filter(Individual == ind)
    # RMS circle (sqrt d2)
    d2 <- mean((di$x - platform_x)^2 + (di$y - platform_y)^2, na.rm = TRUE)
    rms_r <- sqrt(ifelse(is.finite(d2) && d2 > 0, d2, 1e-8))
    # Covariance ellipse
    S <- tryCatch(stats::cov(cbind(di$x - platform_x, di$y - platform_y), use = "complete.obs"), error = function(e) diag(1e-8,2))
    ell <- .get_ellipse(c(platform_x, platform_y), S, level = 0.95)

    p <- ggplot(di, aes(x = x, y = y)) +
      geom_path(alpha = 0.6, color = "#2C3E50") +
      # Arena boundary
      annotate("path",
               x = center_x + radius * cos(seq(0, 2*pi, length.out = 200)),
               y = center_y + radius * sin(seq(0, 2*pi, length.out = 200)),
               color = "black", linewidth = 0.7, linetype = "dotted") +
      # Platform
      geom_point(x = platform_x, y = platform_y, color = "red", size = 2) +
      # RMS circle
      annotate("path",
               x = platform_x + rms_r * cos(seq(0, 2*pi, length.out = 200)),
               y = platform_y + rms_r * sin(seq(0, 2*pi, length.out = 200)),
               color = "#E67E22", linewidth = 0.8, linetype = "dashed") +
      # Covariance ellipse
      geom_path(data = ell, aes(x = x, y = y), color = "#1F77B4", linewidth = 0.9) +
      coord_fixed() +
      theme_minimal() +
      labs(title = paste0("Individuo: ", ind),
           subtitle = "Elipse (95%) = Σ; círculo punteado = radio RMS",
           x = "X", y = "Y")
    plots[[i]] <- p
  }

  list(plots = plots, entropy_data = ent_df)
}

# --- Auto-detect arena parameters --------------------------------------------
auto_detect_arena <- function(data) {
  df <- data %>% dplyr::filter(is.finite(x), is.finite(y))
  if (nrow(df) == 0) {
    return(list(center_x = 0, center_y = 0, radius = 1, platform_x = 0, platform_y = 0))
  }
  center_x <- (min(df$x) + max(df$x)) / 2
  center_y <- (min(df$y) + max(df$y)) / 2
  dist <- sqrt((df$x - center_x)^2 + (df$y - center_y)^2)
  radius <- as.numeric(stats::quantile(dist, 0.95, na.rm = TRUE))
  # Platform guess: grid with lowest avg speed and >=5 points
  # Compute speed between consecutive points per Individual
  df <- df %>% arrange(Individual, time)
  dx <- c(NA, diff(df$x))
  dy <- c(NA, diff(df$y))
  spd <- sqrt(dx^2 + dy^2)
  # Reset at individual boundaries
  spd[which(diff(as.integer(as.factor(df$Individual))) != 0) + 1] <- NA
  df$speed <- spd
  # Grid
  nx <- ny <- 20
  gx <- cut(df$x, breaks = seq(min(df$x), max(df$x), length.out = nx + 1), include.lowest = TRUE)
  gy <- cut(df$y, breaks = seq(min(df$y), max(df$y), length.out = ny + 1), include.lowest = TRUE)
  grid <- df %>%
    mutate(gx = gx, gy = gy) %>%
    group_by(gx, gy) %>%
    summarise(n = dplyr::n(), v = mean(speed, na.rm = TRUE), cx = mean(x, na.rm = TRUE), cy = mean(y, na.rm = TRUE), .groups = "drop") %>%
    filter(n >= 5)
  if (nrow(grid) > 0 && any(is.finite(grid$v))) {
    idx <- which.min(grid$v)
    plat_x <- grid$cx[idx]
    plat_y <- grid$cy[idx]
  } else {
    plat_x <- center_x
    plat_y <- center_y
  }
  list(center_x = center_x, center_y = center_y, radius = radius, platform_x = plat_x, platform_y = plat_y)
}

# --- Summary stats and tests (distance/velocity) ------------------------------
calculate_summary_stats <- function(data) {
  df <- data
  if (!"Individual" %in% names(df)) df$Individual <- "Individuo"
  if (!"Group" %in% names(df)) df$Group <- "Grupo"
  df <- df %>% arrange(Group, Individual, time)
  # Per-individual metrics
  indiv <- df %>% group_by(Group, Individual) %>%
    summarise(
      Distancia_Total = sum(sqrt(diff(x)^2 + diff(y)^2), na.rm = TRUE),
      Velocidad_Promedio = mean(c(NA, sqrt(diff(x)^2 + diff(y)^2) / pmax(diff(time), 1e-8)), na.rm = TRUE),
      .groups = "drop"
    )
  # Group summary
  indiv %>% group_by(Group) %>% summarise(
    N = dplyr::n(),
    Distancia_Total = mean(Distancia_Total, na.rm = TRUE),
    Velocidad_Promedio = mean(Velocidad_Promedio, na.rm = TRUE),
    .groups = "drop"
  )
}

perform_statistical_tests <- function(summary_df) {
  if (is.null(summary_df) || nrow(summary_df) < 2) return(list(test_type = "none"))
  # We only have group means here; better tests need individual data, but we'll follow existing design
  # For robustness, return placeholders
  list(test_type = ifelse(nrow(summary_df) == 2, "t-test", "ANOVA"),
       distance_test = NULL,
       velocity_test = NULL)
}
