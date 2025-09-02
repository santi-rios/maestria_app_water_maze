# Synthetic trajectories generator for testing/demo
suppressPackageStartupMessages({
  library(dplyr)
})

# Generate synthetic group trajectories with different learning patterns
# Returns data.frame with columns: Time, X, Y, Subject, Treatment

generate_group_trajectories <- function(n_subjects_per_group = 8,
                                         groups = c("Control", "Tratamiento"),
                                         n_points = 120,
                                         max_time = 45,
                                         center_x = 90.13,
                                         center_y = 61.3,
                                         radius = 65,
                                         drift_control = 0.015,
                                         drift_treatment = 0.006) {
  set.seed(sample.int(1e6, 1))
  all <- list()
  for (g in groups) {
    for (s in seq_len(n_subjects_per_group)) {
      subj_id <- paste0(g, "_", sprintf("%02d", s))
      
      # Each subject gets different trajectory length (like real data)
      subj_n_points <- sample(60:180, 1)
      subj_max_time <- sample(25:60, 1)
      
      # Platform differs slightly by group to simulate bias
      # Use safer radius to ensure platform stays within bounds
      plat_ang <- if (g == groups[1]) pi/3 else 5*pi/4
      plat_r <- radius * 0.5  # Reduced from 0.6 to 0.5 for safety
      plat_x <- center_x + plat_r * cos(plat_ang)
      plat_y <- center_y + plat_r * sin(plat_ang)
      # Double check bounds and adjust if needed
      plat_dist <- sqrt((plat_x - center_x)^2 + (plat_y - center_y)^2)
      if (plat_dist > radius * 0.9) {
        plat_r <- radius * 0.4
        plat_x <- center_x + plat_r * cos(plat_ang)
        plat_y <- center_y + plat_r * sin(plat_ang)
      }

      t <- seq(0, subj_max_time, length.out = subj_n_points)
      # Radial tendency: better learners drift toward platform faster
      drift_strength <- if (g == groups[1]) drift_control else drift_treatment
      x <- numeric(subj_n_points)
      y <- numeric(subj_n_points)
      x[1] <- center_x + runif(1, -radius/2, radius/2)
      y[1] <- center_y + runif(1, -radius/2, radius/2)
      for (i in 2:subj_n_points) {
        vx <- drift_strength * (plat_x - x[i-1]) + rnorm(1, 0, radius*0.04)
        vy <- drift_strength * (plat_y - y[i-1]) + rnorm(1, 0, radius*0.04)
        x[i] <- x[i-1] + vx
        y[i] <- y[i-1] + vy
        # reflect at arena boundary
        d <- sqrt((x[i]-center_x)^2 + (y[i]-center_y)^2)
        if (d > radius) {
          ang <- atan2(y[i]-center_y, x[i]-center_x)
          x[i] <- center_x + radius * cos(ang)
          y[i] <- center_y + radius * sin(ang)
        }
      }
      all[[length(all)+1]] <- data.frame(
        Time = t,
        X = x,
        Y = y,
        Subject = subj_id,
        Treatment = g,
        stringsAsFactors = FALSE
      )
    }
  }
  dplyr::bind_rows(all)
}
