# Análisis de Métodos de Detección de Plataforma
# Evaluación de alternativas para diferentes protocolos experimentales

library(dplyr)
library(ggplot2)

# ==============================================================================
# MÉTODO ACTUAL: Velocidad Mínima
# ==============================================================================
# Fortalezas:
# - Funciona bien con videos completos donde animales pasan tiempo en plataforma
# - Simple de implementar
# - No requiere parámetros adicionales

# Debilidades:
# - Falla cuando video termina al llegar a plataforma
# - Sensible a ruido en coordenadas
# - Puede confundir zonas de "freezing" con plataforma

detect_platform_velocity <- function(data) {
  if (!"time" %in% names(data)) return(NULL)
  
  data$distance_from_prev <- c(0, sqrt(diff(data$x)^2 + diff(data$y)^2))
  
  grid_size <- 20
  x_range <- range(data$x, na.rm = TRUE)
  y_range <- range(data$y, na.rm = TRUE)
  x_breaks <- seq(x_range[1], x_range[2], length.out = grid_size)
  y_breaks <- seq(y_range[1], y_range[2], length.out = grid_size)
  
  data$x_bin <- cut(data$x, breaks = x_breaks, include.lowest = TRUE)
  data$y_bin <- cut(data$y, breaks = y_breaks, include.lowest = TRUE)
  
  platform_area <- data %>%
    group_by(x_bin, y_bin) %>%
    summarise(
      avg_movement = mean(distance_from_prev, na.rm = TRUE),
      count = n(),
      avg_x = mean(x, na.rm = TRUE),
      avg_y = mean(y, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(count >= 5) %>%
    arrange(avg_movement)
  
  if (nrow(platform_area) > 0) {
    return(list(x = platform_area$avg_x[1], y = platform_area$avg_y[1], method = "velocity"))
  }
  return(NULL)
}

# ==============================================================================
# MÉTODO ALTERNATIVO 1: Punto Final Más Frecuente
# ==============================================================================
# Ideal para protocolos donde video termina al llegar a plataforma
# Busca el punto final más común entre múltiples trayectorias

detect_platform_endpoint <- function(data) {
  if (!"Individual" %in% names(data)) return(NULL)
  
  # Obtener puntos finales de cada individuo
  endpoints <- data %>%
    group_by(Individual) %>%
    arrange(time) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  if (nrow(endpoints) < 2) return(NULL)
  
  # Clustering de puntos finales para encontrar la zona más común
  coords <- endpoints[, c("x", "y")]
  
  # Método simple: buscar el centroide de puntos finales cercanos
  # Usar distancia euclidiana para agrupar
  threshold <- quantile(dist(coords), 0.3, na.rm = TRUE)  # 30% de distancias más pequeñas
  
  # Encontrar el punto con más vecinos cercanos
  n_points <- nrow(coords)
  neighbor_counts <- numeric(n_points)
  
  for (i in 1:n_points) {
    distances <- sqrt((coords$x - coords$x[i])^2 + (coords$y - coords$y[i])^2)
    neighbor_counts[i] <- sum(distances <= threshold)
  }
  
  # Punto con más vecinos
  best_idx <- which.max(neighbor_counts)
  
  # Si hay un cluster claro (al menos 3 puntos), usar su centroide
  if (neighbor_counts[best_idx] >= 3) {
    center_point <- coords[best_idx, ]
    distances <- sqrt((coords$x - center_point$x)^2 + (coords$y - center_point$y)^2)
    cluster_points <- coords[distances <= threshold, ]
    
    platform_x <- mean(cluster_points$x)
    platform_y <- mean(cluster_points$y)
    
    return(list(x = platform_x, y = platform_y, method = "endpoint_cluster", 
                cluster_size = nrow(cluster_points)))
  }
  
  return(NULL)
}

# ==============================================================================
# MÉTODO ALTERNATIVO 2: Densidad Máxima
# ==============================================================================
# Busca la zona con mayor densidad de puntos
# Funciona bien cuando animales pasan tiempo en plataforma

detect_platform_density <- function(data, grid_resolution = 25) {
  x_range <- range(data$x, na.rm = TRUE)
  y_range <- range(data$y, na.rm = TRUE)
  
  # Crear grid más fino
  x_breaks <- seq(x_range[1], x_range[2], length.out = grid_resolution)
  y_breaks <- seq(y_range[1], y_range[2], length.out = grid_resolution)
  
  data$x_bin <- cut(data$x, breaks = x_breaks, include.lowest = TRUE, labels = FALSE)
  data$y_bin <- cut(data$y, breaks = y_breaks, include.lowest = TRUE, labels = FALSE)
  
  density_grid <- data %>%
    filter(!is.na(x_bin), !is.na(y_bin)) %>%
    group_by(x_bin, y_bin) %>%
    summarise(
      count = n(),
      avg_x = mean(x, na.rm = TRUE),
      avg_y = mean(y, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(count))
  
  if (nrow(density_grid) > 0) {
    # Tomar el top 1% de celdas más densas y promediar
    top_cells <- head(density_grid, max(1, round(nrow(density_grid) * 0.01)))
    platform_x <- weighted.mean(top_cells$avg_x, top_cells$count)
    platform_y <- weighted.mean(top_cells$avg_y, top_cells$count)
    
    return(list(x = platform_x, y = platform_y, method = "density", 
                max_density = max(density_grid$count)))
  }
  
  return(NULL)
}

# ==============================================================================
# MÉTODO ALTERNATIVO 3: Análisis de Trayectorias Convergentes
# ==============================================================================
# Busca el punto hacia donde convergen las trayectorias
# Útil para detectar objetivo sin depender de tiempo en plataforma

detect_platform_convergence <- function(data) {
  if (!"Individual" %in% names(data)) return(NULL)
  
  # Para cada individuo, calcular vector de dirección promedio en últimos puntos
  direction_vectors <- data %>%
    group_by(Individual) %>%
    arrange(time) %>%
    slice_tail(n = min(10, n())) %>%  # Últimos 10 puntos o menos
    summarise(
      start_x = first(x),
      start_y = first(y),
      end_x = last(x),
      end_y = last(y),
      .groups = 'drop'
    ) %>%
    mutate(
      dx = end_x - start_x,
      dy = end_y - start_y,
      direction_angle = atan2(dy, dx)
    )
  
  if (nrow(direction_vectors) < 3) return(NULL)
  
  # Buscar punto de intersección más común de las líneas de dirección
  # Método simplificado: buscar el punto que minimiza distancia a todas las líneas
  
  # Rango para buscar candidatos
  x_range <- range(data$x, na.rm = TRUE)
  y_range <- range(data$y, na.rm = TRUE)
  
  # Grid de candidatos
  candidate_x <- seq(x_range[1], x_range[2], length.out = 50)
  candidate_y <- seq(y_range[1], y_range[2], length.out = 50)
  candidates <- expand.grid(x = candidate_x, y = candidate_y)
  
  # Para cada candidato, calcular distancia promedio a las líneas de dirección
  best_score <- Inf
  best_point <- NULL
  
  for (i in 1:nrow(candidates)) {
    candidate <- candidates[i, ]
    
    # Distancia promedio a las líneas de dirección
    distances <- numeric(nrow(direction_vectors))
    
    for (j in 1:nrow(direction_vectors)) {
      vec <- direction_vectors[j, ]
      # Distancia de punto a línea definida por (start_x, start_y) en dirección (dx, dy)
      # Fórmula: |ax + by + c| / sqrt(a² + b²)
      
      if (abs(vec$dx) < 1e-6 && abs(vec$dy) < 1e-6) {
        # Vector nulo, usar distancia euclidiana al punto final
        distances[j] <- sqrt((candidate$x - vec$end_x)^2 + (candidate$y - vec$end_y)^2)
      } else {
        # Línea normal: ax + by + c = 0, donde (a,b) = (-dy, dx)
        a <- -vec$dy
        b <- vec$dx
        c <- vec$dy * vec$start_x - vec$dx * vec$start_y
        
        distances[j] <- abs(a * candidate$x + b * candidate$y + c) / sqrt(a^2 + b^2)
      }
    }
    
    avg_distance <- mean(distances, na.rm = TRUE)
    if (avg_distance < best_score) {
      best_score <- avg_distance
      best_point <- candidate
    }
  }
  
  if (!is.null(best_point)) {
    return(list(x = best_point$x, y = best_point$y, method = "convergence", 
                avg_distance = best_score))
  }
  
  return(NULL)
}

# ==============================================================================
# MÉTODO HÍBRIDO: Combinación Inteligente
# ==============================================================================
# Aplica múltiples métodos y selecciona el mejor basado en criterios de calidad

detect_platform_hybrid <- function(data) {
  results <- list()
  
  # Intentar todos los métodos
  results$velocity <- detect_platform_velocity(data)
  results$endpoint <- detect_platform_endpoint(data)
  results$density <- detect_platform_density(data)
  results$convergence <- detect_platform_convergence(data)
  
  # Filtrar resultados válidos
  valid_results <- results[!sapply(results, is.null)]
  
  if (length(valid_results) == 0) {
    # Fallback: centro de datos
    return(list(
      x = mean(range(data$x, na.rm = TRUE)),
      y = mean(range(data$y, na.rm = TRUE)),
      method = "fallback_center"
    ))
  }
  
  # Scoring system para seleccionar mejor método
  scores <- numeric(length(valid_results))
  names(scores) <- names(valid_results)
  
  for (i in seq_along(valid_results)) {
    result <- valid_results[[i]]
    method <- result$method
    
    # Criterios de scoring
    score <- 0
    
    # Bonus por método específico según contexto
    if (method == "endpoint_cluster" && "cluster_size" %in% names(result)) {
      score <- score + result$cluster_size * 2  # Más puntos en cluster = mejor
    }
    
    if (method == "density" && "max_density" %in% names(result)) {
      score <- score + log(result$max_density + 1)  # Log para evitar valores extremos
    }
    
    if (method == "convergence" && "avg_distance" %in% names(result)) {
      score <- score + 10 / (result$avg_distance + 1)  # Menor distancia = mejor
    }
    
    if (method == "velocity") {
      score <- score + 5  # Bonus base para método tradicional
    }
    
    # Penalty si está muy cerca del borde
    x_range <- range(data$x, na.rm = TRUE)
    y_range <- range(data$y, na.rm = TRUE)
    x_margin <- diff(x_range) * 0.1
    y_margin <- diff(y_range) * 0.1
    
    if (result$x < (x_range[1] + x_margin) || result$x > (x_range[2] - x_margin) ||
        result$y < (y_range[1] + y_margin) || result$y > (y_range[2] - y_margin)) {
      score <- score * 0.5  # Penalty por estar cerca del borde
    }
    
    scores[i] <- score
  }
  
  # Seleccionar método con mejor score
  best_method <- names(which.max(scores))
  best_result <- valid_results[[best_method]]
  
  # Agregar información del proceso de selección
  best_result$all_methods <- valid_results
  best_result$scores <- scores
  best_result$selected_method <- best_method
  
  return(best_result)
}

cat("🔬 Métodos de detección de plataforma implementados:\n")
cat("1. Velocidad Mínima (método actual)\n")
cat("2. Punto Final Más Frecuente (ideal para videos que terminan en plataforma)\n")
cat("3. Densidad Máxima (busca zona con más puntos)\n")
cat("4. Análisis de Convergencia (hacia dónde van las trayectorias)\n")
cat("5. Método Híbrido (combina todos y selecciona el mejor)\n\n")

cat("💡 Recomendación:\n")
cat("- Para protocolos estándar: método actual o híbrido\n")
cat("- Para videos que terminan al llegar: método de punto final\n")
cat("- Para datos ruidosos: método de densidad\n")
cat("- Para trayectorias dirigidas: método de convergencia\n")
