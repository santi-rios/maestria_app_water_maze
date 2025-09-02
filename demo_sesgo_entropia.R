# Demostración: Relación entre Sesgo, Aprendizaje y Entropía
# Este script genera ejemplos con diferentes niveles de sesgo para mostrar
# cómo afecta al aprendizaje y la entropía resultante

library(ggplot2)
library(gridExtra)
library(dplyr)

# Cargar funciones necesarias
source("app/functions.R")
source("app/simulate_data.R")

# Generar datos con diferentes niveles de sesgo (drift)
cat("🧪 Generando datos con diferentes niveles de sesgo...\n")

# Parámetros fijos
n_subjects = 4
n_points = 120
max_time = 45
center_x = 90.13
center_y = 61.3
radius = 65

# Diferentes niveles de sesgo (drift hacia la plataforma)
sesgos <- c(
  "Muy Bajo Sesgo (Poco Aprendizaje)" = 0.002,    # Casi sin dirección hacia plataforma
  "Sesgo Bajo" = 0.006,                          # Alguna dirección
  "Sesgo Medio" = 0.015,                         # Dirección moderada
  "Sesgo Alto (Mucho Aprendizaje)" = 0.030       # Dirección fuerte hacia plataforma
)

# Generar datos para cada nivel de sesgo
resultados <- list()
entropias <- numeric(length(sesgos))

for (i in seq_along(sesgos)) {
  nombre <- names(sesgos)[i]
  drift_value <- sesgos[i]
  
  cat("  - Generando:", nombre, "(drift =", drift_value, ")\n")
  
  # Generar datos con este nivel de sesgo
  datos <- generate_group_trajectories(
    n_subjects_per_group = n_subjects,
    groups = "Test",
    n_points = n_points,
    max_time = max_time,
    center_x = center_x,
    center_y = center_y,
    radius = radius,
    drift_control = drift_value,  # Usar el mismo drift para todos
    drift_treatment = drift_value
  )
  
  # Calcular entropía promedio
  entropia_promedio <- datos %>%
    group_by(Subject) %>%
    summarise(
      entropy = calculate_entropy(X, Y, 
                                 plat_x = center_x + radius * 0.5 * cos(pi/3), 
                                 plat_y = center_y + radius * 0.5 * sin(pi/3)),
      .groups = 'drop'
    ) %>%
    summarise(entropia_promedio = mean(entropy, na.rm = TRUE)) %>%
    pull(entropia_promedio)
  
  resultados[[nombre]] <- list(
    datos = datos,
    drift = drift_value,
    entropia = entropia_promedio
  )
  
  entropias[i] <- entropia_promedio
}

cat("\n📊 Resultados:\n")
for (i in seq_along(sesgos)) {
  nombre <- names(sesgos)[i]
  cat(sprintf("  %s: Entropía = %.3f\n", nombre, entropias[i]))
}

# Crear visualización comparativa
plots <- list()

for (i in seq_along(resultados)) {
  nombre <- names(resultados)[i]
  datos <- resultados[[nombre]]$datos
  drift <- resultados[[nombre]]$drift
  entropia <- resultados[[nombre]]$entropia
  
  # Posición de la plataforma
  plat_x <- center_x + radius * 0.5 * cos(pi/3)
  plat_y <- center_y + radius * 0.5 * sin(pi/3)
  
  p <- ggplot(datos, aes(x = X, y = Y, color = Subject)) +
    geom_path(alpha = 0.7, linewidth = 0.8) +
    geom_point(size = 0.5, alpha = 0.6) +
    # Arena
    annotate("path",
             x = center_x + radius * cos(seq(0, 2*pi, length.out = 100)),
             y = center_y + radius * sin(seq(0, 2*pi, length.out = 100)),
             color = "black", linewidth = 1) +
    # Plataforma
    annotate("point", x = plat_x, y = plat_y, 
             color = "red", size = 4, shape = 15) +
    annotate("text", x = plat_x, y = plat_y - 8, label = "Plataforma", 
             color = "red", size = 3, fontface = "bold") +
    labs(
      title = paste0(nombre),
      subtitle = paste0("Drift = ", sprintf("%.3f", drift), 
                       " | Entropía = ", sprintf("%.3f", entropia)),
      x = "X", y = "Y"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10, hjust = 0.5),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "darkblue"),
      axis.text = element_text(size = 8)
    ) +
    coord_fixed() +
    scale_color_viridis_d()
  
  plots[[i]] <- p
}

# Crear gráfico combinado
cat("\n📈 Creando visualización comparativa...\n")
combined_plot <- do.call(grid.arrange, c(plots, ncol = 2))

# Guardar el gráfico
ggsave("sesgo_vs_entropia_demo.png", combined_plot, 
       width = 12, height = 10, dpi = 300, bg = "white")

# Crear gráfico de relación sesgo vs entropía
relacion_data <- data.frame(
  Sesgo = sesgos,
  Entropia = entropias,
  Categoria = names(sesgos)
)

p_relacion <- ggplot(relacion_data, aes(x = Sesgo, y = Entropia)) +
  geom_point(size = 4, color = "darkblue") +
  geom_line(color = "darkblue", linewidth = 1.2) +
  geom_text(aes(label = sprintf("%.3f", Entropia)), 
            vjust = -0.5, hjust = 0.5, size = 3.5) +
  labs(
    title = "Relación entre Sesgo (Aprendizaje) y Entropía",
    subtitle = "Menor sesgo → Menor aprendizaje → Mayor entropía",
    x = "Nivel de Sesgo (drift hacia plataforma)",
    y = "Entropía Promedio",
    caption = "Sesgo alto = animales van más directo a la plataforma = menor entropía"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "darkred"),
    plot.caption = element_text(size = 10, color = "darkgreen")
  )

ggsave("relacion_sesgo_entropia.png", p_relacion, 
       width = 10, height = 6, dpi = 300, bg = "white")

cat("\n✅ Análisis completado!\n")
cat("📁 Archivos generados:\n")
cat("   - sesgo_vs_entropia_demo.png (comparación visual)\n")
cat("   - relacion_sesgo_entropia.png (gráfico de relación)\n")
cat("\n🔍 Conclusión:\n")
cat("   MENOR sesgo → MENOR aprendizaje → MAYOR entropía\n")
cat("   MAYOR sesgo → MAYOR aprendizaje → MENOR entropía\n")
