# Ejemplo de uso de las funciones modulares
# Este script muestra cómo usar cada función por separado

# Cargar librerías necesarias
library(dplyr)
library(ggplot2)

# Cargar funciones
source("functions.R")

# Ejemplo 1: Cargar y procesar datos
cat("=== Ejemplo 1: Cargar datos ===\n")

# Opción A: Usar datos de muestra
data_sample <- load_and_process_data(use_sample = TRUE)
cat("Datos de muestra cargados:", nrow(data_sample), "filas\n")
cat("Grupos encontrados:", paste(unique(data_sample$Group), collapse = ", "), "\n")

# Opción B: Cargar archivos específicos (descomenta para usar)
# data_files <- load_and_process_data(
#   file_paths = c("mi_archivo1.csv", "mi_archivo2.csv"),
#   file_names = c("mi_archivo1.csv", "mi_archivo2.csv")
# )

# Ejemplo 2: Calcular entropía por grupo
cat("\n=== Ejemplo 2: Calcular entropía ===\n")

# Definir parámetros de la plataforma
plat_x <- 117.8
plat_y <- 38.4

entropy_results <- calculate_group_entropy(data_sample, plat_x, plat_y)
print(entropy_results)

# Ejemplo 3: Calcular estadísticas de resumen
cat("\n=== Ejemplo 3: Estadísticas de resumen ===\n")

summary_stats <- calculate_summary_stats(data_sample)
print(summary_stats)

# Ejemplo 4: Realizar pruebas estadísticas
cat("\n=== Ejemplo 4: Pruebas estadísticas ===\n")

statistical_results <- perform_statistical_tests(summary_stats)
cat("Tipo de prueba:", statistical_results$test_type, "\n")

if (statistical_results$test_type == "t-test") {
  if (!is.null(statistical_results$distance_test)) {
    cat("P-valor para distancia:", statistical_results$distance_test$p.value, "\n")
  }
  if (!is.null(statistical_results$velocity_test)) {
    cat("P-valor para velocidad:", statistical_results$velocity_test$p.value, "\n")
  }
}

# Ejemplo 5: Crear gráficos
cat("\n=== Ejemplo 5: Crear gráficos ===\n")

# Parámetros del aparato
wm_centr_x <- 90.13
wm_centr_y <- 61.3
radio_wm <- 65

# Crear gráfico de trayectorias
trajectory_plot <- create_trajectory_plot(
  data_sample, plat_x, plat_y, wm_centr_x, wm_centr_y, radio_wm
)

# Crear mapa de calor estándar
heatmap_plot_standard <- create_heatmap_plot(
  data_sample, 
  wm_centr_x, wm_centr_y, radio_wm,  # Parámetros del aparato
  plat_x, plat_y                     # Parámetros de la plataforma
)

cat("Gráficos creados exitosamente\n")
cat("- Gráfico de trayectorias: trajectory_plot\n")
cat("- Mapa de calor estándar: heatmap_plot_standard\n") 

# Ejemplo 6: Guardar gráficos (opcional)
# ggsave("trajectory_plot.png", trajectory_plot, width = 10, height = 8)
# ggsave("heatmap_standard.png", heatmap_plot_standard, width = 12, height = 6)
# ggsave("heatmap_rtrack.png", heatmap_plot_rtrack, width = 12, height = 6)

# Ejemplo 7: Análisis de una sola trayectoria
cat("\n=== Ejemplo 7: Análisis de trayectoria individual ===\n")

# Filtrar datos de un solo grupo para demostración
single_group_data <- data_sample %>% filter(Group == "Control")

# Calcular entropía para este grupo
single_entropy <- calculate_entropy(
  single_group_data$x, 
  single_group_data$y, 
  plat_x, 
  plat_y
)

cat("Entropía para grupo Control:", single_entropy, "\n")

# Calcular distancia total
total_distance <- calculate_distance(single_group_data$x, single_group_data$y)
cat("Distancia total para grupo Control:", total_distance, "\n")

cat("\n=== Ejemplo completado ===\n")
cat("Las funciones están listas para ser utilizadas en tu análisis personalizado.\n")
