# Script de Validación de Entropía: App vs Cooke 2020
# Compara los cálculos de entropía de la app con los valores de referencia
# Author: GitHub Copilot
# Date: 2025-08-31

library(dplyr)
library(ggplot2)

# Función para calcular entropía usando el algoritmo de la app
source("/home/santi/Projects/maestria_app_water_maze/app/functions.R")

cat("=== VALIDACIÓN DE ENTROPÍA: APP vs COOKE 2020 ===\n\n")

# 1. Cargar datos de referencia de Cooke
cooke_ref <- read.csv("/home/santi/Projects/maestria_app_water_maze/data/cooke2020/entropia_validar_juntos.csv", 
                      stringsAsFactors = FALSE)

cat("Datos de referencia de Cooke cargados:\n")
cat("- Total de observaciones:", nrow(cooke_ref), "\n")
cat("- Grupos únicos:", paste(unique(cooke_ref$animal), collapse = ", "), "\n")
cat("- Rango de entropía:", round(min(cooke_ref$entropia), 2), "-", round(max(cooke_ref$entropia), 2), "\n\n")

# 2. Cargar datos convertidos de coordenadas
coord_data_31_2 <- read.csv("/home/santi/Projects/maestria_app_water_maze/data/cooke2020_31-2_converted.csv", 
                            stringsAsFactors = FALSE)
coord_data_37_3 <- read.csv("/home/santi/Projects/maestria_app_water_maze/data/cooke2020_37-3_converted.csv", 
                            stringsAsFactors = FALSE)

# 3. Parámetros de arena estimados (basados en el análisis previo)
center_x <- 54
center_y <- 53
radius <- 50
platform_x <- 50  # Estimación inicial - ajustar según sea necesario
platform_y <- 50  # Estimación inicial - ajustar según sea necesario

cat("Parámetros de arena utilizados:\n")
cat("- Centro: (", center_x, ",", center_y, ")\n")
cat("- Radio:", radius, "\n")
cat("- Plataforma: (", platform_x, ",", platform_y, ")\n\n")

# 4. Función para calcular entropía de la app para un animal específico
calculate_app_entropy <- function(data, individual_id, group_name, normalize = FALSE) {
  # Filtrar datos para el animal específico
  animal_data <- data %>%
    filter(Individual == individual_id)
  
  if (nrow(animal_data) < 3) {
    return(NA)
  }
  
  # Calcular entropía usando la función de la app
  entropy_val <- calculate_entropy(
    x = animal_data$x,
    y = animal_data$y,
    plat_x = platform_x,
    plat_y = platform_y,
    center_x = center_x,
    center_y = center_y,
    radius = radius,
    normalize = normalize
  )
  
  return(entropy_val)
}

# 5. Calcular entropías de la app para todos los animales disponibles
cat("Calculando entropías con la app...\n")

app_results <- data.frame()

# Procesar grupo 31-2
for (individual_id in unique(coord_data_31_2$Individual)) {
  entropy_raw <- calculate_app_entropy(coord_data_31_2, individual_id, "31-2", normalize = FALSE)
  entropy_norm <- calculate_app_entropy(coord_data_31_2, individual_id, "31-2", normalize = TRUE)
  
  if (!is.na(entropy_raw)) {
    app_results <- rbind(app_results, data.frame(
      id = individual_id + 1,  # Ajustar para coincidir con numeración de Cooke (1-based)
      animal = "31-2",
      entropy_app_raw = entropy_raw,
      entropy_app_norm = entropy_norm,
      stringsAsFactors = FALSE
    ))
  }
}

# Procesar grupo 37-3
for (individual_id in unique(coord_data_37_3$Individual)) {
  entropy_raw <- calculate_app_entropy(coord_data_37_3, individual_id, "37-3", normalize = FALSE)
  entropy_norm <- calculate_app_entropy(coord_data_37_3, individual_id, "37-3", normalize = TRUE)
  
  if (!is.na(entropy_raw)) {
    app_results <- rbind(app_results, data.frame(
      id = individual_id + 1,  # Ajustar para coincidir con numeración de Cooke (1-based)
      animal = "37-3",
      entropy_app_raw = entropy_raw,
      entropy_app_norm = entropy_norm,
      stringsAsFactors = FALSE
    ))
  }
}

cat("Entropías calculadas para", nrow(app_results), "animales\n\n")

# 6. Fusionar datos de Cooke con resultados de la app
validation_data <- merge(
  cooke_ref,
  app_results,
  by = c("id", "animal"),
  all = FALSE  # Solo mantener observaciones que coincidan
)

cat("=== DATOS PARA VALIDACIÓN ===\n")
cat("Observaciones coincidentes:", nrow(validation_data), "\n")
cat("Distribución por grupo:\n")
print(table(validation_data$animal))
cat("\n")

# 7. Estadísticas descriptivas
cat("=== ESTADÍSTICAS DESCRIPTIVAS ===\n")
summary_stats <- validation_data %>%
  group_by(animal) %>%
  summarise(
    n = n(),
    cooke_mean = round(mean(entropia), 3),
    cooke_sd = round(sd(entropia), 3),
    app_raw_mean = round(mean(entropy_app_raw, na.rm = TRUE), 3),
    app_raw_sd = round(sd(entropy_app_raw, na.rm = TRUE), 3),
    app_norm_mean = round(mean(entropy_app_norm, na.rm = TRUE), 3),
    app_norm_sd = round(sd(entropy_app_norm, na.rm = TRUE), 3),
    .groups = 'drop'
  )

print(summary_stats)
cat("\n")

# 8. Correlaciones
cat("=== ANÁLISIS DE CORRELACIÓN ===\n")

# Correlación Cooke vs App (valores brutos)
cor_raw <- cor(validation_data$entropia, validation_data$entropy_app_raw, use = "complete.obs")
cat("Correlación Cooke vs App (brutos):", round(cor_raw, 4), "\n")

# Correlación Cooke vs App (valores normalizados)
cor_norm <- cor(validation_data$entropia, validation_data$entropy_app_norm, use = "complete.obs")
cat("Correlación Cooke vs App (normalizados):", round(cor_norm, 4), "\n\n")

# 9. Test de correlación
cor_test_raw <- cor.test(validation_data$entropia, validation_data$entropy_app_raw)
cor_test_norm <- cor.test(validation_data$entropia, validation_data$entropy_app_norm)

cat("Test de correlación (valores brutos):\n")
cat("- r =", round(cor_test_raw$estimate, 4), "\n")
cat("- p-value =", format.pval(cor_test_raw$p.value), "\n")
cat("- IC 95%: [", round(cor_test_raw$conf.int[1], 4), ",", round(cor_test_raw$conf.int[2], 4), "]\n\n")

cat("Test de correlación (valores normalizados):\n")
cat("- r =", round(cor_test_norm$estimate, 4), "\n")
cat("- p-value =", format.pval(cor_test_norm$p.value), "\n")
cat("- IC 95%: [", round(cor_test_norm$conf.int[1], 4), ",", round(cor_test_norm$conf.int[2], 4), "]\n\n")

# 10. Análisis de Bland-Altman
cat("=== ANÁLISIS BLAND-ALTMAN ===\n")

# Para valores brutos
validation_data$mean_raw <- (validation_data$entropia + validation_data$entropy_app_raw) / 2
validation_data$diff_raw <- validation_data$entropy_app_raw - validation_data$entropia

mean_diff_raw <- mean(validation_data$diff_raw, na.rm = TRUE)
sd_diff_raw <- sd(validation_data$diff_raw, na.rm = TRUE)
loa_lower_raw <- mean_diff_raw - 1.96 * sd_diff_raw
loa_upper_raw <- mean_diff_raw + 1.96 * sd_diff_raw

cat("Bland-Altman (valores brutos):\n")
cat("- Diferencia media (bias):", round(mean_diff_raw, 4), "\n")
cat("- SD de diferencias:", round(sd_diff_raw, 4), "\n")
cat("- Límites de acuerdo 95%: [", round(loa_lower_raw, 4), ",", round(loa_upper_raw, 4), "]\n\n")

# Para valores normalizados
validation_data$mean_norm <- (validation_data$entropia + validation_data$entropy_app_norm) / 2
validation_data$diff_norm <- validation_data$entropy_app_norm - validation_data$entropia

mean_diff_norm <- mean(validation_data$diff_norm, na.rm = TRUE)
sd_diff_norm <- sd(validation_data$diff_norm, na.rm = TRUE)
loa_lower_norm <- mean_diff_norm - 1.96 * sd_diff_norm
loa_upper_norm <- mean_diff_norm + 1.96 * sd_diff_norm

cat("Bland-Altman (valores normalizados):\n")
cat("- Diferencia media (bias):", round(mean_diff_norm, 4), "\n")
cat("- SD de diferencias:", round(sd_diff_norm, 4), "\n")
cat("- Límites de acuerdo 95%: [", round(loa_lower_norm, 4), ",", round(loa_upper_norm, 4), "]\n\n")

# 11. Test t pareado para comparar métodos
cat("=== TESTS T PAREADOS ===\n")

# Test para valores brutos
t_test_raw <- t.test(validation_data$entropia, validation_data$entropy_app_raw, paired = TRUE)
cat("Test t pareado (valores brutos):\n")
cat("- Diferencia media:", round(t_test_raw$estimate, 4), "\n")
cat("- t =", round(t_test_raw$statistic, 4), "\n")
cat("- p-value =", format.pval(t_test_raw$p.value), "\n")
cat("- IC 95% diferencia: [", round(t_test_raw$conf.int[1], 4), ",", round(t_test_raw$conf.int[2], 4), "]\n\n")

# Test para valores normalizados
t_test_norm <- t.test(validation_data$entropia, validation_data$entropy_app_norm, paired = TRUE)
cat("Test t pareado (valores normalizados):\n")
cat("- Diferencia media:", round(t_test_norm$estimate, 4), "\n")
cat("- t =", round(t_test_norm$statistic, 4), "\n")
cat("- p-value =", format.pval(t_test_norm$p.value), "\n")
cat("- IC 95% diferencia: [", round(t_test_norm$conf.int[1], 4), ",", round(t_test_norm$conf.int[2], 4), "]\n\n")

# 12. Guardar resultados
write.csv(validation_data, "/home/santi/Projects/maestria_app_water_maze/data/validation_results.csv", 
          row.names = FALSE)

cat("=== RESUMEN DE VALIDACIÓN ===\n")
cat("1. Correlación más alta:", ifelse(cor_raw > cor_norm, "Valores brutos", "Valores normalizados"), "\n")
cat("2. Sesgo menor (Bland-Altman):", ifelse(abs(mean_diff_raw) < abs(mean_diff_norm), "Valores brutos", "Valores normalizados"), "\n")
cat("3. Datos de validación guardados en: validation_results.csv\n")
cat("4. Total de comparaciones exitosas:", nrow(validation_data), "\n\n")

cat("NOTA: Los resultados dependen de los parámetros de arena y plataforma utilizados.\n")
cat("Considere ajustar platform_x y platform_y si las correlaciones son bajas.\n")
