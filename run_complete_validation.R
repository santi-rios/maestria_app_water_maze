# Script Automatizado de Validación Completa
# Ejecuta toda la pipeline de validación de entropía
# Author: GitHub Copilot
# Date: 2025-08-31

# Función principal de validación
run_complete_validation <- function(
  platform_x = 50, 
  platform_y = 50,
  center_x = 54,
  center_y = 53,
  radius = 50,
  output_dir = "/home/santi/Projects/maestria_app_water_maze"
) {
  
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("   VALIDACIÓN AUTOMÁTICA DE ENTROPÍA: APP vs COOKE 2020\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Configurar directorio de salida
  results_dir <- file.path(output_dir, "validation_results")
  if (!dir.exists(results_dir)) {
    dir.create(results_dir, recursive = TRUE)
  }
  
  # 1. Ejecutar script de validación
  cat("🔄 Paso 1: Ejecutando análisis de validación...\n")
  source(file.path(output_dir, "validation_script.R"))
  
  # 2. Ejecutar script de gráficos
  cat("🔄 Paso 2: Generando gráficos de validación...\n")
  source(file.path(output_dir, "validation_plots_script.R"))
  
  # 3. Mover archivos a directorio de resultados
  cat("🔄 Paso 3: Organizando archivos de resultados...\n")
  
  files_to_move <- c(
    "validation_results.csv",
    "validation_plots.png"
  )
  
  for (file in files_to_move) {
    if (file.exists(file.path(output_dir, file))) {
      file.copy(
        from = file.path(output_dir, file),
        to = file.path(results_dir, file),
        overwrite = TRUE
      )
    }
  }
  
  # 4. Crear reporte resumido
  cat("🔄 Paso 4: Generando reporte resumido...\n")
  
  validation_data <- read.csv(file.path(output_dir, "data", "validation_results.csv"))
  
  # Estadísticas clave
  correlation <- cor(validation_data$entropia, validation_data$entropy_app_raw)
  bias <- mean(validation_data$diff_raw)
  rmse <- sqrt(mean(validation_data$diff_raw^2))
  n_comparisons <- nrow(validation_data)
  
  # Test t
  t_test <- t.test(validation_data$entropia, validation_data$entropy_app_raw, paired = TRUE)
  
  # Crear reporte resumido
  summary_report <- paste0(
    "VALIDACIÓN DE ENTROPÍA - REPORTE RESUMIDO\n",
    "========================================\n\n",
    "Fecha: ", Sys.Date(), "\n",
    "Parámetros utilizados:\n",
    "  - Centro arena: (", center_x, ", ", center_y, ")\n",
    "  - Radio arena: ", radius, "\n",
    "  - Plataforma: (", platform_x, ", ", platform_y, ")\n\n",
    "RESULTADOS PRINCIPALES:\n",
    "  ✓ Comparaciones exitosas: ", n_comparisons, "\n",
    "  ✓ Correlación: r = ", round(correlation, 4), "\n",
    "  ✓ Sesgo (bias): ", round(bias, 4), "\n",
    "  ✓ RMSE: ", round(rmse, 4), "\n",
    "  ✓ Test t pareado p-value: ", round(t_test$p.value, 6), "\n\n",
    "INTERPRETACIÓN:\n",
    ifelse(correlation > 0.8, "  ✅ Correlación EXCELENTE\n", "  ⚠️ Correlación necesita mejora\n"),
    ifelse(abs(bias) < 0.5, "  ✅ Sesgo MÍNIMO\n", "  ⚠️ Sesgo significativo\n"),
    ifelse(t_test$p.value > 0.05, "  ✅ Sin diferencia sistemática\n", "  ⚠️ Diferencia sistemática detectada\n"),
    ifelse(rmse < 1.5, "  ✅ Error BAJO\n", "  ⚠️ Error elevado\n"),
    "\nCONCLUSIÓN: ",
    ifelse(correlation > 0.8 & abs(bias) < 0.5 & t_test$p.value > 0.05, 
           "VALIDACIÓN EXITOSA ✅", "NECESITA AJUSTES ⚠️"), "\n\n",
    "Archivos generados:\n",
    "  - validation_results.csv (datos completos)\n",
    "  - validation_plots.png (gráficos)\n",
    "  - VALIDATION_REPORT.md (reporte detallado)\n"
  )
  
  # Guardar reporte resumido
  writeLines(summary_report, file.path(results_dir, "SUMMARY.txt"))
  
  cat("✅ Validación completada exitosamente!\n\n")
  cat("📁 Archivos guardados en:", results_dir, "\n")
  cat("📊 Resultados principales:\n")
  cat("   • Correlación:", round(correlation, 4), "\n")
  cat("   • Sesgo:", round(bias, 4), "\n")
  cat("   • RMSE:", round(rmse, 4), "\n")
  cat("   • Comparaciones:", n_comparisons, "\n\n")
  
  # Mostrar conclusión
  if (correlation > 0.8 & abs(bias) < 0.5 & t_test$p.value > 0.05) {
    cat("🎉 CONCLUSIÓN: VALIDACIÓN EXITOSA! La app es confiable para análisis de entropía.\n")
  } else {
    cat("⚠️  CONCLUSIÓN: Considere ajustar parámetros de arena/plataforma para mejorar la validación.\n")
  }
  
  cat("\n═══════════════════════════════════════════════════════════════\n")
  
  return(list(
    correlation = correlation,
    bias = bias,
    rmse = rmse,
    n_comparisons = n_comparisons,
    p_value = t_test$p.value,
    results_dir = results_dir
  ))
}

# Ejemplo de uso:
if (interactive()) {
  cat("Para ejecutar la validación completa, use:\n")
  cat("results <- run_complete_validation()\n\n")
  cat("Para ajustar parámetros:\n")
  cat("results <- run_complete_validation(\n")
  cat("  platform_x = 52,\n")
  cat("  platform_y = 48,\n")
  cat("  center_x = 54,\n")
  cat("  center_y = 53,\n")
  cat("  radius = 50\n")
  cat(")\n")
}
