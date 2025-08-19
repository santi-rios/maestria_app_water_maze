# Demostración rápida de los mapas de calor mejorados
# Este script muestra la diferencia entre los dos estilos

library(dplyr)
library(ggplot2)

# Cargar funciones
source("functions.R")

cat("=== Demostración de Mapas de Calor Mejorados ===\n\n")

# Cargar datos de muestra
data_sample <- load_and_process_data(use_sample = TRUE)
cat("✓ Datos cargados:", nrow(data_sample), "puntos\n")
cat("✓ Grupos:", paste(unique(data_sample$Group), collapse = ", "), "\n\n")

# Parámetros del aparato
plat_x <- 117.8
plat_y <- 38.4
wm_centr_x <- 90.13
wm_centr_y <- 61.3
radio_wm <- 65

cat("=== Creando Mapas de Calor ===\n")

# Crear mapa de calor estándar
cat("• Creando mapa estándar...")
heatmap_standard <- create_heatmap_plot(
  data_sample, wm_centr_x, wm_centr_y, radio_wm, plat_x, plat_y
)
cat(" ✓\n")

# Crear mapa de calor estilo Rtrack
cat("• Creando mapa estilo Rtrack...")
heatmap_rtrack <- create_heatmap_rtrack_style(
  data_sample, wm_centr_x, wm_centr_y, radio_wm, plat_x, plat_y
)
cat(" ✓\n\n")

cat("=== Características de los Mapas ===\n")
cat("📊 MAPA ESTÁNDAR:\n")
cat("   - Contornos definidos con líneas blancas\n")
cat("   - Paleta de colores personalizada (6 niveles)\n")
cat("   - Fondo de cuadrícula mínima\n")
cat("   - Círculo del aparato en negro\n")
cat("   - Plataforma con punto rojo y centro blanco\n\n")

cat("🎨 MAPA ESTILO RTRACK:\n")
cat("   - Gradientes suaves y continuos (20 niveles)\n")
cat("   - Paleta inspirada en Rtrack (amarillo → naranja → rojo oscuro)\n")
cat("   - Fondo completamente limpio (theme_void)\n")
cat("   - Círculo del aparato más prominente\n")
cat("   - Plataforma con borde negro y centro rojo\n")
cat("   - Contornos sutiles en blanco\n\n")

cat("=== Ventajas de Cada Estilo ===\n")
cat("🔹 ESTÁNDAR: Mejor para análisis detallado de zonas específicas\n")
cat("🔹 RTRACK: Mejor para visualización general y presentaciones\n\n")

# Opcional: Guardar los gráficos
save_plots <- FALSE  # Cambiar a TRUE si quieres guardar
if (save_plots) {
  cat("=== Guardando Gráficos ===\n")
  ggsave("demo_heatmap_standard.png", heatmap_standard, 
         width = 12, height = 6, dpi = 300, bg = "white")
  cat("✓ Guardado: demo_heatmap_standard.png\n")
  
  ggsave("demo_heatmap_rtrack.png", heatmap_rtrack, 
         width = 12, height = 6, dpi = 300, bg = "white")
  cat("✓ Guardado: demo_heatmap_rtrack.png\n")
}

cat("=== Demostración Completada ===\n")
cat("Los mapas de calor están listos para usar en tu aplicación Shiny.\n")
cat("Puedes elegir entre los dos estilos según tus necesidades de visualización.\n")
