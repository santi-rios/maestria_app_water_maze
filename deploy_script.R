# Script para deployar la aplicación a shinyapps.io
# Ejecutar después de configurar rsconnect::setAccountInfo()

library(rsconnect)

# Verificar que estamos en el directorio correcto
setwd("/home/santi/Projects/maestria_app_water_maze")

# Deployar la aplicación (versión optimizada)
rsconnect::deployApp(
  appDir = "app_deploy",             # Carpeta optimizada (92K vs 364K)
  appName = "water-maze-entropy",    # Nombre de tu app en shinyapps.io
  appTitle = "Morris Water Maze - Análisis de Entropía",  # Título visible
  launch.browser = TRUE,             # Abrir browser después del deploy
  forceUpdate = TRUE                 # Forzar actualización si ya existe
)

cat("✅ Deployment completado!\n")
cat("🌐 Tu app estará disponible en: https://tu-usuario.shinyapps.io/water-maze-entropy/\n")
