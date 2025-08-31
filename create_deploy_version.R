# Script para crear una versión optimizada de la app para deployment
# Esto crea una copia limpia sin archivos innecesarios

# Crear directorio para deployment
if (!dir.exists("app_deploy")) {
  dir.create("app_deploy")
}

# Archivos esenciales para el deployment
archivos_esenciales <- c(
  "app.R",              # Aplicación principal
  "functions.R",        # Funciones de análisis
  "simulate_data.R",    # Generación de datos
  "sample_data.csv"     # Un dataset de ejemplo mínimo
)

# Copiar solo archivos esenciales
for (archivo in archivos_esenciales) {
  if (file.exists(file.path("app", archivo))) {
    file.copy(
      from = file.path("app", archivo),
      to = file.path("app_deploy", archivo),
      overwrite = TRUE
    )
    cat("✅ Copiado:", archivo, "\n")
  } else {
    cat("⚠️  No encontrado:", archivo, "\n")
  }
}

# Verificar tamaño de la versión optimizada
size_mb <- round(sum(file.size(list.files("app_deploy", full.names = TRUE))) / 1024 / 1024, 2)
cat("\n📊 Tamaño de app_deploy:", size_mb, "MB\n")

if (size_mb < 10) {
  cat("✅ Tamaño excelente para deployment rápido!\n")
} else if (size_mb < 50) {
  cat("✅ Tamaño bueno para deployment\n")
} else {
  cat("⚠️  Tamaño considerable, pero dentro de límites\n")
}

cat("\n🚀 Directorio 'app_deploy' listo para subir a shinyapps.io\n")
