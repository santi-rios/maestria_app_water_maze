# Morris Water Maze Analysis App

Esta aplicación Shiny está diseñada para analizar datos de laberinto acuático de Morris, permitiendo el cálculo de entropía, análisis de trayectorias, mapas de calor y estadísticas de resumen.

## Estructura del Proyecto

```
app/
├── app.R                           # Aplicación principal Shiny
├── functions.R                     # Funciones modulares para análisis
├── simulate_data.R                 # Script para generar datos artificiales
├── test_functions.R                # Script para probar funciones individuales
├── sample_data.csv                 # Datos de ejemplo simples
├── sample_data_control.csv         # Datos de control (2 grupos)
├── sample_data_treatment.csv       # Datos de tratamiento (2 grupos)
├── sample_data_control_multi.csv   # Datos de control (3 individuos)
├── sample_data_treatment_multi.csv # Datos de tratamiento (3 individuos)
└── sample_data_combined.csv        # Datos combinados
```

## Archivos Principales

### app.R
Aplicación principal Shiny que contiene:
- **UI**: Interfaz de usuario con pestañas para diferentes análisis
- **Server**: Lógica del servidor que utiliza las funciones modulares

### functions.R
Contiene todas las funciones de análisis:
- `calculate_entropy()`: Calcula entropía para una trayectoria individual
- `calculate_group_entropy()`: Calcula entropía para datos agrupados
- `calculate_distance()`: Calcula distancia total recorrida
- `calculate_summary_stats()`: Calcula estadísticas de resumen por grupo
- `perform_statistical_tests()`: Realiza pruebas estadísticas (t-test o ANOVA)
- `load_and_process_data()`: Carga y procesa archivos de datos
- `create_trajectory_plot()`: Crea gráfico de trayectorias
- `create_heatmap_plot()`: Crea mapa de calor/densidad estándar
- `create_heatmap_rtrack_style()`: Crea mapa de calor suave estilo Rtrack

### simulate_data.R
Script para generar datos artificiales realistas:
- `generate_single_trajectory()`: Genera una trayectoria individual
- `generate_group_trajectories()`: Genera trayectorias para múltiples grupos
- `generate_sample_files()`: Crea archivos de datos de muestra

### test_functions.R
Script para validar cada función individualmente antes de usar en la aplicación.

## Uso

### Ejecutar la Aplicación
```r
# Desde el directorio app/
shiny::runApp()
```

### Generar Nuevos Datos de Muestra
```r
# Desde el directorio app/
source("simulate_data.R")
generate_sample_files(".", n_subjects = 5)  # Genera 5 sujetos por grupo
```

### Probar Funciones Individuales
```r
# Desde el directorio app/
source("test_functions.R")
```

## Formato de Datos

Los archivos CSV deben contener las siguientes columnas:
- `Time`: Tiempo en segundos
- `X`: Coordenada X
- `Y`: Coordenada Y  
- `Treatment` (opcional): Identificador del grupo de tratamiento

Si no se proporciona la columna `Treatment`, la aplicación usará los nombres de archivo para agrupar los datos.

## Funcionalidades

### Análisis de Entropía
- Calcula la entropía espacial de las trayectorias
- Compara entre grupos
- Visualiza trayectorias con respecto a la plataforma objetivo

### Mapa de Calor

- Muestra la densidad espacial de las posiciones
- **Dos estilos disponibles:**
  - **Estándar**: Mapa de calor tradicional con contornos definidos
  - **Estilo Rtrack**: Mapa de calor suave con gradientes continuos, similar a la librería Rtrack
- Facetas separadas por grupo de tratamiento
- Visualización del círculo del aparato y ubicación de la plataforma
- Paleta de colores optimizada (amarillo → naranja → rojo oscuro)

### Estadísticas de Resumen
- Distancia total recorrida
- Velocidad promedio
- Tiempo total
- Número de puntos de datos
- Comparaciones estadísticas automáticas (t-test o ANOVA)

## Parámetros Configurables

- **Coordenadas de la Plataforma**: Posición del objetivo (plat_x, plat_y)
- **Centro del Aparato**: Centro del laberinto (wm_centr_x, wm_centr_y)
- **Radio del Aparato**: Radio del laberinto circular

## Dependencias

```r
library(shiny)
library(janitor)
library(dplyr)
library(ggplot2)
library(bslib)
library(purrr)
```

## Datos de Ejemplo

La aplicación incluye varios conjuntos de datos de ejemplo:
- Datos simples (1 individuo por grupo)
- Datos con múltiples individuos (3 por grupo)
- Diferentes patrones de aprendizaje entre grupos control y tratamiento

## Contribución

Para agregar nuevas funcionalidades:
1. Agregue funciones a `functions.R`
2. Pruebe con `test_functions.R`
3. Integre en `app.R`
4. Actualice esta documentación
