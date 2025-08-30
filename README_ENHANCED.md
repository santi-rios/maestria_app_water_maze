# Análisis de Laberinto Acuático de Morris - Aplicación Shiny Mejorada

Esta aplicación Shiny permite analizar datos de trayectorias del laberinto acuático de Morris con funcionalidades avanzadas para el análisis de entropía espacial y configuración visual de la arena.

## Nuevas Funcionalidades Implementadas

### 1. Eliminación de Dependencias Rtrack
- Se eliminaron todas las referencias a la librería Rtrack para evitar problemas de dependencias
- Los mapas de calor ahora usan únicamente ggplot2 nativo para visualizaciones suaves y graduadas

### 2. Análisis de Entropía Individual Mejorado
- **Gráficos por individuo**: Cada animal/pista genera su propio gráfico de entropía
- **Elipses de covarianza**: Visualización de la matriz de covarianza como elipse azul
- **Cálculo robusto**: Manejo mejorado de matrices singulares y casos extremos
- **Descarga automática**: Para casos con muchos individuos, los gráficos se pueden descargar como PDF

#### Interpretación de las Elipses:
- **Elipse azul**: Representa la matriz de covarianza de la trayectoria respecto a la plataforma
- **Punto azul**: Centro de masa de la trayectoria
- **Punto rojo**: Ubicación de la plataforma
- **Valor de entropía**: Mostrado en el título de cada gráfico

### 3. Configuración Visual de Arena
- **Detección automática**: El sistema puede detectar automáticamente las dimensiones de la arena
- **Vista previa**: Pestaña dedicada para visualizar la configuración de la arena antes del análisis
- **Parámetros detectados**: Tabla con los valores automáticamente calculados
- **Configuración manual**: Opción de desactivar la detección automática para configuración manual

#### Parámetros Auto-detectados:
- **Centro X/Y**: Punto central estimado de la arena
- **Radio**: Radio estimado basado en el 95° percentil de distancias
- **Plataforma X/Y**: Ubicación estimada de la plataforma (área de menor movimiento)

## Estructura de Pestañas

### 1. Configuración de Arena
- Vista previa de datos con arena detectada
- Tabla de parámetros detectados
- Controles para activar/desactivar detección automática

### 2. Entropía Individual
- Gráficos individuales por animal con elipses de covarianza
- Tabla resumen de entropía por grupo
- Botón de descarga para PDFs multipágina (cuando hay >6 individuos)

### 3. Entropía Agrupada
- Análisis tradicional de entropía por grupo
- Gráfico de trayectorias combinadas

### 4. Mapa de Calor
- Visualización de densidad espacial
- Facetas por grupo
- Límites de arena y ubicación de plataforma

### 5. Estadísticas de Resumen
- Estadísticas descriptivas por grupo
- Pruebas estadísticas (t-test o ANOVA según el número de grupos)

## Formatos de Datos Soportados

### Archivos CSV con Columnas:
- `time`: Tiempo de la medición
- `x`, `y`: Coordenadas espaciales
- `treatment` o `Group`: Identificador de grupo
- `individual` (opcional): Identificador de individuo

### Datos de Ejemplo:
- **Checkbox "Usar datos de ejemplo"**: Carga datos con identificadores individuales
- **Datos múltiples**: Opción para datos de 3 grupos

## Dependencias

```r
library(shiny)
library(janitor)
library(dplyr)
library(ggplot2)
library(bslib)
library(purrr)
library(gridExtra)  # Para PDFs multipágina
```

## Instalación y Uso

1. **Instalar dependencias**:
```r
install.packages(c("shiny", "janitor", "dplyr", "ggplot2", "bslib", "purrr", "gridExtra"))
```

2. **Ejecutar la aplicación**:
```r
shiny::runApp("app")
```

3. **Cargar datos**:
   - Usar checkbox para datos de ejemplo, o
   - Subir archivos CSV con el formato especificado

4. **Configurar arena**:
   - Revisar la pestaña "Configuración de Arena"
   - Ajustar parámetros manualmente si es necesario

5. **Analizar**:
   - Hacer clic en "Analizar"
   - Explorar las diferentes pestañas de resultados

## Características Técnicas

### Cálculo de Entropía Mejorado
- Manejo de matrices singulares mediante regularización
- Verificación de valores finitos
- Soporte para trayectorias cortas

### Detección Automática de Arena
- Estimación robusta del centro usando rangos de datos
- Radio calculado como 95° percentil de distancias
- Detección de plataforma basada en áreas de menor movimiento

### Visualizaciones
- Elipses de covarianza con nivel de confianza del 95%
- Paletas de colores consistentes
- Layouts responsivos para diferentes números de individuos

## Archivos del Proyecto

- `app.R`: Aplicación principal Shiny
- `functions.R`: Funciones modulares de análisis
- `sample_data_*_individuals.csv`: Datos de ejemplo con identificadores individuales
- `test_enhanced_functions.R`: Script de prueba para validar funcionalidades

## Mejoras Implementadas

1. **Robustez**: Manejo mejorado de casos extremos en cálculos de entropía
2. **Escalabilidad**: Descarga automática para casos con muchos individuos
3. **Usabilidad**: Detección automática de parámetros de arena
4. **Visualización**: Elipses de covarianza para interpretar cálculos de entropía
5. **Modularidad**: Código organizado y fácil de mantener

La aplicación ahora proporciona una herramienta completa y robusta para el análisis del laberinto acuático de Morris, con capacidades tanto para análisis exploratorio rápido como para análisis detallado por individuo.
