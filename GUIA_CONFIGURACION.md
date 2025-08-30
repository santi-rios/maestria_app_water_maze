# Configuración de Parámetros de Arena - Guía de Usuario

## ¿Cómo Funciona la Detección Automática?

### Algoritmos de Detección:

**1. Centro de la Arena:**
```
center_x = (mínimo_x + máximo_x) / 2
center_y = (mínimo_y + máximo_y) / 2
```
- Usa el punto medio de todos los datos de coordenadas
- Asume que los datos están distribuidos simétricamente alrededor del centro

**2. Radio de la Arena:**
```
distancias = sqrt((x - center_x)² + (y - center_y)²)
radio = percentil_95(distancias)
```
- Calcula distancias desde el centro estimado a todos los puntos
- Usa el percentil 95 para evitar outliers (puntos fuera de la arena)

**3. Detección de Plataforma:**
```
1. Divide el espacio en grilla de 20x20 celdas
2. Para cada celda, calcula:
   - Velocidad promedio = mean(sqrt(diff(x)² + diff(y)²))
   - Número de puntos en la celda
3. Plataforma = celda con menor velocidad y ≥5 puntos
```
- Principio: Los animales se mueven menos cuando están en la plataforma
- Si no encuentra una celda válida, usa el centro como fallback

## ¿Cuándo Usar Configuración Manual?

### Casos donde la detección automática puede fallar:

1. **Datos incompletos**: Si faltan partes de la trayectoria
2. **Arena no circular**: El algoritmo asume arenas circulares
3. **Plataforma móvil**: Si la plataforma cambia de posición entre sesiones
4. **Datos con ruido**: Muchos outliers pueden afectar la detección
5. **Configuraciones especiales**: Arenas con formas o tamaños específicos

### Cómo Ajustar Manualmente:

**Paso 1: Revisar Detección Automática**
- Ir a la pestaña "Configuración de Arena"
- Observar la vista previa con los parámetros detectados
- Verificar si el círculo negro abarca correctamente los datos
- Verificar si los puntos rojo (plataforma) y azul (centro) están bien ubicados

**Paso 2: Desactivar Detección Automática**
- Desmarcar "Detectar automáticamente dimensiones"
- Aparecerán los campos de configuración manual

**Paso 3: Copiar y Ajustar Parámetros**
- Clic en "Copiar parámetros detectados" para usar como punto de partida
- Ajustar los valores según sea necesario:
  - **Centro X/Y**: Coordenadas exactas del centro de la arena
  - **Radio**: Distancia desde el centro hasta el borde de la arena
  - **Plataforma X/Y**: Coordenadas exactas de la plataforma

**Paso 4: Verificar en Vista Previa**
- La vista previa se actualiza automáticamente
- Verificar que los parámetros se ven correctos antes de analizar

## Ejemplos de Uso

### Ejemplo 1: Arena de 120cm de diámetro
```
Centro: (0, 0)
Radio: 60
Plataforma: (45, 30)
```

### Ejemplo 2: Arena rectangular (requiere aproximación)
```
Centro: (punto medio del rectángulo)
Radio: (distancia máxima desde el centro a las esquinas)
Plataforma: (coordenadas conocidas)
```

### Ejemplo 3: Datos en píxeles vs cm
- Si tus datos están en píxeles, puedes usar los valores detectados
- Si necesitas convertir a cm, ajusta proporcionalmente todos los parámetros

## Consejos para Mejores Resultados

1. **Usa la vista previa**: Siempre verifica visualmente antes de analizar
2. **Consistencia**: Usa los mismos parámetros para todos los análisis del mismo experimento
3. **Documentación**: Anota los parámetros utilizados para reproducibilidad
4. **Precisión**: Los cálculos de entropía son sensibles a la ubicación exacta de la plataforma

## Impacto en los Análisis

### En el Cálculo de Entropía:
- **Plataforma X/Y**: Punto de referencia para calcular distancias
- **Afecta directamente**: Todos los valores de entropía

### En las Visualizaciones:
- **Centro y Radio**: Dibujan el círculo de la arena en los gráficos
- **Plataforma**: Aparece como punto rojo en todos los plots

### En la Detección de Trayectorias:
- Los parámetros ayudan a interpretar si las trayectorias son normales
- Filtran datos que podrían estar fuera de la arena

## Solución de Problemas Comunes

**"La plataforma detectada está mal ubicada"**
- Desactivar detección automática
- Copiar parámetros detectados
- Ajustar manualmente Plataforma X/Y

**"El círculo de arena es muy grande/pequeño"**
- Ajustar el valor del Radio
- Considerar si hay outliers en los datos

**"El centro no está bien ubicado"**
- Revisar si los datos están completos
- Ajustar Centro X/Y manualmente

**"Los datos parecen cortados"**
- Verificar que el Radio sea suficientemente grande
- Revisar los rangos de datos originales
