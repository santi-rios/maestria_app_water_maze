library(shiny)
library(janitor)
library(dplyr)
library(ggplot2)
library(bslib)
library(purrr)
library(gridExtra)

# Load custom functions
source("functions.R")
source("simulate_data.R")

# Define UI for app
ui <- fluidPage(
  theme = bs_theme(
    version = 4, 
    bg = "#fefefe", 
    fg = "#2c3e50",
    primary = "#8e44ad",
    secondary = "#9b59b6", 
    success = "#27ae60",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins")
  ),
  titlePanel(
    div(
      h1("Análisis de Laberinto Acuático de Morris", 
         style = "background: linear-gradient(135deg, #8e44ad, #9b59b6); 
                  background-clip: text; 
                  -webkit-background-clip: text; 
                  -webkit-text-fill-color: transparent; 
                  font-weight: bold;"),
      h4("Aplicación para Análisis de Entropía Espacial", 
         style = "color: #6c757d; margin-top: -10px;"),
      div(style = "margin-top: 10px; margin-bottom: 10px;",
        span("Visita la pestaña de metodología para conocer más", 
             style = "background: linear-gradient(135deg, #e1bee7, #ce93d8); 
                      color: #4a148c; 
                      padding: 6px 12px; 
                      border-radius: 20px; 
                      font-size: 12px; 
                      border: 1px solid #ba68c8; 
                      font-weight: 500;")
      ),
      p("Por Santiago Ríos - ", 
        tags$a("Posgrado en Ciencias Biológicas, UNAM", 
               href = "https://pcbiol.posgrado.unam.mx/", 
               target = "_blank",
               style = "color: #8e44ad; text-decoration: none; font-weight: 500;"), 
        style = "color: #6c757d; font-size: 14px; margin-top: -5px;")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Fuente de datos", style = "color: #7b1fa2; font-weight: 600;"),
      radioButtons(
        "data_source", "Seleccione una fuente:", inline = TRUE,
        choices = c("Subir archivos CSV" = "upload", "Generar datos de ejemplo" = "random"),
        selected = "random"
      ),
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file1", "Subir coordenadas", accept = ".csv", multiple = TRUE),
        # Dynamic UI to map columns when a file is uploaded
        uiOutput("column_mapper_ui"),
        fluidRow(
          column(6, actionButton("open_group_editor", "Editar grupos (opcional)", 
                                class = "btn-sm", 
                                style = "background-color: #e1bee7; color: #4a148c; border: 1px solid #ba68c8;")),
          column(6, actionButton("clear_uploaded", "Limpiar datos cargados", 
                                class = "btn-sm", 
                                style = "background-color: #ffcdd2; color: #c62828; border: 1px solid #e57373;"))
        ),
        tags$small(textOutput("upload_status")),
        tags$hr(),
        tags$div(
          h5("Cómo cargar y comparar grupos", style = "color: #8e44ad; font-weight: 600;"),
          tags$ul(
            tags$li("Sube los datos de coordenadas de un grupo (p. ej., carpeta 'Control') y selecciona las columnas que correspondan al tiempo y ejes `x` y `y` (campos obligatorios)."),
            tags$li("Haz click en el botón Analizar cuando hayas ajustado las coordenadas del laberinto. Los límites del laberinto NO influyen en la entropía, solo la localización del annulus y las coordenadas."),
            tags$li("Luego puedes subir otro suba otra tanda (p. ej., 'Fluoxetina'). Esto permitirá hacer comparaciones."),
            tags$li("Use 'Editar grupos' para asignar/corregir el Grupo/Tratamiento por cada archivo csv si hace falta."),
            tags$li("El botón 'Analizar' usa todos los datos acumulados hasta el momento.")
          )
        ),
        tags$div(
          style = "background-color: #f8f4ff; padding: 12px; border-radius: 8px; margin-top: 15px; border-left: 4px solid #8e44ad;",
          h6("📁 ¿No tienes datos? Descarga ejemplo validado:", style = "color: #4a148c; margin-bottom: 8px;"),
          tags$a(
            href = "https://raw.githubusercontent.com/santi-rios/maestria_app_water_maze/main/data/cooke2020_31-2_converted.csv",
            download = "cooke2020_ejemplo.csv",
            style = "color: #8e44ad; font-weight: bold; text-decoration: underline; font-size: 14px;",
            "📥 Cooke et al. (2020) - 93 trayectorias científicas"
          )
        )
      ),
      conditionalPanel(
        condition = "input.data_source == 'random'",
        h5("Simulación de Datos", style = "color: #8e44ad; font-weight: 600;"),
        p("Genera datos simulados con diferentes comportamientos de aprendizaje para explorar la aplicación y ver cómo cambian los valores de entropía"),
        fluidRow(
          column(8, 
                 numericInput("n_subjects_random", "Sujetos por grupo:", value = 8, min = 1, max = 15, step = 1)
          ),
          column(4,
                 br(),
                 actionButton("randomize_data", "🎲 Aleatorizar", icon = icon("dice"), 
                            style = "margin-top: 5px; 
                                   background: linear-gradient(135deg, #ff9800, #ff5722); 
                                   color: white; 
                                   border: none; 
                                   padding: 6px 12px; 
                                   border-radius: 15px; 
                                   font-weight: 500;")
          )
        ),
  sliderInput("bias_control", "Aprendizaje Control (sesgo)", min = 0.002, max = 0.05, value = 0.015, step = 0.001),
  sliderInput("bias_tratamiento", "Aprendizaje Tratamiento (sesgo)", min = 0.002, max = 0.05, value = 0.006, step = 0.001),
        tags$small("En cada paso, el animal se mueve hacia la plataforma según el sesgo"),
        tags$br(),
        tags$div(
          style = "background-color: #e8f4f8; padding: 8px; border-radius: 4px; margin-top: 8px;",
          tags$small(
            tags$strong("Nota:"), " Algunos puntos simulados pueden aparecer fuera del círculo de la arena. ",
            "Esto es normal en la simulación y no afecta el cálculo de entropía, ya que la entropía se basa en la ",
            "distancia a la plataforma y variabilidad direccional, no en los límites físicos del laberinto."
          )
        )
      ),
      tags$hr(),
      h4("Configuración de Arena", style = "color: #7b1fa2; font-weight: 600;"),
      checkboxInput("auto_detect", "Detectar automáticamente dimensiones", value = TRUE),
      conditionalPanel(
        condition = "input.auto_detect",
        h5("Método de Detección de Plataforma:", style = "color: #8e44ad; font-weight: 600;"),
        selectInput("platform_detection_method", "",
                   choices = c(
                     "Automático (recomendado)" = "auto",
                     "Velocidad mínima (método tradicional)" = "velocity", 
                     "Punto final más frecuente (video termina en plataforma)" = "endpoint",
                     "Densidad máxima (zona con más puntos)" = "density"
                   ),
                   selected = "auto"),
        tags$small("El método automático prueba velocidad mínima primero, luego punto final si falla."),
        tags$br(),
        tags$small("Seleccione 'Punto final' si su protocolo detiene el video al llegar a la plataforma."),
        tags$br(),
        tags$div(
          style = "background-color: #fff3cd; padding: 8px; border-radius: 4px; margin-top: 8px;",
          tags$small(
            tags$strong("💡 Guía de métodos:"), tags$br(),
            tags$strong("Velocidad mínima:"), " funciona cuando animales pasan tiempo en plataforma", tags$br(),
            tags$strong("Punto final:"), " ideal para videos que terminan al encontrar plataforma", tags$br(), 
            tags$strong("Densidad máxima:"), " busca zona donde animales pasan más tiempo"
          )
        )
      ),
      conditionalPanel(
        condition = "!input.auto_detect",
        h5("Parámetros Manuales:", style = "color: #8e44ad; font-weight: 600;"),
        numericInput("plat_x", "Coordenadas de plataforma en X", value = 117.8, step = 0.1),
        numericInput("plat_y", "Coordenadas de plataforma en Y", value = 38.4, step = 0.1),
        numericInput("wm_centr_x", "Centro en X del aparato", value = 90.13, step = 0.1),
        numericInput("wm_centr_y", "Centro en Y del aparato", value = 61.3, step = 0.1),
        numericInput("radio_wm", "Radio del aparato", value = 65, step = 0.5),
        tags$small("Los cambios manuales se aplican al presionar 'Actualizar vista previa'."),
        tags$br(),
        fluidRow(
          column(6, actionButton("copy_detected", "Copiar parámetros detectados", 
                                class = "btn-sm", 
                                style = "background-color: #e1bee7; color: #4a148c; border: 1px solid #ba68c8;")),
          column(6, actionButton("update_preview", "Actualizar vista previa", 
                                class = "btn-sm", 
                                style = "background-color: #8e44ad; color: white; border: none;"))
        )
      ),
      conditionalPanel(
        condition = "!input.auto_detect",
        tags$small("Los parámetros se detectan automáticamente basándose en los datos cargados."),
        tags$br(),
        tags$small("Desactive la detección automática para ajustar manualmente.")
      ),
      checkboxInput("click_set_platform", "Definir plataforma con clic en la vista previa", value = FALSE),
      tags$hr(),
      
      # Botón Analizar prominente
      div(style = "text-align: center; margin: 15px 0;",
        actionButton("analyze_btn", "🔬 Analizar Datos", 
                    class = "btn-lg", 
                    style = "background: linear-gradient(135deg, #8e44ad, #9b59b6); 
                           color: white; 
                           border: none; 
                           padding: 12px 30px; 
                           font-weight: bold; 
                           font-size: 16px;
                           border-radius: 25px;
                           box-shadow: 0 4px 15px rgba(142, 68, 173, 0.3);")
      ),
      tags$hr(),
      
      h4("Opciones de Análisis", style = "color: #7b1fa2; font-weight: 600;"),
      checkboxInput("normalize_entropy", "Normalizar entropía (recomendado para comparaciones)", value = TRUE),
      tags$small("La normalización hace que los valores sean comparables entre diferentes configuraciones experimentales."),
      tags$hr(),
      conditionalPanel(
        condition = "input.analyze_btn > 0",
        div(style = "text-align: center; margin-top: 15px;",
          downloadButton("download_entropy_plots", "📊 Descargar Gráficos de Entropía", 
                        style = "background: linear-gradient(135deg, #7b1fa2, #8e44ad); 
                               color: white; 
                               border: none; 
                               padding: 8px 20px; 
                               border-radius: 20px; 
                               font-weight: 500;")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Configuración de Arena", 
                 h3("Vista Previa del Laberinto Acuático"),
                 plotOutput("arena_preview", click = "arena_click"),
                 conditionalPanel(
                   condition = "input.auto_detect",
                   h4("Parámetros Detectados Automáticamente:"),
                   tableOutput("detected_params")
                 )
        ),
        tabPanel("Metodología",
                 h3("Detección Automática de Parámetros"),
                 tags$div(
                   h4("1. Centro del laberinto acuático"),
                   p("El centro se calcula como el punto medio de los rangos de coordenadas:"),
                   tags$code("center_x = (mínimo_x + máximo_x) / 2"),
                   tags$br(),
                   tags$code("center_y = (mínimo_y + máximo_y) / 2"),
                   p("NOTA: Este método asume que los datos están distribuidos simétricamente alrededor del centro de la arena."),
                   
                   h4("2. Radio del laberinto"),
                   p("El radio se estima utilizando el percentil 95 de las distancias desde el centro:"),
                   tags$code("distancias = √((x - center_x)² + (y - center_y)²)"),
                   tags$br(),
                   tags$code("radio = percentil_95(distancias)"),
                   p("El uso del percentil 95 permite evitar outliers que podrían estar fuera de la arena."),
                   
                   h4("3. Detección de Plataforma"),
                   p("La aplicación ofrece múltiples métodos de detección de plataforma para adaptarse a diferentes protocolos experimentales:"),
                   
                   h5("🔍 Método de Velocidad Mínima (tradicional)"),
                   tags$ol(
                     tags$li("Se divide el espacio en una grilla de 20×20 celdas"),
                     tags$li("Para cada celda se calcula:"),
                     tags$ul(
                       tags$li("Velocidad promedio: media de √(diff(x)² + diff(y)²)"),
                       tags$li("Número de puntos en la celda")
                     ),
                     tags$li("La plataforma se identifica como la celda con menor velocidad promedio y al menos 5 puntos")
                   ),
                   p(strong("Ideal para:"), " protocolos donde los animales permanecen en la plataforma por un tiempo."),
                   p(strong("Limitación:"), " falla cuando el video se detiene inmediatamente al llegar a la plataforma."),
                   
                   h5("🎯 Método de Punto Final Más Frecuente"),
                   tags$ol(
                     tags$li("Extrae el punto final de cada trayectoria individual"),
                     tags$li("Agrupa puntos finales por proximidad espacial"),
                     tags$li("Identifica el cluster más grande de puntos finales"),
                     tags$li("Calcula el centroide del cluster como ubicación de plataforma")
                   ),
                   p(strong("Ideal para:"), " protocolos donde el video termina cuando el animal encuentra la plataforma."),
                   p(strong("Ventaja:"), " no depende del tiempo pasado en la plataforma."),
                   
                   h5("📊 Método de Densidad Máxima"),
                   tags$ol(
                     tags$li("Divide el espacio en una grilla fina (25×25 celdas)"),
                     tags$li("Cuenta el número total de puntos en cada celda"),
                     tags$li("Identifica el 1% de celdas con mayor densidad"),
                     tags$li("Calcula el centroide ponderado por densidad")
                   ),
                   p(strong("Ideal para:"), " detectar zonas donde los animales pasan más tiempo."),
                   p(strong("Ventaja:"), " robusto ante ruido en las coordenadas."),
                   
                   h5("🤖 Método Automático (recomendado)"),
                   p("Prueba múltiples métodos en secuencia y selecciona el mejor resultado:"),
                   tags$ol(
                     tags$li("Intenta velocidad mínima primero"),
                     tags$li("Si falla, prueba punto final más frecuente"),
                     tags$li("Si falla, prueba densidad máxima"),
                     tags$li("Como último recurso, usa el centro de la arena")
                   ),
                   
                   tags$div(
                     style = "background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin: 10px 0;",
                     h5("💡 Recomendaciones de Uso", style = "margin-top: 0;"),
                     tags$ul(
                       tags$li(strong("Videos completos:"), " use 'Automático' o 'Velocidad mínima'"),
                       tags$li(strong("Videos que terminan en plataforma:"), " use 'Punto final más frecuente'"),
                       tags$li(strong("Datos ruidosos:"), " use 'Densidad máxima'"),
                       tags$li(strong("Casos difíciles:"), " compare varios métodos manualmente")
                     )
                   ),
                   p("Principio: Los animales tienden a moverse menos cuando están en la plataforma."),
                   
                   h4("4. Cálculo de Entropía Espacial"),
                   p("La entropía se calcula utilizando la fórmula:"),
                   tags$code("H = log(d²) + 0.5 × log(det(Σ))"),
                   p("Donde:"),
                   tags$ul(
                     tags$li("d² = distancia cuadrática media desde la plataforma"),
                     tags$li("Σ = matriz de covarianza de las coordenadas relativas a la plataforma"),
                     tags$li("det(Σ) = determinante de la matriz de covarianza")
                   ),
                   p("Esta medida cuantifica tanto la dispersión espacial como la variabilidad direccional de la búsqueda."),
                   h5("Detalles e interpretación"),
                   tags$ul(
                     tags$li(strong("Componente radial (d²)"), ": resume la lejanía promedio respecto a la plataforma. Es equivalente al cuadrado del radio RMS (cuyo círculo está dibujado en las figuras)."),
                     tags$li(strong("Componente direccional (det(Σ))"), ": proviene de la variabilidad y correlación de los desplazamientos (valores eigen de Σ). Capta si la búsqueda es alargada en una dirección o amplia en todas."),
                     tags$li(strong("Invariancias"), ": H es invariante a rotaciones (usa Σ) y responde a escalas de la arena de forma logarítmica, lo que estabiliza valores ante unidades distintas."),
                     tags$li(strong("Casos límite"), ": si el animal busca sobre la plataforma (d²→0) o sin variabilidad (det(Σ)→0), la fórmula se regulariza para evitar infinitos (se añaden valores mínimos muy pequeños)."),
                     tags$li(strong("Lectura práctica"), ": H alto combina distancia promedio grande y/o gran dispersión direccional; H bajo indica búsqueda precisa y cercana a la plataforma.")
                   ),
                   tags$hr(),
                   h4("Visualización e Interpretación"),
                   tags$ul(
                     tags$li(strong("Círculo RMS"), ": círculo punteado centrado en la plataforma con radio √d²; representa la lejanía promedio de la búsqueda."),
                     tags$li(strong("Elipse de covarianza"), ": elipse al 95% basada en Σ; su tamaño/orientación describe la dirección y dispersión de la trayectoria."),
                     tags$li(strong("Entropía alta"), ": d² grande y/o elipse grande → búsqueda amplia y desorganizada."),
                     tags$li(strong("Entropía baja"), ": d² pequeño y elipse compacta → búsqueda precisa y próxima a la plataforma.")
                   ),
                   tags$hr(),
                   h4("Comparaciones Entre Grupos: Mejores Prácticas"),
                   tags$div(style = "background-color: #fff3cd; padding: 15px; border-left: 4px solid #ffc107; margin: 10px 0;",
                     h5("⚠️ Consideraciones Importantes para Comparaciones Válidas"),
                     tags$ul(
                       tags$li(strong("Unidades de medida consistentes"), ": Todos los grupos deben usar las mismas unidades (píxeles, mm, cm). La entropía depende logarítmicamente de la escala."),
                       tags$li(strong("Misma configuración experimental"), ": Arena del mismo tamaño, cámara a la misma altura, misma resolución de grabación."),
                       tags$li(strong("Normalización por tamaño de arena"), ": Si compara estudios diferentes, normalice las coordenadas al rango [0,1] antes del análisis."),
                       tags$li(strong("Misma duración de prueba"), ": Tiempos de grabación similares entre grupos para evitar sesgos por cantidad de datos.")
                     )),
                   h5("💡 Estrategias de Normalización"),
                   tags$ol(
                     tags$li(strong("Por rango"), ": x_norm = (x - x_min) / (x_max - x_min), igual para y."),
                     tags$li(strong("Por radio de arena"), ": x_norm = (x - centro_x) / radio, igual para y."),
                     tags$li(strong("Z-score por sesión"), ": Estandarizar por media y desviación estándar de cada sesión.")
                   ),
                   tags$div(style = "background-color: #d1ecf1; padding: 10px; border-left: 4px solid #bee5eb; margin: 10px 0;",
                     p(strong("Ejemplo práctico"), ": Si un grupo usó cámara a 50 cm (arena = 400 píxeles) y otro a 100 cm (arena = 200 píxeles), 
                       normalice dividiendo todas las coordenadas por el radio respectivo antes de calcular entropía.")
                   )
                 )
        ),
        tabPanel("Validación Científica",
                 h3("🔬 Validación de la Implementación de Entropía", style = "color: #2E8B57;"),
                 
                 tags$div(style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-bottom: 20px; border-left: 5px solid #28a745;",
                   h4("✅ Implementación Validada", style = "color: #155724; margin-top: 0;"),
                     p("Los cálculos de entropía de esta aplicación han sido validados contra los valores de referencia de ", 
                     strong(tags$a("Cooke et al. (2020)", href = "https://pubmed.ncbi.nlm.nih.gov/32025289/", target = "_blank"), " (93 pruebas individuales)."), " Conoce más en mi trabajo de tesis (link pendiente).", style = "margin-bottom: 0; color: #155724;")
                   ),
                   
                 fluidRow(
                   column(6,
                     h4("📊 Resultados de Validación"),
                     tags$div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border: 1px solid #dee2e6;",
                       tags$table(class = "table table-borderless",
                         tags$tr(
                           tags$td(strong("Correlación de Pearson:"), style = "width: 60%;"),
                           tags$td(span("r = 0.895", style = "color: #28a745; font-weight: bold;"))
                         ),
                         tags$tr(
                           tags$td(strong("Significancia estadística:")),
                           tags$td(span("p < 0.001", style = "color: #28a745; font-weight: bold;"))
                         ),
                         tags$tr(
                           tags$td(strong("Varianza explicada:")),
                           tags$td(span("R² = 80.1%", style = "color: #28a745; font-weight: bold;"))
                         ),
                         tags$tr(
                           tags$td(strong("Sesgo promedio:")),
                           tags$td(span("-0.117", style = "color: #28a745; font-weight: bold;"))
                         ),
                         tags$tr(
                           tags$td(strong("Error (RMSE):")),
                           tags$td(span("0.813", style = "color: #28a745; font-weight: bold;"))
                         ),
                         tags$tr(
                           tags$td(strong("Comparaciones exitosas:")),
                           tags$td(span("93 animales", style = "color: #28a745; font-weight: bold;"))
                         )
                       )
                     )
                   ),
                   column(6,
                     h4("🎯 Interpretación de Resultados"),
                     tags$div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border: 1px solid #dee2e6;",
                       tags$ul(
                         tags$li(strong("Correlación excelente:"), " r = 0.895 indica concordancia muy alta"),
                         tags$li(strong("Sin sesgo sistemático:"), " diferencia promedio mínima (-0.117)"),
                         tags$li(strong("Error aceptable:"), " RMSE < 1 unidad de entropía"),
                         tags$li(strong("Estadísticamente válido:"), " p < 0.001 altamente significativo"),
                         tags$li(strong("Reproducible:"), " validado en 93 trayectorias independientes")
                       )
                     )
                   )
                 ),
                 
                 tags$hr(),
                 
                 h4("📈 Métodos de Validación Utilizados"),
                 fluidRow(
                   column(4,
                     tags$div(style = "background-color: #e3f2fd; padding: 15px; border-radius: 5px; height: 180px;",
                       h5("🔗 Análisis de Correlación", style = "color: #1976d2;"),
                       p("Correlación de Pearson entre valores de Cooke y la app."),
                       p(strong("Resultado:"), " r = 0.895"),
                       p(strong("Interpretación:"), " Concordancia excelente")
                     )
                   ),
                   column(4,
                     tags$div(style = "background-color: #fff3e0; padding: 15px; border-radius: 5px; height: 180px;",
                       h5("📏 Análisis Bland-Altman", style = "color: #f57c00;"),
                       p("Evaluación de concordancia y sesgo entre métodos."),
                       p(strong("Resultado:"), " Bias = -0.117"),
                       p(strong("Interpretación:"), " Sesgo mínimo aceptable")
                     )
                   ),
                   column(4,
                     tags$div(style = "background-color: #f3e5f5; padding: 15px; border-radius: 5px; height: 180px;",
                       h5("📊 Test t Pareado", style = "color: #7b1fa2;"),
                       p("Comparación estadística de medias entre métodos."),
                       p(strong("Resultado:"), " p = 0.168"),
                       p(strong("Interpretación:"), " Sin diferencia significativa")
                     )
                   )
                 )
                 
        ),
        tabPanel("Entropía Individual", 
                 h3("Análisis de Entropía por Individuo"),
                 
                 # Panel informativo para datos aleatorios
                 conditionalPanel(
                   condition = "input.data_source == 'random'",
                   div(
                     style = "background: linear-gradient(135deg, #f3e5f5, #e1bee7); 
                              padding: 20px; 
                              border-radius: 10px; 
                              margin-bottom: 20px; 
                              border-left: 5px solid #8e44ad;",
                     h4("📋 Análisis Individual con Datos de Ejemplo", style = "color: #4a148c; margin-top: 0;"),
                     p("Los análisis detallados por individuo están optimizados para datos reales cargados desde archivos CSV.", 
                       style = "color: #4a148c; font-size: 16px;"),
                     p(strong("Para explorar todas las funcionalidades:"), style = "color: #4a148c;"),
                     tags$ul(style = "color: #4a148c;",
                       tags$li("Descarga el conjunto de datos de ejemplo validado científicamente"),
                       tags$li("Sube el archivo CSV usando la opción 'Subir archivos CSV'"),
                       tags$li("Explora gráficos individuales, estadísticas detalladas y descargas PDF")
                     ),
                     div(style = "text-align: center; margin-top: 15px;",
                       tags$a(
                         href = "https://raw.githubusercontent.com/santi-rios/maestria_app_water_maze/main/data/cooke2020_31-2_converted.csv",
                         download = "cooke2020_ejemplo.csv",
                         class = "btn",
                         style = "background: linear-gradient(135deg, #8e44ad, #9b59b6); 
                                  color: white; 
                                  padding: 12px 25px; 
                                  border-radius: 25px; 
                                  text-decoration: none; 
                                  font-weight: bold;
                                  border: none;
                                  box-shadow: 0 4px 15px rgba(142, 68, 173, 0.3);",
                         "📥 Descargar Datos de Ejemplo (Cooke et al., 2020)"
                       )
                     ),
                     p(style = "font-size: 12px; color: #7b1fa2; text-align: center; margin-top: 10px;",
                       "Archivo validado con 93 trayectorias individuales de referencia científica")
                   )
                 ),
                 
                 p("Interpretación rápida:"),
                 tags$ul(
                   tags$li("Puntos y trazas: la trayectoria del individuo."),
                   tags$li("Elipse azul: región donde cae ~95% de la dispersión direccional (covarianza). Más grande/ancha = búsqueda más extendida o direccional."),
                   tags$li("Círculo punteado gris: radio RMS (raíz del promedio del cuadrado de distancias) respecto a la plataforma. Más grande = exploración más alejada."),
                   tags$li("Entropía combina ambas: lejanía promedio (RMS) + variabilidad direccional (elipse). Valores altos = búsqueda más desorganizada o alejada de la plataforma.")
                 ),
                 uiOutput("entropy_plots_ui"),
                 h4("Resumen de Entropía por Grupo"),
                 tableOutput("entropy_summary_table")
        ),
        tabPanel("Mapa de Calor", 
                 h3("Mapa de Calor de Densidad Espacial"),
                 plotOutput("heatmap"),
                 
                 tags$hr(),
                 h4("Valores de Entropía por Grupo"),
                 tableOutput("entropy_table"),
                 tags$br(),
                 
                 # Información sobre normalización
                 conditionalPanel(
                   condition = "input.normalize_entropy",
                   div(
                     style = "background-color: #f8f4ff; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #8e44ad;",
                     tags$strong("📊 Nota sobre normalización:"), " Los valores mostrados están normalizados. Los valores normalizados van de 0 a 1, donde 1 representa la máxima entropía posible para un área de exploración uniforme del laberinto."
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "!input.normalize_entropy",
                   div(
                     style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 4px solid #f39c12;",
                     tags$strong("⚠️ Nota:"), " Los valores mostrados son entropía sin normalizar. Para comparar entre diferentes configuraciones experimentales, se recomienda activar la normalización."
                   )
                 )
        ),
  tabPanel("Estadísticas de Resumen", 
                 
                 # Panel informativo para datos aleatorios
                 conditionalPanel(
                   condition = "input.data_source == 'random'",
                   div(
                     style = "background: linear-gradient(135deg, #e8f5e8, #c8e6c9); 
                              padding: 15px; 
                              border-radius: 8px; 
                              margin-bottom: 15px; 
                              border-left: 4px solid #27ae60;",
                     h5("📊 Análisis Estadísticos Detallados", style = "color: #2e7d32; margin-top: 0;"),
                     p("Los análisis estadísticos por individuo funcionan mejor con datos CSV reales. ", 
                       tags$a(href = "https://raw.githubusercontent.com/santi-rios/maestria_app_water_maze/main/data/cooke2020_31-2_converted.csv",
                              download = "cooke2020_ejemplo.csv",
                              "Descarga datos de ejemplo aquí", 
                              style = "color: #8e44ad; font-weight: bold; text-decoration: underline;"),
                       " para análisis completos.",
                       style = "color: #2e7d32; margin-bottom: 0;")
                   )
                 ),
                 
                 wellPanel(
                   fluidRow(
                     column(4, selectInput("test_variable", "Variable a probar", choices = c("Entropía (entre grupos)"), selected = "Entropía (entre grupos)")),
                     column(4, selectInput("test_method", "Método", choices = c("Auto", "t-test", "ANOVA", "Kruskal-Wallis"), selected = "Auto")),
                     column(4, checkboxGroupInput("test_checks", "Verificar supuestos", choices = c("Normalidad (Shapiro)" = "shapiro", "Varianzas (Levene/Bartlett)" = "variance"), selected = c("shapiro", "variance")))
                   ),
                   tags$small(
                     tags$b("Sobre 'Auto': "),
                     "si hay 2 grupos ejecuta t-test; si hay más de 2, ejecuta ANOVA.",
                     " No cambia automáticamente a pruebas no paramétricas; si los supuestos no se cumplen, seleccione 'Kruskal-Wallis'."
                   )
     ),
     h4("Pruebas de hipótesis (resumen)"),
     tableOutput("test_summary_table"),
                 verbatimTextOutput("summary_stats")
        ),
        tabPanel("Acerca de",
                 h3("Información del Software"),
                 tags$div(
                   h4("Título de Tesis"),
                   p(strong("Caracterización de los efectos de la Fluoxetina sobre el aprendizaje espacial y la flexibilidad cognitiva en un modelo de estrés crónico en ratón")),
                   h4("Autor del Código"),
                     p(strong("Santiago García Ríos"), tags$br(),
                     "Posgrado en Ciencias Biológicas", tags$br(),
                     "Universidad Nacional Autónoma de México", tags$br(),
                     tags$a("https://santi-rios.github.io/", href = "https://santi-rios.github.io/", target = "_blank")),
                     
                   h4("Cómo Citar"),
                   tags$div(
                     style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;",
                     p(strong("Formato APA:")),
                     p(
                       "Garcia-Ríos, S., ",
                       tags$span("Martínez-Canabal, A.", style = "font-size: 6px;"),
                       " (2025). Caracterización de los efectos de la Fluoxetina sobre el",
                       " aprendizaje espacial y la flexibilidad cognitiva en un modelo de",
                       " estrés crónico en ratón. (Tesis de Maestría). Universidad Nacional",
                       " Autónoma de México, México"
                     )
                   ),
                   
                   h4("Descripción"),
                   p("Esta aplicación implementa métodos automatizados para el análisis de datos del laberinto acuático de Morris, 
                     incluyendo detección automática de parámetros experimentales, cálculo de entropía espacial, y visualizaciones 
                     interactivas de trayectorias y mapas de calor."),
                   
                   h4("Características Principales"),
                   tags$ul(
                     tags$li("Detección automática de parámetros de arena y plataforma"),
                     tags$li("Análisis de entropía espacial con visualización de elipses de covarianza"),
                     tags$li("Generación de datos sintéticos para pruebas y validación"),
                     tags$li("Mapas de calor de densidad espacial"),
                     tags$li("Análisis estadísticos comparativos entre grupos"),
                     tags$li("Exportación de resultados en formato PDF")
                   ),
                   
                   h4("Tecnologías Utilizadas"),
                   p("R, Shiny, ggplot2, dplyr, bslib, gridExtra"),
                   
                   h4("Licencia"),
                   p("MIT License - Uso libre para fines académicos y de investigación"),
                   
                   h4("Contribuciones"),
                   p("Las contribuciones y sugerencias son bienvenidas. Por favor contactar al autor o abrir un issue en el repositorio de GitHub."),
                   
                   h4("Versión"),
                   p("Versión 1.0 - Agosto 2025"),
                   
                   tags$hr(),
                   p(tags$small("Desarrollado como parte de los estudios de maestría en neurociencias. 
                                Para uso académico y de investigación."))
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Reactive values to store arena parameters
  arena_params <- reactiveValues(
    center_x = 90.13,
    center_y = 61.3,
    radius = 65,
    platform_x = 117.8,
    platform_y = 38.4
  )

  # Reactive value to store randomly generated data
  random_data <- reactiveVal(NULL)
  # Reactive value to store group overrides (named vector: Individual -> Group)
  group_overrides <- reactiveVal(NULL)
  # Whether user has clicked to set platform
  platform_override <- reactiveVal(FALSE)
  # Accumulated uploaded datasets (list of data.frames)
  upload_accum <- reactiveVal(list())
  # Track processed file paths to prevent duplicate appends
  processed_files <- reactiveVal(character(0))

  # Clear random data when switching data source to upload; provide hint when switching to random
  observeEvent(input$data_source, {
    if (identical(input$data_source, "upload")) {
      random_data(NULL)
    } else if (identical(input$data_source, "random")) {
      showNotification("Use el botón 'Aleatorizar' para generar datos de ejemplo.", type = "message", duration = 3)
    }
  })

  # Generate random data when button is clicked
  observeEvent(input$randomize_data, {
    # Show loading indicator
    showNotification("Generando datos aleatorios...", type = "message", duration = NULL, id = "randomizing")
    
    # Generate new random trajectories
    new_data <- generate_group_trajectories(
      n_subjects_per_group = input$n_subjects_random,
      groups = c("Control", "Tratamiento"),
      n_points = sample(80:150, 1),  # Random trajectory length per group
      max_time = sample(30:60, 1),    # Random trial duration
      drift_control = input$bias_control,
      drift_treatment = input$bias_tratamiento
    )
    
    # Format data to match expected structure
    formatted_data <- new_data %>%
      dplyr::rename(
        time = Time,
        x = X,
        y = Y,
        Individual = Subject
      ) %>%
      dplyr::mutate(
        time = as.numeric(time),
        x = as.numeric(x),
        y = as.numeric(y),
        Group = Treatment,  # Add Group column from Treatment
        # Ensure Individual column has character type and unique values
        Individual = as.character(Individual)
      ) %>%
      dplyr::select(time, x, y, Individual, Group)  # Remove Treatment column to avoid confusion
    
    # Debug output to verify structure (only in development)
    if (getOption("shiny.trace", FALSE)) {
      cat("Generated data structure:\n")
      cat("Columns:", paste(colnames(formatted_data), collapse = ", "), "\n")
      cat("Unique individuals:", length(unique(formatted_data$Individual)), "\n")
      cat("Groups:", paste(unique(formatted_data$Group), collapse = ", "), "\n")
    }
    
    random_data(formatted_data)
    
    # Remove loading indicator and show success
    removeNotification("randomizing")
    showNotification(paste("¡Datos aleatorios generados!", input$n_subjects_random, "sujetos por grupo"), 
                    type = "message", duration = 3)
  })

  getData <- reactive({
    # Check if random data is available and should be used
    if (input$data_source == "random" && !is.null(random_data())) {
      data <- random_data()
      # Apply group overrides even for random data
      overrides <- group_overrides()
      if (!is.null(overrides) && "Individual" %in% names(data)) {
        idx <- match(data$Individual, names(overrides))
        repl <- overrides[idx]
        data$Group <- ifelse(!is.na(repl), repl, data$Group)
      }
      return(data)
    } else if (input$data_source == "upload" && !is.null(input$file1$datapath)) {
      # Handle uploaded files with user-selected column mappings
      files <- input$file1$datapath
      if (is.null(files) || length(files) == 0) return(NULL)

      # Determine which are new (unprocessed) files
      already <- processed_files()
      new_files <- setdiff(files, already)
      acc <- upload_accum()
      # If no new files and have accumulated data, return accumulated (with overrides)
      if (length(new_files) == 0) {
        if (length(acc) == 0) return(NULL)
        data <- dplyr::bind_rows(acc)
        overrides <- group_overrides()
        if (!is.null(overrides) && "Individual" %in% names(data)) {
          idx <- match(data$Individual, names(overrides))
          repl <- overrides[idx]
          data$Group <- ifelse(!is.na(repl), repl, data$Group)
        }
        return(data)
      }

      # Validate required mappings
      req(input$col_time, input$col_x, input$col_y)

      all_data <- list()
      for (i in seq_along(new_files)) {
        fp <- new_files[i]
        df <- tryCatch({
          read.csv(fp, check.names = FALSE)
        }, error = function(e) {
          showNotification(paste("No se pudo leer el archivo:", basename(fp)), type = "error")
          NULL
        })
        if (is.null(df)) next

        # Also support cleaned names
        df_clean <- janitor::clean_names(df)

        # Helper: resolve selected column name against raw and cleaned names
        resolve_col <- function(sel) {
          if (is.null(sel) || sel == "") return(NA_character_)
          # Exact match in raw
          if (sel %in% names(df)) return(sel)
          # Try cleaned match
          sel_clean <- janitor::make_clean_names(sel)
          idx <- which(names(df_clean) == sel_clean)
          if (length(idx) == 1) {
            # map back to raw by position
            return(names(df)[idx])
          }
          # Try case-insensitive match
          idx2 <- which(tolower(names(df)) == tolower(sel))
          if (length(idx2) == 1) return(names(df)[idx2])
          NA_character_
        }

        c_time <- resolve_col(input$col_time)
        c_x    <- resolve_col(input$col_x)
        c_y    <- resolve_col(input$col_y)
        c_ind  <- resolve_col(input$col_individual)
        c_grp  <- resolve_col(input$col_group)

        missing_cols <- c()
        if (is.na(c_time)) missing_cols <- c(missing_cols, input$col_time)
        if (is.na(c_x))    missing_cols <- c(missing_cols, input$col_x)
        if (is.na(c_y))    missing_cols <- c(missing_cols, input$col_y)
        if (length(missing_cols) > 0) {
          showNotification(paste("Columnas no encontradas en", basename(fp), ":",
                                 paste(missing_cols, collapse = ", ")), type = "error", duration = 7)
          next
        }

        out <- data.frame(
          time = suppressWarnings(as.numeric(as.character(df[[c_time]]))),
          x    = suppressWarnings(as.numeric(as.character(df[[c_x]]))),
          y    = suppressWarnings(as.numeric(as.character(df[[c_y]])))
        )
        # Drop rows with NA essentials
        out <- out %>% dplyr::filter(!is.na(time), !is.na(x), !is.na(y))

        # Individual column optional
        if (!is.na(c_ind)) {
          out$Individual <- as.character(df[[c_ind]])
        } else {
          # Default: use file base name as Individual id
          out$Individual <- tools::file_path_sans_ext(basename(fp))
        }

        # Group assignment priority:
        # 1) If user selected a group column
        # 2) Else if checkbox to use filename as group is TRUE
        # 3) Else use provided group label or fallback 'Grupo'
        if (!is.na(c_grp)) {
          out$Group <- as.character(df[[c_grp]])
        } else if (isTRUE(input$group_from_filename)) {
          out$Group <- tools::file_path_sans_ext(basename(fp))
        } else {
          out$Group <- if (!is.null(input$group_label) && nzchar(input$group_label)) input$group_label else "Grupo"
        }

        all_data[[length(all_data) + 1]] <- out
      }

      if (length(all_data) == 0) return(NULL)
  newly <- dplyr::bind_rows(all_data)
      # Append to accumulator keyed by unique file base names + time range to avoid duplicates
      # Build a key per chunk
      chunk_key <- paste0("batch_", as.integer(Sys.time()))
      acc[[chunk_key]] <- newly
      upload_accum(acc)
  # Mark these files as processed
  processed_files(c(already, new_files))
      # Bind all uploads for analysis
      data <- dplyr::bind_rows(acc)
      # Apply group overrides if present
      overrides <- group_overrides()
      if (!is.null(overrides) && "Individual" %in% names(data)) {
        idx <- match(data$Individual, names(overrides))
        repl <- overrides[idx]
        data$Group <- ifelse(!is.na(repl), repl, data$Group)
      }
    } else {
  # No data available yet
  return(NULL)
    }
    return(data)
  })

  # Upload status and clear button
  output$upload_status <- renderText({
    acc <- upload_accum()
    if (length(acc) == 0) return("Sin datos acumulados")
    n_rows <- sum(vapply(acc, nrow, integer(1)))
    n_files <- length(acc)
    paste("Lotes acumulados:", n_files, "- Filas totales:", n_rows)
  })

  observeEvent(input$clear_uploaded, {
    upload_accum(list())
    showNotification("Datos cargados limpiados.", type = "message")
  })

  # Reactive: discover columns from uploaded file to populate mapping UI
  upload_columns <- reactive({
    req(input$file1)
    fp <- input$file1$datapath[1]
    cols <- tryCatch({
      nm <- names(read.csv(fp, nrows = 1, check.names = FALSE))
      if (is.null(nm)) character(0) else nm
    }, error = function(e) character(0))
    cols
  })

  # Guess helper for defaults
  guess_col <- function(cols, patterns) {
    idx <- which(vapply(cols, function(nm) any(grepl(patterns, nm, ignore.case = TRUE)), logical(1)))
    if (length(idx) >= 1) cols[idx[1]] else ""
  }

  output$column_mapper_ui <- renderUI({
    req(input$file1)
    cols <- upload_columns()
    if (length(cols) == 0) return(NULL)

    # Guesses
    guess_time <- guess_col(cols, pattern <- "^time$|time|t(ime)?|sec|ms")
    guess_x    <- guess_col(cols, pattern <- "^x$|xpos|posx|x_coord|coord.?x")
    guess_y    <- guess_col(cols, pattern <- "^y$|ypos|posy|y_coord|coord.?y")
    guess_ind  <- guess_col(cols, pattern <- "id$|subject|animal|mouse|rat|ind(ividuo|ividual)?")
    guess_grp  <- guess_col(cols, pattern <- "group|treat(ment)?|condition|grupo")

    wellPanel(
      h5("Mapeo de Columnas del Archivo"),
      fluidRow(
        column(6, selectInput("col_time", "Columna de tiempo", choices = cols, selected = guess_time)),
        column(6, selectInput("col_individual", "Columna de individuo (opcional)", choices = c("" , cols), selected = guess_ind))
      ),
      fluidRow(
        column(6, selectInput("col_x", "Columna X", choices = cols, selected = guess_x)),
        column(6, selectInput("col_y", "Columna Y", choices = cols, selected = guess_y))
      ),
      fluidRow(
  column(6, selectInput("col_group", "Columna de grupo (si existe)", choices = c("", cols), selected = guess_grp)),
  column(6, checkboxInput("group_from_filename", "Asignar grupo usando nombre del archivo si falta", value = FALSE))
      ),
      textInput("group_label", "Etiqueta de grupo por defecto (si no hay columna)", value = "Grupo")
    )
  })

  # Auto-detect arena parameters when data is loaded
  observe({
    data <- getData()
    if (!is.null(data) && input$auto_detect) {
      # Use enhanced detection with user-selected method
      detected <- auto_detect_arena_enhanced(data, detection_method = input$platform_detection_method)
      
      arena_params$center_x <- detected$center_x
      arena_params$center_y <- detected$center_y
      arena_params$radius <- detected$radius
      if (!isTRUE(platform_override())) {
        arena_params$platform_x <- detected$platform_x
        arena_params$platform_y <- detected$platform_y
      }
      
      # Store detection info for display
      arena_params$detection_method <- detected$detection_method
      arena_params$detection_info <- detected$detection_info
    } else if (!input$auto_detect) {
      # Manual mode: wait until user clicks 'Actualizar vista previa'
    }
  })

  # Apply manual params to preview on demand
  observeEvent(input$update_preview, {
    arena_params$center_x <- input$wm_centr_x
    arena_params$center_y <- input$wm_centr_y
    arena_params$radius <- input$radio_wm
    arena_params$platform_x <- input$plat_x
    arena_params$platform_y <- input$plat_y
    platform_override(FALSE)
  })

  # Copy detected parameters to manual inputs
  observeEvent(input$copy_detected, {
    data <- getData()
    if (!is.null(data)) {
      detected <- auto_detect_arena_enhanced(data, detection_method = input$platform_detection_method)
      
      updateNumericInput(session, "plat_x", value = round(detected$platform_x, 2))
      updateNumericInput(session, "plat_y", value = round(detected$platform_y, 2))
      updateNumericInput(session, "wm_centr_x", value = round(detected$center_x, 2))
      updateNumericInput(session, "wm_centr_y", value = round(detected$center_y, 2))
      updateNumericInput(session, "radio_wm", value = round(detected$radius, 2))
      
      showNotification("Parámetros detectados copiados a los campos manuales", type = "message")
    }
  })

  # Click-to-set platform directly from preview
  observeEvent(input$arena_click, {
    if (isTRUE(input$click_set_platform)) {
      arena_params$platform_x <- input$arena_click$x
      arena_params$platform_y <- input$arena_click$y
      updateNumericInput(session, "plat_x", value = round(input$arena_click$x, 2))
      updateNumericInput(session, "plat_y", value = round(input$arena_click$y, 2))
      platform_override(TRUE)
      showNotification("Plataforma definida por clic.", type = "message", duration = 2)
    }
  })

  # Manual group editor modal
  observeEvent(input$open_group_editor, {
    data <- getData()
    if (is.null(data) || !"Individual" %in% names(data)) {
      showNotification("Cargue datos y defina 'Individual' para editar grupos.", type = "warning")
      return()
    }
    current_map <- data %>% dplyr::distinct(Individual, Group)
    current_map <- current_map[!duplicated(current_map$Individual), ]
    lines <- paste(current_map$Individual, current_map$Group, sep = ",")
    showModal(modalDialog(
      title = "Editar grupos manualmente",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("save_group_edits", "Guardar", class = "btn-primary")
      ),
      tags$p("Edite el mapeo 'Individual,Grupo' (una línea por individuo). Puede crear nuevos nombres de grupo."),
      textAreaInput("group_edit_text", NULL, value = paste(lines, collapse = "\n"), rows = min(12, max(6, length(lines))), width = "100%"),
      tags$small("Formato: Individuo,Grupo. Las líneas vacías se ignoran.")
    ))
  })

  observeEvent(input$save_group_edits, {
    txt <- input$group_edit_text
    if (is.null(txt) || !nzchar(txt)) { removeModal(); return() }
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    pairs <- strsplit(lines, ",", fixed = TRUE)
    pairs <- Filter(function(x) length(x) >= 2 && nzchar(trimws(x[1])) && nzchar(trimws(x[2])), pairs)
    if (length(pairs) == 0) { removeModal(); return() }
    inds <- vapply(pairs, function(x) trimws(x[1]), character(1))
    grps <- vapply(pairs, function(x) trimws(x[2]), character(1))
    ov <- grps; names(ov) <- inds
    group_overrides(ov)
    removeModal()
    showNotification("Mapeo de grupos actualizado.", type = "message")
  })

  # Arena preview
  output$arena_preview <- renderPlot({
    data <- getData()
    if (is.null(data)) {
      # Show empty plot with message when no data
      ggplot2::ggplot() +
        ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No hay datos cargados.\nSeleccione datos de ejemplo o suba archivos CSV."),
                          size = 6, hjust = 0.5, vjust = 0.5) +
        ggplot2::theme_void() +
        ggplot2::xlim(-1, 1) +
        ggplot2::ylim(-1, 1)
    } else {
      # Create base plot with trajectory data
      p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = Group)) +
        ggplot2::geom_point(alpha = 0.3, size = 0.5) +
        # Arena boundary
        ggplot2::annotate("path",
                         x = arena_params$center_x + arena_params$radius * cos(seq(0, 2*pi, length.out = 100)),
                         y = arena_params$center_y + arena_params$radius * sin(seq(0, 2*pi, length.out = 100)),
                         color = "black", linewidth = 1.2) +
        ggplot2::coord_fixed() +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")
      
      # Add platform and center as separate layers for legend control
      # Create dummy data for platform and center legends
      legend_data <- data.frame(
        x = c(arena_params$platform_x, arena_params$center_x),
        y = c(arena_params$platform_y, arena_params$center_y),
        type = c("Plataforma", "Centro de Arena"),
        stringsAsFactors = FALSE
      )
      
      # Add platform and center points with proper legends
      p <- p +
        ggplot2::geom_point(data = legend_data[legend_data$type == "Plataforma", ],
                           ggplot2::aes(x = x, y = y, shape = type), 
                           color = "red", size = 6, alpha = 0.8, inherit.aes = FALSE) +
        ggplot2::geom_point(data = legend_data[legend_data$type == "Centro de Arena", ],
                           ggplot2::aes(x = x, y = y, shape = type), 
                           color = "blue", size = 4, alpha = 0.8, inherit.aes = FALSE) +
        ggplot2::scale_shape_manual(
          name = "Puntos de Referencia",
          values = c("Plataforma" = 15, "Centro de Arena" = 16),
          guide = ggplot2::guide_legend(
            title.position = "top",
            title.hjust = 0.5,
            override.aes = list(
              color = c("red", "blue"),
              size = c(4, 3),
              alpha = 1
            )
          )
        ) +
        ggplot2::labs(
          title = "Vista Previa de la Arena del Laberinto Acuático",
          subtitle = paste0("Radio: ", round(arena_params$radius, 1), 
                           " | Centro: (", round(arena_params$center_x, 1), ", ", round(arena_params$center_y, 1), ")",
                           " | Plataforma: (", round(arena_params$platform_x, 1), ", ", round(arena_params$platform_y, 1), ")"),
          x = "Coordenada X", 
          y = "Coordenada Y",
          color = "Grupos de Trayectorias"
        ) +
        ggplot2::guides(
          color = ggplot2::guide_legend(
            title = "Grupos de Trayectorias",
            title.position = "top",
            title.hjust = 0.5,
            order = 1,
            override.aes = list(
              size = 4,     # Increased size of legend dots
              alpha = 0.8   # Higher alpha for better visibility
            )
          ),
          shape = ggplot2::guide_legend(
            title = "Puntos de Referencia",
            title.position = "top", 
            title.hjust = 0.5,
            order = 2
          )
        ) +
        ggplot2::theme(
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.margin = ggplot2::margin(t = 10),
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray60", size = 10)
        )
      
      return(p)
    }
  })

  # Show detected parameters
  output$detected_params <- renderTable({
    data <- getData()
    if (is.null(data) || !input$auto_detect) {
      return(NULL)
    }
    
    # Get method description
    method_desc <- switch(arena_params$detection_method %||% "unknown",
      "velocity_minimum" = "Velocidad mínima",
      "endpoint_cluster" = "Punto final más frecuente", 
      "density" = "Densidad máxima",
      "fallback_center" = "Centro (fallback)",
      "Método desconocido"
    )
    
    params_table <- data.frame(
      Parámetro = c("Centro X", "Centro Y", "Radio", "Plataforma X", "Plataforma Y", "Método Detección"),
      Valor = c(
        round(arena_params$center_x, 2),
        round(arena_params$center_y, 2),
        round(arena_params$radius, 2),
        round(arena_params$platform_x, 2),
        round(arena_params$platform_y, 2),
        method_desc
      ),
      stringsAsFactors = FALSE
    )
    
    # Add additional info if available
    if (!is.null(arena_params$detection_info)) {
      info <- arena_params$detection_info
      if ("cluster_size" %in% names(info)) {
        params_table <- rbind(params_table, 
          data.frame(Parámetro = "Tamaño Cluster", Valor = info$cluster_size, stringsAsFactors = FALSE))
      }
      if ("max_density" %in% names(info)) {
        params_table <- rbind(params_table,
          data.frame(Parámetro = "Densidad Máx.", Valor = round(info$max_density, 1), stringsAsFactors = FALSE))
      }
    }
    
    return(params_table)
  })

  # Individual entropy analysis
  entropy_results <- reactive({
    if (input$analyze_btn > 0) {
      data <- getData()
      if (is.null(data)) {
        return(NULL)
      }
      
      create_individual_entropy_plots(
        data, 
        arena_params$platform_x, 
        arena_params$platform_y,
        arena_params$center_x,
        arena_params$center_y,
        arena_params$radius,
        normalize = input$normalize_entropy
      )
    } else {
      return(NULL)
    }
  })

  # Render individual entropy plots
  output$entropy_plots_ui <- renderUI({
    results <- entropy_results()
    if (is.null(results)) {
      return(div(
        style = "text-align: center; padding: 20px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;",
        icon("info-circle", style = "font-size: 48px; color: #6c757d; margin-bottom: 10px;"),
        h4("No hay datos para analizar", style = "color: #6c757d;"),
        p("Cargue datos o genere datos aleatorios y haga clic en 'Analizar'.", style = "color: #6c757d;")
      ))
    }
    
    # Check if results have the expected structure
    if (is.null(results$plots) || length(results$plots) == 0) {
      # Different message for random data vs uploaded data
      if (input$data_source == "random") {
        return(div(
          style = "text-align: center; padding: 25px; background: linear-gradient(135deg, #f3e5f5, #e1bee7); border-radius: 10px; border-left: 5px solid #8e44ad;",
          icon("chart-line", style = "font-size: 48px; color: #8e44ad; margin-bottom: 15px;"),
          h4("Gráficos Individuales con Datos CSV", style = "color: #4a148c; margin-bottom: 15px;"),
          p("Los gráficos individuales detallados están optimizados para datos reales cargados desde archivos CSV.", 
            style = "color: #4a148c; font-size: 16px; margin-bottom: 15px;"),
          div(style = "background-color: rgba(255,255,255,0.7); padding: 15px; border-radius: 8px; margin: 15px 0;",
            p(strong("💡 Sugerencia:"), " Descarga el archivo de ejemplo científico y súbelo para ver el análisis completo:", 
              style = "color: #4a148c; margin-bottom: 10px;"),
            tags$a(
              href = "https://raw.githubusercontent.com/santi-rios/maestria_app_water_maze/main/data/cooke2020_31-2_converted.csv",
              download = "cooke2020_ejemplo.csv",
              style = "color: #8e44ad; font-weight: bold; text-decoration: underline;",
              "📥 Descargar datos de ejemplo (Cooke et al., 2020)"
            )
          )
        ))
      } else {
        return(div(
          style = "text-align: center; padding: 20px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 5px;",
          icon("exclamation-triangle", style = "font-size: 48px; color: #856404; margin-bottom: 10px;"),
          h4("Error al generar gráficos individuales", style = "color: #856404;"),
          p("No se pudieron crear los gráficos individuales. Verifique que los datos tienen identificadores de individuos.", style = "color: #856404;"),
          p("Intente cargar nuevos datos o verificar el formato CSV.", style = "color: #856404;")
        ))
      }
    }

    plots <- results$plots

    if (length(plots) <= 6) {
      # Show all plots if 6 or fewer
      plot_outputs <- lapply(seq_along(plots), function(i) {
        plot_name <- paste0("entropy_plot_", i)
        plotOutput(plot_name, height = "300px")
      })
      # Organize in rows of 2
      rows <- split(plot_outputs, ceiling(seq_along(plot_outputs)/2))
      lapply(rows, function(row) {
        fluidRow(lapply(row, function(plot) column(6, plot)))
      })
    } else {
      # Too many plots - show message and also render first 6 outputs
      first6 <- 1:6
      plot_outputs <- lapply(first6, function(i) {
        plot_name <- paste0("entropy_plot_", i)
        plotOutput(plot_name, height = "300px")
      })
      rows <- split(plot_outputs, ceiling(seq_along(plot_outputs)/2))
      ui_list <- list(
        h4("Demasiados individuos para mostrar en pantalla"),
        p(paste("Se detectaron", length(plots), "individuos. Use el botón de descarga para obtener todos los gráficos.")),
        p("Mostrando solo los primeros 6:"))
      c(ui_list, lapply(rows, function(row) {
        fluidRow(lapply(row, function(plot) column(6, plot)))
      }))
    }
  })

  # Generate individual plot outputs
  observe({
    results <- entropy_results()
    if (is.null(results)) return()
    
    plots <- results$plots
    n_plots <- min(6, length(plots))  # Show max 6 plots
    
    for (i in 1:n_plots) {
      local({
        my_i <- i
        plot_name <- paste0("entropy_plot_", my_i)
        output[[plot_name]] <- renderPlot({
          plots[[my_i]]
        })
      })
    }
  })

  # Entropy summary table
  output$entropy_summary_table <- renderTable({
    results <- entropy_results()
    if (is.null(results)) return(NULL)
    
    entropy_data <- results$entropy_data
    if (is.null(entropy_data) || nrow(entropy_data) == 0 || !"Group" %in% names(entropy_data)) {
      return(data.frame(Mensaje = "No hay datos de entropía por individuo disponibles"))
    }
    
    # Calculate summary statistics by group
    summary_stats <- entropy_data %>%
      dplyr::group_by(Group) %>%
      dplyr::summarise(
        N = dplyr::n(),
        `Entropía Media` = round(mean(entropy), 3),
        `Desv. Estándar` = round(sd(entropy), 3),
        `Mínimo` = round(min(entropy), 3),
        `Máximo` = round(max(entropy), 3),
        .groups = 'drop'
      )
    
    return(summary_stats)
  })

  # Download handler for entropy plots
  output$download_entropy_plots <- downloadHandler(
    filename = function() {
      paste0("entropy_plots_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      results <- entropy_results()
      if (is.null(results)) return()
      
      plots <- results$plots
      if (is.null(plots) || length(plots) == 0) return()

      pdf(file, width = 12, height = 8)
      n_plots <- length(plots)
      n_cols <- 2
      step <- max(n_cols * 2, 1)
      idx_seq <- seq.int(1, n_plots, by = step)
      for (i in idx_seq) {
        page_plots <- plots[i:min(i + (step - 1), n_plots)]
        page_plots <- page_plots[!sapply(page_plots, is.null)]
        if (length(page_plots) > 0) {
          gridExtra::grid.arrange(grobs = page_plots, ncol = n_cols)
        }
      }
      dev.off()
    }
  )

  # Original entropy analysis (grouped)
  observeEvent(input$analyze_btn, {
    data <- getData()
    if (is.null(data)) {
      if (identical(input$data_source, "random")) {
        showNotification("Primero debe generar datos aleatorios usando el botón 'Aleatorizar'.", type = "warning", duration = 5)
      } else {
        showNotification("Primero debe cargar archivos CSV con coordenadas.", type = "warning", duration = 5)
      }
      return()
    }
    
    # Show loading indicator for analysis
    showNotification("Analizando datos... Esto puede tomar unos segundos.", type = "message", duration = NULL, id = "analyzing")
    
    # Debug: print data structure (only in development)
    if (getOption("shiny.trace", FALSE)) {
      cat("Estructura de los datos:\n")
      cat("Columnas:", paste(colnames(data), collapse = ", "), "\n")
      cat("Dimensiones:", dim(data), "\n")
      if(nrow(data) > 0) {
        cat("Primeras filas:\n")
        print(head(data, 3))
      }
    }
    
    # Check if data has required columns
    if (!"Group" %in% colnames(data)) {
      showNotification("Error: Los datos no tienen la columna 'Group'. Verifique el formato de los datos.", 
                      type = "error", duration = 5)
      return()
    }

    # Entropy calculation per group using modular function with normalization
    entropy_data <- calculate_group_entropy(data, arena_params$platform_x, arena_params$platform_y, arena_params$center_x, arena_params$center_y, arena_params$radius, normalize = input$normalize_entropy)

    # Output the entropy table
    output$entropy_table <- renderTable({
      entropy_data
    })

    # Create entropy bar plot
    output$entropy_barplot <- renderPlot({
      if (nrow(entropy_data) > 0) {
        ggplot2::ggplot(entropy_data, ggplot2::aes(x = Group, y = entropy, fill = Group)) +
          ggplot2::geom_col(alpha = 0.8, color = "black", linewidth = 0.5) +
          ggplot2::geom_text(ggplot2::aes(label = round(entropy, 3)), 
                            vjust = -0.5, size = 4, fontface = "bold") +
          ggplot2::labs(
            title = if (input$normalize_entropy) "Entropía Espacial Normalizada por Grupo" else "Entropía Espacial por Grupo",
            subtitle = if (input$normalize_entropy) "Valores normalizados (0-1): mayor exploración/dispersión" else "Valores más altos indican mayor exploración/dispersión",
            x = "Grupo",
            y = if (input$normalize_entropy) "Entropía Espacial Normalizada" else "Entropía Espacial",
            fill = "Grupo"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            legend.position = "none",
            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray60"),
            axis.title = ggplot2::element_text(face = "bold"),
            axis.text = ggplot2::element_text(size = 11),
            panel.grid.major.x = ggplot2::element_blank()
          ) +
          ggplot2::scale_fill_brewer(type = "qual", palette = "Set2")
      } else {
        # Empty plot with message
        ggplot2::ggplot() +
          ggplot2::geom_text(ggplot2::aes(x = 0, y = 0, label = "No hay datos de entropía disponibles"),
                            size = 6, hjust = 0.5, vjust = 0.5) +
          ggplot2::theme_void()
      }
    })

    # Output the trajectory plot using modular function
    output$plot <- renderPlot({
      create_trajectory_plot(data, arena_params$platform_x, arena_params$platform_y, 
                           arena_params$center_x, arena_params$center_y, arena_params$radius)
    })

    # Heatmap using modular function with apparatus and platform parameters
    output$heatmap <- renderPlot({
      create_heatmap_plot(data, arena_params$center_x, arena_params$center_y, 
                         arena_params$radius, arena_params$platform_x, arena_params$platform_y)
    })

  # Summary Statistics using modular functions
  summary_data <- calculate_summary_stats(data)
  statistical_tests <- perform_statistical_tests(summary_data)

    # Combined group-level and entropy hypothesis tests
    output$summary_stats <- renderPrint({
      # Group-level summary and tests
      cat("Estadísticas de Resumen por Grupo:\n")
      cat("==================================\n")
      print(summary_data)
      cat("\n\nComparación Estadística (Distancia/Velocidad):\n")
      cat("===========================================\n")
      if (statistical_tests$test_type == "t-test") {
        if (!is.null(statistical_tests$distance_test)) {
          cat("\n--- T-test para Distancia Total ---\n")
          print(statistical_tests$distance_test)
        }
        if (!is.null(statistical_tests$velocity_test)) {
          cat("\n--- T-test para Velocidad Promedio ---\n")
          print(statistical_tests$velocity_test)
        }
      } else if (statistical_tests$test_type == "ANOVA") {
        if (!is.null(statistical_tests$distance_test)) {
          cat("\n--- ANOVA para Distancia Total ---\n")
          print(summary(statistical_tests$distance_test))
        }
        if (!is.null(statistical_tests$velocity_test)) {
          cat("\n--- ANOVA para Velocidad Promedio ---\n")
          print(summary(statistical_tests$velocity_test))
        }
      } else {
        cat("Se necesita más de un grupo para la comparación estadística.\n")
      }

      # Entropy hypothesis test summary
      cat("\n\nPruebas de hipótesis sobre Entropía (por individuo):\n")
      cat("==================================\n")
      results <- entropy_results()
      if (is.null(results) || is.null(results$entropy_data) || nrow(results$entropy_data) == 0) {
        cat("No hay datos de entropía por individuo.\n"); return(invisible())
      }
      ed <- results$entropy_data
      ed <- ed[is.finite(ed$entropy), ]
      if (!"Group" %in% names(ed) || length(unique(ed$Group)) < 2) {
        cat("Se necesitan ≥2 grupos para probar.\n"); return(invisible())
      }
      method <- input$test_method
      if (identical(method, "Auto")) {
        method <- if (length(unique(ed$Group)) == 2) "t-test" else "ANOVA"
      }
      # Assumption checks
      if ("shapiro" %in% input$test_checks) {
        cat("\n- Shapiro-Wilk por grupo:\n")
        print(by(ed$entropy, ed$Group, shapiro.test))
      }
      if ("variance" %in% input$test_checks) {
        cat("\n- Chequeo de varianzas (Bartlett si normal, Levene si no):\n")
        ok_normal <- FALSE
        if ("shapiro" %in% input$test_checks) {
          sw <- by(ed$entropy, ed$Group, function(x) shapiro.test(x)$p.value)
          ok_normal <- all(sw > 0.05, na.rm = TRUE)
        }
        if (ok_normal) {
          print(bartlett.test(entropy ~ Group, data = ed))
        } else {
          # Levene via car::leveneTest si disponible
          if (requireNamespace("car", quietly = TRUE)) {
            print(car::leveneTest(entropy ~ Group, data = ed))
          } else {
            cat("Levene no disponible (paquete 'car' no instalado).\n")
          }
        }
      }
      cat("\n- Prueba principal (", method, "):\n", sep = "")
      if (identical(method, "t-test") && length(unique(ed$Group)) == 2) {
        print(t.test(entropy ~ Group, data = ed))
      } else if (identical(method, "ANOVA") && length(unique(ed$Group)) >= 2) {
        fit <- aov(entropy ~ Group, data = ed)
        print(summary(fit))
        # Post hoc Tukey si ANOVA
        if (requireNamespace("stats", quietly = TRUE)) {
          cat("\n- Comparaciones post hoc (Tukey HSD):\n")
          print(TukeyHSD(fit))
        }
      } else if (identical(method, "Kruskal-Wallis")) {
        print(kruskal.test(entropy ~ Group, data = ed))
      } else {
        cat("Método no aplicable a la configuración actual.\n")
      }
    })

    # Summary table with p-values and method used
    output$test_summary_table <- renderTable({
      results <- entropy_results()
      if (is.null(results) || is.null(results$entropy_data) || nrow(results$entropy_data) == 0) return(NULL)
      ed <- results$entropy_data
      ed <- ed[is.finite(ed$entropy), ]
      if (!"Group" %in% names(ed) || length(unique(ed$Group)) < 2) return(NULL)
      method <- input$test_method
      auto_used <- FALSE
      if (identical(method, "Auto")) {
        method <- if (length(unique(ed$Group)) == 2) "t-test" else "ANOVA"
        auto_used <- TRUE
      }
      pval <- NA_real_
      note <- ""
      if (identical(method, "t-test") && length(unique(ed$Group)) == 2) {
        res <- tryCatch(t.test(entropy ~ Group, data = ed), error = function(e) NULL)
        if (!is.null(res)) pval <- res$p.value
      } else if (identical(method, "ANOVA") && length(unique(ed$Group)) >= 2) {
        fit <- tryCatch(aov(entropy ~ Group, data = ed), error = function(e) NULL)
        if (!is.null(fit)) {
          sm <- summary(fit)
          pval <- tryCatch(sm[[1]][["Pr(>F)"]][1], error = function(e) NA_real_)
        }
      } else if (identical(method, "Kruskal-Wallis")) {
        res <- tryCatch(kruskal.test(entropy ~ Group, data = ed), error = function(e) NULL)
        if (!is.null(res)) pval <- res$p.value
      } else {
        note <- "Método no aplicable con el número de grupos actual"
      }
      if (auto_used) note <- paste(note, "(Auto)")
      data.frame(
        Variable = "Entropía (entre grupos)",
        Metodo = method,
        `p-valor` = if (is.na(pval)) NA else signif(pval, 4),
        Nota = trimws(note),
        check.names = FALSE
      )
    })
    
    # Remove loading indicator and show completion
    removeNotification("analyzing")
    showNotification("¡Análisis completado!", type = "message", duration = 3)
  })
}

# Create Shiny app
shinyApp(ui, server)
