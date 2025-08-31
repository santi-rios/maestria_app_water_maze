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
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel(
    div(
      h1("Análisis de Laberinto Acuático de Morris"),
      h4("Aplicación para Análisis de Entropía Espacial", style = "color: #6c757d; margin-top: -10px;"),
      p("Por Santiago Ríos - Maestría en Neurociencias", style = "color: #6c757d; font-size: 14px; margin-top: -5px;")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Fuente de datos"),
      radioButtons(
        "data_source", "Seleccione una fuente:", inline = TRUE,
        choices = c("Subir archivos CSV" = "upload", "Generar datos aleatorios" = "random"),
        selected = "upload"
      ),
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file1", "Subir coordenadas", accept = ".csv", multiple = TRUE),
        # Dynamic UI to map columns when a file is uploaded
        uiOutput("column_mapper_ui"),
        fluidRow(
          column(6, actionButton("open_group_editor", "Editar grupos (opcional)", class = "btn-sm btn-outline-secondary")),
          column(6, actionButton("clear_uploaded", "Limpiar datos cargados", class = "btn-sm btn-danger"))
        ),
        tags$small(textOutput("upload_status")),
        tags$hr(),
        tags$div(
          h5("Cómo cargar y comparar grupos"),
          tags$ul(
            tags$li("Suba una tanda de archivos (p. ej., carpeta 'Fluoxetina') y mapee columnas."),
            tags$li("Luego suba otra tanda (p. ej., 'Ketamina'). La app acumula las tandas en esta sesión."),
            tags$li("Use 'Editar grupos' para asignar/corregir el Grupo por Individuo si hace falta."),
            tags$li("'Limpiar datos cargados' reinicia la acumulación (útil para empezar un nuevo análisis)."),
            tags$li("El botón 'Analizar' usa todos los datos acumulados hasta el momento.")
          )
        )
      ),
      conditionalPanel(
        condition = "input.data_source == 'random'",
        h5("Generación de Datos Aleatorios"),
        p("Genere datos simulados con diferentes comportamientos de aprendizaje"),
        fluidRow(
          column(8, 
                 numericInput("n_subjects_random", "Sujetos por grupo:", value = 8, min = 1, max = 15, step = 1)
          ),
          column(4,
                 br(),
                 actionButton("randomize_data", "Aleatorizar", icon = icon("dice"), 
                            class = "btn-warning btn-sm", style = "margin-top: 5px;")
          )
        ),
        tags$small("Cada clic genera nuevos datos con comportamientos distintos")
      ),
      tags$hr(),
      h4("Configuración de Arena"),
      checkboxInput("auto_detect", "Detectar automáticamente dimensiones", value = TRUE),
      conditionalPanel(
        condition = "!input.auto_detect",
        h5("Parámetros Manuales:"),
        numericInput("plat_x", "Coordenadas de plataforma en X", value = 117.8, step = 0.1),
        numericInput("plat_y", "Coordenadas de plataforma en Y", value = 38.4, step = 0.1),
        numericInput("wm_centr_x", "Centro en X del aparato", value = 90.13, step = 0.1),
        numericInput("wm_centr_y", "Centro en Y del aparato", value = 61.3, step = 0.1),
        numericInput("radio_wm", "Radio del aparato", value = 65, step = 0.5),
        tags$small("Los cambios manuales se aplican al presionar 'Actualizar vista previa'."),
        tags$br(),
        fluidRow(
          column(6, actionButton("copy_detected", "Copiar parámetros detectados", class = "btn-sm btn-outline-secondary")),
          column(6, actionButton("update_preview", "Actualizar vista previa", class = "btn-sm btn-primary"))
        )
      ),
      conditionalPanel(
        condition = "input.auto_detect",
        tags$small("Los parámetros se detectan automáticamente basándose en los datos cargados."),
        tags$br(),
        tags$small("Desactive la detección automática para ajustar manualmente.")
      ),
      checkboxInput("click_set_platform", "Definir plataforma con clic en la vista previa", value = FALSE),
      tags$hr(),
      actionButton("analyze_btn", "Analizar"),
      tags$hr(),
      conditionalPanel(
        condition = "input.analyze_btn > 0",
        downloadButton("download_entropy_plots", "Descargar Gráficos de Entropía", 
                      class = "btn-primary btn-sm")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Configuración de Arena", 
                 h3("Vista Previa de la Arena"),
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
                   h4("1. Centro de la Arena"),
                   p("El centro se calcula como el punto medio de los rangos de coordenadas:"),
                   tags$code("center_x = (mínimo_x + máximo_x) / 2"),
                   tags$br(),
                   tags$code("center_y = (mínimo_y + máximo_y) / 2"),
                   p("Este método asume que los datos están distribuidos simétricamente alrededor del centro de la arena."),
                   
                   h4("2. Radio de la Arena"),
                   p("El radio se estima utilizando el percentil 95 de las distancias desde el centro:"),
                   tags$code("distancias = √((x - center_x)² + (y - center_y)²)"),
                   tags$br(),
                   tags$code("radio = percentil_95(distancias)"),
                   p("El uso del percentil 95 permite evitar outliers que podrían estar fuera de la arena."),
                   
                   h4("3. Detección de Plataforma"),
                   p("La plataforma se detecta mediante un algoritmo de análisis de movimiento:"),
                   tags$ol(
                     tags$li("Se divide el espacio en una grilla de 20×20 celdas"),
                     tags$li("Para cada celda se calcula:"),
                     tags$ul(
                       tags$li("Velocidad promedio: media de √(diff(x)² + diff(y)²)"),
                       tags$li("Número de puntos en la celda")
                     ),
                     tags$li("La plataforma se identifica como la celda con menor velocidad promedio y al menos 5 puntos")
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
                     tags$li(strong("Componente direccional (det(Σ))"), ": proviene de la variabilidad y correlación de los desplazamientos (eigenvalores de Σ). Capta si la búsqueda es alargada en una dirección o amplia en todas."),
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
                   )
                 )
        ),
        tabPanel("Entropía Individual", 
                 h3("Análisis de Entropía por Individuo"),
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
        tabPanel("Entropía Agrupada", 
                 h3("Trayectorias y Análisis de Entropía por Grupo"),
                 fluidRow(
                   column(6, 
                          h4("Trayectorias por Grupo"),
                          plotOutput("plot")
                   ),
                   column(6,
                          h4("Distribución de Entropía"),
                          plotOutput("entropy_barplot")
                   )
                 ),
                 h4("Valores de Entropía por Grupo"),
                 tableOutput("entropy_table")
        ),
        tabPanel("Mapa de Calor", 
                 plotOutput("heatmap")
        ),
  tabPanel("Estadísticas de Resumen", 
                 wellPanel(
                   fluidRow(
                     column(4, selectInput("test_variable", "Variable a probar", choices = c("Entropía (por individuo)"), selected = "Entropía (por individuo)")),
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
                   h4("Título"),
                   p(strong("Análisis de Laberinto Acuático de Morris: Aplicación Shiny para Análisis de Entropía Espacial")),
                   
                   h4("Autor"),
                   p(strong("Santiago Ríos"), tags$br(),
                     "Estudiante de Maestría en Neurociencias", tags$br(),
                     "Universidad [Nombre de Universidad]", tags$br(),
                     "Email: santiago.rios@[universidad].edu"),
                   
                   h4("Cómo Citar"),
                   tags$div(style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;",
                     p(strong("Formato APA:")), 
                     p("Ríos, S. (2025). Análisis de Laberinto Acuático de Morris: Aplicación Shiny para Análisis de Entropía Espacial [Software]. Universidad [Nombre]. https://github.com/santi-rios/maestria_app_water_maze")
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
    showNotification("Generando datos aleatorios...", type = "message", duration = 2)
    
    # Generate new random trajectories
    new_data <- generate_group_trajectories(
      n_subjects_per_group = input$n_subjects_random,
      groups = c("Control", "Tratamiento"),
      n_points = sample(80:150, 1),  # Random trajectory length
      max_time = sample(30:60, 1)    # Random trial duration
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
        Group = Treatment  # Add Group column from Treatment
      )
    
    random_data(formatted_data)
    
    showNotification(paste("¡Datos aleatorios generados!", input$n_subjects_random, "sujetos por grupo"), 
                    type = "message", duration = 3)
  })

  getData <- reactive({
    # Check if random data is available and should be used
    if (!is.null(random_data()) && is.null(input$file1$datapath)) {
      return(random_data())
    } else if (!is.null(input$file1$datapath)) {
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
      detected <- auto_detect_arena(data)
      
      arena_params$center_x <- detected$center_x
      arena_params$center_y <- detected$center_y
      arena_params$radius <- detected$radius
      if (!isTRUE(platform_override())) {
        arena_params$platform_x <- detected$platform_x
        arena_params$platform_y <- detected$platform_y
      }
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
      detected <- auto_detect_arena(data)
      
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
      p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = Group)) +
        ggplot2::geom_point(alpha = 0.3, size = 0.5) +
        # Arena boundary
        ggplot2::annotate("path",
                         x = arena_params$center_x + arena_params$radius * cos(seq(0, 2*pi, length.out = 100)),
                         y = arena_params$center_y + arena_params$radius * sin(seq(0, 2*pi, length.out = 100)),
                         color = "black", linewidth = 1.2) +
        # Platform
        ggplot2::geom_point(x = arena_params$platform_x, y = arena_params$platform_y,
                           color = "red", size = 6, alpha = 0.8) +
        # Center point
        ggplot2::geom_point(x = arena_params$center_x, y = arena_params$center_y,
                           color = "blue", size = 4, alpha = 0.8) +
        ggplot2::labs(title = "Vista Previa de la Arena",
                     subtitle = "Puntos rojos: plataforma, Puntos azules: centro, Círculo negro: límite de arena",
                     x = "Coordenada X", y = "Coordenada Y") +
        ggplot2::theme_minimal() +
        ggplot2::coord_fixed() +
        ggplot2::theme(legend.position = "bottom")
      
      return(p)
    }
  })

  # Show detected parameters
  output$detected_params <- renderTable({
    data <- getData()
    if (is.null(data) || !input$auto_detect) {
      return(NULL)
    }
    
    data.frame(
      Parámetro = c("Centro X", "Centro Y", "Radio", "Plataforma X", "Plataforma Y"),
      Valor = c(
        round(arena_params$center_x, 2),
        round(arena_params$center_y, 2),
        round(arena_params$radius, 2),
        round(arena_params$platform_x, 2),
        round(arena_params$platform_y, 2)
      )
    )
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
        arena_params$radius
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
      return(div(
        style = "text-align: center; padding: 20px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 5px;",
        icon("exclamation-triangle", style = "font-size: 48px; color: #856404; margin-bottom: 10px;"),
        h4("Error al generar gráficos individuales", style = "color: #856404;"),
        p("No se pudieron crear los gráficos individuales. Verifique que los datos tienen identificadores de individuos.", style = "color: #856404;"),
        p("Intente cargar nuevos datos o generar datos aleatorios.", style = "color: #856404;")
      ))
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
      showNotification("No hay datos para analizar. Cargue archivos o genere datos aleatorios.", type = "warning", duration = 4)
      return()
    }
    
    # Debug: print data structure
    cat("Estructura de los datos:\n")
    cat("Columnas:", paste(colnames(data), collapse = ", "), "\n")
    cat("Dimensiones:", dim(data), "\n")
    if(nrow(data) > 0) {
      cat("Primeras filas:\n")
      print(head(data, 3))
    }
    
    # Check if data has required columns
    if (!"Group" %in% colnames(data)) {
      showNotification("Error: Los datos no tienen la columna 'Group'. Verifique el formato de los datos.", 
                      type = "error", duration = 5)
      return()
    }

    # Entropy calculation per group using modular function
    entropy_data <- calculate_group_entropy(data, arena_params$platform_x, arena_params$platform_y)

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
            title = "Entropía Espacial por Grupo",
            subtitle = "Valores más altos indican mayor exploración/dispersión",
            x = "Grupo",
            y = "Entropía Espacial",
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
        Variable = "Entropía (por individuo)",
        Metodo = method,
        `p-valor` = if (is.na(pval)) NA else signif(pval, 4),
        Nota = trimws(note),
        check.names = FALSE
      )
    })
  })
}

# Create Shiny app
shinyApp(ui, server)
