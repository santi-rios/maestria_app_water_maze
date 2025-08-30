library(shiny)
library(janitor)
library(dplyr)
library(ggplot2)
library(bslib)
library(purrr)
library(gridExtra)

# Load custom functions
source("functions.R")

# Define UI for app
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel("Análisis de Laberinto Acuático de Morris"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Subir coordenadas", accept = ".csv", multiple = TRUE),
      checkboxInput("use_sample_data", "Usar datos de ejemplo (2 individuos)", value = TRUE),
      checkboxInput("use_multi_sample_data", "Usar datos de ejemplo (3 individuos)", value = FALSE),
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
        tags$small("Tip: Use la vista previa para verificar que los parámetros son correctos"),
        tags$br(),
        actionButton("copy_detected", "Copiar parámetros detectados", class = "btn-sm btn-outline-secondary")
      ),
      conditionalPanel(
        condition = "input.auto_detect",
        tags$small("Los parámetros se detectan automáticamente basándose en los datos cargados."),
        tags$br(),
        tags$small("Desactive la detección automática para ajustar manualmente.")
      ),
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
                 plotOutput("arena_preview"),
                 conditionalPanel(
                   condition = "input.auto_detect",
                   h4("Parámetros Detectados Automáticamente:"),
                   tableOutput("detected_params")
                 )
        ),
        tabPanel("Entropía Individual", 
                 h3("Análisis de Entropía por Individuo"),
                 p("Cada gráfico muestra la trayectoria individual, la elipse de covarianza (azul) y el valor de entropía calculado."),
                 uiOutput("entropy_plots_ui"),
                 h4("Resumen de Entropía por Grupo"),
                 tableOutput("entropy_summary_table")
        ),
        tabPanel("Entropía Agrupada", 
                 plotOutput("plot"),
                 tableOutput("entropy_table")
        ),
        tabPanel("Mapa de Calor", 
                 plotOutput("heatmap")
        ),
        tabPanel("Estadísticas de Resumen", 
                 verbatimTextOutput("summary_stats")
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

  getData <- reactive({
    if (input$use_sample_data) {
      data <- load_and_process_data(use_sample = TRUE)
    } else if (input$use_multi_sample_data) {
      # Load the new multi-subject sample data
      files <- c("sample_data_control_multi.csv", "sample_data_treatment_multi.csv")
      data_list <- lapply(files, read.csv)
      names(data_list) <- c("Control", "Treatment")
      data <- dplyr::bind_rows(data_list, .id = "Group")
      data <- janitor::clean_names(data)
      if ("treatment" %in% names(data)) {
        data$Group <- data$treatment
      }
      data$time <- as.numeric(as.character(data$time))
      data$x <- as.numeric(as.character(data$x))
      data$y <- as.numeric(as.character(data$y))
      data <- data %>% dplyr::filter(!is.na(time), !is.na(x), !is.na(y))
    } else if (!is.null(input$file1$datapath)) {
      data <- load_and_process_data(
        file_paths = input$file1$datapath,
        use_sample = FALSE
      )
    } else {
      # No data available
      return(NULL)
    }
    return(data)
  })

  # Auto-detect arena parameters when data is loaded
  observe({
    data <- getData()
    if (!is.null(data) && input$auto_detect) {
      detected <- auto_detect_arena(data)
      
      arena_params$center_x <- detected$center_x
      arena_params$center_y <- detected$center_y
      arena_params$radius <- detected$radius
      arena_params$platform_x <- detected$platform_x
      arena_params$platform_y <- detected$platform_y
    } else if (!input$auto_detect) {
      arena_params$center_x <- input$wm_centr_x
      arena_params$center_y <- input$wm_centr_y
      arena_params$radius <- input$radio_wm
      arena_params$platform_x <- input$plat_x
      arena_params$platform_y <- input$plat_y
    }
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
      return(p("No hay datos para analizar. Cargue datos y haga clic en 'Analizar'."))
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
        fluidRow(
          lapply(row, function(plot) {
            column(6, plot)
          })
        )
      })
    } else {
      # Too many plots - show message and download option
      list(
        h4("Demasiados individuos para mostrar en pantalla"),
        p(paste("Se detectaron", length(plots), "individuos. Use el botón de descarga para obtener todos los gráficos.")),
        p("Mostrando solo los primeros 6:")
      )
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
    
    # Calculate summary statistics by group
    summary_stats <- entropy_data %>%
      dplyr::group_by(Group) %>%
      dplyr::summarise(
        N = dplyr::n(),
        `Entropía Media` = round(mean(Entropy), 3),
        `Desv. Estándar` = round(sd(Entropy), 3),
        `Mínimo` = round(min(Entropy), 3),
        `Máximo` = round(max(Entropy), 3),
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
      
      # Create PDF with all plots
      pdf(file, width = 12, height = 8)
      
      # Arrange plots in grid
      n_plots <- length(plots)
      n_cols <- 2
      
      for (i in seq(1, n_plots, by = n_cols * 2)) {
        # Create page with up to 4 plots
        page_plots <- plots[i:min(i + 3, n_plots)]
        page_plots <- page_plots[!sapply(page_plots, is.null)]
        
        if (length(page_plots) > 0) {
          gridExtra::grid.arrange(grobs = page_plots, ncol = 2)
        }
      }
      
      dev.off()
    }
  )

  # Original entropy analysis (grouped)
  observeEvent(input$analyze_btn, {
    data <- getData()

    # Entropy calculation per group using modular function
    entropy_data <- calculate_group_entropy(data, arena_params$platform_x, arena_params$platform_y)

    # Output the entropy table
    output$entropy_table <- renderTable({
      entropy_data
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

    output$summary_stats <- renderPrint({
      cat("Estadísticas de Resumen por Grupo:\n")
      cat("==================================\n")
      print(summary_data)
      cat("\n\nComparación Estadística:\n")
      cat("========================\n")
      
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
        cat("Se necesita más de un grupo para la comparación estadística.")
      }
    })
  })
}

# Create Shiny app
shinyApp(ui, server)
