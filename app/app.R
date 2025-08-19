library(shiny)
library(janitor)
library(dplyr)
library(ggplot2)
library(bslib)
library(purrr)

# Load custom functions
source("functions.R")

# Define UI for app
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel("Análisis de Laberinto Acuático de Morris"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Subir coordenadas", accept = ".csv", multiple = TRUE),
      checkboxInput("use_sample_data", "Usar datos de ejemplo (2 individuos)", value = FALSE),
      checkboxInput("use_multi_sample_data", "Usar datos de ejemplo (3 individuos)", value = FALSE),
      tags$hr(),
      numericInput("plat_x", "Coordenadas de plataforma en X", value = 117.8),
      numericInput("plat_y", "Coordenadas de plataforma en Y", value = 38.4),
      numericInput("wm_centr_x", "Centro en X del aparato", value = 90.13),
      numericInput("wm_centr_y", "Centro en Y del aparato", value = 61.3),
      numericInput("radio_wm", "Radio del aparato", value = 65),
      tags$hr(),
      radioButtons("heatmap_style", "Estilo de mapa de calor:",
                  choices = list("Estándar" = "standard", 
                               "Estilo Rtrack (Suave)" = "rtrack"),
                  selected = "standard"),
      tags$hr(),
      actionButton("analyze_btn", "Analizar")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Entropía", 
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
    } else {
      data <- load_and_process_data(
        file_paths = input$file1$datapath,
        use_sample = FALSE,
        file_names = input$file1$name
      )
    }
    return(data)
  })

  observeEvent(input$analyze_btn, {
    data <- getData()
    
    # Use input values for platform and center coordinates
    plat_x <- input$plat_x
    plat_y <- input$plat_y
    wm_centr_x <- input$wm_centr_x
    wm_centr_y <- input$wm_centr_y
    radio_wm <- input$radio_wm

    # Entropy calculation per group using modular function
    entropy_data <- calculate_group_entropy(data, plat_x, plat_y)

    # Output the entropy table
    output$entropy_table <- renderTable({
      entropy_data
    })

    # Output the trajectory plot using modular function
    output$plot <- renderPlot({
      create_trajectory_plot(data, plat_x, plat_y, wm_centr_x, wm_centr_y, radio_wm)
    })

    # Heatmap using modular function with apparatus and platform parameters
    output$heatmap <- renderPlot({
      if (input$heatmap_style == "rtrack") {
        create_heatmap_rtrack_style(data, wm_centr_x, wm_centr_y, radio_wm, plat_x, plat_y)
      } else {
        create_heatmap_plot(data, wm_centr_x, wm_centr_y, radio_wm, plat_x, plat_y)
      }
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
