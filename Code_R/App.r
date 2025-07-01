library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

# UI
ui <- fluidPage(
  titlePanel("Visualización de Datos de Salud ENSANUT"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("region", "Región:", 
                  choices = c("Todas", unique(datos_exportar$region))),
      selectInput("estrato", "Estrato:", 
                  choices = c("Todos", unique(datos_exportar$Estrato))),
      selectInput("sexo", "Sexo:", 
                  choices = c("Ambos", unique(datos_exportar$sexo))),
      sliderInput("edad", "Rango de edad:",
                  min = min(datos_exportar$edad, na.rm = TRUE),
                  max = max(datos_exportar$edad, na.rm = TRUE),
                  value = c(20, 60)),
      selectInput("ingreso", "Nivel de ingreso:",
                  choices = c("Todos", unique(datos_exportar$IngresoMedio))),
      selectInput("diagnostico", "Seleccionar diagnóstico:",
                  choices = c("Diabetes" = "Diagnostico_Diabetes",
                              "Hipertensión" = "Diagnostico_Hipertension",
                              "Colesterol" = "Diagnostico_Colesterol",
                              "Triglicéridos" = "Diagnostico_Trigliceridos",
                              "Enfermedad Renal" = "Diagnostico_Enfermedad_Renal",
                              "Cálculos Renales" = "Diagnostico_Calculos_Renales"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen", 
                 plotlyOutput("edad_ingreso_plot"),
                 plotlyOutput("condiciones_plot")),
        tabPanel("Distribución por Diagnóstico", 
                 plotlyOutput("diagnostico_edad_plot"),
                 plotlyOutput("diagnostico_ingreso_plot")),
        tabPanel("Datos", 
                 DTOutput("datos_tabla"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Filtrar datos según selección del usuario
  datos_filtrados <- reactive({
    data <- datos_exportar
    
    if (input$region != "Todas") {
      data <- data %>% filter(region == input$region)
    }
    
    if (input$estrato != "Todos") {
      data <- data %>% filter(Estrato == input$estrato)
    }
    
    if (input$sexo != "Ambos") {
      data <- data %>% filter(sexo == input$sexo)
    }
    
    if (!is.null(input$edad)) {
      data <- data %>% filter(edad >= input$edad[1], edad <= input$edad[2])
    }
    
    if (input$ingreso != "Todos") {
      data <- data %>% filter(IngresoMedio == input$ingreso)
    }
    
    data
  })
  
  # Gráfico de relación edad-ingreso
  output$edad_ingreso_plot <- renderPlotly({
    datos <- datos_filtrados()
    
    ggplotly(
      ggplot(datos, aes(x = edad, fill = IngresoMedio)) +
        geom_density(alpha = 0.5) +
        labs(title = "Distribución de Edad por Nivel de Ingreso",
             x = "Edad", y = "Densidad") +
        theme_minimal()
    )
  })
  
  # Gráfico de condiciones de salud
  output$condiciones_plot <- renderPlotly({
    datos <- datos_filtrados()
    
    # Resumir datos para el gráfico
    resumen <- datos %>%
      summarise(
        Diabetes = sum(grepl("con diagnostico", Diagnostico_Diabetes)),
        Hipertension = sum(grepl("con diagnostico", Diagnostico_Hipertension)),
        Colesterol = sum(grepl("y diagnosticado", Diagnostico_Colesterol)),
        Trigliceridos = sum(grepl("con diagnostico", Diagnostico_Trigliceridos)),
        Enfermedad_Renal = sum(grepl("y diagnosticado", Diagnostico_Enfermedad_Renal)),
        Calculos_Renales = sum(grepl("diagnosticado con", Diagnostico_Calculos_Renales))
      ) %>%
      tidyr::pivot_longer(everything(), names_to = "Condicion", values_to = "Conteo")
    
    ggplotly(
      ggplot(resumen, aes(x = reorder(Condicion, -Conteo), y = Conteo, fill = Condicion)) +
        geom_bar(stat = "identity") +
        labs(title = "Prevalencia de Condiciones de Salud Diagnosticadas",
             x = "Condición", y = "Número de Casos") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  # Gráfico de diagnóstico seleccionado por edad
  output$diagnostico_edad_plot <- renderPlotly({
    datos <- datos_filtrados()
    
    ggplotly(
      ggplot(datos, aes(x = edad, fill = .data[[input$diagnostico]])) +
        geom_density(alpha = 0.5) +
        labs(title = paste("Distribución por Edad -", input$diagnostico),
             x = "Edad", y = "Densidad") +
        theme_minimal()
    )
  })
  
  # Gráfico de diagnóstico seleccionado por ingreso
  output$diagnostico_ingreso_plot <- renderPlotly({
    datos <- datos_filtrados()
    
    ggplotly(
      ggplot(datos, aes(x = IngresoMedio, fill = .data[[input$diagnostico]])) +
        geom_bar(position = "fill") +
        labs(title = paste("Distribución por Ingreso -", input$diagnostico),
             x = "Nivel de Ingreso", y = "Proporción") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  # Tabla de datos
  output$datos_tabla <- renderDT({
    datos <- datos_filtrados() %>%
      select(FOLIO_INT, edad, sexo, region, Estrato, IngresoMedio, 
             starts_with("Diagnostico_"))
    
    datatable(datos, 
              options = list(scrollX = TRUE, pageLength = 10),
              rownames = FALSE)
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)