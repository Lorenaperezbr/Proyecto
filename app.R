library(shiny)
library(tidyverse)
library(DT)


base <- readRDS("data/processed/base_app.rds")

ui <- fluidPage(
  titlePanel("Datos de Propina"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("fecha",
                     "Selecciona un rango de fechas",
                     format='yyyy-mm-dd',
                     separator = " hasta ",
                     language="es",
                     min="2021-02-01",
                     max="2021-03-31",
                     start = "2021-02-01",
                     end = "2021-03-31"
                    ),
      selectInput('CCZ', 
                    'Selecciona un Centro Comunal Zonal',
                    c(1:17),
                  multiple = T
      ),
      selectInput('cat_hora', 
                    'Selecciona un rango horario',
                    c("Madrugada", "Mañana", "Media tarde", "Medio dia", "Noche"),
                  multiple = T
      ),
      selectInput('cat_fecha', 
                    'Selecciona un momento del mes',
                    c("Principio de mes", "Mitad de mes", "Fin de mes"),
                  multiple = T
      ),
      actionButton("runif", "Mostrar")
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Mapa",
          
          h2("Algun titulo interesante", align = "center"),
          plotOutput("map")
          ),
        tabPanel("Gráfico de Mosaico",
                 h2("Algun titulo interesante", align = "center"),
                 fluidRow(
                   column(6,
                          selectInput("x_tile",
                                      "Graficar:",
                                      c("cat_hora", "Momento del mes", "Día de la semana")
                          )
                          ),
                   column(6,
                          conditionalPanel(
                            condition = "input.x_tile == 'cat_hora'",
                            selectInput("y_tile",
                                      "Contra:",
                                      c("cat_fecha", "Día de la semana"))),
                          
                          conditionalPanel(
                            condition = "input.x_tile == 'Momento del mes'",
                            selectInput("y_tile",
                                        "Contra:",
                                        c("Rango horario", "Día de la semana"))),
                          
                          conditionalPanel(
                            condition = "input.x_tile == 'Día de la semana'",
                            selectInput("y_tile",
                                        "Contra:",
                                        c("Rango horario", "Momento del mes")))
                   )
                   
                   # Cambios pendientes:
                   # - En el input condicional ver cómo hacer para que los nombres de las variables aparezcan con etiquetas.
                   # - Alternativamente, ver cómo hacer para que el gráfico funcione pasándole etiquetas en vez del nombre de la variable desde el input.
                 ),
                 fluidRow(
                   column(6,plotOutput("tile_1")),
                   column(6,plotOutput("tile_2"))
                 )),
        tabPanel("Resumen",
                 h2("Algun titulo interesante", align = "center"),
                 fluidRow(
                   column(6,plotOutput("bar_1")),
                   column(6,plotOutput("bar_2"))),
                 dataTableOutput("tab")
        )
))
))

server <- function(input, output){
  
  
  
# Pestaña Mapa: gráfico de Mdeo.
  # Opciones: 
  # Pintar municipios en escala de color según conteo o velocidad.
  # Tamaño de los puntos en relación a conteo o velocidad registrada.
  # 

# Pestaña Gráfico de Mosaico:
  # Gráfico con datos de conteo.
  # Gráfico con datos de velocidad.
  
  # Opciones: elegir variables en eje x y y, filtrar info.

  
  var_x <- reactive(
    if (input$x_tile == "Día de la semana") {
      var_x <- "dia_semana" 
    } else if (input$x_tile == "Momento del mes") {
      var_x <- "cat_fecha"
    } else {
      var_x <- "cat_hora"
    }
  )
  
  var_y <- reactive(
    if (input$y_tile == "Día de la semana") {
      var_y <- "dia_semana" 
    } else if (input$y_tile == "Momento del mes") {
      var_y <- "cat_fecha"
    } else {
      var_y <- "cat_hora"
    }
  )
  
  grafico <- reactive(
    ggplot(base, aes(x = .data[[input$x_tile]],
                     y = .data[[input$y_tile]],
                     fill = velocidad_promedio)) +
      geom_tile() +
      labs(x = .data[[input$x_tile]],
           y = .data[[input$y_tile]],
           fill = "Velocidad Promedio") +
      
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, vjust = 0.5))
  )
  
  grafico2 <- reactive(
    ggplot(base, aes(x = .data[[input$x_tile]],
                     y = .data[[input$y_tile]],
                     fill = volume)) +
      geom_tile() +
      labs(x = .data[[input$x_tile]],
           y = .data[[input$y_tile]],
           fill = "Cantidad") +
      
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, vjust = 0.5))
  )
  
  output$tile_1 <- renderPlot({
    plot(grafico())
  })
  
  output$tile_2 <- renderPlot({
    plot(grafico2())
  })
  
  # Cambios pendientes:
  # - Colores de la escala, que el oscuro sea el mayor.
  # - Orden de las etiquetas en los ejes.
  # - Revisar si el color azul es el más adecuado.
  # - Dejar el gráfico reactivo a los filtros que elegimos en los input.
  
  
  
  
# Pestaña Resumen:
  # Gráfico de barras con datos de conteo.
  # Gráfico de barras con datos de velocidad.
  # Tabla de estadísticas desciptivas con group_by(variable elegida).
  # Min, max, promedio diario, sd.
  
  # Opciones: elegir variable en eje x y filtrar info.

 
}

shinyApp(ui, server)
  

