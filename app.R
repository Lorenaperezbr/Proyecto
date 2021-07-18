library(shiny)
library(tidyverse)
library(DT)
library(stringr)


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
                                      c("cat_hora", "cat_fecha", "dia_semana")
                          )
                          ),
                   column(6,
                          conditionalPanel(
                            condition = "input.x_tile == 'cat_hora'",
                            selectInput("y_tile",
                                      "Contra:",
                                      c("cat_fecha", "dia_semana"))),
                          
                          conditionalPanel(
                            condition = "input.x_tile == 'cat_fecha'",
                            selectInput("y_tile",
                                        "Contra:",
                                        c("cat_hora", "dia_semana"))),
                          
                          conditionalPanel(
                            condition = "input.x_tile == 'dia_semana'",
                            selectInput("y_tile",
                                        "Contra:",
                                        c("cat_hora", "cat_fecha")))
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
# 

  
  datos <- reactive({
 
    select_CCZ <- if (is.null(input$CCZ)) { unique(base$CCZ)} else {as.list(input$CCZ)}
    select_cat_hora <- if (is.null(input$cat_hora)) {unique(base$cat_hora)} else {as.list(input$cat_hora)}
    select_cat_fecha <- if (is.null(input$cat_fecha)) {unique(base$cat_fecha)} else {as.list(input$cat_fecha)}
    
    
    as.data.frame(select (base, -c(AREA, PERIMETER, geometry))) %>% 
    filter(CCZ %in% select_CCZ,
           cat_hora %in% select_cat_hora,
           cat_fecha %in% select_cat_fecha,
           fecha>=input$fecha[1] & fecha <=input$fecha[2]
           )
    
  })
  
  
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
    ggplot(datos(), aes(x = .data[[input$x_tile]],
                     y = .data[[input$y_tile]],
                     fill = velocidad_promedio)) +
      geom_tile() +
      labs(fill = "Velocidad Promedio") +
      
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, vjust = 0.5))+
      scale_fill_gradient(low="#cf5d6b",high = "#54020c")
  )
  
  grafico2 <- reactive(
    ggplot(datos(), aes(x = .data[[input$x_tile]],
                     y = .data[[input$y_tile]],
                     fill = volume)) +
      geom_tile() +
      labs(x = .data[[input$x_tile]],
           y = .data[[input$y_tile]],
           fill = "Cantidad") +
      
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, vjust = 0.5))+
      scale_fill_gradient(low="#61c968",high = "#023606")
  )
  
  grafico3 <- reactive({
    datos() %>%
      mutate(CCZ = fct_reorder(CCZ,-volume, .fun = "sum")) %>%
      ggplot(aes(x = CCZ,
                 y = volume,
                 fill = cat_fecha)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = scales::comma, expand = c(0, 0)) + theme(legend.position = c(.78, .85),panel.background = element_blank(),axis.line = element_line(size = 0.5, colour = "black")) +
      labs(x = "Centros Comunales Zonales", y = "Cantidad de vehículos detectados", fill =
             "Momento del mes") +
      scale_fill_brewer(palette = "Dark2")
  })
 
  grafico4 <- reactive({
    datos() %>%
      group_by(CCZ) %>%
      summarise(Máximo = max(velocidad_promedio)) %>%
      mutate(CCZ = fct_reorder(CCZ, -Máximo, .fun = "mean")) %>%
      ggplot(aes(x = CCZ,
                 y = Máximo,)) +
      geom_bar(stat = "identity", fill = "#941020") +
      scale_y_continuous(expand = c(0, 0)) + theme(
        legend.position = "bottom",
        panel.background = element_blank(),
        axis.line = element_line(size = 0.5, colour = "black")
      ) +
      labs(x = "Centros Comunales Zonales", y = "Máxima Velocidad promedio (km/h)", fill =
             "Momento del mes")
  })
  
  # OUTPUTS 
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
  
  output$bar_1 <- renderPlot({
    plot(grafico3())
  })
  
  output$bar_2 <- renderPlot({
    plot(grafico4())
  })
# Pestaña Resumen:
  # Gráfico de barras con datos de conteo.
  # Gráfico de barras con datos de velocidad.
  # Tabla de estadísticas desciptivas con group_by(variable elegida).
  # Min, max, promedio diario, sd.
  
  # Opciones: elegir variable en eje x y filtrar info.

 
}

shinyApp(ui, server)
  

