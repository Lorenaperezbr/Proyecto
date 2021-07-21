library(shiny)
library(tidyverse)
library(DT)
library(stringr)
library(sf)
library(ggpmisc)

base <- readRDS("./data/processed/base_app.rds")
municipios <- st_read("./data/geo/mdeo_barrios")
st_crs(x = municipios) <- 5382
municipios <- st_transform(municipios, "+init=epsg:4326")

resumenes <- base %>% 
    group_by(CCZ) %>% 
    summarise(res_volume = sum(volume),
              res_velocidad_promedio = max(velocidad_promedio))


st_crs(resumenes) <- st_crs(municipios)
municipios <- st_join(municipios, resumenes, left = T)


ui <- fluidPage(
  titlePanel("Estudio de circulación vehicular en Montevideo: febrero-marzo 2021"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("fecha",
                     "Selecciona un rango de fechas:",
                     format = 'yyyy-mm-dd',
                     separator = " hasta ",
                     language = "es",
                     min = "2021-02-01",
                     max = "2021-03-31",
                     start = "2021-02-01",
                     end = "2021-03-31"
                    ),
      selectInput('CCZ', 
                  'Selecciona un Centro Comunal Zonal:',
                  c(1:17),
                  multiple = T
      ),
      selectInput('cat_hora', 
                  'Selecciona un rango horario:',
                  c("Madrugada",
                    "Mañana",
                    "Media tarde",
                    "Medio dia",
                    "Noche"),
                  multiple = T
      ),
      selectInput('cat_fecha', 
                  'Selecciona un momento del mes:',
                  c("Principio de mes",
                    "Mitad de mes",
                    "Fin de mes"),
                  multiple = T
      ),
      actionButton("runif", "Mostrar")
      ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Resumen",
                 h2("Principales Estadísticas", align = "center"),
                 fluidRow(
                   column(6,plotOutput("bar_1")),
                   column(6,plotOutput("bar_2"))),
                 dataTableOutput("tab")
        ),
        
        tabPanel("Mapa",
                 h2("Visualización Geográfica", align = "center"),
                 plotOutput("map"),
                 selectInput("var_map",
                             "Selecciona una variable:",
                             c("Cantidad de vehículos" = "res_volume",
                               "Velocidad promedio" = "res_velocidad_promedio"))
        ),
        
        tabPanel("Gráfico de Mosaico",
                 h2("Asociación entre categorías", align = "center"),
                 
                 fluidRow(
                   column(6,
                          selectInput("x_tile",
                                      "Graficar:",
                                      c("Rango horario" = "cat_hora",
                                        "Momento del mes" = "cat_fecha",
                                        "Día de la semana" = "dia_semana")
                          )
                   ),
                   column(6,
                          conditionalPanel(
                            condition = "input.x_tile == 'cat_hora'",
                            selectInput("y_tile1",
                                        "Contra:",
                                        c("Momento del mes" = "cat_fecha",
                                          "Día de la semana" = "dia_semana"))
                          ),

                          conditionalPanel(
                            condition = "input.x_tile == 'cat_fecha'",
                            selectInput("y_tile2",
                                        "Contra:",
                                        c("Rango horario" = "cat_hora",
                                          "Día de la semana" = "dia_semana"))
                          ),

                          conditionalPanel(
                            condition = "input.x_tile == 'dia_semana'",
                            selectInput("y_tile3",
                                        "Contra:",
                                        c("Rango horario" = "cat_hora",
                                          "Momento del mes" = "cat_fecha")))
                 ),
                 
                 conditionalPanel(
                   condition = "input.x_tile == 'cat_hora'",
                   fluidRow(
                     column(6,plotOutput("tile_1_1")),
                     column(6,plotOutput("tile_1_2"))
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.x_tile == 'cat_fecha'",
                   fluidRow(
                     column(6,plotOutput("tile_2_1")),
                     column(6,plotOutput("tile_2_2"))
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.x_tile == 'dia_semana'",
                   fluidRow(
                     column(6,plotOutput("tile_3_1")),
                     column(6,plotOutput("tile_3_2"))
                   )
                 )
                 )
        )))))



server <- function(input, output, session){
  

  
  datos <- reactive({
    
    select_CCZ <- if (is.null(input$CCZ)) {unique(base$CCZ)} else {as.list(input$CCZ)}
    select_cat_hora <- if (is.null(input$cat_hora)) {unique(base$cat_hora)} else {as.list(input$cat_hora)}
    select_cat_fecha <- if (is.null(input$cat_fecha)) {unique(base$cat_fecha)} else {as.list(input$cat_fecha)}
    
    as.data.frame(select (base, -c(AREA, PERIMETER, geometry))) %>% 
      filter(CCZ %in% select_CCZ,
             cat_hora %in% select_cat_hora,
             cat_fecha %in% select_cat_fecha,
             fecha>=input$fecha[1] & fecha <=input$fecha[2]
      )
    })
  
  
############# Pestana MAPA


  mapa <- reactive({
    ggplot() +
      geom_sf(data = municipios, aes(fill = .data[[input$var_map]])) +  
      geom_sf(data = base) +
      theme_minimal() +
      scale_fill_gradient(low="#f0a678",
                          high = "#d92d02",
                          labels = scales::comma) +
      labs(fill = "Cantidad de vehículos")
  })
    
  # OUTPUT
  output$map <- renderPlot({
    plot(mapa())
  })
  
  
  
  
  
############# Pestana GRÁFICO DE MOSAICO:
  
  lab_x <- reactive(
    if (input$x_tile == "cat_hora") {
      lab_x <- "Rango horario"
    } else if (input$x_tile == "cat_fecha") {
      lab_x <- "Momento del mes"
    } else {
      lab_x <- "Día de la semana"
    }
  )

  lab_y <- reactive(
    if (input$x_tile == "cat_hora") {
      ifelse(input$y_tile1 == "cat_fecha",
             lab_y <- "Momento del mes",
             lab_y <- "Día de la semana")
    } else if (input$x_tile == "cat_fecha") {
      ifelse(input$y_tile2 == "cat_hora",
             lab_y <- "Rango horario",
             lab_y <- "Día de la semana")
    } else {
      ifelse(input$y_tile3 == "cat_hora",
             lab_y <- "Rango horario",
             lab_y <- "Momento del mes")
    }
  )
  

  
  graf_tile_1_1 <- reactive({
    
    ggplot(datos(),
           aes(x = .data[[input$x_tile]],
               y = .data[[input$y_tile1]],
               fill = velocidad_promedio)) +
    geom_tile() +
    labs(
         x = lab_x(),
         y = lab_y(),
         fill = "Velocidad Promedio") +
    theme(legend.position = "bottom",
          aspect.ratio = 1,
          legend.title = element_text(face = "bold",
                                      hjust = 0.5),
          axis.title.x = element_text(face = "bold",
                                      hjust = 0.5),
          axis.title.y = element_text(face = "bold",
                                      hjust = 0.5),
          axis.text.x = element_text(angle = 90,
                                     vjust = 0.5)) +
    scale_fill_gradient(low="#cf5d6b",
                        high = "#54020c")
  })
  
  graf_tile_1_2 <- reactive({
    
    ggplot(datos(),
           aes(x = .data[[input$x_tile]],
               y = .data[[input$y_tile1]],
               fill = volume)) +
    geom_tile() +
    labs(x = lab_x(),
         y = lab_y(),
         fill = "Cantidad") +
    theme(legend.position = "bottom",
          aspect.ratio = 1,
          legend.title = element_text(face = "bold",
                                      hjust = 0.5),
          axis.title.x = element_text(face = "bold",
                                      hjust = 0.5),
          axis.title.y = element_text(face = "bold",
                                      hjust = 0.5),
          axis.text.x = element_text(angle = 90,
                                     vjust = 0.5)) +
    scale_fill_gradient(low="#61c968",
                        high = "#023606")
  })
  
  
  # OUTPUTS 
  output$tile_1_1 <- renderPlot({
    plot(graf_tile_1_1())
  })
  
  output$tile_1_2 <- renderPlot({
    plot(graf_tile_1_2())
  })
  
  
  graf_tile_2_1 <- reactive({
    
    ggplot(datos(),
           aes(x = .data[[input$x_tile]],
               y = .data[[input$y_tile2]],
               fill = velocidad_promedio)) +
      geom_tile() +
      labs(
        x = lab_x(),
        y = lab_y(),
        fill = "Velocidad Promedio") +
      theme(legend.position = "bottom",
            aspect.ratio = 1,
            legend.title = element_text(face = "bold",
                                        hjust = 0.5),
            axis.title.x = element_text(face = "bold",
                                        hjust = 0.5),
            axis.title.y = element_text(face = "bold",
                                        hjust = 0.5),
            axis.text.x = element_text(angle = 90,
                                       vjust = 0.5)) +
      scale_fill_gradient(low="#cf5d6b",
                          high = "#54020c")
  })
  
  graf_tile_2_2 <- reactive({
    
    ggplot(datos(),
           aes(x = .data[[input$x_tile]],
               y = .data[[input$y_tile2]],
               fill = volume)) +
      geom_tile() +
      labs(x = lab_x(),
           y = lab_y(),
           fill = "Cantidad") +
      theme(legend.position = "bottom",
            aspect.ratio = 1,
            legend.title = element_text(face = "bold",
                                        hjust = 0.5),
            axis.title.x = element_text(face = "bold",
                                        hjust = 0.5),
            axis.title.y = element_text(face = "bold",
                                        hjust = 0.5),
            axis.text.x = element_text(angle = 90,
                                       vjust = 0.5)) +
      scale_fill_gradient(low="#61c968",
                          high = "#023606")
  })
  
  
  # OUTPUTS 
  output$tile_2_1 <- renderPlot({
    plot(graf_tile_2_1())
  })
  
  output$tile_2_2 <- renderPlot({
    plot(graf_tile_2_2())
  })
  
  
  
  graf_tile_3_1 <- reactive({
    
    ggplot(datos(),
           aes(x = .data[[input$x_tile]],
               y = .data[[input$y_tile3]],
               fill = velocidad_promedio)) +
      geom_tile() +
      labs(
        x = lab_x(),
        y = lab_y(),
        fill = "Velocidad Promedio") +
      theme(legend.position = "bottom",
            aspect.ratio = 1,
            legend.title = element_text(face = "bold",
                                        hjust = 0.5),
            axis.title.x = element_text(face = "bold",
                                        hjust = 0.5),
            axis.title.y = element_text(face = "bold",
                                        hjust = 0.5),
            axis.text.x = element_text(angle = 90,
                                       vjust = 0.5)) +
      scale_fill_gradient(low="#cf5d6b",
                          high = "#54020c")
  })
  
  graf_tile_3_2 <- reactive({
    
    ggplot(datos(),
           aes(x = .data[[input$x_tile]],
               y = .data[[input$y_tile3]],
               fill = volume)) +
      geom_tile() +
      labs(x = lab_x(),
           y = lab_y(),
           fill = "Cantidad") +
      theme(legend.position = "bottom",
            aspect.ratio = 1,
            legend.title = element_text(face = "bold",
                                        hjust = 0.5),
            axis.title.x = element_text(face = "bold",
                                        hjust = 0.5),
            axis.title.y = element_text(face = "bold",
                                        hjust = 0.5),
            axis.text.x = element_text(angle = 90,
                                       vjust = 0.5)) +
      scale_fill_gradient(low="#61c968",
                          high = "#023606")
  })
  
  
  # OUTPUTS 
  output$tile_3_1 <- renderPlot({
    plot(graf_tile_3_1())
  })
  
  output$tile_3_2 <- renderPlot({
    plot(graf_tile_3_2())
  })
  

############# Pestana RESUMEN
  
  grafico3 <- reactive({
    
    datos() %>%
      mutate(CCZ = fct_reorder(CCZ, -volume, .fun = "sum")) %>%
        ggplot(aes(x = CCZ,
                   y = volume,
                   fill = cat_fecha)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = scales::comma,
                           expand = c(0, 0)) +
        theme(legend.position = c(.78, .85),
              panel.background = element_blank(),
              axis.line = element_line(size = 0.5,
                                       colour = "black"),
              aspect.ratio = 1,
              legend.title = element_text(face = "bold",
                                          hjust = 0.5),
              axis.title.x = element_text(face = "bold",
                                          hjust = 0.5),
              axis.title.y = element_text(face = "bold",
                                          hjust = 0.5),
              axis.text.x = element_text(angle = 90,
                                         vjust = 0.5)) +
        labs(x = "Centros Comunales Zonales",
             y = "Cantidad de vehículos detectados",
             fill = "Momento del mes") +
        scale_fill_brewer(palette = "Dark2")
  })
  
  grafico4 <- reactive({
    
    datos() %>%
      group_by(CCZ) %>%
        summarise(Máximo = max(velocidad_promedio)) %>%
          mutate(CCZ = fct_reorder(CCZ, -Máximo, .fun = "mean")) %>%
            ggplot(aes(x = CCZ,
                       y = Máximo,)) +
            geom_bar(stat = "identity",
                     fill = "#941020") +
            scale_y_continuous(expand = c(0, 0)) +
            theme(
              legend.position = "bottom",
              panel.background = element_blank(),
              axis.line = element_line(size = 0.5,
                                       colour = "black"),
              aspect.ratio = 1,
              legend.title = element_text(face = "bold",
                                          hjust = 0.5),
              axis.title.x = element_text(face = "bold",
                                          hjust = 0.5),
              axis.title.y = element_text(face = "bold",
                                          hjust = 0.5),
              axis.text.x = element_text(angle = 90,
                                         vjust = 0.5)
            ) +
            labs(x = "Centros Comunales Zonales",
                 y = "Máxima Velocidad promedio (km/h)",
                 fill = "Momento del mes")
  })
  
 
  tabla1 <- reactive({
    
    datos() %>% 
      group_by(CCZ,dia_semana) %>% 
        summarise(
          min = min(velocidad_promedio),
          max = max(velocidad_promedio),
          mean_velocidad = mean(velocidad_promedio),
          median = median(velocidad_promedio),
          sd = sd(velocidad_promedio),
          conteo = sum(volume),
          mean_conteo = mean(volume)
        ) %>% 
          mutate(
            across(
              where(is.double),
              function (x) {round(x, digits = 2)}
            )
          ) %>% datatable(rownames = F,
                          colnames = c("Día de la semana" = "dia_semana",
                                       "Mínimo" = "min",
                                       "Máximo" = "max",
                                       "Velocidad media" = "mean_velocidad",
                                       "Velocidad mediana" = "median",
                                       "Desvío de velocidad" = "sd",
                                       "Cantidad de vehículos" = "conteo",
                                       "Cantidad media de vehículos" = "mean_conteo"))
  })
  
  # OUTPUT
  output$bar_1 <- renderPlot({
    plot(grafico3())
  })
  
  output$bar_2 <- renderPlot({
    plot(grafico4())
  })
  
  output$tab <- renderDT({
    print(tabla1())
  })
  
 
}

shinyApp(ui, server)
  

