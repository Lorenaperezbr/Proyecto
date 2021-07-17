library(shiny)
library(tidyverse)
library(DT)


base <- readRDS("data/processed/febrero_marzo_join.rds")

ui <- fluidPage(
  titlePanel("Datos de Propina"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("fecha",
                     "Selecciona un rango de fechas",
                     format='dd-mm-yyyy',
                     separator = " hasta ",
                     language="es",
                     min="01-02-2021",
                     max="31-03-2021"
                      ),
      checkboxInput('CCZ', 
                  'Selecciona un Centro Comunal Zonal',
                  c(1:17)
                  ),
      checkboxInput('cat_hora', 
                  'Selecciona un rango horario',
                  c("Madrugada", "Mañana", "Media tarde", "Medio dia", "Noche")
                  ),
      checkboxInput('cat_fecha', 
                  'Selecciona un momento del mes',
                  c("Principio de mes", "Mitad de mes", "Fin de mes")
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
                     column(6,plotOutput("tile_1")),
                     column(6,plotOutput("tile_2"))
                   ),
                   fluidRow(
                     column(6,plotOutput("tile_3")),
                     column(6,plotOutput("tile_4")))),
        
        
        
          tabPanel("Resumen",
               h2("Algun titulo interesante", align = "center"),
               fluidRow(
                 column(6,plotOutput("bar_1")),
                 column(6,plotOutput("bar_2"))),
                 dataTableOutput("tab")
               )
)
    
    
    
    )
))

server <- function(input, output){
  

  
  
}

shinyApp(ui, server)
  

