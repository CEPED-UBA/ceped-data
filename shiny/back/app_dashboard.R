library(shiny)
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(shinydashboard)

options(scipen = 9999)

Serie_salarios <- readRDS("../data/salarios.RDS")
tipo_cambio_argentina <- readRDS("../data/Tipo_Cambio_Arg.RDS")
mercado_de_trabajo_arg <- readRDS("../data/Mercado_de_Trabajo_Arg.RDS")
poblacion_eph <- readRDS("../data/Poblacion_eph.RDS")
diccionario_variables <- read.xlsx("../data/diccionario_cod.variable.xlsx")

#Voy agregando a una lista los dataframes que vamos a subir
base_binded <- bind_rows(Serie_salarios,tipo_cambio_argentina)
lista_dfs <- list()
lista_dfs[[1]] <- Serie_salarios
lista_dfs[[2]] <- tipo_cambio_argentina
lista_dfs[[3]] <- mercado_de_trabajo_arg
lista_dfs[[4]] <- poblacion_eph

#Armo vectores para inputs
vector_bases <- unique(diccionario_variables$base)

#Las opciones aparecen en función de lo que se elija, lo saco de acá
#vector_variables <- setNames(diccionario_variables$cod.variable, diccionario_variables$nombre.variable)
#vector_paises <- unique(Serie_salarios$nombre.pais)
vector_desagreg <- c("País","etc")



header <- dashboardHeader(title = "CEPED DATA")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar",
    style = "position: relative; overflow: visible;",
    menuItem(text = "Mercado de Trabajo", icon = icon("th"), tabName = "Mercado De Trabajo"),
    div( id = 'sidebar_salario',
         conditionalPanel("input.sidebar === 'Mercado De Trabajo'",
                          selectizeInput("serie_salario",
                                         "Seleccionar una Serie", 
                                         choices =  unique(Serie_salarios$cod.variable), 
                                         selected = NULL,  width = "200px",
                                         multiple = F))
         ),
    menuItem(text = "Tipo de Cambio", icon = icon("th"), tabName = "Tipo de Cambio"),
    div( id = 'sidebar_tc',
         conditionalPanel("input.sidebar === 'Tipo de Cambio'",
                          selectizeInput("serie_tc",
                                         "Seleccionar una Serie", 
                                         choices =  unique(tipo_cambio_argentina$cod.variable), 
                                         selected = NULL,  width = "200px",
                                         multiple = F))
         ),
    actionButton("actualizar", "Ver")

    )
  )
  

body <- dashboardBody(
  fluidRow(
    box(tableOutput("tablita"))))
                      
server <- function(input, output) {

#cual <- observe(input$sidebar) 

  
  tab_filtrada <- eventReactive(input$actualizar, {
 
    variables_adecuadas <- 
      if(input$sidebar == "Mercado De Trabajo"){input$serie_salario} else
        if(input$sidebar == "Tipo de Cambio"){input$serie_tc}
    
    df <- base_binded %>%
      filter(cod.variable  %in%  variables_adecuadas)
#      filter(nombre.pais %in% input$pais_id) %>%
#      filter(ANO4 %in% c(input$id_periodo[1]:input$id_periodo[2]))


  })
  
  output$tablita <- renderTable({
    tab_filtrada()
  })
  
}

shinyApp(
  ui = dashboardPage(header = header,sidebar =  sidebar,body =  body,skin = "red"),
  server = server
)
