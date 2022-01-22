library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(readr)
library(openxlsx)
library(shinydashboard)
library(DT)

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
    menuItem(text = "Mercado de Trabajo", icon = icon("th"), tabName = "Salarios"),
    div( id = 'sidebar_salario',
         conditionalPanel("input.sidebar === 'Salarios'",
                          selectizeInput("serie_salario",
                                         "Seleccionar una Serie", 
                                         choices =  unique(Serie_salarios$cod.variable), 
                                         selected = NULL,  width = "200px",
                                         multiple = F))
         ),
    menuItem(text = "Tipo de Cambio", icon = icon("th"), tabName = "TC"),
    div( id = 'sidebar_tc',
         conditionalPanel("input.sidebar === 'TC'",
                          selectizeInput("select_country",
                                         "Select or search for one or multiple markets", 
                                         choices =  unique(tipo_cambio_argentina$cod.variable), 
                                         selected = NULL,  width = "200px",
                                         multiple = F))
    )

    )
  )
  

body <- dashboardBody(fluidRow(dataTableOutput("tablita"),
                               options= list(pageLength = 100)))
                      
server <- function(input, output) {

# NO PUDE HACER FUNCIONAR ESTA PARTE REACTIVA  
    
  # base <- reactive({
  #   
  #   if(input$serie %in% unique(Serie_salarios$cod.variable)){
  #     lista_dfs[[1]]
  #   } else if(input$serie %in% unique(tipo_cambio_argentina$cod.variable)){
  #     lista_dfs[[2]]
  #   }
  #   
  # })
  # 
  # tab_filtrada <- eventReactive(input$actualizar, {
  #   
  #   df <- base() %>% 
  #     filter(cod.variable == input$var1_id) %>% 
  #     filter(nombre.pais %in% input$pais_id) %>% 
  #     filter(ANO4 %in% c(input$id_periodo[1]:input$id_periodo[2])) 
  #   
  #   
  # })
  
  output$tablita <- renderDataTable({
    Sale <- Serie_salarios %>% filter(cod.variable == input$serie_salario)
    
    datatable(Sale,filter="top",selection="multiple", escape=FALSE,rownames = FALSE,
              options = list(pageLength = 100))
    
  })
  
}

shinyApp(
  ui = dashboardPage(header = header,sidebar =  sidebar,body =  body,skin = "red"),
  server = server
)
