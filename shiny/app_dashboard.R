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


#Armo vectores para inputs
vector_bases <- unique(diccionario_variables$base)

#Las opciones aparecen en función de lo que se elija, lo saco de acá
vector_variables <- setNames(diccionario_variables$cod.variable, diccionario_variables$nombre.variable)
vector_paises <- unique(Serie_salarios$nombre.pais)
vector_desagreg <- c("País","etc")



header <- dashboardHeader(title = "CEPED DATA")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar",
    style = "position: relative; overflow: visible;",
    menuItem(text = "Areas Temáticas", icon = icon("folder"), tabName = "Areas Temáticas"),#Para que aparezca seleccionado al ppio
    menuItem(text = "Mercado de Trabajo", icon = icon("tools"), tabName = "Mercado De Trabajo"),
    div( id = 'sidebar_salario',
         conditionalPanel("input.sidebar === 'Mercado De Trabajo'",
                          selectizeInput("serie_salario",
                                         "Seleccionar una Serie", 
                                         choices =  unique(Serie_salarios$cod.variable), 
                                         selected = unique(Serie_salarios$cod.variable)[9],
                                         width = "300px",
                                         multiple = F))
         ),
    div( id = 'sidebar_salario_pais',
         conditionalPanel("input.sidebar === 'Mercado De Trabajo'",
                          selectInput(inputId = "pais_id",
                                      label ="País:",
                                      choices =  unique(Serie_salarios$nombre.pais),
                                      multiple = T,
                                      selected = vector_paises[1:2]
                          ),
    )
    ),
    div( id = 'sidebar_salario_periodo',
         conditionalPanel("input.sidebar === 'Mercado De Trabajo'",
    sliderInput("id_periodo", "Período:", value = c(2001,2015), min = 1970, max = 2022)
    )
    ),
    menuItem(text = "Tipo de Cambio", icon = icon("money-bill-alt"), tabName = "Tipo de Cambio",selected = F),
    div( id = 'sidebar_tc',
         conditionalPanel("input.sidebar === 'Tipo de Cambio'",
                          selectizeInput("serie_tc",
                                         "Seleccionar una Serie", 
                                         choices =  unique(tipo_cambio_argentina$cod.variable), 
                                         selected = NULL,  width = "200px",
                                         multiple = F))
         ),
    
    actionButton("actualizar", "Actualizar Series")

    )
  )
  

body <- dashboardBody(
  fluidRow(
    box(footer = "CEPED-ROCKANDROLL",solidHeader = T,width = 20,
        plotOutput("ploteado"))),
  fluidRow(box(tableOutput("tablita"))))
                      
server <- function(input, output) {

#cual <- observe(input$sidebar) 

  
  tab_filtrada <- eventReactive(input$actualizar, {
 
    variables_adecuadas <- 
      if(input$sidebar == "Mercado De Trabajo"){input$serie_salario} else
        if(input$sidebar == "Tipo de Cambio"){input$serie_tc}
    
    df <- base_binded %>%
      filter(cod.variable  %in%  variables_adecuadas) %>% 
      filter(nombre.pais %in% input$pais_id) %>% 
      filter(ANO4 %in% c(input$id_periodo[1]:input$id_periodo[2]))


  })
  
  output$tablita <- renderTable({
    tab_filtrada()
  })
  
  output$ploteado <- renderPlot(
    
    tab_filtrada() %>% 
      ggplot(
      aes(y = valor, x = as.factor(ANO4),group = iso3c,color = iso3c))+
      geom_line(size = 1) + 
      labs(color= "País",
           #title= str_wrap(titulo(),60),
          # y = str_wrap(nombre_variable,40),
           x = "Año")+
      theme_minimal()+
      theme(text = element_text(size = 9),
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            legend.position = "bottom", 
            plot.title= element_text(size=12, face="bold"))+ 
      theme(axis.text.x = element_text(angle = 90)))
    
}

shinyApp(
  ui = dashboardPage(header = header,sidebar =  sidebar,body =  body,skin = "red"),
  server = server
)

