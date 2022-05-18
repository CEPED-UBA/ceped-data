library(ggplot2)
library(tidyverse)
library(openxlsx)
library(cowplot)
library(magick)

serie_salarios <- readRDS("www/data/salarios.RDS")
tipo_cambio_argentina <- readRDS("www/data/Tipo_Cambio_Arg.RDS")
diccionario_variables <- read.xlsx("www/data/diccionario_cod.variable.xlsx")

#Voy agregando a una lista los dataframes que vamos a subir
base_binded <- bind_rows(serie_salarios,tipo_cambio_argentina)

#Armo vectores para inputs
vector_bases <- unique(diccionario_variables$base)

#Vector con nombres para cod.variable (series salario)
v_variables <- diccionario_variables %>% filter(base=="Serie_salarios") %>% select(cod.variable) 
v_nombres <- diccionario_variables %>% filter(base=="Serie_salarios") %>% select(nombre.variable)
vector_variables_serie_salario <- setNames(v_variables$cod.variable, v_nombres$nombre.variable)

#Vector con nombres para cod.variable (series "Tipo_Cambio_Arg")
v_variables <- diccionario_variables %>% filter(base=="Tipo_Cambio_Arg") %>% select(cod.variable) 
v_nombres <- diccionario_variables %>% filter(base=="Tipo_Cambio_Arg") %>% select(nombre.variable)
vector_variables_tipo_cambio_argentina <- setNames(v_variables$cod.variable, v_nombres$nombre.variable)

vector_paises <- unique(serie_salarios$nombre.pais)
vector_desagreg <- c("País","etc")

series_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    armar_tabla <- function(base, variables, paises, periodo){
      base %>% ungroup() %>%
        filter(cod.variable  %in%  variables) %>% 
        filter(nombre.pais %in% paises) %>% 
        filter(ANO4 %in% periodo)
    }
    
    plot <- function(input_cut){
      
      d <- diamonds[sample(nrow(diamonds), 1000), ] %>% 
        filter(cut==input_cut)
      
      p <- ggplot(data = d, aes(x = carat, y = price)) +
        geom_point(aes(text = paste("Clarity:", clarity))) +
        geom_smooth(aes(colour = cut, fill = cut))
      
      
      ggplotly(p)
    }
    
    output$plot1 <- renderPlotly({plot(input$input1)})
    output$tabla1 <- renderTable({
      armar_tabla()
    })
    
    
  })
}

series_plot_ui <- function(id) {
  ns <- NS(id)
  
  navbarMenu(title = 'Series',
           
           tabPanel('Serie de salarios',
                    value = 'salarios',
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('serie_salario'),label = 'Seleccionar una Serie',
                           choices =  vector_variables_serie_salario, 
                           selected = vector_variables_serie_salario[1],
                           width = "300px",
                           multiple = F),
               selectInput(ns('pais_id'),label = 'País:',
                           choices =  unique(serie_salarios$nombre.pais),
                           multiple = T,
                           selected = vector_paises[1:2],
                           width = "300px"),
               sliderInput("id_periodo_sal", "Período:", value = c(min(serie_salarios$ANO4),max(serie_salarios$ANO4)), min = min(serie_salarios$ANO4), max = max(serie_salarios$ANO4))
             ),
             mainPanel(
               plotlyOutput(ns('plot1')),
               
               fluidRow(box(tableOutput(ns('tabla1'))),
                        column(width = 6,
                               #             box(width = NULL, textOutput("titulo")),
                               box(title = "Metadata", width = NULL, textOutput(ns('metadata1'))),
                               box(width = NULL,
                                   downloadButton(ns('downloadTable1'),'Descargar tabla')),
                               box(width = NULL,
                                   downloadButton(ns('downloadPlot1'),'Descargar gráfico')))
               )
               
               
             )
           )
           ),
           
           tabPanel('Tipo de cambio',
                    sidebarLayout(
                      sidebarPanel(
                        selectInput(ns('input2'),label = 'select cut',
                                    choices = diamonds$cut %>% unique(),
                                    selected = 'Premium',
                                    multiple = FALSE)
                      ),
                      mainPanel(plotlyOutput(ns('plot2'))
                      )
                    )
           ),
           tabPanel('Balanza de pagos',
                    sidebarLayout(
                      sidebarPanel(
                        selectInput(ns('input3'),label = 'select cut',
                                    choices = diamonds$cut %>% unique(),
                                    selected = 'Premium',
                                    multiple = FALSE)
                      ),
                      mainPanel(plotlyOutput(ns('plot3'))
                      )
                    )
           ),
           tabPanel('IPC Argentina',
                    sidebarLayout(
                      sidebarPanel(
                        selectInput(ns('input4'),label = 'select cut',
                                    choices = diamonds$cut %>% unique(),
                                    selected = 'Premium',
                                    multiple = FALSE)
                      ),
                      mainPanel(plotlyOutput(ns('plot4'))
                      )
                    ))
           
  )
}
