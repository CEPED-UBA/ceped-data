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


v_bp <- c() #cuales?

v_salarios <- diccionario_variables %>% filter(base== "Serie_salarios") %>%  select(nombre.variable) %>% 
  filter(nombre.variable != "Indice de Precios al Consumidor (base 2005)")



v_monetario <- diccionario_variables %>% filter(base=="Tipo_Cambio_Arg") %>%  select(cod.variable) 

v_ipc <- grep("IPC",v_monetario$cod.variable, value = T)
v_tc <- v_monetario$cod.variable[!v_monetario$cod.variable %in% v_ipc]

v_ipc <- diccionario_variables %>% filter(cod.variable %in% v_ipc | nombre.variable == "Indice de Precios al Consumidor (base 2005)") %>%  select(nombre.variable)

v_tc <- diccionario_variables %>% filter(cod.variable %in% v_tc) %>%  select(nombre.variable)

vector_paises <- unique(serie_salarios$nombre.pais)
vector_desagreg <- c("País","etc")



####genero server######

series_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    armar_tabla <- function(variables, paises, periodo_i, periodo_f){
      base_binded %>% ungroup() %>%
        filter(cod.variable  ==  unique(diccionario_variables$cod.variable[diccionario_variables$nombre.variable == variables]) ) %>% 
        filter(nombre.pais %in% paises) %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        mutate(ANO4 = round(ANO4,0)) %>% 
        rename("Serie" = "cod.variable",
               "País" = "nombre.pais",
               "Período" = "ANO4") 
    }
    
    generar_titulo <- function(variables,paises, periodo_i, periodo_f){
      lista_paises <-  paste0(paises, collapse = ", ")
      lista_paises <- sub(",([^,]*)$", " y\\1", lista_paises)   
      nombre_variable <- unique(diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==variables])
      titulo <- paste0(variables ," para ", lista_paises , ". Años: ", periodo_i, " al ", periodo_f)
    }
    
    plot <- function(variables, paises, periodo_i, periodo_f){
      
      p <- base_binded %>% ungroup() %>%
        filter(cod.variable  ==  unique(diccionario_variables$cod.variable[diccionario_variables$nombre.variable == variables])) %>% 
        filter(nombre.pais %in% paises) %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        ggplot(
          aes(x = as.factor(ANO4), y = valor, group = iso3c, color = iso3c
              ,text=paste0('</br>',iso3c,'</br>valor: ',round(valor,1), '</br>Período: ',ANO4)))+
        geom_line(size = 1) +
        labs(color= "País",
             y = "",
             x = "Año")+
        theme_minimal()+
        theme(text = element_text(size = 9),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",
              plot.title= element_text(size=12, face="bold"))+
        theme(axis.text.x = element_text(angle = 90))
      
      #p
      ggplotly(p, tooltip = c("text"))
    }
    
    generar_metadata <- function(variables){
      diccionario_variables$metadata[diccionario_variables$nombre.variable == variables] 
    }
    output$titulo <- renderText({
      generar_titulo(input$var_serie,input$pais_id, input$id_periodo[1],input$id_periodo[2])
    })
    output$plot <- renderPlotly({
      plot(input$var_serie,input$pais_id, input$id_periodo[1],input$id_periodo[2])
      })
    output$tabla <- renderTable({
      armar_tabla(input$var_serie,input$pais_id, input$id_periodo[1],input$id_periodo[2])
    })
    output$metadata <- renderText({
      generar_metadata(input$var_serie)
    })
    
    
  })
}


####genero ui########

series_plot_ui <- function(id, title,v_variables) {
  ns <- NS(id)
  
  tabPanel(title,
           value = id,
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('var_serie'),label = 'Seleccionar una Serie',
                           choices =  v_variables, 
                           selected = v_variables[1],
                           width = "300px",
                           multiple = F),
               selectInput(ns('pais_id'),label = 'País:',
                           choices =  unique(serie_salarios$nombre.pais),
                           multiple = T,
                           selected = vector_paises[1:2],
                           width = "300px"),
               sliderInput(ns('id_periodo'), "Período:", value = c(1980,2005), min = 1950, max = 2010)
             ),
             mainPanel(
               box(width = NULL, textOutput(ns('titulo'))),
               plotlyOutput(ns('plot')),
               
               fluidRow(
                 column(12,
                        column(6, 
                                   box(tableOutput(ns('tabla')))),
                        column(6,          
                               box(title = "Metadata", width = NULL, textOutput(ns('metadata'))),
                               br(),
                               box(width = NULL,
                                   downloadButton(ns('downloadTable'),'Descargar tabla')),
                               br(),
                               box(width = NULL,
                                   downloadButton(ns('downloadPlot'),'Descargar gráfico'))
                               
                               ))
               )
               
               
               
             )
           
  )
  
  )
  
}



