library(ggplot2)
library(tidyverse)
library(openxlsx)
library(cowplot)
library(magick)


mercado_de_trabajo_arg <- readRDS("www/data/Mercado_de_Trabajo_Arg.RDS") %>% 
  group_by(ANO4, cod.variable) %>% 
  mutate(valor=mean(valor, na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(-ANO4.trim) %>% 
  unique() 
### PRUEBO PRIMERO CON PROMEDIO ANUAL


diccionario_variables <- read.xlsx("www/data/diccionario_cod.variable.xlsx")

v_variables <- diccionario_variables %>% filter(base=="Mercado_de_Trabajo_Arg") %>%  select(nombre.variable) 
v_variables <- as.vector(v_variables[1])

# Cambio cod.variable por nombre de la variable
etiquetas <- diccionario_variables %>% filter(base=="Mercado_de_Trabajo_Arg") %>% select(nombre.variable, cod.variable)

mercado_de_trabajo_arg <- left_join(mercado_de_trabajo_arg, etiquetas, by=c("cod.variable")) %>% 
  mutate(cod.variable=nombre.variable) %>% 
  select(-nombre.variable)

estad_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    

    armar_tabla <- function(variables, periodo_i, periodo_f){
      mercado_de_trabajo_arg  %>%
        filter(cod.variable  %in%   variables) %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        rename("Serie" = "cod.variable",
               "País" = "nombre.pais",
               "Período" = "ANO4") %>% 
        select("País","iso3c","Período","Serie", "valor")
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      
      
      lista_variables <-  paste0(variables, collapse = ", ")
      lista_variables <- sub(",([^,]*)$", " y\\1", lista_variables)  
      titulo <- paste0(lista_variables ," desde ", periodo_i, " hasta ", periodo_f)

       }
    
    plot <- function(variables, periodo_i, periodo_f){
      
      p <- mercado_de_trabajo_arg %>% 
        filter(cod.variable  %in%   variables) %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        ggplot(
          aes(x = as.factor(ANO4), y = valor, group = cod.variable, color = cod.variable,
                text=paste0('</br>valor: ',round(valor,1), '</br>Período: ',ANO4)))+
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(y = "",
             x = "Año",
             color = "")+
        theme_minimal()+
        theme(text = element_text(size = 9),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",
              plot.title= element_text(size=12, face="bold"))+
        theme(axis.text.x = element_text(angle = 90))
      
      
      p
      #ggplotly(p, tooltip = c("text"))
    }
    
    
    plot_interact <- function(p){
      ggplotly(p, tooltip = c("text"))
    }
    
    generar_metadata <- function(variables){
    
    i <- length(variables)  
        
    paste0(variables[1:i], ": ", diccionario_variables$metadata[diccionario_variables$nombre.variable %in% variables[1:i]])
      
    }
    
    output$titulo1 <- renderText({
      generar_titulo(input$var_serie,input$id_periodo[1],input$id_periodo[2])
    })
     output$titulo2 <- renderText({
       generar_titulo(input$var_serie,input$id_periodo[1],input$id_periodo[2])
     })
    output$plot <- renderPlotly({
      plot_interact(plot(input$var_serie,input$id_periodo[1],input$id_periodo[2]))
    })
    
    output$tabla <- renderTable({
      armar_tabla(input$var_serie, input$id_periodo[1],input$id_periodo[2])
    })
    output$metadata1 <- renderText({
      generar_metadata(input$var_serie)
    })
    output$metadata2 <- renderText({
      generar_metadata(input$var_serie)
    })
    output$downloadTable <- downloadHandler(

      filename = function(){paste(input$var_serie[1],'.xlsx',sep='')},
      content = function(file){

        write.xlsx(armar_tabla(input$var_serie, input$id_periodo[1],input$id_periodo[2]),
                   file)    }
    )
    output$downloadPlot <- downloadHandler(
      filename = function(){paste(input$var_serie[1],'.png',sep='')},
      content = function(file){


        ggsave(file,plot=plot(input$var_serie, input$id_periodo[1],input$id_periodo[2]),
               width=8, height=4)
      }
    )
    
    
     })
}

estad_plot_ui <- function(id, title,v_variables) {
  ns <- NS(id)
  
  tabPanel(title,
           value = id,
           
           
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('var_serie'),label = 'Seleccionar una Serie',
                           choices =  unique(mercado_de_trabajo_arg$cod.variable),
                           selected = unique(mercado_de_trabajo_arg$cod.variable)[1],
                           width = "300px",
                           multiple = T
               ),
               sliderInput(ns('id_periodo'), "Período:",
                           value = c(2003,2021),
                           min = 2003, 
                           max = 2021
               )
             ),
             
             mainPanel( 
               
               tabsetPanel(
                 
                 tabPanel("Gráfico",
                          value = "g_estad",
                          
                          box(width = NULL, textOutput(ns('titulo1'))), 
                          br(),
                          plotlyOutput(ns('plot')),
                          br(),
                          box(title = "Metadata", width = NULL, textOutput(ns('metadata1'))),
                          br(),
                          box(width = NULL,
                              downloadButton(ns('downloadPlot'),'Descargar gráfico'))
                          
                 ),
                 
                 tabPanel("Tabla",
                          value = "t_estad",
                          
                          box(width = NULL, textOutput(ns('titulo2'))), 
                          br(),
                          fluidRow(
                            column(12,
                                   column(6, 
                                          box(tableOutput(ns('tabla')))),
                                   column(6,          
                                          box(title = "Metadata", width = NULL, textOutput(ns('metadata2'))),
                                          br(),
                                          box(width = NULL,
                                              downloadButton(ns('downloadTable'),'Descargar tabla'))
                                          
                                          
                                   ))
                          )
                 )
                 
               )
               
             )
             
           )
           
           
           
  )
  
  
}

