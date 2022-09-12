# library(ggplot2)
# library(tidyverse)
# library(openxlsx)
# library(cowplot)
# library(magick)

# serie_salarios <- readRDS("www/data/salarios.RDS")
# tipo_cambio_argentina <- readRDS("www/data/Tipo_Cambio_Arg.RDS")
# diccionario_variables <- read.xlsx("www/data/diccionario_cod.variable.xlsx")


#serie_salarios <- readRDS("opcion-modulos-paneles/www/data/salarios.RDS")
#tipo_cambio_argentina <- readRDS("opcion-modulos-paneles/www/data/Tipo_Cambio_Arg.RDS")
#diccionario_variables <- read.xlsx("opcion-modulos-paneles/www/data/diccionario_cod.variable.xlsx")


#Voy agregando a una lista los dataframes que vamos a subir
# base_binded <- bind_rows(serie_salarios,tipo_cambio_argentina)
# 
# 
# 
# 
# v_monetario <- diccionario_variables %>% filter(base=="Tipo_Cambio_Arg") %>%  select(cod.variable) 
# 
# v_ipc <- grep("IPC",v_monetario$cod.variable, value = T)
# v_tc <- v_monetario$cod.variable[!v_monetario$cod.variable %in% v_ipc]
# 
# v_ipc <- diccionario_variables %>% filter(cod.variable %in% v_ipc | nombre.variable == "Indice de Precios al Consumidor (base 2005)") %>%  select(nombre.variable)
# 
# v_tc <- diccionario_variables %>% filter(cod.variable %in% v_tc) %>%  select(nombre.variable)
# 
# v_tc <- unique(v_tc$nombre.variable)

#base_binded_tc <- base_binded %>% filter(cod.variable %in% v_tc)





####genero server######

tc_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    armar_tabla <- function(variables, periodo_i, periodo_f){
      base_binded %>% ungroup() %>%
        filter(cod.variable  ==  unique(diccionario_variables$cod.variable[diccionario_variables$nombre.variable %in% variables]) ) %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        mutate(ANO4 = round(ANO4,0)) %>% 
        rename("Serie" = "cod.variable",
               "Período" = "ANO4",
               "País" = "nombre.pais") %>% 
        select(-iso3c) %>%   
        datatable(rownames = FALSE,
         options = list(
           searching=FALSE, 
           pageLength = 10, 
           dom='tip')) %>% 
        formatRound("valor")
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      
      lista_variables <-  paste0(variables, collapse = ", ")
      lista_variables <- sub(",([^,]*)$", " y\\1", lista_variables)  
      titulo <- paste0("</br><font size='+2'>",lista_variables,".</font>" , 
                       "</br><font size='+1'>Años: ", periodo_i, " al ", periodo_f,"</font>")
    }
    
    plot <- function(variables, periodo_i, periodo_f){
      
      aux <- diccionario_variables %>% 
        filter(nombre.variable %in% variables) %>% 
        select(nombre.variable,cod.variable)
      
      p <- base_binded %>% ungroup() %>%
        filter(cod.variable  ==  unique(diccionario_variables$cod.variable[diccionario_variables$nombre.variable %in% variables])) %>% 
        left_join(aux,by = "cod.variable") %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        ggplot(
          aes(x = as.factor(ANO4), y = valor, group = cod.variable, color = cod.variable
              ,text=paste0('</br>',nombre.variable,'</br>valor: ',round(valor,1), '</br>Período: ',ANO4)))+
        geom_line(size = 1) +
        labs(color= "Serie",
             y = "",
             x = "Año")+
        theme_minimal()+
        theme(text = element_text(size = 9),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",
              plot.title= element_text(size=12, face="bold"))+
        theme(axis.text.x = element_text(angle = 90))+
        scale_color_manual(values =paleta_colores_extendida)
      
      p
      #ggplotly(p, tooltip = c("text"))
    }
    
    plot_interact <- function(p){
      ggplotly(p, tooltip = c("text"))%>% 
        layout(font = list(family ="Times New Roman"))
    }
    
    generar_metadata <- function(variables){
      diccionario_variables$metadata[diccionario_variables$nombre.variable == variables] 
    }
    output$titulo1 <- renderText({
      generar_titulo(input$var_serie, input$id_periodo[1],input$id_periodo[2])
    })
    output$titulo2 <- renderText({
      generar_titulo(input$var_serie, input$id_periodo[1],input$id_periodo[2])
    })
    output$plot <- renderPlotly({
      plot_interact(plot(input$var_serie, input$id_periodo[1],input$id_periodo[2]))
      })
    output$tabla <- renderDT({
      armar_tabla(input$var_serie, input$id_periodo[1],input$id_periodo[2])
    })
    output$metadata1 <- renderText({
      generar_metadata(input$var_serie)
    })
    output$metadata2 <- renderText({
      generar_metadata(input$var_serie)
    })
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste(input$var_serie,'.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(input$var_serie, input$id_periodo[1],input$id_periodo[2]), 
                   file)    }
    )
    output$downloadPlot <- downloadHandler(
      filename = function(){paste(input$var_serie,'.png',sep='')},
      content = function(file){
        
        
        ggsave(file,plot=plot(input$var_serie, input$id_periodo[1],input$id_periodo[2]), 
               width=8, height=4)
      }
    )
    
  })
}


####genero ui########


tc_plot_ui <- function(id, title) {
  ns <- NS(id)
  
  tabPanel(title,
           value = id,
           titlePanel(title),
           sidebarLayout(
            
             sidebarPanel(
               selectInput(ns('var_serie'),label = 'Seleccionar una Serie',
                           choices =  v_tc, 
                           selected = v_tc[2],
                           width = "300px",
                           multiple = T),
               sliderInput(ns('id_periodo'), "Período:", value = c(1980,2005), min = 1882, max = 2019), 
               hr(), 
               h4(strong(titulo_cita)), 
               h5(cita)
             ),
             
             
             mainPanel(
               
               tabsetPanel(
                 
                 tabPanel("Gráfico",
                          value = "g_series",
                          
               box(width = NULL,br(), htmlOutput(ns('titulo1'))),
               br(),
               plotlyOutput(ns('plot')),
               br(),
               box(title = "Metadata", width = NULL, htmlOutput(ns('metadata1'),style = "text-align: justify")),
               br(),
               box(width = NULL,
                   downloadButton(ns('downloadPlot'),'Descargar gráfico'))
               
                 ),
               
               tabPanel("Tabla",
                        value = "t_series",
               
               fluidRow(
                 column(12,
                        column(8, 
                               box(width = NULL, br(),htmlOutput(ns('titulo2'))),
                               br(),
                               box(DTOutput(ns('tabla')), width = NULL)),
                        column(4,          
                               box(title = "Metadata", width = NULL, htmlOutput(ns('metadata2'),style = "text-align: justify")),
                               br(),
                               box(width = NULL,
                                   downloadButton(ns('downloadTable'),'Descargar tabla'))
                               
                               ))
               ))
               
               )
               
             )
             
             
             
           
  )
  
  )
  
}



