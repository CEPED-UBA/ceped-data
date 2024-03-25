##modelo: https://matbarofex.com.ar/IndiceDolarExportacion

## vamos a probar el push de la rama local

####Server

tipo_cambio_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    df <-  reactive({ ##base sería un recorte del df pobreza que reacciona a la solicitud del usuario
      
      base <- pobreza %>% ##pobreza es un objeto que esta en www, debo hacer una base de tipo de cambio
        filter(Serie %in% input$var_serie)  %>% ##filtrame para que serie sea los inputs que puso el usuario, ojo con input$var_serie fijarse en la interfaz de usuario
        filter(ANO4 >=input$id_periodo[1], ANO4 <= input$id_periodo[2] ) %>% 
        filter(metodologia==input$metodologia)
      
      base
      
    })
    
    df_canastas <-  reactive({
      
      base <- canastas %>%
        filter(year(periodo) >=input$id_periodo[1], year(periodo) <= input$id_periodo[2] ) %>% 
        mutate(periodo=format(periodo, "%b %Y"))
      
      base
      
    })
  
##Arranca el gráfico    
    
      
    generar_titulo <- function(variables, periodo_i, periodo_f){ ##función que me genera el título del gráfico, de acuerdo a los filtros y años aplicados
      
      
      lista_variables <-  paste0(variables, collapse = ", ")
      lista_variables <- sub(",([^,]*)$", " y\\1", lista_variables)  
      titulo <- paste0("<font size='+2'></br>Porcentaje de ",lista_variables ,". Sobre total poblacional.</font>",
                       "</br><font size='+1'>Gran Buenos Aires. Años ", periodo_i, " - ", periodo_f,"</font>")
      
    }
    
    plot <- function(variables){  ##este es el gráfico, el diseño del gráfico
      
      
      p <- df() %>% ##parte del objeto df que reacciono arriba 
        ggplot(
          aes(x = Periodo, y = valor/100, group = Serie, color = Serie,
              text=paste0('</br>valor: ',round(valor,1), '</br>Período: ', Periodo)))+
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(y = "",
             x = "Período",
             color = "")+
        theme_minimal()+
        theme(text = element_text(size = 9),
              axis.text.x = element_text(size=6),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",
              plot.title= element_text(size=12, face="bold"))+
        theme(axis.text.x = element_text(angle = 90))+
        scale_color_manual(values =paleta_colores_extendida) +
        scale_y_continuous(labels=scales::percent) +
        scale_y_continuous(breaks=c(5,7.5, 20, 25))
      p
      
    }
    
    
    plot_interact <- function(p){
      ggplotly(p, tooltip = c("text"))%>% ##interactividad del gráfico
        layout(font = list(family ="Times New Roman"))
    }
    
    output$titulo1 <- renderText({
      generar_titulo(input$var_serie,input$id_periodo[1],input$id_periodo[2])
    })
    
    output$titulo2 <- renderText({
      generar_titulo(input$var_serie,input$id_periodo[1],input$id_periodo[2])
    })
    
    output$titulo3 <- renderText({
      "<font size='+2'></br>Valores de la Canasta Básica Total (CBT) y Canasta Básica Alimentaria (CBA) </font></br><font size='+1'>Gran Buenos Aires.</font>"
    })
    
    
    output$plot <- renderPlotly({
      plot_interact(plot(input$var_serie))
    })
    
    output$tabla <- renderDT({
      df() %>%   datatable(rownames = FALSE,
                           options = list(
                             searching=FALSE, 
                             pageLength = 10, 
                             dom='tip')) %>% 
        formatRound("valor")
    })
    
    output$tabla_canastas <- renderDT({
      df_canastas() %>%   datatable(rownames = FALSE,
                                    options = list(
                                      searching=FALSE, 
                                      pageLength = 10, 
                                      dom='tip')) %>% 
        formatRound(c(2,3))
    })
    
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste("ceped_data_pobreza_",  Sys.Date(), ".xlsx" ,sep='')},
      content = function(file){
        write.xlsx(df(), 
                   file)       }
    )
    
    output$downloadTable_canastas <- downloadHandler(
      
      filename = function(){paste("ceped_data_canastas_",  Sys.Date(), ".xlsx" ,sep='')},
      content = function(file){
        write.xlsx(df_canastas(), 
                   file)       }
    )
    
    output$download_database <- downloadHandler(
      
      filename = function(){paste("database",'.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(list("Base_completa" =pobreza),
                   file)    
      }
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

###Interfaz de usuario


tipo_cambio_plot_ui <- function(id, title) {
  ns <- NS(id)
  
  tabPanel(title,
           value = id,
           titlePanel(title),
           
           sidebarLayout(
             sidebarPanel(
               
               selectInput(ns('var_serie'),label = 'Seleccionar series',
                           choices =  c("Indigentes EPH-puntual", "No indigentes EPH-puntual",     
                                        "Indigentes EPH-continua", "No indigentes EPH-continua",
                                        "Pobres EPH-puntual","No Pobres EPH-puntual",         
                                        "Pobres EPH-continua", "No Pobres EPH-continua"),
                           selected = c("Indigentes EPH-puntual", "Indigentes EPH-continua", 
                                        "Pobres EPH-puntual", "Pobres EPH-continua"),
                           width = "350px",
                           multiple = T
               ),
               sliderInput(ns('id_periodo'), "Período:",
                           value = c(1974, 2022),
                           min = 1974, 
                           max = 2022,
                           sep=""
               ), 
               hr(), 
               radioButtons(ns("metodologia"), 
                            "Elija la metodología de estimación", 
                            choices=c("CEPA", "INDEC")
               ),
               h6("Las estimaciones se realizan siguiendo las siguientes metodologías: "),
               h6("CEPA (1993), Evolución reciente de la pobreza en el Gran Buenos Aires, 1988-1992, Documento de trabajo Nº 2, Buenos Aires: Ministerio de Economía y Obras y Servicios Públicos. Agosto", style="text-align: justify;"),
               h6("INDEC (2016), La medición de la pobreza y la indigencia en Argentina, Metodología INDEC Nº 22, CABA: INDEC. Noviembre", style="text-align: justify;"),               
               hr(), 
               h4(strong(titulo_cita)), 
               h6("Arakaki, A. (2018). “Hacia una serie de pobreza por ingresos de largo plazo. El problema de la canasta”, Realidad Económica, 316. Datos actualizados al 2022.")
             ),
             
             mainPanel( 
               
               tabsetPanel(
                 
                 tabPanel("Gráfico",
                          value = "g_pobreza_eph",
                          
                          box(width = NULL, br(),htmlOutput(ns('titulo1'))), 
                          br(),
                          plotlyOutput(ns('plot'))%>% withSpinner(type = 7, color =paleta_colores[1]),
                          br(),
                          box(title = "Aclaración sobre la construcción de los datos", width = NULL, 
                              h6(nota_aclaratoria_pobreza1),
                              h6(nota_aclaratoria_pobreza2),
                              h6(nota_aclaratoria_pobreza3),
                              h6(nota_aclaratoria_pobreza4),
                              h6(nota_aclaratoria_pobreza5),
                              h6(nota_aclaratoria_pobreza6),
                              h6(nota_aclaratoria_pobreza7)),
                          
                          hr(),
                          br(),
                          box(width = NULL,
                              downloadButton(ns('downloadPlot'),'Descargar gráfico')), 
                          br(),
                          downloadButton(ns('download_database'),'Descargar base completa')
                          
                 ),
                 
                 tabPanel("Tabla",
                          value = "t_pobeza_eph",
                          
                          box(width = NULL, br(),htmlOutput(ns('titulo2'))), 
                          br(),
                          fluidRow(
                            column(12,
                                   column(8, 
                                          box(DTOutput(ns('tabla')), width = NULL)),
                                   column(4,          
                                          box(title = "Aclaración sobre la construcción de los datos", width = NULL, 
                                              h6(nota_aclaratoria_pobreza1),
                                              h6(nota_aclaratoria_pobreza2),
                                              h6(nota_aclaratoria_pobreza3),
                                              h6(nota_aclaratoria_pobreza4),
                                              h6(nota_aclaratoria_pobreza5),
                                              h6(nota_aclaratoria_pobreza6),
                                              h6(nota_aclaratoria_pobreza7)),
                                          hr(),
                                          br(),
                                          box(width = NULL,
                                              downloadButton(ns('downloadTable'),'Descargar tabla'))
                                   )
                            )
                          )
                 ),
                 tabPanel("Tabla Canastas",
                          value = "t_canastas_eph", 
                          box(width = NULL, br(),htmlOutput(ns('titulo3'))), 
                          br(), 
                          fluidRow(
                            column(12,
                                   column(8,
                                          box(DTOutput(ns('tabla_canastas')), width = NULL)
                                   ), 
                                   column(4,
                                          box(width = NULL,
                                              downloadButton(ns('downloadTable_canastas'),'Descargar tabla de canastas'))
                                   )
                            )
                          )
                          
                 )
               )
             )
           )
  )}








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



