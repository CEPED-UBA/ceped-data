####genero server######

df_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$dicc <- renderDT({diccionario_dt24 %>% 
        
        datatable(rownames = FALSE,filter = "none",
                  options = list(
                    searching=FALSE, 
                    dom='tip'))
      })
    
     
    armar_tabla <- function(sector_c,serie_c, periodo_i, periodo_f,descarga){
      tabla <- base_dt24 %>%
        filter(variable %in% serie_c,
               sector %in% sector_c,
               Anio %in% c(periodo_i:periodo_f)) %>% 
        rename("Período" = "Anio") %>% 
        mutate(valor = round(valor)) %>% 
        select("variable","sector","valor","Período") %>% 
        pivot_wider(names_from = "Período",values_from = "valor") 
      
      if (descarga == FALSE) {
        tabla <- tabla %>% 
          datatable(rownames = FALSE,filter = "top",
                    options = list(
                      searching=FALSE, 
                      dom='tip')) #%>% 
        #        formatRound("valor")
      } else {
        tabla <- tabla
      }
      
      
      
      return(tabla)
    }
    
    generar_titulo <- function(sector_c,serie_c, periodo_i, periodo_f){
      nombre_variable <-  paste0(str_to_title(serie_c), collapse = ", ")
      nombre_sector <-  paste0(str_replace_all(sector_c,"\\."," "), collapse = ", ")
      nombre_sector <- sub(",([^,]*)$", " y\\1", nombre_sector)   
      titulo <- paste0("</br><font size='+2'>",nombre_variable , " ",nombre_sector,".</font>",
                       "</br><font size='+1'>Desde ", periodo_i, " hasta ", periodo_f,".</font>")
    }
    

    
    
        
    plot <- function(tipo.grafico,sector_c,serie_c, periodo_i, periodo_f){
      
      if(tipo.grafico == "Columnas horizontales"){
        tipo  <- geom_col(position = position_dodge())
      }
      if(tipo.grafico == "Linea"){
        tipo  <- geom_line()
      }
      if(tipo.grafico == "Columnas apiladas"){
        tipo  <- geom_col(position = position_stack())
      }
      


      options(scipen = 999)
      
      
      p <- base_dt24 %>%
        filter(variable %in% serie_c,
               sector %in% sector_c,
               Anio %in% c(periodo_i:periodo_f)) %>% 
        ggplot(aes(x= Anio,
                   y = valor,
                   fill = sector,
                   color = sector,
                   group = sector,
                   text=paste0('</br>',sector,'</br>Valor: ',number(valor,big.mark = ".",decimal.mark = ","),
                               '</br>Período: ',Anio)))+
        tipo+
        theme_tufte()+
        theme(axis.title.x = element_blank(),
              panel.grid.minor = element_line(),
              legend.title =  element_blank(),
              panel.grid.major.y = element_line(colour = "grey90"),
              legend.position = "bottom")+
        scale_x_continuous(breaks = seq(periodo_i,periodo_f,2),
                           labels = seq(periodo_i,periodo_f,2))+
        scale_y_continuous(labels =number_format(big.mark = ".",
                                                 decimal.mark = ","))+
        scale_color_manual(values =paleta_colores_extendida)+
        scale_fill_manual(values =paleta_colores_extendida)
      
      p
      #ggplotly(p, tooltip = c("text"))
    }
    
    plot_interact <- function(p){
      ggplotly(p, tooltip = c("text"))%>% 
        layout(font = list(family ="Times New Roman"))
    }
    
    # generar_metadata <- function(variables){
    #   diccionario_variables$metadata[diccionario_variables$nombre.variable == variables] 
    # }
    
     output$titulo1 <- renderText({
      generar_titulo(input$sector, input$serie, input$id_periodo[1],input$id_periodo[2])
    })
    output$plot <- renderPlotly({
      plot_interact(plot(input$tipo_graf,input$sector,input$serie, input$id_periodo[1],input$id_periodo[2]))
    })
    output$metadata1 <- renderText({
"La serie elaborada por el CEPED es la correspondiente al período 1994-2015: entre 1994 y 2003 por el empalme de las series de las bases de Cuentas Nacionales; entre 2005-2015 corresponde a la estimación realizada en el CEPED ante el faltante de información oficial. Desde el año 2016 en adelante se trata de la información oficial, con la particularidad que se estima la desagregación por tipo de vínculo laboral. También se presenta el salario real, utilizando el IPC empalmado por el CEPED."
      })
    
    output$tabla <- renderDT({
      armar_tabla(input$sector, input$serie,input$id_periodo[1],input$id_periodo[2],descarga = F) 
      
    })
    output$downloadTable <- downloadHandler(
      filename = function(){paste("Datos_Completos",'.xlsx',sep='')},
      content = function(file){
        write.xlsx(base_export_dt24,
                   file)
      }
    )   
    
    output$downloadPlot <- downloadHandler(
      filename = function(){paste(input$serie[1],'.png',sep='')},
      content = function(file){
        
        
        ggsave(file,plot=plot(input$tipo_graf,input$sector,input$serie, input$id_periodo[1],input$id_periodo[2]), 
               width=8, height=4)
      }
    )
    
    
    
  })
}


####genero ui########

df_plot_ui <- function(id, title,v_variables) {
  ns <- NS(id)
  
  tabPanel(title,
           value = id,
           titlePanel(title),
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('serie'),label =  "Elegir Variable: ",
                           choices = unique(base_dt24$variable),
                           selected = base_dt24$variable[1],
                           multiple = FALSE,
                           width = 400
                           ),
               selectInput(ns('tipo_graf'),
                           label =  "Tipo de Grafico: ",
                           choices = c("Linea","Columnas apiladas","Columnas horizontales"),
                           multiple = F,
                           selected = "Linea"
                           ),
               selectizeInput(ns('sector'),
                              label =  "Elegir sector: ",
                              choices = unique(base_dt24$sector),
                              selected = unique(base_dt24$sector)[1],
                              width = "300px",
                              multiple = T
                              ),
               sliderInput(ns('id_periodo'),
                           "Período:",
                           value = c(1993,2020),
                           min = 1993, 
                           max = 2020,
                           sep=""
                           ),
               hr(),
               
#               p(style="text-align: justify;","La metodología detallada de estimación de las distintas variables se encuentra en el ",
#                 a(href = 'http://bibliotecadigital.econ.uba.ar/econ/collection/docin/document/docin_ceped_d_024', 'Documento de Trabajo N°24 del CEPED', .noWS = "outside"), 
#                 .noWS = c("after-begin", "before-end")), 
               h4(strong(titulo_cita)), 
               h5(cita, style="text-align: justify;"),
               h5(doi, style="text-align: justify;"),
               h4(strong("Documento metodológico: ")), 
               h5(a(href = 'http://bibliotecadigital.econ.uba.ar/econ/collection/docin/document/docin_ceped_d_024', 'Documento de Trabajo N°24 del CEPED', .noWS = "outside"))
             ),
             mainPanel(
               
               tabsetPanel(
                 tabPanel("Gráfico",
                          value = "g_df",
                          
                          box(width = NULL,br(), htmlOutput(ns('titulo1'))),
                          br(),
                          plotlyOutput(ns('plot'))%>% withSpinner(type = 7, color =paleta_colores[1]),
                          br(),
                          box(title = "Aclaración sobre la construcción de los datos", width = NULL, htmlOutput(ns('metadata1'),style = "text-align: justify")),
                          br(),
                          box(width = NULL,
                              downloadButton(ns('downloadPlot'),'Descargar gráfico')),
                          br(),
                          box(width = NULL,downloadButton(ns('downloadTable'),'Descargar base completa'))
                          
                          
                 ),
                 tabPanel(title = "Diccionario",
                          value = "d_df",
                          dataTableOutput(ns("dicc"))
                          ),
                 tabPanel("Tabla",
                          value = "d_df",
                          box(DTOutput(ns('tabla')), width = NULL),
                          br(),
                          box(title = "Aclaración sobre la construcción de los datos", width = NULL, htmlOutput(ns('metadata2'),style = "text-align: justify"))
                          )

               )
               
             )
             
           )
           
  )
  
  
}