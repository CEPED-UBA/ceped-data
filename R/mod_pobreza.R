
pobreza_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    df <-  reactive({
      
      base <- pobreza %>% 
         filter(Serie %in% input$var_serie)  %>% 
         filter(ANO4 >=input$id_periodo[1], ANO4 <= input$id_periodo[2] ) %>% 
         filter(metodologia==input$metodologia)

      base
      
    })
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      
      
      lista_variables <-  paste0(variables, collapse = ", ")
      lista_variables <- sub(",([^,]*)$", " y\\1", lista_variables)  
      titulo <- paste0("<font size='+2'></br>Porcentaje de ",lista_variables ,". Sobre total poblacional.</font>",
                       "</br><font size='+1'>Gran Buenos Aires. Años ", periodo_i, " - ", periodo_f,"</font>")

       }
    
    plot <- function(variables){
      
        
      p <- df() %>% 
          ggplot(
            aes(x = Periodo, y = valor/100, group = Serie, color = Serie,
                text=paste0('</br>valor: ',round(valor,1), '</br>Período: ', Periodo)))+
          geom_line(size = 1) +
          geom_point(size = 2) +
          labs(y = "",
               x = "Año",
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
      ggplotly(p, tooltip = c("text"))%>% 
        layout(font = list(family ="Times New Roman"))
    }
    
    output$titulo1 <- renderText({
      generar_titulo(input$var_serie,input$id_periodo[1],input$id_periodo[2])
    })
    
    output$titulo2 <- renderText({
       generar_titulo(input$var_serie,input$id_periodo[1],input$id_periodo[2])
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
  

    output$downloadTable <- downloadHandler(

      filename = function(){paste("ceped_data_pobreza_",  Sys.Date(), ".xlsx" ,sep='')},
      content = function(file){
        write.xlsx(df(), 
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

pobreza_plot_ui <- function(id, title) {
  ns <- NS(id)
  
  tabPanel(title,
           value = id,
           titlePanel(title),
           
           sidebarLayout(
             sidebarPanel(
               
               selectInput(ns('var_serie'),label = 'Seleccionar series',
                           choices =  unique(pobreza$Serie),
                           selected = c("Indigentes Serie empalmada", "Pobres Serie empalmada"),
                           width = "350px",
                           multiple = T
               ),
               sliderInput(ns('id_periodo'), "Período:",
                           value = c(1974, 2021),
                           min = 1974, 
                           max = 2021,
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
                                          
                                          
                                   ))
                          )
                 )
                 ,

                 tabPanel("Tabla canastas",
                          br(),
                          box(DTOutput(ns('tabla_aglos')), width = NULL)

                 )
                 
                 
                 
                 
                 
                 
                 
                 
                 
               )
               
             )
             
           )
           
           
           
  )
  
  
}

