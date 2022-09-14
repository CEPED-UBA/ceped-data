####genero server######

bp_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    df <-  reactive({
      Agrupam <- get(input$desagregacion)
    })
    observe({
      df <- df()
      # dataframe <- input$df
      updateSelectizeInput(session, 
                           inputId = "variables_serie",
                           label = "Seleccionar Series", 
                           choices = unique(df$codigo_y_variable),
                           selected = unique(df$codigo_y_variable)[1])
    })
    
    
    armar_tabla <- function(valu, variable,periodo_i, periodo_f,descarga){
      tabla <- df() %>%
        filter(codigo_y_variable == variable) %>% 
        filter(valuacion == valu) %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        rename("Período" = "ANO4",
               "Valuación" = "valuacion") %>% 
        mutate(valor = round(valor)) %>% 
        select("codigo_y_variable","Valuación", "valor","Período") %>% 
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
    
    generar_titulo <- function(variables, valu, periodo_i, periodo_f){
      nombre_variable <-  paste0(variables, collapse = ", ")
      nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)   
      titulo <- paste0("</br><font size='+2'>",nombre_variable , " en millones de ",valu,".</font>",
                       "</br><font size='+1'>Desde ", periodo_i, " hasta ", periodo_f,".</font>")
    }
   
    
    plot <- function(tipog,variables,valu, periodo_i, periodo_f){
      
      if(tipog== "Barra apilada"){
        tipo  <- geom_col()
      }
      if(tipog  == "Linea"){
        tipo  <- geom_line()
      }
      
      
      p <- df() %>%
        filter(codigo_y_variable  %in%   variables,valuacion == valu) %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        ggplot(
          aes(x = as.factor(ANO4), y = valor,group = cod.variable, color = cod.variable,fill = cod.variable,
              text=paste0('</br>valor: ',round(valor,1), '</br>Período: ',ANO4)))+
        tipo+
        labs(y = "",
             x = "Año",
             color = "")+
        theme_minimal()+
        theme(text = element_text(size = 9),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",
              plot.title= element_text(size=12, face="bold"))+
        theme(axis.text.x = element_text(angle = 90))+
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
      generar_titulo(paste0(unique(
        df()$cod.variable[df()$codigo_y_variable %in% input$variables_serie]),
        collapse = " - "),
                    #input$variables_serie,
                     input$valuacion,
                     input$id_periodo[1],
                     input$id_periodo[2])
    })
    output$titulo2 <- renderText({
      generar_titulo(input$variables_serie,input$valuacion, input$id_periodo[1],input$id_periodo[2])
    })
    output$plot <- renderPlotly({
      plot_interact(plot(input$tipo_graf,input$variables_serie,input$valuacion, input$id_periodo[1],input$id_periodo[2]))
    })
    
    output$tabla <- renderDT({
      armar_tabla(input$valuacion,input$variables_serie, input$id_periodo[1],input$id_periodo[2],descarga = F) 
        
    })
    output$diccionario_bp <- renderDataTable({
      bop_dolares_diccionario %>% 
        datatable(rownames = FALSE,
                  options = list(
                    
                  ))
      })
    
    output$diccionario_bp_sectores <- renderDataTable({
      bop_sectores_diccionario  %>% 
        datatable(rownames = FALSE,
                  options = list(
                    
                  ))
    })
    
    output$diccionario_bp_aclara <- renderDataTable({
      bop_dolares_diccionario_aclaracion %>% 
        datatable(rownames = FALSE,
                  options = list(
                    searching=FALSE,
                    lengthChange = FALSE,
                    dom='tip'))
    })
    
 output$metadata1 <- renderText({
  
"La compatibilización de las series de las metodologías 2017 y 2007 del INDEC y su clasificación por sector institucional (público y privado) a partir de una reclasificación propia de las partidas se encuentra detallada en el Documento de Trabajo 25 del CEPED. Para la presentación de los datos en dólares constantes se utilizó el Índice de Precios al Consumidor (CPI-U) publicado por el Bureau of Labor Statistics (BLS). Estos datos están expresados manteniendo el poder adquisitivo constante del último año de la serie"
   
 })
    # output$metadata2 <- renderText({
    #   generar_metadata(input$var_serie)
    # })
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste(input$variables_serie[1],'.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(input$variables_serie,input$valuacion, input$id_periodo[1],input$id_periodo[2],descarga = T), 
                   file)    }
    )
    output$download_database <- downloadHandler(
      
      filename = function(){paste("database",'.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(list("Cuentas y partidas" = bop_arg_dolares,
                        "Resultado Sector " = bop_sectores), 
                   file)    
        }
    )
    
    output$downloadPlot <- downloadHandler(
      filename = function(){paste(input$variables_serie[1],'.png',sep='')},
      content = function(file){
        
        
        ggsave(file,plot=plot(input$variables_serie,input$valuacion, input$id_periodo[1],input$id_periodo[2]), 
               width=8, height=4)
      }
    )
    
    
    
  })
}


####genero ui########

bp_plot_ui <- function(id, title,v_variables) {
  ns <- NS(id)

    tabPanel(title,
             value = id,
             titlePanel(title),
             sidebarLayout(
               sidebarPanel(
                 selectInput(ns('desagregacion'),label =  "Elegir base de datos: ",
                             choices = c("Cuentas y partidas (presentación con enfoque de saldo)" = 'bop_arg_dolares',
                                         "Resultado por Sector Institucional (Público y Privado)" = 'bop_sectores'),
                             selected = "Partidas desagregadas",
                             width = "300px",
                             multiple = F
                 ),
                 selectInput(ns('tipo_graf'),label =  "Tipo de Grafico: ",
                              choices = c("Linea","Barra apilada"),
                              selected = "Linea",
                              width = "300px",
                              multiple = F
                             ),
                 selectInput(ns('variables_serie'),label = 'Seleccionar Series',
                             choices =  unique(bop_arg_dolares$codigo_y_variable),
                             selected = unique(bop_arg_dolares$codigo_y_variable)[5:8],
                             width = "300px",
                             multiple = T
                 ),
                 selectInput(ns('valuacion'),label = 'Elegir valuacion',
                             choices =  unique(bop_arg_dolares$valuacion),
                             selected = unique(bop_arg_dolares$valuacion)[1],
                             width = "300px",
                             multiple = F
                             ),
                 sliderInput(ns('id_periodo'),
                             "Período:",
                             value = c(1992,2020),
                             min = 1992, 
                             max = 2020,
                             sep=""
                             ),
                 hr(), 
                 h4(strong(titulo_cita)), 
                 h5(cita, style="text-align: justify;"),
                 hr(),
                 h4(strong("Documento metodológico: ")), 
                 h5(a(href = 'https://drive.google.com/file/d/1m2yybwPTtkJTmtdnWH5DisY9-x1mQ02f/view', 'Documento de Trabajo N°25 del CEPED', .noWS = "outside"))
                 ),
               mainPanel(
                 
                 tabsetPanel(
                   tabPanel("Gráfico",
                            value = "g_bp",
                            
                            box(width = NULL,br(), htmlOutput(ns('titulo1'))),
                            br(),
                            plotlyOutput(ns('plot'))%>% withSpinner(type = 7, color =paleta_colores[1]),
                            br(),
                            box(title = "Aclaración sobre la construcción de los datos", width = NULL, htmlOutput(ns('metadata1'),style = "text-align: justify")),
                            br(),
                            box(width = NULL,
                                downloadButton(ns('downloadPlot'),'Descargar gráfico')),
                            br(),
                            box(width = NULL,downloadButton(ns('download_database'),'Descargar base completa'))
                            
                   ),
                   tabPanel("Tabla",
                            value = "t_bp",
                            box(DTOutput(ns('tabla')), width = NULL),
                            br(),
                            box(title = "Metadata", width = NULL, htmlOutput(ns('metadata2'),style = "text-align: justify")),
                            br(),
                            box(width = NULL,downloadButton(ns('downloadTable'),'Descargar tabla'))
                            ),
                   tabPanel("Diccionario Cuentas y Partidas",
                            value = "d_bp",
                            
                            fluidRow(
                              column(width = 12, 
                                     box(DTOutput(ns('diccionario_bp_aclara')),
                                         width = 12),
                                     br(),
                                     box(DTOutput(ns('diccionario_bp')),
                                         width = 12)
                              ))),

                   tabPanel("Diccionario Sectores Institucionales",
                            value = "d_bp2",
                            
                            fluidRow(
                              column(width = 12, 
                                     box(DTOutput(ns('diccionario_bp_sectores')),
                                         width = 12)
                              )))
                 )
                 
               )
             
           )
           
  )
           

}