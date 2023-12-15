####genero server######

salarios_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    armar_tabla <- function(variables, periodo_i, periodo_f,descarga){
      tabla <- salarios %>%
        #filter(cod.variable  %in%   variables) %>% 
        #filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        rename("Serie" = "cod.variable",
               "País" = "nombre.pais",
               "Período" = "ANO4") %>% 
        select("País","iso3c","Período","Serie", "valor") 
      
      if (descarga == FALSE) {
        tabla <- tabla %>%   
          datatable(rownames = FALSE,
                    options = list(
                      searching=FALSE, 
                      pageLength = 10, 
                      dom='tip'))          %>% 
          formatRound("valor")
      } else {
        tabla <- tabla
      }
      
      return(tabla)
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      nombre_variable <-  paste0(variables, collapse = ", ")
      nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)   
      titulo <- paste0("</br><font size='+2'>",nombre_variable ,
                       "</br><font size='+1'>Desde ", periodo_i, " hasta ", periodo_f,"</font>")
    }
   
    
    plot <- function(variables,paises, periodo_i, periodo_f){
      
      p <- salarios %>%
        filter(cod.variable ==  variables) %>% 
        filter(nombre.pais %in% paises) %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        ggplot(
          aes(x = ANO4, y = valor,group = nombre.pais, color = nombre.pais,
          text=paste0('</br>Pais: ',nombre.pais,'</br>valor: ',round(valor,2), '</br>Período: ',ANO4)))+
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
      generar_titulo(input$variables_serie, input$id_periodo[1],input$id_periodo[2])
    })
    output$titulo2 <- renderText({
      generar_titulo(input$variables_serie, input$id_periodo[1],input$id_periodo[2])
    })
    output$plot <- renderPlotly({
      plot_interact(plot(diccionario_variables$cod.variable[diccionario_variables$nombre.variable %in% input$variables_serie],input$pais_id ,input$id_periodo[1],input$id_periodo[2]))
    })
    
    output$tabla <- renderDT({
      armar_tabla(diccionario_variables$cod.variable[diccionario_variables$nombre.variable %in% input$variables_serie], input$id_periodo[1],input$id_periodo[2],descarga = F)
    })
    
    output$metadata1 <- renderText({
"Las series presentadas corresponden al salario doblemente bruto promedio que surge de las estimaciones de Cuentas Nacionales de los respectivos países. El salario promedio surge del cociente entre la masa salarial anual y la cantidad de asalariados promedio del año. Los valores están expresados en términos mensuales y convertidos a dólares de paridad de poder adquisitivo constante del año 2017, tomando los coeficientes de Paridad de Poder Adquisitivo (PPA) estimados en la ronda 2017 del Programa de Comparación Internacional del Banco Mundial y utilizando los Índices de Precios al Consumidor de cada país para obtener coeficientes PPA para los restantes años."
        })
    output$metadata2 <- renderText({
      generar_metadata(input$variables_serie)
    })
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste(diccionario_variables$cod.variable[diccionario_variables$nombre.variable %in% input$variables_serie][1],'.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(diccionario_variables$cod.variable[diccionario_variables$nombre.variable %in% input$variables_serie],input$id_periodo[1],input$id_periodo[2],descarga = T), 
                   file)    }
    )
    
    output$download_database <- downloadHandler(
      
      filename = function(){paste("database",'.xlsx',sep='')},
      content = function(file){
        
        salarios_export<- salarios %>% 
          left_join(diccionario_variables) %>% 
          select(-cod.variable)
        
        write.xlsx(list("Base_completa" =salarios_export),
                   file)    
      }
    )
    
    
    output$downloadPlot <- downloadHandler(
      filename = function(){paste(diccionario_variables$cod.variable[diccionario_variables$nombre.variable %in% input$variables_serie][1],'.png',sep='')},
      content = function(file){
        
        
        ggsave(file,plot=plot(diccionario_variables$cod.variable[diccionario_variables$nombre.variable %in% input$variables_serie],input$pais_id,input$id_periodo[1],input$id_periodo[2]), 
               width=8, height=4)
      }
    )
    
    
    
  })
}


####genero ui########

diccionario_variables$nombre.variable[diccionario_variables$cod.variable %in%  unique(salarios$cod.variable)]

salarios_plot_ui <- function(id, title,v_variables) {
  ns <- NS(id)

    tabPanel(title = title,
             value = id,
             titlePanel(title),
             sidebarLayout(
               sidebarPanel(
  #               conditionalPanel(
  #                 condition =  "input.tabs == 'Gráfico'",
  #                 ns = NS(id),
                 
                 selectInput(ns('variables_serie'),label = 'Seleccionar Serie',
                             choices =  unique(diccionario_variables$nombre.variable[diccionario_variables$cod.variable %in%  unique(salarios$cod.variable)]),
                             selected = unique(diccionario_variables$nombre.variable[diccionario_variables$cod.variable %in%  unique(salarios$cod.variable)])[1],
                             width = "300px",
                             multiple = F
                             ),
                 selectInput(ns('pais_id'),label = 'Elegir paises',
                             choices =  unique(salarios$nombre.pais),
                             selected = unique(salarios$nombre.pais)[1:8],
                             width = "300px",
                             multiple = T
                             ),
                 sliderInput(ns('id_periodo'),
                             "Período:",
                             value = c(1970,max_salarios),
                             min = min_salarios, 
                             max = max_salarios,
                             sep=""
                             ), 
                 hr(), 
                 h4(strong(titulo_cita)), 
                 h5(a(href = 'https://drive.google.com/file/d/18A1MMN83ZclgX8_5HuFXD8HevJhj6UEB/view', cita_salarios, .noWS = "outside"),style="text-align: justify;"),
                 h5(recuperado_de, style="text-align: justify;"),
                 hr(), 
                 h4(strong("Documento metodológico: ")), 
                 h5(a(href = 'https://drive.google.com/file/d/18A1MMN83ZclgX8_5HuFXD8HevJhj6UEB/view', 'Documento de Trabajo N°28 del CEPED', .noWS = "outside"))
             
                 ),
               mainPanel(
                 
                 tabsetPanel(type = "tabs", id = "tabs",
                   
                   tabPanel(title = "Gráfico",
                            value = "Gráfico",
                            br(),
                            box(width = NULL, htmlOutput(ns('titulo1'))),
                            br(),
                            plotlyOutput(ns('plot'))%>% withSpinner(type = 7, color =paleta_colores[1]),
                            br(),
                            box(title = "Aclaración sobre la construcción de los datos", width = NULL,
                                htmlOutput(ns('metadata1'),style = "text-align: justify")),
                            br(),
                            box(width = NULL,
                                downloadButton(ns('downloadPlot'),'Descargar gráfico')),
                            br(),
                            downloadButton(ns('download_database'),'Descargar base completa')
                   
                   ),
                   
                   tabPanel(title = "Tabla",
                            
                            fluidRow(
                              column(12,
                                     column(9, 
                                            br(),
                                            box(width = NULL, htmlOutput(ns('titulo2'))),
                                            br(),
                                            box(DTOutput(ns('tabla')), width = NULL)),
                                     column(3,          
                                            box(title = "Aclaración sobre la construcción de los datos", width = NULL, htmlOutput(ns('metadata2'),style = "text-align: justify")),
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