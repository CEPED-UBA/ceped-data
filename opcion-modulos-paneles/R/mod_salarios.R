####genero server######

salarios_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    armar_tabla <- function(variables, periodo_i, periodo_f){
      salarios %>%
        #filter(cod.variable  %in%   variables) %>% 
        #filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        rename("Serie" = "cod.variable",
               "País" = "nombre.pais",
               "Período" = "ANO4") %>% 
        select("País","iso3c","Período","Serie", "valor")
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
    
    output$tabla <- renderTable({
      armar_tabla(diccionario_variables$cod.variable[diccionario_variables$nombre.variable %in% input$variables_serie], input$id_periodo[1],input$id_periodo[2])
    })
    
    output$metadata1 <- renderText({
      generar_metadata(input$variables_serie)
    })
    output$metadata2 <- renderText({
      generar_metadata(input$variables_serie)
    })
    
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste(diccionario_variables$cod.variable[diccionario_variables$nombre.variable %in% input$variables_serie][1],'.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(diccionario_variables$cod.variable[diccionario_variables$nombre.variable %in% input$variables_serie],input$id_periodo[1],input$id_periodo[2]), 
                   file)    }
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
                             value = c(1970,2018),
                             min = 1955, 
                             max = 2020
                             ), 
                 hr(), 
                 h4(strong(titulo_cita)), 
                 h5(cita)
                 ),
               mainPanel(
                 
                 tabsetPanel(type = "tabs", id = "tabs",
                   
                   tabPanel(title = "Gráfico",
                            value = "Gráfico",
                            br(),
                            box(width = NULL, htmlOutput(ns('titulo1'))),
                            br(),
                            plotlyOutput(ns('plot')),
                            br(),
                            box(title = "Metadata", width = NULL, textOutput(ns('metadata1'))),
                            br(),
                            box(width = NULL,
                                downloadButton(ns('downloadPlot'),'Descargar gráfico'))
                   ),
                   
                   tabPanel(title = "Tabla",
                            
                            fluidRow(
                              column(12,
                                     column(9, 
                                            br(),
                                            box(width = NULL, htmlOutput(ns('titulo2'))),
                                            br(),
                                            box(tableOutput(ns('tabla')))),
                                     column(3,          
                                            box(title = "Metadata", width = NULL, textOutput(ns('metadata2'))),
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