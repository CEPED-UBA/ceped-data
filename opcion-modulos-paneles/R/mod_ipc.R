####genero server######

ipc_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    armar_tabla <- function(variables, periodo_i, periodo_f){
      base_binded %>% ungroup() %>%
        filter(cod.variable  ==  unique(diccionario_variables$cod.variable[diccionario_variables$nombre.variable == variables]) ) %>% 
        filter(nombre.pais == "Argentina") %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        mutate(ANO4 = round(ANO4,0)) %>% 
        rename("Serie" = "cod.variable",
               "Período" = "ANO4",
               "País" = "nombre.pais") %>% 
        select(-iso3c)
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      nombre_variable <- unique(diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==variables])
      titulo <- paste0("<font size='+2'>",variables ," desde ", periodo_i, " hasta ", periodo_f,"</font>")
    }
    
    plot <- function(variables, periodo_i, periodo_f){
      
      p <- base_binded %>% ungroup() %>%
        filter(cod.variable  ==  unique(diccionario_variables$cod.variable[diccionario_variables$nombre.variable == variables])) %>% 
        
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        filter(nombre.pais == "Argentina") %>% 
        ggplot(
          aes(x = as.factor(ANO4), y = valor, group = nombre.pais, color = nombre.pais
              ,text=paste0('</br>',nombre.pais,'</br>valor: ',round(valor,1), '</br>Período: ',ANO4)))+
        geom_line(size = 1, color = "blue") +
        labs(color= "País",
             y = "",
             x = "Año")+
        theme_minimal()+
        theme(text = element_text(size = 9),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10),
              legend.position = "none",
              plot.title= element_text(size=12, face="bold"))+
        theme(axis.text.x = element_text(angle = 90))
      
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

ipc_plot_ui <- function(id, title,v_variables) {
  ns <- NS(id)
  
  tabPanel(title,
           value = id,
           titlePanel(title),
           
                   
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('var_serie'),label = 'Seleccionar una Serie',
                           choices =  v_variables, 
                           selected = v_variables[1],
                           width = "300px",
                           multiple = F),
               sliderInput(ns('id_periodo'), "Período:", value = c(1980,2005), min = 1950, max = 2010)
             ),
             
             mainPanel( 
               
               tabsetPanel(
                 
                 tabPanel("Gráfico",
                          value = "g_ipc",
                          
               box(width = NULL,br(), htmlOutput(ns('titulo1'))), 
               br(),
               plotlyOutput(ns('plot')),
               br(),
               box(title = "Metadata", width = NULL, textOutput(ns('metadata1'))),
               br(),
               box(width = NULL,
                   downloadButton(ns('downloadPlot'),'Descargar gráfico'))
               
               ),
               
               tabPanel("Tabla",
                        value = "t_ipc",
               
               box(width = NULL,br(), htmlOutput(ns('titulo2'))), 
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