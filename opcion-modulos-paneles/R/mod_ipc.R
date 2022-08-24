####genero server######

ipc_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    armar_tabla <- function(variables, periodo_i, periodo_f){
      
      tabla <- base_ipc %>% ungroup() %>%
        filter(cod.variable  ==  unique(diccionario_variables$cod.variable[diccionario_variables$nombre.variable == variables]) ) %>% 
        filter(nombre.pais == "Argentina") %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        mutate(ANO4 = paste0(round(ANO4,0))) %>% 
        rename("Serie" = "cod.variable",
               "Período" = "ANO4",
               "País" = "nombre.pais",
               "Valor" = "valor",
               "Variación (%)" = "var")
      
      if (variables == v_ipc[1]) {
        
        tabla <- tabla %>% 
          select(-MES)
        
      } else if (variables == v_ipc[2]) {
        
        tabla <- tabla %>% 
          mutate(MES = paste0(round(MES,0))) %>% 
          rename("Mes"= "MES")
        
      }
      
      
      return(tabla)
      
    }
    
   
    generar_titulo <- function(variables, periodo_i, periodo_f){
      nombre_variable <- unique(diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==variables])
      titulo <- paste0("</br><font size='+2'>",variables,".</font>" ,
                       "</br><font size='+1'>Desde ", periodo_i, " hasta ", periodo_f,"</font>")
    }
    
    generar_titulob <- function(variables, periodo_i, periodo_f){
      nombre_variable <- unique(diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==variables])
      titulo <- paste0("</br><font size='+2'> Variación del ",variables,".</font>" ,
                       "</br><font size='+1'>Desde ", periodo_i, " hasta ", periodo_f,"</font>")
    }
    
    plot <- function(variables, periodo_i, periodo_f){
      
      p <- base_ipc %>% ungroup() %>%
        filter(cod.variable  ==  unique(diccionario_variables$cod.variable[diccionario_variables$nombre.variable == variables])) %>% 
        
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>%
        arrange(ANO4, MES) %>% 
        rowwise() %>% 
        mutate(Per = ifelse(is.na(MES), paste0(ANO4),paste0(ANO4,"-",MES))) %>% 
        ungroup() %>% 
        mutate(id = row_number()) %>% 
        
        
        filter(nombre.pais == "Argentina") %>% 
        ggplot(
          aes(x = fct_reorder(as.factor(Per), id), y = valor, group = nombre.pais
              ,text=paste0('</br>',nombre.pais,'</br>Valor: ',round(valor,1), '</br>Período: ',Per)))+
        geom_line(size = 1, color = paleta_colores[1]) +
        labs(y = "",
             x = "Año")+
        theme_minimal()+
        theme(text = element_text(size = 9),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10),
              legend.position = "none",
              plot.title= element_text(size=12, face="bold"))+
        theme(axis.text.x = element_text(angle = 90))
      
      p
      
    }
    
    
    
    plot_var <- function(variables, periodo_i, periodo_f){
      
      p <- base_ipc %>% ungroup() %>%
        filter(cod.variable  ==  unique(diccionario_variables$cod.variable[diccionario_variables$nombre.variable == variables])) %>% 
        
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        arrange(ANO4, MES) %>% 
        rowwise() %>% 
        mutate(Per = ifelse(is.na(MES), paste0(ANO4),paste0(ANO4,"-",MES))) %>% 
        ungroup() %>% 
        mutate(id = row_number()) %>% 
        
        filter(nombre.pais == "Argentina") %>% 
        ggplot(
          aes(x = fct_reorder(as.factor(Per), id), y = var, group = nombre.pais
              ,text=paste0('</br>',nombre.pais,'</br>Variación: ',round(var,1),'%' ,'</br>Período: ',Per)))+
        geom_col(size = 1, fill = paleta_colores[2]) +
        labs(y = "",
             x = "Año")+
        theme_minimal()+
        theme(text = element_text(size = 9),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10),
              legend.position = "none",
              plot.title= element_text(size=12, face="bold"))+
        theme(axis.text.x = element_text(angle = 90))+
        scale_y_continuous(labels = function(x) paste0(x,"%"))
      
      p

    }

    
    plot_interact <- function(p){
      ggplotly(p, tooltip = c("text"))%>% 
        layout(font = list(family ="Times New Roman"))
    }
    
    generar_metadata <- function(variables){
      diccionario_variables$metadata[diccionario_variables$nombre.variable == variables] 
    }
    output$titulob1 <- renderText({
      generar_titulob(input$var_serie, input$id_periodo[1],input$id_periodo[2])
    })
    output$titulo1 <- renderText({
      generar_titulo(input$var_serie, input$id_periodo[1],input$id_periodo[2])
    })
    output$titulo2 <- renderText({
      generar_titulo(input$var_serie, input$id_periodo[1],input$id_periodo[2])
    })
    output$plot <- renderPlotly({
      plot_interact(plot(input$var_serie, input$id_periodo[1],input$id_periodo[2]))
    })
    output$plot_var <- renderPlotly({
      plot_interact(plot_var(input$var_serie, input$id_periodo[1],input$id_periodo[2]))
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
    output$downloadPlot_var <- downloadHandler(
      filename = function(){paste(input$var_serie,'.png',sep='')},
      content = function(file){
        
        
        ggsave(file,plot=plot_var(input$var_serie, input$id_periodo[1],input$id_periodo[2]), 
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
               sliderInput(ns('id_periodo'), "Período:", value = c(2000,2010), min = min_ipc, max = max_ipc), 
               hr(), 
               h4(strong(titulo_cita)), 
               h5(cita)
             ),
             
             mainPanel( 
               
               tabsetPanel(
                 
                 tabPanel("Gráfico",
                          value = "g_ipc",
                          
               box(width = NULL,br(), htmlOutput(ns('titulob1'))), 
               br(),
               plotlyOutput(ns('plot_var')),
               br(),
               box(width = NULL,
                   downloadButton(ns('downloadPlot_var'),'Descargar gráfico')),
               br(),
               box(width = NULL,br(), htmlOutput(ns('titulo1'))), 
               br(),
               plotlyOutput(ns('plot')),
               br(),
               box(width = NULL,
                   downloadButton(ns('downloadPlot'),'Descargar gráfico')),
               br(),
               box(title = "Metadata", width = NULL, textOutput(ns('metadata1'))),
               br()
               
               
               ),
               
               tabPanel("Tabla",
                        value = "t_ipc",
               
               box(width = NULL,br(), htmlOutput(ns('titulo2'))), 
               br(),
               fluidRow(
                 column(12,
                        column(8, 
                               box(tableOutput(ns('tabla')))),
                        column(4,          
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