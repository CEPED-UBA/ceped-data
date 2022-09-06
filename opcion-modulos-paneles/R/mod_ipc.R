####genero server######

ipc_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    armar_tabla <- function(variables, periodo_i, periodo_f){
      
      tabla <- base_ipc %>% ungroup() %>%
        filter(cod.variable  ==  unique(diccionario_variables$cod.variable[diccionario_variables$nombre.variable == variables]) ) %>% 
        
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        mutate(ANO4 = paste0(round(ANO4,0))) %>% 
        rename("Serie" = "cod.variable",
               "Período" = "ANO4",
               
               "Valor" = "valor",
               "Variación (%)" = "var")
      
      if (variables == v_ipc[1]) {
        
        tabla <- tabla %>% 
          select(-sub)
        
      } else if (variables == v_ipc[2]) {
        
        tabla <- tabla %>% 
         
          rename("Mes"= "sub")
        
      } else if (variables == v_ipc[3]) {
        
        tabla <- tabla %>% 
         
          rename("Trimestre"= "sub") %>% 
          select(-c( "Variación (%)" ))
        
      } else if (variables == v_ipc[4]) {
        
        tabla <- tabla %>% 
         
          rename("Onda"= "sub") %>% 
          select(-c( "Variación (%)" ))
        
      }
      
      tabla <- tabla %>% datatable(rownames = FALSE,
                                   options = list(
                                     searching=FALSE, 
                                     pageLength = 10, 
                                     dom='tip')) %>% 
        formatRound("Valor")
      
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
        arrange(ANO4, sub) %>% 
        rowwise() %>% 
        mutate(Per = ifelse(is.na(sub), paste0(ANO4),paste0(ANO4,"-",sub))) %>% 
        ungroup() %>% 
        mutate(id = row_number()) %>% 
        
        
       
        ggplot(
          aes(x = fct_reorder(as.factor(Per), id), y = valor, group = cod.variable
              ,text=paste0('</br>',cod.variable,'</br>Valor: ',round(valor,1), '</br>Período: ',Per)))+
        geom_line(size = 1, color = paleta_colores[1]) +
        labs(y = "",
             x = "Año")+
        theme_minimal()+
        theme(text = element_text(size = 9),
              axis.text.x = element_text(size=10,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "none",
              plot.title= element_text(size=12, face="bold"))+
        scale_x_discrete(labels = function(x) ifelse((nchar(x)==4|grepl("-6", x)|grepl("-Mayo", x)|grepl("-T1", x)),paste0(x),paste0("") ))
      
      p
      
    }
    
    
    
    plot_var <- function(variables, periodo_i, periodo_f){
      
      p <- base_ipc %>% ungroup() %>%
        filter(cod.variable  ==  unique(diccionario_variables$cod.variable[diccionario_variables$nombre.variable == variables])) %>% 
        
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        arrange(ANO4, sub) %>% 
        rowwise() %>% 
        mutate(Per = ifelse(is.na(sub), paste0(ANO4),paste0(ANO4,"-",sub))) %>% 
        ungroup() %>% 
        mutate(id = row_number()) %>% 
        
       
        ggplot(
          aes(x = fct_reorder(as.factor(Per), id), y = var, group = cod.variable
              ,text=paste0('</br>',cod.variable,'</br>Variación: ',round(var,1),'%' ,'</br>Período: ',Per)))+
        geom_col(size = 1, fill = paleta_colores[2]) +
        labs(y = "",
             x = "Año")+
        theme_minimal()+
        theme(text = element_text(size = 9),
              axis.text.x = element_text(size=10,angle = 90),
              axis.text.y = element_text(size=10),
              legend.position = "none",
              plot.title= element_text(size=12, face="bold"))+
        scale_y_continuous(labels = function(x) paste0(x,"%"))+
        scale_x_discrete(labels = function(x) ifelse((nchar(x)==4|grepl("-6", x)),paste0(x),paste0("") ))
      
      p

    }

    
    plot_interact <- function(p){
      ggplotly(p, tooltip = c("text"))%>% 
        layout(font = list(family ="Tisub New Roman"))
    }
    
    generar_metadata <- function(variables){
      diccionario_variables$metadata[diccionario_variables$nombre.variable == variables] 
    }
    
    
    observe({
      x <- input$var_serie
      
      if (x == v_ipc[4]) {
        up_min <- min_ondas
        up_max <- max_ondas
        up_value <- c(min_ondas,max_ondas)
        
      } else if (x == v_ipc[3]) {
        up_min <- min_trim
        up_max <- max_trim
        up_value <- c(min_trim,max_trim)
        
      } else if (x %in% c(v_ipc[1],v_ipc[2])) {
       up_min <- min_ipc
       up_max <- max_ipc
       up_value <- c(2000,2010)
      
        
         
      }
     
      
      
      updateSliderInput(session, 'id_periodo',
                        value = up_value, 
                        min = up_min, 
                        max = up_max
                        
      )
    })
    
    
    observe({
      if (input$var_serie %in% c(v_ipc[1],v_ipc[2])) {
        
        output$titulob1 <- renderText({
          generar_titulob(input$var_serie, input$id_periodo[1],input$id_periodo[2])
        })
     
       
       
       
        output$plot_var <- renderPlotly({
          plot_interact(plot_var(input$var_serie, input$id_periodo[1],input$id_periodo[2]))
        })
        output$downloadPlot <- downloadHandler(
          filename = function(){paste(input$var_serie,'.png',sep='')},
          content = function(file){
            
            
            ggsave(file,plot=plot(input$var_serie, input$id_periodo[1],input$id_periodo[2]), 
                   width=8, height=4)
          }
        )
      
      } else if (input$var_serie %in% c(v_ipc[3],v_ipc[4])) {
        
        output$titulob1 <- NULL
       
        
        output$plot_var <- NULL
        
        output$downloadPlot <- downloadHandler(
          filename = function(){paste(input$var_serie,'.png',sep='')},
          content = function(file){
            
            
            ggsave(file,plot=plot(input$var_serie, input$id_periodo[1],input$id_periodo[2]), 
                   width=8, height=4)
          }
        )
        
        
      }
      
    })
    
    output$downloadPlot_var <- renderUI({
      if(input$var_serie %in% c(v_ipc[1],v_ipc[2])) {
        downloadButton('ipc-downloadPlot_var_b','Descargar gráfico')
      }
    })
    
    output$downloadPlot_var_b <- downloadHandler(
      filename = function(){paste(input$var_serie,'.png',sep='')},
      content = function(file){


        ggsave(file,plot=plot_var(input$var_serie, input$id_periodo[1],input$id_periodo[2]),
               width=8, height=4)
      }
    )
    
    output$titulo2 <- renderText({
      generar_titulo(input$var_serie, input$id_periodo[1],input$id_periodo[2])
    })
    
    output$titulo1 <- renderText({
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
                          
               
               box(width = NULL,br(), htmlOutput(ns('titulo1'))), 
               br(),
               plotlyOutput(ns('plot'))%>% withSpinner(type = 7, color =paleta_colores[1]),
               br(),
               box(width = NULL,
                   downloadButton(ns('downloadPlot'),'Descargar gráfico')),
               br(),
               box(title = "Metadata", width = NULL, textOutput(ns('metadata1'))),
               br(),
               box(width = NULL,br(), htmlOutput(ns('titulob1'))), 
               br(),
               plotlyOutput(ns('plot_var'))%>% withSpinner(type = 7, color =paleta_colores[2]),
               br(),
               box(width = NULL,
                  
                   uiOutput(ns('downloadPlot_var'))
                   ),
               br()
               
               
               ),
               
               tabPanel("Tabla",
                        value = "t_ipc",
               
               box(width = NULL,br(), htmlOutput(ns('titulo2'))), 
               br(),
               fluidRow(
                 column(12,
                        column(8, 
                               box(DTOutput(ns('tabla')), width = NULL)),
                        column(4,          
                               box(title = "Metadata", width = NULL, textOutput(ns('metadata2'))),
                               br(),
                               box(width = NULL,
                                   downloadButton(ns('downloadTable'),'Descargar tabla')
                                   )
                               
                               
                        ))
               )
                 )
               
             )
               
             )
             
           )
           
  
           
  )
  
  
}