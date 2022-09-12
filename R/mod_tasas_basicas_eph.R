
tasas_basicas_eph_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    df <-  reactive({
      
      if(input$id_periodicidad == "Promedio anual"){
        
        base <- tasas_basicas_eph  %>%
          filter(cod.variable  %in%   input$var_serie) %>% 
          filter(ANO4 %in% c(input$id_periodo[1]:input$id_periodo[2])) %>% 
          group_by(ANO4, cod.variable) %>% 
          mutate(valor=mean(valor, na.rm=TRUE)) %>% 
          rename("Serie" = "cod.variable",
                 "Pais" = "nombre.pais",
                 "Periodo"= "ANO4") %>% 
          select("Pais","iso3c", "Periodo","Serie", "valor") %>% 
          unique()                                                   
      }
      
      if(input$id_periodicidad == "Trimestral/Onda"){
        
        base <-  tasas_basicas_eph  %>%
          filter(cod.variable  %in%   input$var_serie) %>% 
          filter(ANO4 %in% c(input$id_periodo[1]:input$id_periodo[2])) %>% 
          rename("Serie" = "cod.variable",
                 "Pais" = "nombre.pais",
                 "Periodo" = "ANO4.trim") %>% 
          select("Pais","iso3c","Periodo","Serie", "valor")
      }
      
      base
      
    })
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      
      
      lista_variables <-  paste0(variables, collapse = ", ")
      lista_variables <- sub(",([^,]*)$", " y\\1", lista_variables)  
      titulo <- paste0("<font size='+2'></br>",lista_variables ,".</font>",
                       "</br><font size='+1'>Años ", periodo_i, " - ", periodo_f,"</font>")

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
          scale_y_continuous(labels=scales::percent)
      p
      
    }
    
    
    plot_interact <- function(p){
      ggplotly(p, tooltip = c("text"))%>% 
        layout(font = list(family ="Times New Roman"))
    }
    
    # generar_metadata <- function(variables){
    # 
    # i <- length(variables)  
    #     
    # paste0( variables[1:i], ": ", diccionario_variables$metadata[diccionario_variables$nombre.variable %in% variables[1:i]])
    #   
    # }
    
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
    

      # output$metadata1 <- renderText({
    #   generar_metadata(input$var_serie)
    # })
    # output$metadata2 <- renderText({
    #   generar_metadata(input$var_serie)
    # })
    
    
    output$downloadTable <- downloadHandler(

      filename = function(){paste(input$var_serie[1],'.xlsx',sep='')},
      content = function(file){

        write.xlsx(armar_tabla(input$var_serie, input$id_periodo[1],input$id_periodo[2]),
                   file)    }
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

tasas_basicas_eph_plot_ui <- function(id, title) {
  ns <- NS(id)
  
  tabPanel(title,
           value = id,
           titlePanel(title),
           
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('id_periodicidad'),label = 'Tipo de infromación:',
                           choices =  c("Promedio anual", "Trimestral/Onda"),
                           selected = "Promedio Anual",
                           width = "300px",
                           multiple = F
               ),
               selectInput(ns('var_serie'),label = 'Seleccionar una serie:',
                           choices =  unique(tasas_basicas_eph$cod.variable),
                           selected = unique(tasas_basicas_eph$cod.variable),
                           width = "300px",
                           multiple = T
               ),
               sliderInput(ns('id_periodo'), "Período:",
                           value = c(1995,2021),
                           min = 1995, 
                           max = 2021,
                           sep=""
               ), 
               hr(), 
               h4("Nota aclaratoria"), 
              p(nota_aclaratoria_eph1, style="text-align: justify;"),
               p(nota_aclaratoria_eph2, style="text-align: justify;"),
               hr(),  
               h4(strong(titulo_cita)), 
               p(cita, style="text-align: justify;")
             ),
             
             mainPanel( 
               
               tabsetPanel(
                 
                 tabPanel("Gráfico",
                          value = "g_tasas_basicas_eph",
                          
                          box(width = NULL, br(),htmlOutput(ns('titulo1'))), 
                          br(),
                          plotlyOutput(ns('plot'))%>% withSpinner(type = 7, color =paleta_colores[1]),
                          br(),
                          #box(title = "Metadata", width = NULL, textOutput(ns('metadata1'))),
                          box(title = "Metadata", width = NULL, 
                              p(metadata_eph,style = "text-align: justify")),
                          br(),
                          box(width = NULL,
                              downloadButton(ns('downloadPlot'),'Descargar gráfico'))
                          
                 ),
                 
                 tabPanel("Tabla",
                          value = "t_tasas_basicas_eph",
                          
                          box(width = NULL, br(),htmlOutput(ns('titulo2'))), 
                          br(),
                          fluidRow(
                            column(12,
                                   column(8, 
                                          box(DTOutput(ns('tabla')), width = NULL)),
                                   column(4,          
                                          box(title = "Metadata", width = NULL, 
                                              p(metadata_eph,style = "text-align: justify")),
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

