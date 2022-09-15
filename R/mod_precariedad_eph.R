
precariedad_eph_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    

    df <-  reactive({
      
      base <- eph %>% 
        filter(modulo=="precariedad")
      
      if(input$aglos ==  "Total aglomerados urbanos"){
        
        base <- base %>% 
          filter(aglomerados=="total_aglos")
      }
      
      if(input$aglos == "Filtro Gran Buenos Aires"){
        
        base <- base %>% 
          filter(aglomerados=="gba")
      }
      
      
      if(input$id_periodicidad == "Promedio anual"){
               
              base <- base  %>%
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
        
                base <-  base  %>%
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
                text=paste0('</br>valor: ',round(valor,1), '</br>Período: ',Periodo)))+
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

    output$titulo1 <- renderText({
      generar_titulo(input$var_serie,input$id_periodo[1],input$id_periodo[2])
    })
     output$titulo2 <- renderText({
       generar_titulo(input$var_serie,input$id_periodo[1],input$id_periodo[2])
     })
     
     observeEvent(input$var_serie, {
     
    output$plot <- renderPlotly({
      plot_interact(plot(input$var_serie))
    })
    
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

      filename = function(){paste(input$var_serie[1],'.xlsx',sep='')},
      content = function(file){

        write.xlsx(df(),
                   file)    }
    )
    
    
    output$download_database <- downloadHandler(
      
      filename = function(){paste("database",'.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(list("Base_completa" =eph),
                   file)    
      }
    )
    
    output$downloadPlot <- downloadHandler(
      filename = function(){paste(input$var_serie[1],'.png',sep='')},
      content = function(file){


        ggsave(file,plot=plot(input$var_serie),
               width=8, height=4)
      }
    )
    
    
     })
}

precariedad_eph_plot_ui <- function(id, title) {
  ns <- NS(id)
  
  tabPanel(title,
           value = id,
           titlePanel(title),
           
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('id_periodicidad'),label = 'Tipo de infromación',
                             choices =  c("Promedio anual", "Trimestral/Onda"),
                             selected = "Promedio Anual",
                             width = "350px",
                             multiple = F
                 ),
               
               pickerInput(ns('var_serie'),label = 'Seleccionar series',
                           choices =  unique(eph$cod.variable[eph$modulo=="precariedad"]),
                           selected = unique(eph$cod.variable[eph$modulo=="precariedad"]),
                           options = list(`actions-box` = TRUE),
                           width = "350px",
                           multiple = T
               ),
               sliderInput(ns('id_periodo'), "Período",
                           value = c(1974,2021),
                           min = 1974, 
                           max = 2021,
                           sep=""
               ), 
               hr(), 
               radioButtons(ns("aglos"), 
                            "Cantidad de aglomerados", 
                            choices=c("Total aglomerados urbanos", "Filtro Gran Buenos Aires")
               ),
               h6("Por defecto, los datos se estiman sobre total de aglomerados disponibles para cada período de tiempo (ver tabla auxiliar). Activando este filtro, las estimaciones se calculan sólo sobre los aglomerados de GBA, obteniendo series de más largo plazo para algunas estadísticas"),
               hr(), 
               h4("Nota aclaratoria"), 
               h6(nota_aclaratoria_eph1, style="text-align: justify;"),
               h6(nota_aclaratoria_eph2, style="text-align: justify;"),
               hr(),  
               h4(strong(titulo_cita)), 
               h6(cita, style="text-align: justify;"),
               h6(doi, style="text-align: justify;")
             ),
             
             mainPanel( 
               
               tabsetPanel(
                 
                 tabPanel("Gráfico",
                          value = "g_precariedad_eph",
                          
                          box(width = NULL, br(),htmlOutput(ns('titulo1'))), 
                          br(),
                          plotlyOutput(ns('plot'))%>% withSpinner(type = 7, color =paleta_colores[1]),
                          br(),
                          #box(title = "Aclaración sobre la construcción de los datos", width = NULL, textOutput(ns('metadata1'))),
                          box(title = "Aclaración sobre la construcción de los datos", width = NULL, 
                              p(metadata_eph,style = "text-align: justify")),
                          br(),
                          box(width = NULL,
                              downloadButton(ns('downloadPlot'),'Descargar gráfico')), 
                          br(),
                          downloadButton(ns('download_database'),'Descargar base completa')
                          
                 ),
                 
                 tabPanel("Tabla",
                          value = "t_precariedad_eph",
                          
                          box(width = NULL, br(),htmlOutput(ns('titulo2'))), 
                          br(),
                          fluidRow(
                            column(12,
                                   column(8, 
                                          box(DTOutput(ns('tabla')), width = NULL)),
                                   column(4,          
                                          box(title = "Aclaración sobre la construcción de los datos", width = NULL, 
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

