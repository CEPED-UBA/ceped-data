
rama_eph_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    df <-  reactive({
      
      base <- eph %>% 
        filter(modulo=="empleo_ramas") 
      
      if(input$id_periodicidad == "Promedio anual"){
        
        base <- base  %>%
          filter(cod.variable  %in%   input$var_serie) %>% 
          filter(ANO4 %in% c(input$id_periodo[1]:input$id_periodo[2])) %>% 
          filter(!(ANO4==2003)) %>% 
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
      
      
      titulo <- paste0("<font size='+2'></br>", "Porcentaje de personas ocupadas por rama sobre total de ocupados.</font>",
                       "</br><font size='+1'>Años ", periodo_i, " - ", periodo_f,"</font>")

       }
    
    plot <- function(variables){
      
        p <- df() %>% 
        ggplot(
          aes(x = Periodo, y = valor/100, fill = Serie, 
                text=paste0('</br>valor: ',round(valor,1), '</br>Período: ',Periodo)))+
        scale_fill_manual(values =paleta_colores_extendida2) +
        geom_bar(stat="identity") +
        labs(y = "",
             x = "Periodo",
             color = "")+
        theme_minimal()+
        theme(text = element_text(size = 9),
              axis.text.x = element_text(size=6),
              axis.text.y = element_text(size=10),
              plot.title= element_text(size=12, face="bold"))+
        theme(axis.text.x = element_text(angle = 90)) +
        guides(fill=guide_legend(title=NULL)) +
        theme(legend.title=element_blank()) +
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
    
     
     output$tabla_aglos <- renderDT({
       tabla_aglos %>%   datatable(rownames = FALSE,
                                   options = list(
                                     searching=FALSE, 
                                     pageLength = 10, 
                                     dom='tip')) 
     })

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

rama_eph_plot_ui <- function(id, title) {
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
               pickerInput(ns('var_serie'),label = 'Seleccionar series',
                           choices =  unique(eph[eph$modulo=="empleo_ramas", ]$cod.variable),
                           selected = unique(eph[eph$modulo=="empleo_ramas", ]$cod.variable),
                           options = list(`actions-box` = TRUE),
                           width = "350px",
                           multiple = T
               ),
               sliderInput(ns('id_periodo'), "Período:",
                           value = c(1995,2022),
                           min = 1995, 
                           max = 2022,
                           sep=""
               ), 
               hr(), 
               h4(strong(titulo_cita)), 
               h6(cita, style="text-align: justify;"),
               h6(doi, style="text-align: justify;")
             ),
             
             mainPanel( 
               
               tabsetPanel(
                 
                 tabPanel("Gráfico",
                          value = "g_rama_eph",
                          
                          box(width = NULL,br(), htmlOutput(ns('titulo1'))), 
                          br(),
                          plotlyOutput(ns('plot'))%>% withSpinner(type = 7, color =paleta_colores[1]),
                          br(),
                          box(title = "Aclaración sobre la construcción de los datos", width = NULL, 
                              h6(metadata_eph,style = "text-align: justify")),
                          h6("El cómputo de empleo por ramas fue realizado sobre 28 aglomerados urbanos", style="text-align: justify;"),
                          h6(nota_aclaratoria_eph1, style="text-align: justify;"),
                          h6(nota_aclaratoria_eph2, style="text-align: justify;"),
                          br(),
                          box(width = NULL,
                              downloadButton(ns('downloadPlot'),'Descargar gráfico'))
                          
                 ),
                 
                 tabPanel("Tabla",
                          value = "t_rama_eph",
                          
                          box(width = NULL, br(),htmlOutput(ns('titulo2'))), 
                          br(),
                          fluidRow(
                            column(12,
                                   column(8, 
                                          box(DTOutput(ns('tabla')), width = NULL)),
                                   column(4,          
                                          box(title = "Aclaración sobre la construcción de los datos", width = NULL, 
                                          h6(metadata_eph , style = "text-align: justify")),
                                          h6("El cómputo de empleo por ramas fue realizado sobre 28 aglomerados urbanos", style="text-align: justify;"),
                                          h6(nota_aclaratoria_eph1, style="text-align: justify;"),
                                          h6(nota_aclaratoria_eph2, style="text-align: justify;"),
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

