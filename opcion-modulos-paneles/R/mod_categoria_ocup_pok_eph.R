
categoria_ocup_pok_eph_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    

    armar_tabla <- function(variables, periodo_i, periodo_f){
      categoria_ocup_pok_eph  %>%
        filter(cod.variable  %in%   variables) %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        rename("Serie" = "cod.variable",
               "País" = "nombre.pais",
               "Período" = "ANO4.trim") %>% 
        select("País","iso3c","Período","Serie", "valor")
    }
    
    generar_titulo <- function(variables, periodo_i, periodo_f){
      
      
      lista_variables <-  paste0(variables, collapse = ", ")
      lista_variables <- sub(",([^,]*)$", " y\\1", lista_variables)  
      titulo <- paste0("<font size='+2'></br>", "Porcentaje de personas ocupadas como ", lista_variables ," sobre total de ocupados.</font>",
                       "</br><font size='+1'>Años ", periodo_i, " - ", periodo_f,"</font>")

       }
    
    plot <- function(variables, periodo_i, periodo_f){
      
      p <- categoria_ocup_pok_eph %>% 
        filter(cod.variable  %in%   variables) %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        ggplot(
          aes(x = ANO4.trim, y = valor/100, fill = cod.variable, 
                text=paste0('</br>valor: ',round(valor,1), '</br>Período: ',ANO4.trim)))+
        # geom_line(size = 1) +
        # geom_point(size = 2) + 
        scale_fill_manual(values =paleta_colores_extendida) +
        geom_bar(stat="identity") +
        labs(y = "",
             x = "Año",
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
      #ggplotly(p, tooltip = c("text"))
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
    
    observeEvent(input$var_tipo_serie, {
      
      updateSelectInput(session, 'var_serie',
                        choices =  unique(categoria_ocup_pok_eph$cod.variable[categoria_ocup_pok_eph$tipo==input$var_tipo_serie]),
                        selected = unique(categoria_ocup_pok_eph$cod.variable[categoria_ocup_pok_eph$tipo==input$var_tipo_serie])[1])
      
    })
    
    output$titulo1 <- renderText({
      generar_titulo(input$var_serie,input$id_periodo[1],input$id_periodo[2])
    })
     output$titulo2 <- renderText({
       generar_titulo(input$var_serie,input$id_periodo[1],input$id_periodo[2])
     })
    
     output$plot <- renderPlotly({
      plot_interact(plot(input$var_serie,input$id_periodo[1],input$id_periodo[2]))
    })
    
    output$tabla <- renderTable({
      armar_tabla(input$var_serie, input$id_periodo[1],input$id_periodo[2])
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

categoria_ocup_pok_eph_plot_ui <- function(id, title,v_categoria_ocup_pok_eph) {
  ns <- NS(id)
  
  tabPanel(title,
           value = id,
           titlePanel(title),
           
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('var_serie'),label = 'Seleccionar una serie:',
                           choices =  unique(categoria_ocup_pok_eph$cod.variable),
                           selected = unique(categoria_ocup_pok_eph$cod.variable)[1],
                           width = "300px",
                           multiple = T
               ),
               sliderInput(ns('id_periodo'), "Período:",
                           value = c(2003,2021),
                           min = 2003, 
                           max = 2021
               ), 
               hr(), 
               h4("Nota aclaratoria"), 
               h5(nota_aclaratoria_eph1),
               
               h5(nota_aclaratoria_eph2),
               hr(), 
               h4(strong(titulo_cita)), 
               h5(cita)
             ),
             
             mainPanel( 
               
               tabsetPanel(
                 
                 tabPanel("Gráfico",
                          value = "g_categoria_ocup_pok_eph",
                          
                          box(width = NULL,br(), htmlOutput(ns('titulo1'))), 
                          br(),
                          plotlyOutput(ns('plot'))%>% withSpinner(type = 7, color =paleta_colores[1]),
                          br(),
                         # box(title = "Metadata", width = NULL, textOutput(ns('metadata1')), 
                          box(title = "Metadata", width = NULL, 
                              "Estimación del CEPED sobre datos de mercado de trabajo en base a la Encuesta Permanente de Hogares (EPH-INDEC). Estimaciones absolutas para 28 aglomerados urbanos. Beneficiarios del plan Jefes y Jegas de Hogar considerados como ocupados."),
                          br(),
                          box(width = NULL,
                              downloadButton(ns('downloadPlot'),'Descargar gráfico'))
                          
                 ),
                 
                 tabPanel("Tabla",
                          value = "t_categoria_ocup_pok_eph",
                          
                          box(width = NULL, br(),htmlOutput(ns('titulo2'))), 
                          br(),
                          fluidRow(
                            column(12,
                                   column(6, 
                                          box(tableOutput(ns('tabla')))),
                                   column(6,          
                                          box(title = "Metadata", width = NULL, 
                                              "Estimación del CEPED sobre datos de mercado de trabajo en base a la Encuesta Permanente de Hogares (EPH-INDEC). Estimaciones absolutas trimestrales para 28 aglomerados urbanos. Beneficiarios de planes sociales considerados como ocupados."),
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

