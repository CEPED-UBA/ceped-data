
categoria_ocup_eph_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    df <-  reactive({
      
      if(input$id_periodicidad == "Promedio anual"){
        
        base <- categoria_ocup_eph  %>%
          filter(cod.variable  %in%   input$var_serie) %>% 
          filter(ANO4 %in% c(input$id_periodo[1]:input$id_periodo[2])) %>% 
          filter(!(ANO4==2003 & tipo.eph=="Puntual")) %>% 
          group_by(ANO4, cod.variable) %>% 
          mutate(valor=mean(valor, na.rm=TRUE)) %>% 
          rename("Serie" = "cod.variable",
                 "Pais" = "nombre.pais",
                 "Periodo"= "ANO4") %>% 
          select("Pais","iso3c", "Periodo","Serie", "valor", "definicion") %>% 
          unique()                                                   
      }
      
      if(input$id_periodicidad == "Trimestral/Onda"){
        
        base <-  categoria_ocup_eph  %>%
          filter(cod.variable  %in%   input$var_serie) %>% 
          filter(ANO4 %in% c(input$id_periodo[1]:input$id_periodo[2])) %>% 
          rename("Serie" = "cod.variable",
                 "Pais" = "nombre.pais",
                 "Periodo" = "ANO4.trim") %>% 
          select("Pais","iso3c","Periodo","Serie", "valor", "definicion")
      }
      
      if(input$pok == "Definición clásica"){
        
        base <- base %>% 
          filter(definicion=="clasica") %>% 
          select(-definicion)
      }
         
      if(input$pok == "Filtrar por sector y registro"){
        
        base <- base %>% 
          filter(definicion=="pok") %>% 
          select(-definicion)
      }
      
      
      base
      
    })
    

    observe({
      
      if(input$pok == "Definición clásica"){
        
        opciones <- categoria_ocup_eph %>% 
          filter(definicion=="clasica") %>%
          select(cod.variable) %>% 
          unique() 
        
        opciones <- opciones$cod.variable
        
      }
      
      if(input$pok == "Filtrar por sector y registro"){
        
        opciones <- categoria_ocup_eph %>% 
          filter(definicion=="pok") %>%
          select(cod.variable) %>% 
          unique() 
        
        opciones <- opciones$cod.variable
        
      }
      
      
      updateSelectizeInput(session,
                           inputId = "var_serie",
                           choices = opciones, 
                           selected= opciones)
    })

    generar_titulo <- function(variables, periodo_i, periodo_f){
      
      
      lista_variables <-  paste0(variables, collapse = ", ")
      lista_variables <- sub(",([^,]*)$", " y\\1", lista_variables)  
      titulo <- paste0("<font size='+2'></br>", "Porcentaje de personas ocupadas como ", lista_variables ," sobre total de ocupados.</font>",
                       "</br><font size='+1'>Años ", periodo_i, " - ", periodo_f,"</font>")

       }
    
    plot <- function(variables){
       
        p <- df() %>% 
        ggplot(
          aes(x = Periodo, y = valor/100, fill = Serie, 
                text=paste0('</br>valor: ',round(valor,1), '</br>Período: ', Periodo)))+
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

categoria_ocup_eph_plot_ui <- function(id, title,v_categoria_ocup_eph) {
  ns <- NS(id)
  
  tabPanel(title,
           value = id,
           titlePanel(title),
           
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('id_periodicidad'),label = 'Tipo de infromación',
                           choices =  c("Promedio anual", "Trimestral/Onda"),
                           selected = "Promedio Anual",
                           width = "300px",
                           multiple = F
               ),
               selectInput(ns('var_serie'),label = 'Seleccionar series',
                           choices =  unique(categoria_ocup_eph$cod.variable)[1:5],
                           selected = unique(categoria_ocup_eph$cod.variable)[1:5],
                           width = "300px",
                           multiple = T
               ),
               sliderInput(ns('id_periodo'), "Período",
                           value = c(1995,2021),
                           min = 1995, 
                           max = 2021,
                           sep=""
               ), 
               hr(), 
               radioButtons(ns("pok"), 
                            "Definición de las categorías ocupacionales", 
                            choices=c("Definición clásica", "Filtrar por sector y registro")
               ),
               p("Activando este filtro, las categorías ocupacionales se subdividen según sector (público o privado) y condición de registro para la población asalariada"),
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
                          value = "g_categoria_ocup_eph",
                          
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
                          value = "t_categoria_ocup_eph",
                          
                          box(width = NULL, br(),htmlOutput(ns('titulo2'))), 
                          br(),
                          fluidRow(
                            column(12,
                                   column(8, 
                                          box(DTOutput(ns('tabla')), width = NULL)),
                                   column(4,          
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

