library(openxlsx)

bop_arg_dolares <- readRDS("www/data/bop_arg_dolares.RDS")
diccionario_variables <- read.xlsx("www/data/diccionario_cod.variable.xlsx")

#Voy agregando a una lista los dataframes que vamos a subir


v_bp <- c() #cuales?

# v_salarios <- diccionario_variables %>% filter(base== "Serie_salarios") %>%  select(nombre.variable) %>% 
#   filter(nombre.variable != "Indice de Precios al Consumidor (base 2005)")
# 
# 
# 
# v_monetario <- diccionario_variables %>% filter(base=="Tipo_Cambio_Arg") %>%  select(cod.variable) 
# 
# v_ipc <- grep("IPC",v_monetario$cod.variable, value = T)
# v_tc <- v_monetario$cod.variable[!v_monetario$cod.variable %in% v_ipc]
# 
# v_ipc <- diccionario_variables %>% filter(cod.variable %in% v_ipc | nombre.variable == "Indice de Precios al Consumidor (base 2005)") %>%  select(nombre.variable)
# 
# v_tc <- diccionario_variables %>% filter(cod.variable %in% v_tc) %>%  select(nombre.variable)
# 

####genero server######

bp_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    armar_tabla <- function(variables, valu, periodo_i, periodo_f){
      bop_arg_dolares %>%
        filter(cod.variable  %in%   variables,valuacion == valu) %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        rename("Serie" = "cod.variable",
               "País" = "nombre.pais",
               "Período" = "ANO4",
               "Valuación" = "valuacion") %>% 
        select("País","iso3c","Período","Serie","Valuación", "valor")
    }
    
    generar_titulo <- function(variables, valu, periodo_i, periodo_f){
      nombre_variable <-  paste0(variables, collapse = ", ")
      nombre_variable <- sub(",([^,]*)$", " y\\1", nombre_variable)   
      titulo <- paste0("<font size='+2'>",nombre_variable , " en ",valu," desde ", periodo_i, " hasta ", periodo_f,"</font>")
    }
   
    
    plot <- function(variables,valu, periodo_i, periodo_f){
      
      p <- bop_arg_dolares %>%
        filter(cod.variable  %in%   variables,valuacion == valu) %>% 
        filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
        ggplot(
          aes(x = as.factor(ANO4), y = valor, shape = Sector,group = cod.variable, color = cod.variable,
              text=paste0('</br>valor: ',round(valor,1), '</br>Período: ',ANO4)))+
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
        theme(axis.text.x = element_text(angle = 90))
      
      p
      #ggplotly(p, tooltip = c("text"))
    }
    
    plot_interact <- function(p){
      ggplotly(p, tooltip = c("text"))%>% 
        layout(font = list(family ="Times New Roman"))
    }
    
    # generar_metadata <- function(variables){
    #   diccionario_variables$metadata[diccionario_variables$nombre.variable == variables] 
    # }
    
    output$titulo1 <- renderText({
      generar_titulo(input$variables_serie,input$valuacion, input$id_periodo[1],input$id_periodo[2])
    })
    output$titulo2 <- renderText({
      generar_titulo(input$variables_serie,input$valuacion, input$id_periodo[1],input$id_periodo[2])
    })
    output$plot <- renderPlotly({
      plot_interact(plot(input$variables_serie,input$valuacion, input$id_periodo[1],input$id_periodo[2]))
    })
    
    output$tabla <- renderTable({
      armar_tabla(input$variables_serie,input$valuacion, input$id_periodo[1],input$id_periodo[2])
    })
    # output$metadata1 <- renderText({
    #   generar_metadata(input$var_serie)
    # })
    # output$metadata2 <- renderText({
    #   generar_metadata(input$var_serie)
    # })
    output$downloadTable <- downloadHandler(
      
      filename = function(){paste(input$variables_serie[1],'.xlsx',sep='')},
      content = function(file){
        
        write.xlsx(armar_tabla(input$variables_serie,input$valuacion, input$id_periodo[1],input$id_periodo[2]), 
                   file)    }
    )
    output$downloadPlot <- downloadHandler(
      filename = function(){paste(input$variables_serie[1],'.png',sep='')},
      content = function(file){
        
        
        ggsave(file,plot=plot(input$variables_serie,input$valuacion, input$id_periodo[1],input$id_periodo[2]), 
               width=8, height=4)
      }
    )
    
    
    
  })
}


####genero ui########

bp_plot_ui <- function(id, title,v_variables) {
  ns <- NS(id)

    tabPanel(title,
             value = id,
             titlePanel(title),
             sidebarLayout(
               sidebarPanel(
                 selectInput(ns('variables_serie'),label = 'Seleccionar Series',
                             choices =  unique(bop_arg_dolares$cod.variable),
                             selected = unique(bop_arg_dolares$cod.variable)[1],
                             width = "300px",
                             multiple = T
                             ),
                 selectInput(ns('valuacion'),label = 'Elegir valuacion',
                             choices =  unique(bop_arg_dolares$valuacion),
                             selected = unique(bop_arg_dolares$valuacion)[1],
                             width = "300px",
                             multiple = F
                             ),
                 sliderInput(ns('id_periodo'),
                             "Período:",
                             value = c(1993,2005),
                             min = 1993, 
                             max = 2020
                             )
                 ),
               mainPanel(
                 
                 tabsetPanel(
                   
                   tabPanel("Gráfico",
                            value = "g_bp",
                            
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
                            value = "t_bp",
                            
                            fluidRow(
                              column(12,
                                     column(9, 
                                            box(width = NULL,br(), htmlOutput(ns('titulo2'))),
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