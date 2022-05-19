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
    
    
    # armar_tabla <- function(variables, periodo_i, periodo_f){
    #   base_binded %>% ungroup() %>%
    #     filter(cod.variable  ==  unique(diccionario_variables$cod.variable[diccionario_variables$nombre.variable == variables]) ) %>% 
    #     
    #     filter(ANO4 %in% c(periodo_i:periodo_f)) %>% 
    #     mutate(ANO4 = round(ANO4,0)) %>% 
    #     rename("Serie" = "cod.variable",
    #            
    #            "Período" = "ANO4") %>% 
    #     select(-iso3c)
    # }
    # 
    # generar_titulo <- function(variables, periodo_i, periodo_f){
    #   lista_paises <-  paste0(paises, collapse = ", ") 
    #   nombre_variable <- unique(diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==variables])
    #   titulo <- paste0(variables ," para ", ". Años: ", periodo_i, " al ", periodo_f)
    # }
    
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
             x = "Año")+
        theme_minimal()+
        theme(text = element_text(size = 9),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",
              plot.title= element_text(size=12, face="bold"))+
        theme(axis.text.x = element_text(angle = 90))
      
      #p
      ggplotly(p, tooltip = c("text"))
    }
    
    # generar_metadata <- function(variables){
    #   diccionario_variables$metadata[diccionario_variables$nombre.variable == variables] 
    # }
    # output$titulo <- renderText({
    #   generar_titulo(input$var_serie, input$id_periodo[1],input$id_periodo[2])
    # })
    output$plot <- renderPlotly({
      plot(input$variables_serie,input$valuacion, input$id_periodo[1],input$id_periodo[2])
    })
    # output$tabla <- renderTable({
    #   armar_tabla(input$var_serie, input$id_periodo[1],input$id_periodo[2])
    # })
    # output$metadata <- renderText({
    #   generar_metadata(input$var_serie)
    # })
    
    
  })
}


####genero ui########

bp_plot_ui <- function(id, title,v_variables) {
  ns <- NS(id)

    tabPanel(title,
             value = id,
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
                 box(width = NULL, textOutput(ns('titulo'))),
                 plotlyOutput(ns('plot')),
                 fluidRow(
                   column(12,
                          column(6,
                          box(tableOutput(ns('tabla')))),
                   column(6,
                          box(title = "Metadata", width = NULL, textOutput(ns('metadata'))),
                          br(),
                          box(width = NULL,
                          downloadButton(ns('downloadTable'),'Descargar tabla')),
                          br(),
                          box(width = NULL,
                          downloadButton(ns('downloadPlot'),'Descargar gráfico')))
                        )
                   )
             )
             
           )
           
  )
           

}