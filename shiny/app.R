# library(shiny)
# library(ggplot2)
# library(plotly)
# library(tidyverse)
# library(readr)
# library(openxlsx)
# 
# options(scipen = 9999)
# 
# #Cargo bases
# Serie_salarios <- readRDS("../data/salarios.RDS")
# tipo_cambio_argentina <- readRDS("../data/Tipo_Cambio_Arg.RDS")
# mercado_de_trabajo_arg <- readRDS("../data/Mercado_de_Trabajo_Arg.RDS")
# poblacion_eph <- readRDS("../data/Poblacion_eph.RDS")
# eph_mercado_de_trabajo_categoria_ocupacional <- readRDS("../data/eph_mercado_de_trabajo_categoria_ocupacional.RDS")
# 
# #Cargo diccionario de variables, bases y metadatos
# diccionario_variables <- read.xlsx("../data/diccionario_cod.variable.xlsx")
# 
# #Voy agregando a una lista los dataframes que vamos a subir
# lista_dfs <- list()
# lista_dfs[[1]] <- Serie_salarios
# lista_dfs[[2]] <- tipo_cambio_argentina
# lista_dfs[[3]] <- mercado_de_trabajo_arg
# lista_dfs[[4]] <- poblacion_eph
# lista_dfs[[5]] <- eph_mercado_de_trabajo_categoria_ocupacional 
# 
# #Armo vectores para inputs
# vector_bases <- unique(diccionario_variables$base)
# vector_desagreg <- c("País","etc")
# 
# 
# ui <- fluidPage(
#   
#   theme = bslib::bs_theme(bootswatch = "flatly"),
#   
#   titlePanel("CEPED-data"),
#   
#   sidebarLayout(
#     
#     sidebarPanel(
#       selectInput("tema",
#                   "Elija un tema:",
#                   vector_bases ,
#                   multiple = F,
#                   selected = vector_bases[1]
#                   ),
#       selectInput("var1_id",
#                   "Variable:",
#                   NULL,
#                   #vector_variables,
#                   multiple = F
#                   #selected = vector_variables[1]
#                   ),
#       selectInput(inputId = "pais_id",
#                   label ="País:",
#                   NULL,
#                   #choices = vector_paises,
#                   multiple = T
#                   #selected = vector_paises[1:2]
#       ),
#       selectInput("desag_id",
#                   "Desagregación:",
#                   vector_desagreg),
#       sliderInput("id_periodo", "Período:", value = c(2001,2015), min = 1970, max = 2022)
#       ,br()
#       ,actionButton("actualizar", "Ver")
#     ),
#     
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Tabla",   
#                  br(),
#                  textOutput("texto"),
#                  br(),
#                  downloadButton('downloadTable','Descargar tabla'),
#                  br(),
#                  tableOutput("table_data")),
#         tabPanel("Gráfico", 
#                  br(),
#                  fluidRow(column(6, sliderInput("height", "Altura del gráfico", min = 100, max = 1000, value = 380)), 
#                           column(6, sliderInput("width", "Ancho del gráfico", min = 100, max = 1000, value = 800))),
#                  plotOutput("plot_id"),
#                  downloadButton('downloadPlot','Descargar gráfico')),
#         tabPanel("Metadatos", 
#                  br(),
#                  textOutput("metadata"),
#                  br(),
#                  downloadButton('downloadTable_md','Descargar metadata')) ,
# 
#         
#       )
#     )
#   )
# )
# 
# server <- function(input, output, session){
#   
#   thematic::thematic_shiny()
#   
#   base <- reactive({
#   
#      if(input$tema=="Serie_salarios"){
#        lista_dfs[[1]]
#      } else if(input$tema=="Tipo_Cambio_Arg"){
#        lista_dfs[[2]]
#      } else if(input$tema=="Mercado_de_Trabajo_Arg"){
#        lista_dfs[[3]]
#      } else if(input$tema=="Poblacion_eph"){
#        lista_dfs[[4]]
#      } else if(input$tema=="eph_mercado_de_trabajo_categoria_ocupacional"){
#        lista_dfs[[5]]
#      }
#     
#     
#   })
#   
#   observeEvent(base(), {
#     variables_tema <- diccionario_variables %>% filter(base==input$tema)
#     var_ops_etiquetadas <- setNames(variables_tema$cod.variable, variables_tema$nombre.variable)
#     pais_ops <- unique(base()$nombre.pais)
#     min_year <- min(base()$ANO4)
#     max_year <- max(base()$ANO4)
#     updateSelectInput(inputId = "var1_id", choices = var_ops_etiquetadas, selected=var_ops_etiquetadas[1])
#     updateSelectInput(session,inputId = "pais_id", choices = pais_ops, selected=pais_ops[1])
#     updateSliderInput(session,inputId = "id_periodo", min = min_year, max = max_year, value=c(min_year, max_year))
#   })
#   
#   tab_filtrada <- eventReactive(input$actualizar, {
#     
#     df <- base() %>% 
#       filter(cod.variable == input$var1_id) %>% 
#       filter(nombre.pais %in% input$pais_id) %>% 
#       filter(ANO4 %in% c(input$id_periodo[1]:input$id_periodo[2])) 
#       
#     
#   })
#   
#  titulo <- eventReactive(input$actualizar, { 
#    
#   lista_paises <-  paste0(input$pais_id, collapse = ", ")
#   
#   lista_paises <- sub(",([^,]*)$", " y\\1", lista_paises)   
#   
#   nombre_variable <- diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==input$var1_id]
#   
#   titulo <- paste0(nombre_variable ," para ", lista_paises , ". Años: ", input$id_periodo[1], " al ", input$id_periodo[2])
#   })
# 
#   
#   
#     
#   output$texto <- renderText({titulo()})
#   
#   output$table_data <- renderTable({
#     
#     
#     tab_filtrada()
#     
#   })
#   
#   
#   output$downloadTable <- downloadHandler(
#     filename = function(){paste(input$var1_id,"_",input$pais_id,'.xlsx',sep='')},
#     content = function(file){
#       
#       
#       write.xlsx(tab_filtrada(), file)    }
#   )
#   
#   plot <- eventReactive(input$actualizar, { 
#     
#     nombre_variable <- diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==input$var1_id]
#     
#     tab_filtrada() %>% ggplot(
#       aes(y = valor, x = as.factor(ANO4),group = iso3c,color = iso3c))+
#     geom_line(size = 1) + 
#     labs(title= str_wrap(titulo(),60),
#          color= "País",
#          y = str_wrap(nombre_variable,40),
#          x = "Año")+
#     theme_minimal()+
#     theme(text = element_text(size = 9),
#           axis.text.x = element_text(size=10),
#           axis.text.y = element_text(size=10),
#           legend.position = "bottom", 
#           plot.title= element_text(size=12, face="bold"))+ 
#     theme(axis.text.x = element_text(angle = 90))
#     
#   })
#   
#   output$plot_id <- renderPlot(
#     
#       width = function() input$width,
#       height = function() input$height,
#       res = 96,
#       
#       {
#       plot()
#        }
#     )
#   
#   output$downloadPlot <- downloadHandler(
#     filename = function(){paste(input$var1_id,"_",input$pais_id,'.png',sep='')},
#     content = function(file){
#       
#       
#       ggsave(file,plot=plot(), width=8, height=4)
#     }
#   )
#   
#   metadatos <- eventReactive(input$actualizar, { 
#     diccionario_variables$metadata[diccionario_variables$cod.variable == input$var1_id]
#   })
#   
#   output$metadata <- renderText({metadatos()})
#   
# }
# 
# shinyApp(ui,server)

library(shiny)
library(ggplot2)

#data change only once at the begenin
dt <- data.frame(x=runif(100), y=runif(100))


ui <- fluidPage(
  sliderInput("slider1","slider1", value=5, min=1, max=10),
  sliderInput("slider2","slider2", value=0.8, min=0, max=1),
  radioButtons("check1","check1",choices = c("red","blue","green")),
  actionButton("refreshColours","refreshColours"),
  plotOutput("rys")
)



server <- function(input,output){
  
  col1 <- eventReactive(input$refreshColours,{input$check1}) 
  
  pp <- reactive({ ggplot(dt, aes(x,y)) + 
      geom_point(size=input$slider1, 
                 alpha=input$slider2, colour=col1()) }) 
  
  output$rys <- renderPlot({ pp() }) }

shinyApp(ui=ui, server=server)