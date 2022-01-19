library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(readr)
library(openxlsx)

Serie_salarios <- readRDS("../data/salarios.RDS")

tipo_cambio_argentina <- readRDS("../data/Tipo_Cambio_Arg.RDS")

diccionario_variables <- read.xlsx("../data/diccionario_cod.variable.xlsx")

#Voy agregando a una lista los dataframes que vamos a subir
lista_dfs <- list()
lista_dfs[[1]] <- Serie_salarios
lista_dfs[[2]] <- tipo_cambio_argentina

#Armo vectores para inputs
vector_bases <- unique(diccionario_variables$base)
vector_variables <- setNames(diccionario_variables$cod.variable, diccionario_variables$nombre.variable)
vector_paises <- unique(Serie_salarios$nombre.pais)
vector_desagreg <- c("País","etc")


ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("CEPED-data"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("unidad_tematica",
                  "Unidad Temática:",
                  vector_bases ,
                  multiple = F,
                  selected = vector_bases[1]),
      selectInput("var1_id",
                  "Variable:",
                  vector_variables,
                  multiple = F,
                  selected = vector_variables[8]),
      selectInput(inputId = "pais_id",
                  label ="País:",
                  choices = vector_paises,
                  multiple = T,
                  selected = vector_paises[1:2]
      ),
      selectInput("desag_id",
                  "Desagregación:",
                  vector_desagreg),
      sliderInput("id_periodo", "Período:", value = c(2001,2015), min = 1970, max = 2022)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tabla",   
                 br(),
                 textOutput("texto"),
                 br(),
                 downloadButton('downloadTable','Descargar tabla'),
                 br(),
                 tableOutput("table_data")),
        tabPanel("Gráfico", 
                 br(),
                 fluidRow(column(6, sliderInput("height", "Altura del gráfico", min = 100, max = 1000, value = 380)), 
                          column(6, sliderInput("width", "Ancho del gráfico", min = 100, max = 1000, value = 800))),
                 plotOutput("plot_id"),
                 downloadButton('downloadPlot','Descargar gráfico')),
        tabPanel("Metadatos", 
                 br(),
                 textOutput("metadata"),
                 br(),
                 downloadButton('downloadTable_md','Descargar metadata')) ,

        
      )
    )
  )
)

server <- function(input, output, session){
  
  thematic::thematic_shiny()
  
  base <- reactive({
  
     # if(input$var1_id %in% diccionario_variables$cod.variable[diccionario_variables$base == "Serie_salarios"]){
     #   lista_dfs[[1]]
     # }
    
    if(input$unidad_tematica=="Serie_salarios")  {Serie_salarios}
    
    if(input$unidad_tematica=="Tipo_Cambio_Arg")  {tipo_cambio_argentina}  
    
  })
  
  tab_filtrada <- reactive({
    
    df <- base() %>% 
      filter(cod.variable == input$var1_id) %>% 
      filter(nombre.pais %in% input$pais_id) %>% 
      filter(ANO4 %in% c(input$id_periodo[1]:input$id_periodo[2])) 
      
    
  })
  
 titulo <- reactive ({ 
   
  lista_paises <-  paste0(input$pais_id, collapse = ", ")
  
  lista_paises <- sub(",([^,]*)$", " y\\1", lista_paises)   
  
  nombre_variable <- diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==input$var1_id]
  
  titulo <- paste0(nombre_variable ," para ", lista_paises , ". Años: ", input$id_periodo[1], " al ", input$id_periodo[2])
  })

  
  
    
  output$texto <- renderText({titulo()})
  
  output$table_data <- renderTable({
    
    
    tab_filtrada()
    
  })
  
  
  output$downloadTable <- downloadHandler(
    filename = function(){paste(input$var1_id,"_",input$pais_id,'.xlsx',sep='')},
    content = function(file){
      
      
      write.xlsx(tab_filtrada(), file)    }
  )
  
  plot <- reactive({
    
    nombre_variable <- diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==input$var1_id]
    
    tab_filtrada() %>% ggplot(
      aes(y = valor, x = as.factor(ANO4),group = iso3c,color = iso3c))+
    geom_line(size = 1) + 
    labs(title= titulo(),
         color= "País",
         y=nombre_variable,
         x = "Año")+
    theme_minimal()+
    theme(text = element_text(size = 9),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          legend.position = "bottom", 
          plot.title= element_text(size=12, face="bold"))+ 
    theme(axis.text.x = element_text(angle = 90))
    
  })
  
  output$plot_id <- renderPlot(
    
      width = function() input$width,
      height = function() input$height,
      res = 96,
      
      {
      plot()
       }
    )
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste(input$var1_id,"_",input$pais_id,'.png',sep='')},
    content = function(file){
      
      
      ggsave(file,plot=plot(), width=8, height=4)
    }
  )
  
  metadatos <- reactive({
    diccionario_variables$metadata[diccionario_variables$cod.variable == input$var1_id]
  })
  
  output$metadata <- renderText({metadatos()})
  
  
  #Actualización de opciones de input segun la unidad tematica/base seleccionada
  
  observeEvent(input$unidad_tematica, {
    
    variables_unidad_tematica <- diccionario_variables %>% filter(base==input$unidad_tematica)
    
    vector_variables_unidad_tematica <- setNames(variables_unidad_tematica$cod.variable, variables_unidad_tematica$nombre.variable)
    
    updateSelectInput(inputId = "var1_id", choices = vector_variables_unidad_tematica)
  })  
  
  
}

shinyApp(ui,server)
