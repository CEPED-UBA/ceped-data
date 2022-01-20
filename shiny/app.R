library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(readr)
library(openxlsx)
options(scipen = 9999)

Serie_salarios <- readRDS("../data/salarios.RDS")
tipo_cambio_argentina <- readRDS("../data/Tipo_Cambio_Arg.RDS")
diccionario_variables <- read.xlsx("../data/diccionario_cod.variable.xlsx")


#Voy agregando a una lista los dataframes que vamos a subir
lista_dfs <- list()
lista_dfs[[1]] <- Serie_salarios
lista_dfs[[2]] <- tipo_cambio_argentina

#Armo vectores para inputs
vector_bases <- unique(diccionario_variables$base)

#Las opciones aparecen en función de lo que se elija, lo saco de acá
#vector_variables <- setNames(diccionario_variables$cod.variable, diccionario_variables$nombre.variable)
#vector_paises <- unique(Serie_salarios$nombre.pais)
vector_desagreg <- c("País","etc")


ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("CEPED-data"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("tema",
                  "Elija un tema:",
                  vector_bases ,
                  multiple = F,
                  selected = vector_bases[1]
                  ),
      selectInput("var1_id",
                  "Variable:",
                  NULL,
                  #vector_variables,
                  multiple = F
                  #selected = vector_variables[1]
                  ),
      selectInput(inputId = "pais_id",
                  label ="País:",
                  NULL,
                  #choices = vector_paises,
                  multiple = T
                  #selected = vector_paises[1:2]
      ),
      selectInput("desag_id",
                  "Desagregación:",
                  vector_desagreg),
      sliderInput("id_periodo", "Período:", value = c(2001,2015), min = 1970, max = 2022)
      ,br()
      ,actionButton("actualizar", "Ver")
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
  
     if(input$tema=="Serie_salarios"){
       lista_dfs[[1]]
     } else if(input$tema=="Tipo_Cambio_Arg"){
       lista_dfs[[2]]
     }
    
  })
  
  observeEvent(base(), {
    var_ops <- unique(base()$cod.variable)
    pais_ops <- unique(base()$nombre.pais)
    min_year <- min(base()$ANO4)
    max_year <- max(base()$ANO4)
    updateSelectInput(session,inputId = "var1_id", choices = var_ops)
    updateSelectInput(session,inputId = "pais_id", choices = pais_ops)
    updateSliderInput(session,inputId = "id_periodo", min = min_year, max = max_year)
  })
  
  tab_filtrada <- eventReactive(input$actualizar, {
    
    df <- base() %>% 
      filter(cod.variable == input$var1_id) %>% 
      filter(nombre.pais %in% input$pais_id) %>% 
      filter(ANO4 %in% c(input$id_periodo[1]:input$id_periodo[2])) 
      
    
  })
  
 titulo <- eventReactive(input$actualizar, { 
   
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
  
  plot <- eventReactive(input$actualizar, { 
    
    nombre_variable <- diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==input$var1_id]
    
    tab_filtrada() %>% ggplot(
      aes(y = valor, x = as.factor(ANO4),group = iso3c,color = iso3c))+
    geom_line(size = 1) + 
    labs(title= str_wrap(titulo(),60),
         color= "País",
         y = str_wrap(nombre_variable,40),
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
  
  metadatos <- eventReactive(input$actualizar, { 
    diccionario_variables$metadata[diccionario_variables$cod.variable == input$var1_id]
  })
  
  output$metadata <- renderText({metadatos()})
  
  
  #Actualización de opciones de input segun la unidad tematica/base seleccionada
  
  observeEvent(input$tema, {
    
    variables_tema <- diccionario_variables %>% filter(base==input$tema)
    
    vector_variables_tema <- setNames(variables_tema$cod.variable, variables_tema$nombre.variable)
    
    updateSelectInput(inputId = "var1_id", choices = vector_variables_tema)
  })  
  
  
}

shinyApp(ui,server)
