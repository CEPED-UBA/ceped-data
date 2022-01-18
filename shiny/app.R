library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(readr)
library(openxlsx)

lista_dfs <- list()

Serie_salarios <- readRDS("../data/salarios.RDS")

#Voy agregando a una lista los dataframes que vamos a subir
lista_dfs[[1]] <- Serie_salarios

diccionario_variables <- read.xlsx("../data/diccionario_cod.variable.xlsx")

vector_variables <- setNames(diccionario_variables$cod.variable, diccionario_variables$nombre.variable)
vector_paises <- unique(Serie_salarios$nombre.pais)
vector_desagreg <- c("País","etc")


ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("CEPED-data"),
  
  sidebarLayout(
    
    sidebarPanel(
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
        tabPanel("Gráfico", 
                 fluidRow(column(6, sliderInput("height", "Altura del gráfico", min = 100, max = 1000, value = 380)), 
                          column(6, sliderInput("width", "Ancho del gráfico", min = 100, max = 1000, value = 800))),
                 plotOutput("plot_id"),
                 downloadButton('downloadPlot','Descargar gráfico')),
        tabPanel("Tabla",   
                 br(),
                 textOutput("texto"),
                 br(),
                 downloadButton('downloadTable','Descargar tabla'),
                 br(),
                 tableOutput("table_data")),
        tabPanel("Metadatos", 
                 textOutput("metadata"),
                 downloadButton('downloadTable_md','Descargar metadata')) ,

        
      )
    )
  )
)

server <- function(input, output, session){
  
  thematic::thematic_shiny()
  
  base <- reactive({
  
    if(input$var1_id %in% diccionario_variables$cod.variable[diccionario_variables$base == "Serie_salarios"]){
      lista_dfs[[1]]
    }
    
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
  
}

shinyApp(ui,server)
