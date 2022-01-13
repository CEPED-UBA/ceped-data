library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(readr)
library(openxlsx)


Serie_salarios <- readRDS("../data/salarios.RDS")
diccionario_variables <- read.xlsx("../data/diccionario_cod.variable.xlsx")

vector_variables <- setNames(diccionario_variables$cod.variable, diccionario_variables$nombre.variable)
vector_paises <- unique(Serie_salarios$nombre.pais)
vector_desagreg <- c("Todos","etc")


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
        tabPanel("Tabla",                 
                 textOutput("texto"),
                 tableOutput("table_data"),
                 downloadButton('downloadTable','Descargar tabla')),
        tabPanel("Metadatos", 
                 textOutput("metadata"),
                 downloadButton('downloadTable_md','Descargar metadata')) ,
        tabPanel("Gráfico", 
                 fluidRow(column(6, sliderInput("height", "Altura del gráfico", min = 100, max = 1000, value = 380)), 
                          column(6, sliderInput("width", "Ancho del gráfico", min = 100, max = 1000, value = 800))),
                 plotOutput("plot_id"),
                 downloadButton('downloadPlot','Descargar gráfico'))
        
      )
    )
  )
)

server <- function(input, output, session){
  
  thematic::thematic_shiny()
  
  tab_filtrada <- reactive({
    
    df <- Serie_salarios %>% 
      filter(cod.variable == input$var1_id) %>% 
      filter(nombre.pais %in% input$pais_id) %>% 
      filter(ANO4 %in% c(input$id_periodo[1]:input$id_periodo[2])) 
      
    
  })
  
 titulo <- reactive ({ titulo <- paste0("Serie de ", diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==input$var1_id],
                                        " para ", paste0(input$pais_id, collapse = ", "), ". Años: ", input$id_periodo[1], " al ", input$id_periodo[2])})

  
  
    
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
    
    tab_filtrada() %>% ggplot(
      aes(y = valor, x = as.factor(ANO4),group = iso3c,color = iso3c))+
    geom_line(size = 1) + 
    labs(y=paste0(input$var1_id),
         x = "Año")+
    theme_minimal()+
    theme(text = element_text(size = 9),
          legend.position = "left")+ 
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
