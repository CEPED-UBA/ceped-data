library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(readr)
library(openxlsx)

Serie_salarios <- readRDS("data/salarios.RDS")

vector_variables <- unique(Serie_salarios$cod.variable)
vector_paises <- unique(Serie_salarios$nombre.pais)
vector_desagreg <- c("Todos","etc")


ui <- fluidPage(
  
  titlePanel("CEPED-data"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("var1_id",
                  "Variable:",
                  vector_variables),
      selectInput("pais_id",
                  "País:",
                  vector_paises),
      selectInput("desag_id",
                  "Desagregación:",
                  vector_desagreg),
      sliderInput("id_periodo", "Período:", value = c(2001,2015), min = 1970, max = 2022)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Table", tableOutput("table_data"),
                 downloadButton('downloadTable','Descargar tabla')),
        tabPanel("Metadata", textOutput("metadata"),
                 downloadButton('downloadTable_md','Descargar metadata')) ,
        tabPanel("Plot", plotOutput("plot_id", width = "950px", height = "600px"),
                 downloadButton('downloadPlot','Descargar gráfico'))
        
      )
    )
  )
)

server <- function(input, output, session){
  
  tab_filtrada <- reactive({
    
    df <- Serie_salarios %>% 
      filter(cod.variable == input$var1_id) %>% 
      filter(nombre.pais == input$pais_id) %>% 
      filter(ANO4 %in% c(input$id_periodo[1]:input$id_periodo[2])) 
      
    
  })
  
  output$table_data <- renderTable({
    
    
    tab_filtrada()
    
  })
  
  
  output$downloadTable <- downloadHandler(
    filename = function(){paste(input$var1_id,"_",input$pais_id,'.xlsx',sep='')},
    content = function(file){
      
      
      write.xlsx(tab_filtrada(), file)    }
  )
  
  plot <- reactive({
    
    tab_filtrada() %>% ggplot(aes(y = valor, x = as.factor(ANO4), group = 1))+
    geom_line(size = 1, color = "cadetblue3") + 
    labs(y=paste0(input$var1_id),
         x = "Año")+
    theme_minimal()+
    theme(text = element_text(size = 9))+ 
    theme(axis.text.x = element_text(angle = 90))
    
  })
  
  output$plot_id <- renderPlot({
    
    
    plot()})
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste(input$var1_id,"_",input$pais_id,'.png',sep='')},
    content = function(file){
      
      
      ggsave(file,plot=plot(), width=8, height=4)
    }
  )
  
}

shinyApp(ui,server)
