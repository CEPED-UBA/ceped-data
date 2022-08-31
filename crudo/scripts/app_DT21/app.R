#Librerias ####
library(openxlsx)
library(tidyverse)
library(ggthemes)
library(plotly)
library(shiny)
library(DT)
library(scales)
library(shinycssloaders)

#Homenaje####
# Lectura de Datos####
## Localidades####
diccionario_dt21 <- read.xlsx(xlsxFile = "diccionario_dt21.xlsx") 
base_dt21 <- readRDS(file = "base_dt21.RDS") %>% 
  mutate(valor = round(valor,digits = 2))
base_export <- base_dt21 %>% 
  pivot_wider(names_from = sector,values_from = valor) %>% 
  arrange(variable)
###APP####
####genero ui########
ui <-  fluidPage(
  titlePanel(title = "La masa salarial y su composición según el vínculo laboral. Argentina. 1993-2017"),
  tabsetPanel(
    id = 'Display',
#    tabPanel("Localidades", DTOutput("table")),
#    tabPanel("Provincias", DTOutput("table2")),
    tabPanel("Gráficos interactivos",
             fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectizeInput(
                   inputId = "serie",
                   label =  "Elegir Variable:",
                   choices = unique(base_dt21$variable),
                   selected = base_dt21$variable[1],
                   multiple = FALSE,
                   width = 400),
                 selectInput(
                   inputId = "tipograf",
                   label =  "Tipo de Grafico: ",
                   choices = c("Linea","Columnas apiladas","Columnas horizontales"),
                   multiple = F,
                   selected = "Linea"),
                 selectizeInput('sector',
                             label =  "Elegir sector: ",
                               choices = unique(base_dt21$sector),
                               selected = unique(base_dt21$sector)[1],
                               width = "300px",
                               multiple = T
                   ),
               hr(),
               
                p("La metodología detallada de estimación de las distintas variables se encuentra en el ",
                  a(href = 'http://bibliotecadigital.econ.uba.ar/econ/collection/docin/document/docin_ceped_d_021', 'Documento de Trabajo N°21 del CEPED', .noWS = "outside"), 
                  .noWS = c("after-begin", "before-end")), 
               ),
               mainPanel(
                 tabsetPanel(type = "tabs", id = "tabs",
                             
                 tabPanel(title = "Gráfico",
                          value = "g_bp",
                          
                 plotlyOutput(outputId = "Grafico") %>% withSpinner(type = 7),
                 br(),
                 downloadButton(outputId = 'downloadTable',label = 'Descargar datos completos'),

                 ))))),
                 tabPanel(title = "Diccionario",
                          value = "d_bp",
                          tableOutput(outputId = "dicc")
                 
    )
  )
)

server <- function(input, output,session) {
  
  output$dicc <- renderTable({diccionario_dt21})
    
  
  output$Grafico <- renderPlotly({
    
    tipo.grafico <- input$tipograf

    
    
    data_graf <-  base_dt21 %>%
      filter(variable %in% input$serie,
             sector %in% input$sector)
             
    if(tipo.grafico == "Columnas horizontales"){
      tipo  <- geom_col(position = position_dodge())
    }
   if(tipo.grafico == "Linea"){
      tipo  <- geom_line()
   }
    if(tipo.grafico == "Columnas apiladas"){
      tipo  <- geom_col(position = position_stack())
    }
    
    # if(serie_elegida == "poblacion_urbana"){
    #   eje_y  <- scale_y_continuous(
    #     labels =number_format(
    #     big.mark = ".",decimal.mark = ","))
    # }
    # if(serie_elegida == "tasa_crecim_anualizada"){
    #   eje_y  <- scale_y_continuous(
    #     labels =percent_format(accuracy = 0.01))
    # }
    
    
    options(scipen = 999)

a <- ggplot(data_graf,
            aes(x= Anio,
                y = valor,
                fill = sector,
                color = sector,
                group = sector,
                text=paste0('</br>',sector,'</br>Valor: ',number(valor,big.mark = ".",decimal.mark = ","),
                            '</br>Período: ',Anio)))+
  tipo+
      theme_tufte()+
      theme(axis.title.x = element_blank(),
            panel.grid.minor = element_line(),
            legend.title =  element_blank(),
            panel.grid.major.y = element_line(colour = "grey90"),
            legend.position = "bottom")+
      scale_fill_gdocs()+
      scale_color_gdocs()+
  scale_x_continuous(breaks = seq(1993,2021,2),
                     labels = seq(1993,2021,2))+
  scale_y_continuous(labels =number_format(big.mark = ".",
                                           decimal.mark = ","))

  

ggplotly(a, tooltip = c("text"))%>% 
  layout(font = list(family ="Tisub New Roman"))
    
  })
  
  output$downloadTable <- downloadHandler(
    filename = function(){paste("Datos_Completos",'.xlsx',sep='')},
    content = function(file){
      write.xlsx(base_export,
                 file)
    }
  )   
  
  # output$downloadData <- downloadHandler(
  #   
  #   
  #   filename = function() {
  #     paste("Datos", ".xlsx", sep = "")
  #   },
  #   content = function(file) {
  #     write_excel_csv(localidades, file)
  #   }
  # )

}

shinyApp(ui = ui, server = server)

