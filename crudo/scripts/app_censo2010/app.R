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
homenaje<- "El desarrollo de esta aplicación es un pequeño homenaje personal a Javier Lindenboim, quien me acompaño en mis primeros pasos en la profesión. Javier participó en el desarrollo original de la metodología y en las sucesivas rondas de actualización de las estimaciones a partir de la publicación de resultados de nuevos Censos Nacionales"
# Lectura de Datos####
## Localidades####
localidades <- readRDS(file = "localidades.RDS")
regiones <- readRDS(file = "regiones.RDS")
provincias <- readRDS(file = "provincias.RDS")

###APP####
####genero ui########
ui <-  fluidPage(
  tags$head(includeHTML("google-analytics.html")),
  titlePanel(title = "Población urbana argentina - Homogeneización censos nacionales de 1960 a 2010 "),
  tabsetPanel(
    id = 'Display',
#    tabPanel("Localidades", DTOutput("table")),
#    tabPanel("Provincias", DTOutput("table2")),
    tabPanel("Gráficos interactivos",
             fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectizeInput(
                   inputId = "Serie",
                   label =  "Serie:",
                   choices = c("Poblacion urbana total" = "poblacion_urbana",
                               "Tasa de crecimiento intercensal (anualizada)" = "tasa_crecim_anualizada"),
                   selected = "Poblacion urbana total",
                   multiple = FALSE,
                   width = 400),
                 selectInput(
                   inputId = "tipograf",
                   label =  "Tipo de Grafico: ",
                   choices = c("Linea","Columnas"),
                   multiple = F,
                   selected = "Linea"),
                 selectInput('desagregacion',
                             label =  "Elegir desagregacion: ",
                               choices = c("localidades" = 'localidades',
                                           "provincias" = 'provincias',
                                           "regiones" = 'regiones'
                                           ),
                               selected = "localidades",
                               width = "300px",
                               multiple = F
                   ),
                 selectizeInput(
                   inputId = "elegir_localidad",
                   label =  "Elegir localidades",
                   choices = unique(localidades$Localidad),
                   multiple = T,
                   selected = unique(localidades$Localidad)[2:4]),
               #),
               hr(),
               
                p("La metodología de homogeneización de los datos censales aquí presentados se encuentra detallada en el ",
                  a(href = 'http://bibliotecadigital.econ.uba.ar/econ/collection/docin/document/docin_ceped_d_023', 'Documento de Trabajo N°23 del CEPED', .noWS = "outside"), 
                  # '. La misma constituye la continuación de una linea de trabajo desarrolada en el ',
                  # a(href = 'http://bibliotecadigital.econ.uba.ar/econ/collection/docin/document/docin_ceped_c_001', 'Cuaderno de Trabajo N°1 del CEPED', .noWS = "outside"),
                  .noWS = c("after-begin", "before-end")), 
               ),
               mainPanel(
                 tabPanel(title = "Gráfico",
                          value = "g_bp",
                          
                 plotlyOutput(outputId = "Grafico") %>% withSpinner(type = 7),
                 br(),
                 downloadButton(outputId = 'downloadTable',label = 'Descargar datos completos según desagregación elegida'),
                 br(),
                 br(),
                 p(homenaje)
                 )
                 )
             )
    )
  )
)

server <- function(input, output,session) {
  
 
  
  
  df <-  reactive({
    Agrupam <- get(input$desagregacion)
  })
  observe({
    df <- df()
    # dataframe <- input$df
    updateSelectizeInput(session,
                      inputId = "elegir_localidad",
                      label = "Elegir localidades", 
                      choices = unique(df$Localidad),
                      selected = unique(df$Localidad)[2:4])
  })
  
  output$Grafico <- renderPlotly({
    
    localidades_elegidas <- input$elegir_localidad
    tipo.grafico <- input$tipograf
    serie_elegida <- input$Serie
    
    
    data_graf <-  df() %>%
      filter(Localidad %in% localidades_elegidas)
    
    if(tipo.grafico == "Columnas"){
      tipo  <- geom_col(position = position_dodge())
    }
   if(tipo.grafico == "Linea"){
      tipo  <- geom_line()
    }
    if(serie_elegida == "poblacion_urbana"){
      eje_y  <- scale_y_continuous(
        labels =number_format(
        big.mark = ".",decimal.mark = ","))
    }
    if(serie_elegida == "tasa_crecim_anualizada"){
      eje_y  <- scale_y_continuous(
        labels =percent_format(accuracy = 0.01))
    }
    
    
    options(scipen = 999)

a <- ggplot(data_graf,
            aes_string(x= "Anio",
                y = serie_elegida,
                fill = "Localidad",
                color = "Localidad",
                group = "Localidad"))+
      tipo+
      theme_tufte()+
      theme(axis.title.x = element_blank(),
            panel.grid.minor = element_line(),
            legend.title =  element_blank(),
            panel.grid.major.y = element_line(colour = "grey90"),
            legend.position = "bottom")+
      scale_fill_gdocs()+
      scale_color_gdocs()+
  eje_y
  

    
    ggplotly(a)
    
  })
  
  output$downloadTable <- downloadHandler(
    filename = function(){paste("Datos_",as.character(input$desagregacion),'.xlsx',sep='')},
    content = function(file){
      write.xlsx(df(),
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

