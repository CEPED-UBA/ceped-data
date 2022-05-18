library(ggplot2)
library(tidyverse)
library(openxlsx)
library(cowplot)
library(magick)



mercado_de_trabajo_arg <- readRDS("www/data/Mercado_de_Trabajo_Arg.RDS")
poblacion_eph <- readRDS("www/data/Poblacion_eph.RDS")

diccionario_variables <- read.xlsx("www/data/diccionario_cod.variable.xlsx")

base_binded <- bind_rows(poblacion_eph, mercado_de_trabajo_arg)

#Armo vectores para inputs
vector_bases <- unique(diccionario_variables$base)

#Vector con nombres para cod.variable (series "poblacion_eph")
v_variables <- diccionario_variables %>% filter(base=="Poblacion_eph") %>% select(cod.variable) 
v_nombres <- diccionario_variables %>% filter(base=="Poblacion_eph") %>% select(nombre.variable)
vector_variables_poblacion_eph <- setNames(v_variables$cod.variable, v_nombres$nombre.variable)

#Vector con nombres para cod.variable (series "mercado_de_trabajo_arg")
v_variables <- diccionario_variables %>% filter(base=="Mercado_de_Trabajo_Arg") %>% select(cod.variable) 
v_nombres <- diccionario_variables %>% filter(base=="Mercado_de_Trabajo_Arg") %>% select(nombre.variable)
vector_variables_mercado_de_trabajo_arg <- setNames(v_variables$cod.variable, v_nombres$nombre.variable)


estad_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    plot <- function(input_cut){
      
      d <- diamonds[sample(nrow(diamonds), 1000), ] %>% 
        filter(cut==input_cut)
      
      p <- ggplot(data = d, aes(x = carat, y = price)) +
        geom_point(aes(text = paste("Clarity:", clarity))) +
        geom_smooth(aes(colour = cut, fill = cut))
      
      
      ggplotly(p)
    }
    
    output$plot1 <- renderPlotly({plot(input$input1)})
    output$plot2 <- renderPlotly({plot(input$input2)})
    output$plot3 <- renderPlotly({plot(input$input3)})
  })
}

estad_plot_ui <- function(id) {
  ns <- NS(id)
  
  navbarMenu(title = 'Estadísticas',
             
             tabPanel('EPH tasas básicas',
                      value = 'tasas',
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(ns('input1'),label = 'select cut',
                                      choices = diamonds$cut %>% unique(),
                                      selected = 'Premium',
                                      multiple = FALSE)
                        ),
                        mainPanel(plotlyOutput(ns('plot1'))
                        )
                      )
             ),
             
             tabPanel('EPH población',
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(ns('input2'),label = 'select cut',
                                      choices = diamonds$cut %>% unique(),
                                      selected = 'Premium',
                                      multiple = FALSE)
                        ),
                        mainPanel(plotlyOutput(ns('plot2'))
                        )
                      )
             ),
             tabPanel('EPH categoría ocupacional',
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(ns('input3'),label = 'select cut',
                                      choices = diamonds$cut %>% unique(),
                                      selected = 'Premium',
                                      multiple = FALSE)
                        ),
                        mainPanel(plotlyOutput(ns('plot3'))
                        )
                      )
             )
             
  )
}
