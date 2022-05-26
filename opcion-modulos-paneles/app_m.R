library(shiny)
library(shinythemes)
library(shinycssloaders)
library(glue)
library(DT)
library(plotly)
library(gridExtra)
library(shinyjs)
library(shinydashboard)
library(openxlsx)
library(tidyverse)

options(scipen=999)

btn_style <- "float:right;border-radius: 15px;"



ui <- fluidPage( 
  theme = shinytheme("journal"),
  
  #define fakeClick for buttons
        (tags$head(tags$script(HTML('var fakeClick = function(tabName) {
                                                         var dropdownList = document.getElementsByTagName("a");
                                                         for (var i = 0; i < dropdownList.length; i++) {
                                                         var link = dropdownList[i];
                                                         if(link.getAttribute("data-value") == tabName) {
                                                         link.click();
                                                         };
                                                         }
                                                         };
                                                         '))) ),
  
          navbarPage('ceped-data', id = "pag",
             
             
             tabPanel('Home',
                      fluidRow(
                        
                        column(12,
                               column(6,
                                      tags$div( class="panel panel-primary",
                                                tags$div(class= "panel-heading",
                                                         h3('Estadísticas procesadas')),
                                                tags$div(class="panel-body",
                                                         
                                                         #img(height = 250, width = 250,src = "img/data_preview.png"),
                                                         p('Estadísticas derivadas de bases de datos e información pública procesadas por el CEPED'),
                                                         
                                                         
                                                         tags$a("EPH | Mercado de Trabajo",
                                                                style=btn_style,
                                                                onclick="fakeClick('trabajo_eph')",
                                                                class="btn btn-primary btn-s"
                                                         ),
                                                         
                                                         tags$a("EPH | Población",
                                                                style=btn_style,
                                                                onclick="fakeClick('poblacion_eph')",
                                                                class="btn btn-primary btn-s"
                                                         ), 
                                                         
                                                         tags$a("EPH | Categorías ocupacionales",
                                                                style=btn_style,
                                                                onclick="fakeClick('categoria_ocup_eph')",
                                                                class="btn btn-primary btn-s"
                                                         )
                                                )
                                      )
                               ),  
                               column(6,
                                      tags$div( class="panel panel-warning",
                                                tags$div( class= "panel-heading",
                                                          h3('Series')),
                                                tags$div(class="panel-body",
                                                         #img(height = 250, width = 250,src = "img/methods_preview.png"),
                                                         p('Datos que surgen de líneas de investigación desarrolladas por integrantes del CEPED'),
                                                         tags$a("ver series", style=btn_style,
                                                                onclick="fakeClick('salarios')",
                                                                class="btn btn-warning btn-s")
                                                )
                                      )
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                        column(6,
                               tags$div( class="panel panel-danger",
                                         tags$div(class= "panel-heading",
                                                  h3('Datos de artículos')),
                                         tags$div(class="panel-body",
                                                  
                                                  #img(height = 250, width = 250,src = "img/data_preview.png"),
                                                  p('Presentación interactiva de datos que fueron publicados en artículos de integrantes del CEPED'),
                                                  tags$a("ver presentación",
                                                         style=btn_style,
                                                         onclick="fakeClick('pres')",
                                                         class="btn btn-danger btn-s"
                                                  )
                                         )
                               )
                      ),
                      column(6,
                             tags$div( class="panel panel-default",
                                       tags$div(class= "panel-heading",
                                                h3('Sobre el CEPED')),
                                       tags$div(class="panel-body",
                                                
                                                #img(height = 250, width = 250,src = "img/data_preview.png"),
                                                p('¿Quiénes somos?'),
                                                
                                                tags$a("conocer",
                                                       style=btn_style,
                                                       onclick="fakeClick('CEPED')",
                                                       class="btn btn-default btn-s"
                                                )
                                       )
                             )
                      )
                      
                        )
                      
                      
                      )),
             
             
             
             navbarMenu(title = 'Estadísticas',
                        
                        poblacion_eph_plot_ui(id ='poblacion_eph', title ='Población EPH', v_poblacion_eph) ,
                        categoria_ocup_eph_plot_ui(id ='categoria_ocup_eph', title ='Categorías ocupacionales EPH', v_categoria_ocup_eph),
                        trabajo_eph_plot_ui(id ='trabajo_eph', title ='Mercado de Trabajo EPH', v_trabajo_eph)
                         

                        
             ),
             
             
             navbarMenu(title = 'Series',
                        
             #series_plot_ui(id ='salarios', title ='Serie de salarios', v_salarios),
             salarios_plot_ui(id ='salarios', title ='Salarios Internacionales'),
             series_plot_ui(id ='tc', title ='Tipo de cambio',v_tc),
             #series_plot_ui(id ='bp', title ='Balanza de pagos',v_bp),
             bp_plot_ui(id ='bp', title ='Balance de Pagos Argentina'),
             ipc_plot_ui(id ='ipc', title ='IPC Argentina', v_ipc)
             
             ),
             
             papers_plot_ui('ejemplo3'),
             ceped_plot_ui('ejemplo4'),
             
             #etc
             
             #footer = "Portal de difusión de datos del Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED-UBA)"
  )
  
  
)


##### server #####

server <- function (input, output,session) {
  
  # observeEvent(input$conocer, {
  #   updateNavbarPage(session, "pag",
  #                     selected = "CEPED")
  # })


  

  # ##
  # 
  # # Output modules ----------------------------------------------------------
   #sample_plot_server('ejemplo')
  categoria_ocup_eph_plot_server('categoria_ocup_eph')
  trabajo_eph_plot_server('trabajo_eph')
  poblacion_eph_plot_server('poblacion_eph')
  #series_plot_server('salarios')
  series_plot_server('tc')
 # series_plot_server('bp')
  bp_plot_server('bp')
  salarios_plot_server('salarios')
  ipc_plot_server('ipc')
  papers_plot_server('ejemplo3')
  ceped_plot_server('ejemplo4')
  # 
}


##### RUN #####

shinyApp(ui, server)


