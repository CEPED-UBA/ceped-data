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
  
          navbarPage('', id = "pag",
             
             
             tabPanel('Home',
                      
                      tags$div(
                        h1("ceped.data | Portal de difusión de datos del Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED)")),
                      
                      hr(),
                      
                      fluidRow(
                        
                        column(12,
                               column(6,
                                      tags$div( class="panel panel-primary",
                                                tags$div(class= "panel-heading",
                                                         h3('Encuesta Permanente de Hogares')),
                                                tags$div(class="panel-body",
                                                         
                                                         #img(height = 250, width = 250,src = "img/data_preview.png"),
                                                         p('Estadísticas procesadas por el CEPED en base a microdatos de la Encuesta Permanente de Hogares (EPH-INDEC)'),
                                                         
                                                         br(),
                                                         
                                                         
                                                         tags$a("Mercado de Trabajo",
                                                                style=btn_style,
                                                                onclick="fakeClick('trabajo_eph')",
                                                                class="btn btn-primary btn-s"
                                                         ),
                                                         
                                                         br(),
                                                         p(''),
                                                         br(),
                                                         
                                                         tags$a("Población",
                                                                style=btn_style,
                                                                onclick="fakeClick('poblacion_eph')",
                                                                class="btn btn-primary btn-s"
                                                         ), 
                                                         
                                                         br(),
                                                         p(''),
                                                         br(),
                                                         
                                                         tags$a("Categorías ocupacionales",
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
                                                         tags$a("Internacional | Salarios", style=btn_style,
                                                                onclick="fakeClick('salarios')",
                                                                class="btn btn-warning btn-s"), 
                                                         
                                                         br(),
                                                         p(''),
                                                         br(),
                                                         
                                                         tags$a("Argentina | Tipo de cambio", style=btn_style,
                                                                onclick="fakeClick('tc')",
                                                                class="btn btn-warning btn-s"), 
                                                         
                                                         br(),
                                                         p(''),
                                                         br(),
                                                         
                                                         tags$a("Argentina | Balance de Pagos", style=btn_style,
                                                                onclick="fakeClick('bp')",
                                                                class="btn btn-warning btn-s"), 
                                                         
                                                         br(),
                                                         p(''),
                                                         br(),
                                                         
                                                         tags$a("Argentina | IPC", style=btn_style,
                                                                onclick="fakeClick('ipc')",
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
                                                  tags$a("Explorar líneas de investigación",
                                                         style=btn_style,
                                                         onclick="fakeClick('publicaciones')",
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
                                                
                                                tags$a("Conocer",
                                                       style=btn_style,
                                                       onclick="fakeClick('CEPED')",
                                                       class="btn btn-default btn-s"
                                                )
                                       )
                             )
                      )
                      
                        )
                      
                      
                      )),
             
             
             
             navbarMenu(title = 'Encuesta Permanente de Hogares',
                        
                        poblacion_eph_plot_ui(id ='poblacion_eph', title ='Población', v_poblacion_eph) ,
                        categoria_ocup_eph_plot_ui(id ='categoria_ocup_eph', title ='Categorías ocupacionales', v_categoria_ocup_eph),
                        trabajo_eph_plot_ui(id ='trabajo_eph', title ='Mercado de Trabajo', v_trabajo_eph)
                         

                        
             ),
             
             
             navbarMenu(title = 'Series',
                        
             #series_plot_ui(id ='salarios', title ='Serie de salarios', v_salarios),
             salarios_plot_ui(id ='salarios', title ='Salarios Internacionales'),
             tc_plot_ui(id ='tc', title ='Tipo de cambio'),
             #series_plot_ui(id ='bp', title ='Balanza de pagos',v_bp),
             bp_plot_ui(id ='bp', title ='Balance de Pagos Argentina'),
             ipc_plot_ui(id ='ipc', title ='IPC Argentina', v_ipc)
             
             ),
             
            publicaciones_ui('publicaciones'),
            
             ceped_plot_ui('ceped_presenta')#,
             
            
             
             #footer = "Portal de difusión de datos del Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED-UBA)"
  )
  
  
)


