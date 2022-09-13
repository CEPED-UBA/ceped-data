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
library(scales)

options(scipen=999)

btn_style <- "float:right;border-radius: 15px;"

jscode <- "
shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.unbind('click.tab');
tab.removeClass('disabled');
}
"
css <- "
.nav li a.disabled {
background-color: #FFFFFF !important;
color: #333 !important;
cursor: default  !important;
border-color: #FFFFFF !important;
}"


ui <- fluidPage( 
  

  theme = shinytheme("journal"), 
  
  #define fakeClick for buttons
        (tags$head(tags$script(HTML(
                          'var fakeClick = function(tabName) {
                           var dropdownList = document.getElementsByTagName("a");
                           for (var i = 0; i < dropdownList.length; i++) {
                           var link = dropdownList[i];
                           if(link.getAttribute("data-value") == tabName) {
                           link.click();
                           };
                           }
                           };
                           ')),
                   tags$style(HTML("                                 
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(5) {
                           float: right;
                           }
                           "))
                   ) ),
  
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jscode, functions = c("disableTab","enableTab")),
  shinyjs::inlineCSS(css),
  
 
  
  
  
          navbarPage('', id = "pag",
                     
                    
             tabPanel('Inicio',
                      
                      fluidRow(#column(12, 
                               #column(10, 
                      tags$div(
                        h1("Portal de difusión de datos del CEPED", 
                           style ="text-align: center" ))#), 
                          #      column(2,
                          # img(src = "img/logo_ceped2.png", width = 270))
                      #)
                      ),
                      hr(),
                      
                      tags$div(style = "display:flex;align-items: center;justify-content: center;",
                      tags$p("Este es el Portal de difusión de datos del Centro de Estudios sobre Población Empleo y Desarrollo (CEPED) de la Facultad de Cs. Económicas – UBA. El objetivo de este sitio es poner a disposición un conjunto de estadísticas que constituyen una de las bases de los distintos trabajos de investigación realizados en el Centro. Por un lado, encontrarán diversas series que fueron publicadas previamente en formato de artículos en revistas científicas o como Documentos de Trabajo de nuestro Centro (donde podrán encontrar con mayor detalle los criterios metodológicos adoptados). Por el otro, se presentan datos que surgen del procesamiento de bases oficiales (dentro de los que se destaca la Encuesta Permanente de Hogares del INDEC). La difusión de estos datos no busca reemplazar a los datos publicados por los organismos oficiales de estadística. En este sentido, se recomienda consultar las fuentes originales para una mejor información sobre la metodología de recolección de datos.",
                             style = "color:black;text-align: justify;width:90%"))
                      ,
                      
                      
                      hr(),
                      
                      HTML('&nbsp;'),
                      
                      fluidRow(
                        
                        column(12,
                               tags$div(style = "display:flex",
                              # column(6,
                                      tags$div( class="panel panel-primary",
                                                style = "border-color: #20639b;width:50%",
                                                
                                                tags$div(class= "panel-heading",
                                                         style="background:#20639b;border-color: #20639b",
                                                         h3('Encuesta Permanente de Hogares', style = "color: white")),
                                                tags$div(class="panel-body",
                                                         
                                                         
                                                         #img(height = 250, width = 250,src = "img/data_preview.png"),
                                                         p('Estadísticas procesadas por el CEPED en base a microdatos de la Encuesta Permanente de Hogares (EPH-INDEC)', style = "color: black")),
                                                tags$div(class="panel-body",
                                                         style = "display:flex; flex-direction: row; justify-content: center; align-items: center",
                                                         
                                                         br(),
                                                         
                                                         
                                                         tags$a("Tasas básicas",
                                                                style=paste0(btn_style,"background:#20639b;border-color: #20639b;color: white;font-size:14px"),
                                                                onclick="fakeClick('tasas_basicas_eph')",
                                                                class="btn btn-primary btn-s"
                                                         ),
                                                         
                                                         br(),
                                                         HTML('&nbsp;'),
                                                         br(),
                                                         
                                                         tags$a("Categorías ocupacionales",
                                                                style=paste0(btn_style,"background:#20639b;border-color: #20639b;color: white;font-size:14px"),
                                                                onclick="fakeClick('categoria_ocup_eph')",
                                                                class="btn btn-primary btn-s"
                                                         )), 
                                                         
                                                tags$div(class="panel-body",
                                                         style = "display:flex; flex-direction: row; justify-content: center; align-items: center",
                                                         

                                                         tags$a("Empleo según registro",
                                                                style=paste0(btn_style,"background:#20639b;border-color: #20639b;color: white;font-size:14px"),
                                                                onclick="fakeClick('precariedad_eph')",
                                                                class="btn btn-primary btn-s"
                                                         ), 
                                                         
                                                         br(),
                                                         HTML('&nbsp;'),
                                                         br()
                                                         
                                                         ,
                                                         
                                                         tags$a("Empleo por ramas de actividad",
                                                                style=paste0(btn_style,"background:#20639b;border-color: #20639b;color: white;font-size:14px"),
                                                                onclick="fakeClick('rama_eph')",
                                                                class="btn btn-primary btn-s"
                                                         )
                                                         
                                                )
                                #      )
                               ),  
                               #column(6,
                              
                              #espacio entre cajas
                              HTML('&nbsp;'),
                              HTML('&nbsp;'),
                              HTML('&nbsp;'),
                              HTML('&nbsp;'),
                              
                                      tags$div( class="panel panel-warning",
                                                style = "border-color: #3e83b5;width:50%",
                                                
                                                tags$div( class= "panel-heading",
                                                          style="background:#3e83b5;border-color: #3e83b5",
                                                          h3('Series')),
                                                tags$div(class="panel-body",
                                                         #img(height = 250, width = 250,src = "img/methods_preview.png"),
                                                         p('Series de largo plazo que surgen de líneas de investigación desarrolladas por integrantes del CEPED', style = "color: black")),
                                                         
                                                         tags$div(class="panel-body",
                                                                  style = "display:flex; flex-direction: row; justify-content: center; align-items: center",
                                                         tags$a("Internacional | Salarios", style=paste0(btn_style,"background:#3e83b5;border-color: #3e83b5;color: black;font-size:14px"),
                                                                onclick="fakeClick('salarios')",
                                                                class="btn btn-warning btn-s"), 
                                                         
                                                         br(),
                                                         HTML('&nbsp;'),
                                                         br(),
                                                         
                                                         # tags$a("Argentina | Tipo de cambio", style=paste0(btn_style,"background:#3e83b5;border-color: #3e83b5;color: black;font-size:14px"),
                                                         #        onclick="fakeClick('tc')",
                                                         #        class="btn btn-warning btn-s"), 
                                                         # 
                                                         # br(),
                                                         # p(''),
                                                         # br()
                                                        
                                                         
                                                         tags$a("Argentina | Balance de Pagos", style=paste0(btn_style,"background:#3e83b5;border-color: #3e83b5;color: black;font-size:14px"),
                                                                onclick="fakeClick('bp')",
                                                                class="btn btn-warning btn-s"), 
                                                         
                                                         br(),
                                                         HTML('&nbsp;'),
                                                         br()
                                                         ),
                                                tags$div(class="panel-body",
                                                         style = "display:flex; flex-direction: row; justify-content: center; align-items: center",
                                                         
                                                         
                                                         tags$a("Argentina | Distrib. Funcional", style=paste0(btn_style,"background:#3e83b5;border-color: #3e83b5;color: black;font-size:14px"),
                                                                onclick="fakeClick('df')",
                                                                class="btn btn-warning btn-s"), 
                                                         
                                                         br(),
                                                         HTML('&nbsp;'),
                                                         br(),
                                                         
                                                         tags$a("Argentina | IPC", style=paste0(btn_style,"background:#3e83b5;border-color: #3e83b5;color: black;font-size:14px"),
                                                                onclick="fakeClick('ipc')",
                                                                class="btn btn-warning btn-s")
                                                )
                                                
                                      )
                              )
                        )
                      ),
                      fluidRow(
                        column(12,
                               tags$div(style = "display:flex;align-items: center;justify-content: center;",
                        #column(6,
                               tags$div( class="panel panel-danger",
                                         style = "border-color: #f6d55c",
                                         
                                         tags$div(class= "panel-heading",
                                                  style="background:#f6d55c;border-color: #f6d55c",
                                                  h3('Datos de artículos')),
                                         tags$div(class="panel-body",
                                                  style = "display:flex; flex-direction: row; justify-content: center; align-items: center",
                                                  #img(height = 250, width = 250,src = "img/data_preview.png"),
                                                  p('Presentación interactiva de datos que fueron publicados en artículos de integrantes del CEPED', style = "color: black;align-text:center")),
                                         tags$div(class="panel-body",
                                                  style = "display:flex; flex-direction: row; justify-content: center; align-items: center",
                                                  tags$a("Explorar líneas de investigación",
                                                         style=paste0(btn_style,"background:#f6d55c;border-color: #f6d55c;color: black;font-size:14px"),
                                                         onclick="fakeClick('publicaciones')",
                                                         class="btn btn-danger btn-s"
                                                  )
                                         )
                             #  )
                      ),
                      #espacio entre cajas
                    #  HTML('&nbsp;'),
                    #  HTML('&nbsp;'),
                    #  HTML('&nbsp;'),
                    #  HTML('&nbsp;'),
                      #column(6,
                            
                      ),
                    
                    tags$div( class="panel panel-default",
                              style = "border-color: #3caea3",
                              
                              tags$div(class= "panel-heading",
                                       style="background:#3caea3;border-color: #3caea3",
                                       h3('Sobre el CEPED', style="text-align:center")),
                              tags$div(class="panel-body",
                                       style = "display:flex; flex-direction: row; justify-content: center; align-items: center",
                                       
                                       #img(height = 250, width = 250,src = "img/data_preview.png"),
                                       p('¿Quiénes somos?', style = "color: black")),
                              
                              tags$div(class="panel-body",
                                       style = "display:flex; flex-direction: row; justify-content: center; align-items: center",
                                       
                                       tags$a("Conocer",
                                              style=paste0(btn_style,"background:#3caea3;border-color: #3caea3;color: black;font-size:14px"),
                                              href="http://www.economicas.uba.ar/institutos_y_centros/ceped/", target="_blank",
                                              class="btn btn-default btn-s"
                                       )
                              )
                    )
                      
                        )
                      
                      
                      )),
             
             
             
             navbarMenu(title = 'Encuesta Permanente de Hogares',
                        
                        categoria_ocup_eph_plot_ui(id ='categoria_ocup_eph', title ='Categorías ocupacionales'),
                        tasas_basicas_eph_plot_ui(id ='tasas_basicas_eph', title ='Tasas básicas'), 
                        precariedad_eph_plot_ui(id ='precariedad_eph', title ='Empleo según registro'), 
                        rama_eph_plot_ui(id ='rama_eph', title ='Empleo por ramas')
                         

                        
             ),
             
             
             navbarMenu(title = 'Series',
                        
        
             salarios_plot_ui(id ='salarios', title ='Salarios Internacionales'),
             #tc_plot_ui(id ='tc', title ='Tipo de cambio'),
             
             bp_plot_ui(id ='bp', title ='Balance de Pagos Argentina'),
             df_plot_ui(id ='df', title ='Distrib. Funcional Argentina', v_df),
             ipc_plot_ui(id ='ipc', title ='IPC Argentina', v_ipc)
             
             ),
             
            publicaciones_ui('publicaciones'),
            
            #ceped_plot_ui('ceped_presenta'),
            
            tabPanel(value = "logo_ceped",title=div(style = "display:flex; flex-direction: row; justify-content: flex-end; align-items: start; width: 100px", 
                                                    img(src="img/logo_ceped2.png",height="100%", width="100%", style = "add padding: 0px" )
                                          
                                                    
                                                    
            ))
            
            
            
             
            
             
             #footer = "Portal de difusión de datos del Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED-UBA)"
  )
  
  
)


