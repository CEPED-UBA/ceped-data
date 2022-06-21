publicaciones_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}


publicaciones_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel('Líneas de investigación',
           value = 'publicaciones',
           
           fluidPage(
             
             tags$div(
              
               
               h3("Líneas de investigación actuales"),
               hr(), 
               h4("Reproducción familiar"), 
               p("Se estudia relación entre las condiciones de inserción laboral de las personas y las condiciones de reproducción de los hogares."),
               
               a(href="https://caropradier.shinyapps.io/app_ALAST/", "Ver datos interactivos") , 
               
               h4("Precariedad mundial"), 
               p("Esta línea de investigación busca realizar estimaciones y comparaciones internacionales de la calidad del empleo en todo el mundo."),
               #dejo puesto el de reproducción familiar porque no tengo el link
               a(href="https://caropradier.shinyapps.io/app_ALAST/", "Ver datos interactivos") , 
             )
           )
           
  )
  
  
}