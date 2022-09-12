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
               # h4("Reproducción familiar"), 
               # p("Se estudia relación entre las condiciones de inserción laboral de las personas y las condiciones de reproducción de los hogares."),
               # 
               # a(href="https://caropradier.shinyapps.io/app_ALAST/", "Ver datos interactivos") , 
               
               h4("Población Urbana Argentina"), 
               p("Esta línea de investigación se basa en la homogeneización de las estimaciones de población urbana total por localidades, de los Censos Nacionales de Argentina."),
               #p("Esta línea de investigación busca realizar estimaciones y comparaciones internacionales de la calidad del empleo en todo el mundo."),
               #dejo puesto el de reproducción familiar porque no tengo el link
               a(href="https://guidowe45.shinyapps.io/app_censo2010/", "Ver datos interactivos") , 
             )
           )
           
  )
  
  
}