
vector_publis <- c("Reproducción familiar", "Precariedad mundial")

base_resumenes <- data.frame() #hacer un excel

papers_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    resumen <- function(publi){
      base_resumenes %>% 
        filter(publicacion == publi) %>% 
        select(resumen)
    }
    
    #output$resumen_publi <- renderText({resumen(input$publicacion_id)})
  })
}


papers_plot_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel('Publicaciones',
           value = 'pres',
           sidebarLayout(
             sidebarPanel(
               selectInput(ns('publicacion_id'),label = 'Elegir una publicación:',
                           choices = vector_publis,
                           selected = vector_publis[1],
                           multiple = FALSE)
             ),
             mainPanel(
               box(title = "Resumen", width = NULL, textOutput(ns('resumen_publi'))),
               box(title = "Acceder a publicación completa", width = NULL, textOutput(ns('link_revista'))),
               box(title = "Acceder a app interactiva", width = NULL, textOutput(ns('link_app')))
                           
                                    
                                    
             )
           )
  )
  
  
}
