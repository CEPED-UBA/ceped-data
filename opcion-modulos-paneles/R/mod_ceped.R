

ceped_plot_server <- function(id) {
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
  })
}

ceped_plot_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel('CEPED',
                      value = 'CEPED',
           titlePanel("CEPED"),
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
             )
             
  
}
