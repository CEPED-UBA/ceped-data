

series_plot_server <- function(id) {
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
    output$plot4 <- renderPlotly({plot(input$input4)})
  })
}

series_plot_ui <- function(id) {
  ns <- NS(id)
  
  navbarMenu(title = 'Series',
           
           tabPanel('Serie de salarios',
                    value = 'salarios',
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
           
           tabPanel('Tipo de cambio',
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
           tabPanel('Balanza de pagos',
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
           ),
           tabPanel('IPC Argentina',
                    sidebarLayout(
                      sidebarPanel(
                        selectInput(ns('input4'),label = 'select cut',
                                    choices = diamonds$cut %>% unique(),
                                    selected = 'Premium',
                                    multiple = FALSE)
                      ),
                      mainPanel(plotlyOutput(ns('plot4'))
                      )
                    ))
           
  )
}
