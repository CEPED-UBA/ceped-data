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
  tc_plot_server('tc')
 # series_plot_server('bp')
  bp_plot_server('bp')
  salarios_plot_server('salarios')
  ipc_plot_server('ipc')
  papers_plot_server('ejemplo3')
  ceped_plot_server('ejemplo4')
  # 
}


