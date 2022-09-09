server <- function (input, output,session) {
  
  shinyjs::js$disableTab("logo_ceped")

  rama_eph_plot_server('rama_eph')
  categoria_ocup_eph_plot_server('categoria_ocup_eph')
  tasas_basicas_eph_plot_server('tasas_basicas_eph')
  precariedad_eph_plot_server('precariedad_eph')
  
  #tc_plot_server('tc')
  bp_plot_server('bp')
  salarios_plot_server('salarios')
  ipc_plot_server('ipc')
  df_plot_server('df')
  

  ceped_plot_server('ceped_presenta')
  
  publicaciones_server('publicaciones')
}


