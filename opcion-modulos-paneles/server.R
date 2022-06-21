server <- function (input, output,session) {

  categoria_ocup_eph_plot_server('categoria_ocup_eph')
  trabajo_eph_plot_server('trabajo_eph')
  poblacion_eph_plot_server('poblacion_eph')
  
  tc_plot_server('tc')
  bp_plot_server('bp')
  salarios_plot_server('salarios')
  ipc_plot_server('ipc')

  ceped_plot_server('ceped_presenta')
  
  publicaciones_server('publicaciones')
}


