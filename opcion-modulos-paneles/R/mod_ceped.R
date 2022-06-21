ceped_plot_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}

ceped_plot_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel('Sobre el CEPED',
           value = 'CEPED',

           fluidPage(
             
             tags$div(
               h3("Quiénes somos?"),
               hr(),
               p("El", strong(" Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED)"), "es un espacio académico de Investigación y Docencia. Fue creado en 1993, producto del interés de un grupo de investigadores/as, como ámbito de trabajo y discusión.")

               ,
               p("Es parte constitutiva del Instituto de Investigaciones Económicas de la Universidad de Buenos Aires, con sede en la Facultad de Ciencias Económicas. Fue instituido por resolución del Consejo Directivo de la Facultad. Es miembro del Consejo Latinoamericano de Ciencias Sociales, CLACSO, desde 1997.")
               
               ,
               p("El CEPED tiene por objetivos la investigación económica y social, la formación de docentes e investigadoras/es y la asistencia técnica a organismos públicos y privados.")
               
               ,
               p("El campo de estudio del Centro está constituido por las siguientes temáticas: el mercado de trabajo, la dinámica e inserción ocupacional de la población, la problemática del desarrollo argentino y de América Latina, las cuestiones urbana y regional y la acción gubernamental en relación a los tópicos anteriores.")
               
               ,
               p("Son integrantes del CEPED investigadoras/es y docentes de la Universidad de Buenos Aires, algunas/os de amplia y reconocida trayectoria académica. Un conjunto importante de jóvenes profesionales y estudiantes permanece en constante vínculo de trabajo, insertos/as en los proyectos de investigación y docencia.")
               
               ,
               p("El CEPED lleva adelante tareas de divulgación a través de la realización de eventos científicos, la edición de publicaciones con los resultados de sus estudios e investigaciones, el intercambio de experiencias con otras instituciones y la participación de sus integrantes en eventos académicos.")
               
               ,
               p("A los fines del desarrollo de las actividades de asesoramiento y consultoría, el CEPED cuenta, entre sus integrantes, con profesionales de vasta experiencia profesional."),

               a(href="http://www.economicas.uba.ar/institutos_y_centros/ceped/", "Página web del CEPED") 
             )
           )
           
           )
  
  
}