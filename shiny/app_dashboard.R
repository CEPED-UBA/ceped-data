library(shiny)
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(shinydashboard)
library(cowplot)
library(magick)

options(scipen = 9999)

serie_salarios <- readRDS("../data/salarios.RDS")
tipo_cambio_argentina <- readRDS("../data/Tipo_Cambio_Arg.RDS")
mercado_de_trabajo_arg <- readRDS("../data/Mercado_de_Trabajo_Arg.RDS")
poblacion_eph <- readRDS("../data/Poblacion_eph.RDS")
diccionario_variables <- read.xlsx("../data/diccionario_cod.variable.xlsx")

# serie_salarios <- readRDS("data/salarios.RDS")
# tipo_cambio_argentina <- readRDS("data/Tipo_Cambio_Arg.RDS")
# mercado_de_trabajo_arg <- readRDS("data/Mercado_de_Trabajo_Arg.RDS")
# poblacion_eph <- readRDS("data/Poblacion_eph.RDS")
# diccionario_variables <- read.xlsx("data/diccionario_cod.variable.xlsx")

#Voy agregando a una lista los dataframes que vamos a subir
base_binded <- bind_rows(serie_salarios,tipo_cambio_argentina, poblacion_eph)

#Armo vectores para inputs
vector_bases <- unique(diccionario_variables$base)

#Las opciones aparecen en función de lo que se elija, lo saco de acá
#vector_variables <- setNames(diccionario_variables$cod.variable, diccionario_variables$nombre.variable)

#Vector con nombres para cod.variable (series salario)
v_variables <- diccionario_variables %>% filter(base=="Serie_salarios") %>% select(cod.variable) 
v_nombres <- diccionario_variables %>% filter(base=="Serie_salarios") %>% select(nombre.variable)
vector_variables_serie_salario <- setNames(v_variables$cod.variable, v_nombres$nombre.variable)

#Vector con nombres para cod.variable (series "Tipo_Cambio_Arg")
v_variables <- diccionario_variables %>% filter(base=="Tipo_Cambio_Arg") %>% select(cod.variable) 
v_nombres <- diccionario_variables %>% filter(base=="Tipo_Cambio_Arg") %>% select(nombre.variable)
vector_variables_tipo_cambio_argentina <- setNames(v_variables$cod.variable, v_nombres$nombre.variable)

#Vector con nombres para cod.variable (series "poblacion_eph")
v_variables <- diccionario_variables %>% filter(base=="Poblacion_eph") %>% select(cod.variable) 
v_nombres <- diccionario_variables %>% filter(base=="Poblacion_eph") %>% select(nombre.variable)
vector_variables_poblacion_eph <- setNames(v_variables$cod.variable, v_nombres$nombre.variable)


#vector_variables_serie_salario <- setNames(diccionario_variables$cod.variable[diccionario_variables$base=="Series_salario"], diccionario_variables$nombre.variable[base=="Series_salario"])


vector_paises <- unique(serie_salarios$nombre.pais)
vector_desagreg <- c("País","etc")

header <- dashboardHeader(title = "ceped.data")




sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar",
    style = "position: relative; overflow: visible;",
    menuItem(text = "Areas Temáticas", icon = icon("folder"), tabName = "Areas Temáticas"),#Para que aparezca seleccionado al ppio

    
    menuItem(text = "Series de salario", icon = icon("tools"), tabName = "Series de salario"),
    div( id = 'sidebar_salario',
         conditionalPanel("input.sidebar === 'Series de salario'",
                          selectizeInput("serie_salario",
                                         "Seleccionar una Serie", 
                                         choices =  vector_variables_serie_salario, 
                                         selected = vector_variables_serie_salario[1],
                                         width = "300px",
                                         multiple = F))
         ),
    div( id = 'sidebar_salario_pais',
         conditionalPanel("input.sidebar === 'Series de salario'",
                          selectInput(inputId = "pais_id",
                                      label ="País:",
                                      choices =  unique(serie_salarios$nombre.pais),
                                      multiple = T,
                                      selected = vector_paises[1:2]
                          ),
    )
    ),
    div( id = 'sidebar_salario_periodo',
         conditionalPanel("input.sidebar === 'Series de salario'",
    sliderInput("id_periodo_sal", "Período:", value = c(min(serie_salarios$ANO4),max(serie_salarios$ANO4)), min = min(serie_salarios$ANO4), max = max(serie_salarios$ANO4))
    )
    ),
    
    menuItem(text = "Tipo de Cambio Argentina", icon = icon("money-bill-alt"), tabName = "Tipo de Cambio Argentina",selected = F),
    div( id = 'sidebar_tc',
         conditionalPanel("input.sidebar === 'Tipo de Cambio Argentina'",
                          selectizeInput("serie_tc",
                                         "Seleccionar una Serie", 
                                         choices =  vector_variables_tipo_cambio_argentina, 
                                         selected = NULL,  
                                         width = "200px",
                                         multiple = F))
    ),
    div( id = 'sidebar_tc_periodo',
         conditionalPanel("input.sidebar === 'Tipo de Cambio Argentina'",
                          sliderInput("id_periodo_tc", "Período:", value = c(min(tipo_cambio_argentina$ANO4), max(tipo_cambio_argentina$ANO4)), min = min(tipo_cambio_argentina$ANO4), max = max(tipo_cambio_argentina$ANO4))
         )
    ),
    
    
    
    
    menuItem(text = "Población EPH Argentina", icon = icon("users", lib = "font-awesome"), tabName = "Población EPH Argentina",selected = F),
    div( id = 'sidebar_poblacion_eph',
         conditionalPanel("input.sidebar === 'Población EPH Argentina'",
                          selectizeInput("serie_poblacion_eph",
                                         "Seleccionar una Serie", 
                                         choices =  vector_variables_poblacion_eph, 
                                         selected = NULL,  
                                         width = "200px",
                                         multiple = F))
    ),
    div( id = 'sidebar_poblacion_eph_periodo',
         conditionalPanel("input.sidebar === 'Población EPH Argentina'",
                          sliderInput("id_periodo_poblacion_eph", "Período:", 
                                      value = c(as.numeric(min(poblacion_eph$ANO4.trim)), as.numeric(max(poblacion_eph$ANO4.trim))), 
                                      min = as.numeric(min(poblacion_eph$ANO4.trim)), 
                                      max = as.numeric(max(poblacion_eph$ANO4.trim)))
         )
    ),
    
    actionButton("actualizar", "Actualizar Series")
    
  )
)

body <- dashboardBody(
  fluidRow(
    box(title = textOutput("titulo"), footer= "ceped.data | Portal de difusión de datos del Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED-UBA)", 
    solidHeader = T,width = 20,
        plotOutput("ploteado"))),
  fluidRow(box(tableOutput("tablita")),
           column(width = 6,
#             box(width = NULL, textOutput("titulo")),
             box(title = "Metadata", width = NULL, textOutput("metadata")),
             box(width = NULL,
                 downloadButton('downloadTable','Descargar tabla')),
             box(width = NULL,
                 downloadButton('downloadPlot','Descargar gráfico')),
             box(width = NULL,
                 downloadButton('downloadTable_md','Descargar metadata')))
           ))
                      
server <- function(input, output) {

#cual <- observe(input$sidebar) 
  
  variables_adecuadas <- reactive({
    if(input$sidebar == "Series de salario"){input$serie_salario} else
      if(input$sidebar == "Tipo de Cambio Argentina"){input$serie_tc} else
        if(input$sidebar == "Población EPH Argentina"){input$serie_poblacion_eph}
  })

  
  tab_filtrada <- eventReactive(input$actualizar, {
 
    
    periodo_adecuado <- 
      if(input$sidebar == "Series de salario"){c(input$id_periodo_sal[1]:input$id_periodo_sal[2])} else
        if(input$sidebar == "Tipo de Cambio Argentina"){c(input$id_periodo_tc[1]:input$id_periodo_tc[2])} else
          if(input$sidebar == "Población EPH Argentina"){c(input$id_periodo_poblacion_eph[1]:input$id_periodo_poblacion_eph[2])}
    
    
    
    df <- 
    if(input$sidebar == "Series de salario"){
      base_binded %>% ungroup() %>%
      filter(cod.variable  %in%  variables_adecuadas()) %>% 
      filter(nombre.pais %in% input$pais_id) %>% 
      filter(ANO4 %in% periodo_adecuado)} else
    
    if(input$sidebar == "Tipo de Cambio Argentina"){  
      base_binded %>% ungroup() %>%
      filter(cod.variable  %in%  variables_adecuadas()) %>% 
      filter(ANO4 %in% periodo_adecuado) } else
        
    if(input$sidebar == "Población EPH Argentina"){
      base_binded %>% ungroup() %>%
        filter(cod.variable  %in%  variables_adecuadas()) %>% 
        select(-c(ANO4))}


  })
  
  output$tablita <- renderTable({
    tab_filtrada()
  })
  
  output$downloadTable <- downloadHandler(
    
    
    filename = function(){paste(variables_adecuadas(),"_",input$pais_id,'.xlsx',sep='')},
    content = function(file){
      
      
      write.xlsx(tab_filtrada(), file)    }
  )
  

  plot <- eventReactive(input$actualizar, {

   
    if(input$sidebar == "Series de salario"){    
      tab_filtrada() %>%
        ggplot(
          aes(y = valor, x = as.factor(ANO4),group = iso3c,color = iso3c))+
        geom_line(size = 1) +
        labs(color= "País",
             #title= str_wrap(titulo(),60),
             # y = str_wrap(nombre_variable,40),
             x = "Año")+
        theme_minimal()+
        theme(text = element_text(size = 9),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10),
              legend.position = "bottom",
              plot.title= element_text(size=12, face="bold"))+
        theme(axis.text.x = element_text(angle = 90))
      
        } else
          
          
      if(input$sidebar == "Tipo de Cambio Argentina"){
        tab_filtrada() %>%
          ggplot(
            aes(y = valor, x = as.factor(ANO4),group = iso3c))+
          geom_line(size = 1) +
          labs(color= "País",
               #title= str_wrap(titulo(),60),
               # y = str_wrap(nombre_variable,40),
               x = "Año")+
          theme_minimal()+
          theme(text = element_text(size = 9),
                axis.text.x = element_text(size=10),
                axis.text.y = element_text(size=10),
                legend.position = "bottom",
                plot.title= element_text(size=12, face="bold"))+
          theme(axis.text.x = element_text(angle = 90))
          } else
            
        if(input$sidebar == "Población EPH Argentina"){    
          
          tab_filtrada() %>%
            ggplot(
              aes(y = valor, x = ANO4.trim ,group = iso3c))+
            geom_line(size = 1) +
            labs(color= "País",
                 #title= str_wrap(titulo(),60),
                 # y = str_wrap(nombre_variable,40),
                 x = "Año")+
            theme_minimal()+
            theme(text = element_text(size = 9),
                  axis.text.x = element_text(size=10),
                  axis.text.y = element_text(size=10),
                  legend.position = "bottom",
                  plot.title= element_text(size=12, face="bold"))+
            theme(axis.text.x = element_text(angle = 90))}


  })


  output$ploteado <- renderPlot(
    
    if(!input$actualizar) {
      
      ggdraw() +
        draw_image("logo_ceped.png")
    }
    
     else{
       
    plot()
    
     }
    )
  
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste(variables_adecuadas(),"_",input$pais_id,'.png',sep='')},
    content = function(file){
      
      
      ggsave(file,plot=plot(), width=8, height=4)
    }
  )
  
  
  # metadatos <- eventReactive(input$actualizar, { 
  #   
  #   diccionario_variables$metadata[diccionario_variables$cod.variable == variables_adecuadas()]
  #   
  #   })
  
   metadatos <-  eventReactive(input$actualizar, {
     if(input$sidebar == "Series de salario") 
             {diccionario_variables$metadata[diccionario_variables$cod.variable == input$serie_salario] } else
     
     if(input$sidebar == "Tipo de Cambio Argentina")
             {diccionario_variables$metadata[diccionario_variables$cod.variable == input$serie_tc] } else
               
     if(input$sidebar == "Población EPH Argentina")
             {diccionario_variables$metadata[diccionario_variables$cod.variable == input$serie_poblacion_eph] }           
  
        })
  
  
  output$metadata <- renderText({metadatos()})
  
  
  titulo <- eventReactive(input$actualizar, {
   
     if(input$sidebar == "Series de salario")  {
      
    lista_paises <-  paste0(input$pais_id, collapse = ", ")
    lista_paises <- sub(",([^,]*)$", " y\\1", lista_paises)   
    nombre_variable <- diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==input$serie_salario]
    titulo <- paste0(nombre_variable ," para ", lista_paises , ". Años: ", input$id_periodo_sal[1], " al ", input$id_periodo_sal[2]) 
   
     } else
      
    if(input$sidebar == "Tipo de Cambio Argentina")  {
 
           nombre_variable <- diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==input$serie_tc]
      titulo <- paste0(nombre_variable ," para Argentina", ". Años: ", input$id_periodo_tc[1], " al ", input$id_periodo_tc[2]) 
        
      } else
    
    if(input$sidebar == "Población EPH Argentina")  {
      
      nombre_variable <- diccionario_variables$nombre.variable[diccionario_variables$cod.variable ==input$serie_poblacion_eph]
      titulo <- paste0(nombre_variable ," para Argentina", ". Años: ", input$id_periodo_poblacion_eph[1], " al ", input$id_periodo_poblacion_eph[2]) 
      
       }
    
    
  })
    
    
  output$titulo <- renderText({titulo()})
    
}

shinyApp(
  ui = dashboardPage(header = header,sidebar =  sidebar,body =  body,skin = "red"),
  server = server
)

