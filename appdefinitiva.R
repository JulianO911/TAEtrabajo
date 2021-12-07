
# Load R packages
library(shiny)
library(shinythemes)
library(DT)
library(rgdal)
library(leaflet)
library(mapview)
library(shinycssloaders)
library(shinyWidgets)
library(data.table)
library(ggplot2)
library(kableExtra)
library(tidyverse)
library(knitr)
library(sqldf)
library(reticulate)
library(FSinR)
library(caret)
library(hash)
library(plotly)
library(lubridate)
library(dplyr)
load("./BD_nueva/accidentesMDE2.RData")

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage(
                  "Accidentalidad en Medellin",
                  tabPanel("Presentacion",
                           fluidRow(id='header1',
                                    column(12,
                                           HTML("
                              <div style='text-align: center;margin-top:80px;'>
                                <img src='Collision.png' width='500px'/>  
                                  <strong><p style='font-size: 40px;margin-top:-5px'>Accidentalidad en Medellín</p></strong>
                              </div>
                              <hr style='border-top: 1px solid rgba(0, 0, 0, 0.4);'/>
                              <strong><h1 style='text-align: center'>Presentación</h1></strong>
                              <p style='font-size: 20px;text-align: justify;'>En este aplicativo web se puede encontrar toda la información
                                  acerca de la accidentalidad en Medellin en los años 2014-2020. Para mas detalle se puede consultar en
                                  el siguiente <a href='https://rpubs.com/scuartasr/TAE_P1_Collision_Informe'> informe técnico </a> y
                                  <a href='https://github.com/JulianO911/TAE'> repositorio </a>.
                              </p>
                              <br>
                              <br>
                              <p style='font-size: 20px;'>Este trabajo fue elaborado por:

                                  <br>
                                  <br>
                              </p>
                            "),
                                           
                                    )
                           ), 
                           fluidRow(
                             column(2,offset = 1,align="center",
                                    HTML("
                           <img src='Simono.jpg' width='100%'/>
                           <p style='text-align:center;'><strong>Simon Cuartas Rendon</strong>
                            <br>Estadistica
                           </p>
                           ")
                                    
                             ),
                             column(2,align="center",
                                    HTML("
                           <img src='Camilo.png' width='100%'/>
                           <p style='text-align:center;'><strong>Camilo Jose Funez Garcia</strong>
                            <br>Ingenieria de sistemas
                           </p>
                           ")
                                    
                             ),
                             column(2,align="center",
                                    HTML("
                           <img src='Juliano.png' width='100%'/>
                           <p style='text-align:center;'><strong>Julián Ospina Murgueitio</strong>
                            <br>Ingenieria de sistemas
                           </p>
                           ")
                                    
                             ),
                             column(2,align="center",
                                    HTML("
                           <img src='gato.png' width='100%'/>
                           <p style='text-align:center;'><strong>Deivid Zhang Figueroa</strong>
                            <br>Estadistica
                           </p>
                           ")
                                    
                             ),
                             column(2,align="center",
                                    HTML("
                           <img src='Juliana.png' width='100%'/>
                           <p style='text-align:center;'><strong>Juliana Zuluaga Serna</strong>
                            <br>Estadistica
                           </p>
                           ")
                                    
                             ),
                             
                           ),
                           fluidRow(
                             column(12,
                                    HTML("<strong><h1 style='text-align: center'>Video Promocional</h1></strong>"), 
                                    HTML("<iframe src='https://drive.google.com/file/d/1cVct8EeiJCJKz0fw_7WZ5IINaP6cyGJo/preview' height=720 style='margin: auto;width: 100%;border: 0px;padding: 10px;display: block;'></iframe>")
                             )
                           ),
                           
                  ),
                  tabPanel("Visualizacion de Datos",
                           
                           HTML("
                     <div style='text-align: center;margin-top:80px;'>
                        <img src='Collision.png' width='200px'/>  
                        <strong><p style='font-size: 40px;margin-top:-5px'>Accidentalidad en Medellin</p></strong>
                    </div>
                    <hr style='border-top: 1px solid rgba(0, 0, 0, 0.4);'/>
                    <strong><h1 style='text-align: center'>Visualizacion de datos </h1></strong>
                    <p style='margin-bottom: 30px;font-size: 20px'>En la siguiente tabla se podra visualizar toda la informacion depurada acerca de la accidentalidad en Medellin, como tambien un mapa
                    de la ciudad de Medellin donde se puede visualizar la cantidad de accidentes que hay por barrio.
                    </p>
                    
                  "),
                           sidebarPanel(
                             tags$h3("Intervalo de fecha"),
                             dateInput("fechaInicio","Fecha Inicio",value = "2014-07-04",min="2014-07-04",max = "2020-08-31"),
                             dateInput("fechaFinal","Fecha Final",value = "2014-07-05",min="2014-07-04",max = "2020-08-31"),
                             selectInput("InputCLASE_ACCIDENTE", "Clase de accidente:", choices=c("Todos","Atropello","Caída del ocupante",
                                                                                                  "Choque", "Incendio", "Otro", "Volcamiento")),
                             selectInput("InputGRAVEDAD_ACCIDENTE", "Gravedad del accidente:", choices=c("Todos","Con heridos","Con muertos","Solo daños"))
                             
                           ), # sidebarPanel
                           mainPanel(
                             fluidRow(id='mapaData',
                                      column(12,
                                             tags$h3("Tabla de accidentes"),
                                             DTOutput('table1')%>% withSpinner(color="#0dc5c1"),
                                             
                                      ),
                                      column(12,
                                             tags$h3("Mapa de accidentes"),
                                             leafletOutput("mymap",width = "100%")%>% withSpinner(color="#0dc5c1")
                                      )
                                      
                                      
                                      
                                      
                             ),
                             
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  
                  
                  tabPanel("Agrupamiento", 
                           fluidRow(id='header',
                                    column(12,
                                           HTML("
                          <div style='text-align: center;margin-top:80px;'>
                              <img src='Collision.png' width='200px'/>
                              <strong><p style='font-size: 40px;margin-top:-5px'>Accidentalidad en Medellín</p></strong>
                          </div>
                          <hr style='border-top: 1px solid rgba(0, 0, 0, 0.4);'/>
                        ")
                                    )
                           ), # Corregir
                           fluidRow(id='mapa',
                                    column(7,
                                           style='margin-top:0px;',
                                           HTML("<strong><h1 style='text-align:center;'>Mapa Interactivo </h1></strong>"),
                                           leafletOutput("mapaBarrios",width = "100%",height = '600')%>% withSpinner(color="#0dc5c1")
                                    ),
                                    column(5,
                                           HTML("
                      <strong><h1 style='text-align:center;'>Agrupamiento </h1></strong> 
                      <p style='margin-bottom: 30px;font-size: 20px;text-align: justify;'>
                      En el siguiente mapa se puede visualizar cada uno de los barrios que compone la ciudad de
                      Medellín, con un color el cual indica un agrupamiento según el nivel de riesgo respecto a 
                      accidentalidad.
                      <br>
                      <br>
                      El color verde indica los de bajo riesgo, el color amarillo indica los de riesgo moderado,
                      el color rojo indica los de alto riesgo y el color gris no aplica.
                      </p>
                    
                      ")
                                    )
                                    
                                    
                           ),
                           
                           
                           
                  ),
                  tabPanel("Desarrollo del modelo", 
                           fluidRow(id='header',
                                    column(12,
                                           HTML("
                    <div style='text-align: center;margin-top:80px;'>
                        <img src='Collision.png' width='200px'/>
                        <strong><p style='font-size: 40px;margin-top:-5px'>Accidentalidad en Medellín</p></strong>
                    </div>
                    
                  ")
                                    )
                           ),
                           fluidRow(
                             column(12,HTML("<hr style='border-top: 1px solid rgba(0, 0, 0, 0.4);'/>"))
                           ),
                           
                           fluidRow(
                             column(12,
                                    HTML("
                              <strong><h1 style='text-align:center;'>Modelo de predicción KNN </h1></strong>
                              <p style='margin-bottom: 30px;font-size: 20px;text-align: justify;'>
                                En esta sección se puede visualizar la implementación de un modelo predictivo KNN 
                                para la accidentalidad de la ciudad de Medellín para el año 2021.

                              </p>
                              ")
                             )
                           ),
                           fluidRow(
                             
                             column(3,
                                    style = "background-color: #e3e3e3;",
                                    selectInput(inputId = "clase", 
                                                label = "Clase", 
                                                choices = c("Atropello", "Caída del ocupante", "Choque", "Incendio",
                                                            "Volcamiento", "Otro"))   
                             ),
                             column(3,
                                    style = "background-color: #e3e3e3;",
                                    selectInput(inputId = "intervalo", 
                                                label = "Intervalo", 
                                                choices = c("Dia", "Semana", "Mes")),   
                             ),
                             column(3,
                                    style = "background-color: #e3e3e3;",
                                    dateInput(inputId = "fecha_ini",
                                              label = "Fecha inicial",
                                              value = "2021-01-01",
                                              min = "2021-01-01",
                                              max = "2021-12-31",
                                              format = "yyyy-mm-dd"),  
                             ),
                             column(3,
                                    style = "background-color: #e3e3e3;",
                                    dateInput(inputId = "fecha_fin",
                                              label = "Fecha final",
                                              value = "2021-12-31",
                                              min = "2021-01-01",
                                              max = "2021-12-31",
                                              format = "yyyy-mm-dd"),
                             ),
                             
                           ),
                           fluidRow(id='modelos',
                                    column(3,dataTableOutput("tablaPred")%>% withSpinner(color="#0dc5c1")),
                                    column(9,style = "margin-top:65px;",plotOutput("distPlot")%>% withSpinner(color="#0dc5c1"))
                                    
                           )
                           
                  ),
                  
                  
                  position = c("fixed-top")
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  barrios_med=readOGR("BD_nueva/Mapa/Limite_Barrio_Vereda_Catastral.shp",layer="Limite_Barrio_Vereda_Catastral") # Corregir
  nombres_barrios=iconv(barrios_med@data$NOMBRE,"UTF-8","ISO_8859-1") # Corregir
  grupos <- read.csv(file = 'pruebadecsv.csv' ) # Corregir
  colorMaker <- colorFactor(palette = c("#808080", "#006400", "#ffa333","#d90000"), 
                            levels = c("No aplica", "Bajo", "Moderado", "Alto"))
  pal <- colorFactor(
    palette = c("#d90000", "#006400","#ffa333", "#808080"),
    domain = c("No aplica", "Bajo", "Moderado", "Alto")
  )
  accidentes <- data.table(read.csv(file = 'BD_nueva/accidentesMDE2.csv'))
  accidentesFilter <- accidentes[,-c(11,12,13,14)] # Corregir
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
  
  
  output$table1 <- renderDT({
    if(input$InputCLASE_ACCIDENTE != "Todos" && input$InputGRAVEDAD_ACCIDENTE == "Todos"){
      stateFilter<- accidentesFilter[
        FECHA >= input$fechaInicio & 
          FECHA <= input$fechaFinal &
          CLASE_ACCIDENTE == input$InputCLASE_ACCIDENTE, ]
      
    }else if(input$InputGRAVEDAD_ACCIDENTE == "Todos" && input$InputGRAVEDAD_ACCIDENTE != "Todos"){
      stateFilter<- accidentesFilter[
        FECHA >= input$fechaInicio & 
          FECHA <= input$fechaFinal &
          GRAVEDAD_ACCIDENTE == input$InputGRAVEDAD_ACCIDENTE,]
      
    }else if(input$InputGRAVEDAD_ACCIDENTE != "Todos" && input$InputGRAVEDAD_ACCIDENTE != "Todos"){
      stateFilter<- accidentesFilter[
        FECHA >= input$fechaInicio & 
          FECHA <= input$fechaFinal &
          GRAVEDAD_ACCIDENTE == input$InputGRAVEDAD_ACCIDENTE &
          CLASE_ACCIDENTE == input$InputCLASE_ACCIDENTE,]    
      
    }else{
      stateFilter<- accidentesFilter[
        FECHA >= input$fechaInicio & 
          FECHA <= input$fechaFinal,] 
      
      
    }
    
  },options = list(lengthChange = FALSE,
                   scrollX = TRUE
  )
  )
  
  output$mymap <- renderLeaflet({
    if(input$InputCLASE_ACCIDENTE != "Todos" && input$InputGRAVEDAD_ACCIDENTE == "Todos"){
      stateFilter<- accidentes[ 
        FECHA >= input$fechaInicio & 
          FECHA <= input$fechaFinal &
          CLASE_ACCIDENTE == input$InputCLASE_ACCIDENTE,]
      
    }else if(input$InputGRAVEDAD_ACCIDENTE == "Todos" && input$InputGRAVEDAD_ACCIDENTE != "Todos"){
      stateFilter<- accidentes[
        FECHA >= input$fechaInicio & 
          FECHA <= input$fechaFinal &
          GRAVEDAD_ACCIDENTE == input$InputGRAVEDAD_ACCIDENTE,]
      
    }else if(input$InputGRAVEDAD_ACCIDENTE != "Todos" && input$InputGRAVEDAD_ACCIDENTE != "Todos"){
      stateFilter<- accidentes[ 
        FECHA >= input$fechaInicio & 
          FECHA <= input$fechaFinal &
          GRAVEDAD_ACCIDENTE == input$InputGRAVEDAD_ACCIDENTE &
          CLASE_ACCIDENTE == input$InputCLASE_ACCIDENTE,]  
      
    }else{
      stateFilter<- accidentes[
        FECHA >= input$fechaInicio & 
          FECHA <= input$fechaFinal,]
      
      
    }
    
    leaflet() %>%
      addTiles() %>% 
      #addMarkers(data = points())
      addMarkers(data = stateFilter,
                 lng= ~LONGITUD, 
                 lat=~LATITUD,
                 clusterOptions = markerClusterOptions()
      )
    
  })
  
  output$mapaBarrios <- renderLeaflet({ # corregir
    leaflet() %>%
      addPolygons(data=barrios_med, # Corregir
                  weight = 1,
                  color = "white",
                  fillColor =colorMaker(grupos$Riesgo),
                  fillOpacity = 0.7,
                  
                  label=paste0("<p style='font-size:20px'> <strong>Barrio: </strong>",grupos$NOMBRE_BAR,
                               "<br><strong>Riesgo: </strong>",grupos$Riesgo,
                               "<br><strong>Comuna: </strong>",grupos$NOMBRE_COM
                  )%>% lapply(htmltools::HTML),
                  
                  
      )%>%
      addTiles()%>%
      addLegend(position = "bottomright",pal=pal,values = c("No aplica","Bajo", "Moderado", "Alto"))
    
  })
  

# Librerías
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
  
# días festivos
diasFestivos <- function(){
  fest14 <- c("2014-01-01", "2014-01-06", "2014-03-24",
                "2014-04-17", "2014-04-18", "2014-05-01",
                "2014-06-02", "2014-06-23", "2014-06-30",
                "2014-07-20", "2014-08-07", "2014-08-18",
                "2014-10-13", "2014-11-03", "2014-11-17",
                "2014-12-08", "2014-12-25")
    
  fest15 <- c("2015-01-01", "2015-01-12", "2015-03-23",
                "2015-04-02", "2015-04-03", "2015-05-01",
                "2015-06-08", "2015-06-15", "2015-06-29",
                "2015-07-20", "2015-08-07", "2015-08-17",
                "2015-10-12", "2015-11-02", "2015-11-16",
                "2015-12-08", "2015-12-25", "2015-05-18")
    
  fest16 <- c("2016-01-01", "2016-01-11", "2016-03-21",
                "2016-03-24", "2016-03-25", "2016-05-01",
                "2016-05-30", "2016-06-06", "2016-07-04",
                "2016-07-20", "2016-08-07", "2016-08-15",
                "2016-10-17", "2016-11-07", "2016-11-14",
                "2016-12-08", "2016-12-25", "2016-05-09")
    
  fest17 <- c("2017-01-01", "2017-01-09", "2017-03-20",
                "2017-04-13", "2017-04-14", "2017-05-01",
                "2017-05-29", "2017-06-19", "2017-07-03",
                "2017-07-20", "2017-08-07", "2017-08-21",
                "2017-10-16", "2017-11-06", "2017-11-13",
                "2017-12-08", "2017-12-25", "2017-06-26")
    
  fest18 <- c("2018-01-01", "2018-01-08", "2018-03-19",
                "2018-03-19", "2018-03-30", "2018-05-01",
                "2018-05-14", "2018-06-04", "2018-07-02",
                "2018-07-20", "2018-08-07", "2018-08-20",
                "2018-10-15", "2018-11-05", "2018-11-12",
                "2018-12-08", "2018-12-25", "2018-06-11")
    
  fest19 <- c("2019-01-01", "2019-01-07", "2019-03-25",
                "2019-04-19", "2019-04-18", "2019-05-01",
                "2019-06-03", "2019-06-24", "2019-07-01",
                "2019-07-20", "2019-08-07", "2019-08-19",
                "2019-10-14", "2019-11-04", "2019-11-11",
                "2019-12-08", "2019-12-25")
    
  fest20 <- c("2020-01-01", "2020-01-06", "2020-03-23",
                "2020-04-10", "2020-04-09", "2020-05-01",
                "2020-06-15", "2020-06-22", "2020-06-29",
                "2020-07-20", "2020-08-07", "2020-08-17",
                "2020-10-12", "2020-11-02", "2020-11-16",
                "2020-12-08", "2020-12-25", "2020-05-25")
  fest21 <- c("2021-01-01", "2021-01-11", "2021-03-22",
                "2021-04-01", "2021-04-02", "2021-05-01",
                "2021-05-17", "2021-06-07", "2021-06-14",
                "2021-07-05", "2021-07-20", "2021-08-07",
                "2021-08-16", "2021-10-18", "2021-11-01",
                "2021-11-15", "2021-12-08", "2021-12-25")
  fest22 <- c("2022-01-01", "2022-01-10", "2022-03-21",
                "2022-04-14", "2022-04-15", "2022-05-01",
                "2022-05-30", "2022-06-20", "2022-06-27",
                "2022-07-04", "2022-07-20", "2022-08-07",
                "2022-08-15", "2022-10-17", "2022-11-07",
                "2022-11-14", "2022-12-08", "2022-12-25")
  fest23 <- c("2023-01-01", "2023-01-09", "2023-03-20",
                "2023-04-06", "2023-04-07", "2023-05-01",
                "2023-05-22", "2023-06-12", "2023-06-19",
                "2023-07-03", "2023-07-20", "2023-08-07",
                "2023-08-21", "2023-10-16", "2023-11-06",
                "2023-11-13", "2023-12-08", "2023-12-25")
    
  fest24 <- c("2024-01-01", "2024-01-08", "2024-03-25",
                "2024-03-28", "2024-03-29", "2024-05-01",
                "2024-05-13", "2024-06-03", "2024-06-10",
                "2024-07-01", "2024-07-20", "2024-08-07",
                "2024-08-19", "2024-10-14", "2024-11-04",
                "2024-11-11", "2024-12-08", "2024-12-25")
    
  fest25 <- c("2025-01-01", "2025-01-06", "2025-03-24",
                "2025-04-13", "2025-04-17", "2025-04-18",
                "2025-04-20", "2025-05-01", "2025-06-02",
                "2025-06-23", "2025-06-30", "2025-07-20",
                "2025-08-07", "2025-08-18", "2025-10-13",
                "2025-11-03", "2025-11-17", "2025-12-08",
                "2025-12-25")
    
    
    
    
  festivos <- c(fest14,fest15,fest15,fest16,fest17,fest18,fest19,fest20,fest21,fest22,
                  fest23,fest24,fest25)
    
  festivos <- as.Date(festivos)
  return(festivos)
}
  
# Función

gen_datos <- function(fecha_ini,
                      fecha_fin,
                      intervalo)
{
  # Creación de fechas
  df_fechas <- seq(from=as.Date(fecha_ini), to=as.Date(fecha_fin), format = "days", by=1)
    
  return(df_fechas)
}
  

# Cargar modelos
load("./BD_nueva/ModeloChoque.Rdata")
load("./BD_nueva/ModeloAtropello.Rdata")
load("./BD_nueva/ModeloCaida.Rdata")
load("./BD_nueva/ModeloVolcamiento.Rdata")
load("./BD_nueva/ModeloOtro.Rdata")
load("./BD_nueva/Modeloincendio.Rdata")
festivos <- diasFestivos()


# Funciones de fecha
  
  
df_fechas <- reactive({
  gen_datos(input$fecha_ini, input$fecha_fin, input$intervalo)
})
  
df_prediccion <- reactive({
  prediccion(input$clase, df_fechas(), input$intervalo)
})
  
prediccion <- function(clase, 
                       df_fechas,
                       intervalo){
    
  choques <- data.frame(DIA_SEMANA = weekdays(df_fechas),
                        FESTIVO = ifelse((df_fechas %in% festivos), 1, 0),
                        SEMANAXX = strftime(df_fechas, format = "%V")) 
    
  atropellos <- data.frame( DIA_SEMANA = weekdays(df_fechas),
                            FESTIVO = ifelse((df_fechas %in% festivos), 1, 0),
                            SEMANAXX = strftime(df_fechas, format = "%V"))
  volcamientos <- data.frame(DIA_SEMANA = weekdays(df_fechas),
                             FESTIVO = ifelse((df_fechas %in% festivos), 1, 0),
                             SEMANAXX = strftime(df_fechas, format = "%V"),
                             AÑOX = year(df_fechas),
                             MES_SEMANA = months(df_fechas))
  caidasDelOcupante <- data.frame(
    DIA_SEMANA = weekdays(df_fechas),
    FESTIVO = ifelse((df_fechas %in% festivos), 1, 0),
    SEMANAXX = strftime(df_fechas, format = "%V"),
    AÑOX = year(df_fechas),
    MES_SEMANA = months(df_fechas))
    
  incendios <- data.frame(
    DIA_SEMANA = weekdays(df_fechas))
    
  otros <- data.frame(
    DIA_SEMANA = weekdays(df_fechas),
    FESTIVO = ifelse((df_fechas %in% festivos), 1, 0))
    
    
  choques <- as.data.frame(lapply(choques, as.factor))
  choques <- as.data.frame(lapply(choques, as.numeric))
    
  volcamientos <- as.data.frame(lapply(volcamientos, as.factor))
  volcamientos <- as.data.frame(lapply(volcamientos, as.numeric))
    
  atropellos <- as.data.frame(lapply(atropellos, as.factor))
  atropellos <- as.data.frame(lapply(atropellos, as.numeric))
    
  caidasDelOcupante <- as.data.frame(lapply(caidasDelOcupante, as.factor))
  caidasDelOcupante <- as.data.frame(lapply(caidasDelOcupante, as.numeric))
    
  incendios <- as.data.frame(lapply(incendios, as.factor))
  incendios <- as.data.frame(lapply(incendios, as.numeric))
    
  otros <- as.data.frame(lapply(otros, as.factor))
  otros <- as.data.frame(lapply(otros, as.numeric))
    
  if (clase == "Atropello"){
    prediccion <- predict(modeloAtropello,atropellos)
    prediccion <- as.integer(prediccion)
  } 
    
  else if (clase == "Caída del ocupante"){
    prediccion <- predict(modeloCaida,caidasDelOcupante)
    prediccion <- as.integer(prediccion)
  }
    
  else if (clase == "Choque"){
    prediccion <- predict(modeloChoque,choques)
    prediccion <- as.integer(prediccion)
  }
    
  else if (clase == "Volcamiento"){
    prediccion <- predict(modeloVolcamiento,volcamientos)
    prediccion <- as.integer(prediccion)
  }
    
  else if (clase == "Otro"){
    prediccion <- predict(modeloOtro,otros)
    prediccion <-as.integer(prediccion)
  }
    
  else if (clase == "Incendio"){
    prediccion <- predict(modeloIncendio,incendios)
    prediccion <- as.integer(prediccion)
  }
    
  if (intervalo == "Dia"){
    df_prediccion <- data.frame(fecha = df_fechas, Total = round(prediccion))
  } 
    
  else if (intervalo == "Semana"){
      
    df_prediccion <- data.frame(fecha = df_fechas, Total = prediccion) %>% 
      mutate(fecha = week(fecha)) %>%    
      group_by(fecha) %>% summarise(Total = round(sum(Total)))
      
  }
    
  else if (intervalo == "Mes"){
      
    df_prediccion <- data.frame(fecha = df_fechas, Total = prediccion) %>%
      mutate(fecha = month(fecha)) %>%  
      group_by(fecha) %>% summarise(Total = round(sum(Total))) 
      
  }
    
  return(df_prediccion)
}
  
# Función para cambio de nombre #
  
# Tabla de intervalo
cambio_nombre <- function(df_prediccion, intervalo){
  if(intervalo == "Dia"){
    names(df_prediccion) <- c("Dia", "Total")
  } 
    
  else if (intervalo == "Semana"){
    names(df_prediccion) <- c("Semana", "Total")    
  }
    
  else if (intervalo == "Mes"){
    names(df_prediccion) <- c("Semana", "Total")    
  }
    
  return(df_prediccion)
    
}
  
  
  
# Renderplot
  
  output$distPlot <- renderPlot({
    
    ggplot(df_prediccion()) +
      geom_area(aes(x = fecha, y = Total), color="darkblue", fill="lightblue") +
      geom_point(aes(x = fecha, y = Total), col = "black") +
      theme_minimal(base_size = 14) +
      ggtitle(paste("Predicciones de", paste0(input$clase, "s"), "por", input$intervalo, "en Medellín")) +
      labs(x = input$intervalo, y = paste("Total de", paste0(input$clase, "s"))) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            panel.border = element_rect(colour = "black", fill=NA, size=1))
    
    
    
  })
  
  output$tablaPred <- renderDataTable({
    cambio_nombre(df_prediccion(), input$intervalo)
    
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
