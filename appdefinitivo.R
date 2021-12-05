
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
load("BD_nueva/accidentesMDE2.RData")

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                #setBackgroundColor(
                # color = "Snow",
                # gradient = c("linear", "radial"),
                #  direction = c("bottom", "top", "right", "left"),
                # shinydashboard = FALSE
                #  ),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Accidentalidad en Medellín",
                  tabPanel("Presentación",
                           fluidRow(id='header1',
                                    column(12,
                                           HTML("
                              <div style='text-align: center;margin-top:80px;'>
                                <img src='cero accidente.png' width='200px'/>  
                                  <strong><p style='font-size: 40px;margin-top:-5px'>Accidentalidad en Medellín</p></strong>
                              </div>
                              <hr style='border-top: 1px solid rgba(0, 0, 0, 0.4);'/>
                              <strong><h1 style='text-align: center'>Presentación</h1></strong>
                              <p style='font-size: 20px;text-align: justify;'>En este aplicativo web se puede encontrar toda la información
                                  acerca de la accidentalidad en Medellín en los años 2014-2020.
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
                           <img src='Simón.png' width='100%'/>
                           <p style='text-align:center;'><strong>Simón Cuartas Rendón</strong>
                            <br>Estadística
                           </p>
                           ")
                                    
                             ),
                             column(2,align="center",
                                    HTML("
                           <img src='Camilo.png' width='100%'/>
                           <p style='text-align:center;'><strong>Camilo José Fúnez García</strong>
                            <br>Ingeniería de sistemas
                           </p>
                           ")
                                    
                             ),
                             column(2,align="center",
                                    HTML("
                           <img src='Julián.png' width='100%'/>
                           <p style='text-align:center;'><strong>Julián Ospina Murgueitio</strong>
                            <br>Ingeniería de sistemas
                           </p>
                           ")
                                    
                             ),
                             column(2,align="center",
                                    HTML("
                           <img src='gato.png' width='100%'/>
                           <p style='text-align:center;'><strong>Deivid Zhang Figueroa</strong>
                            <br>Estadística
                           </p>
                           ")
                                    
                             ),
                             column(2,align="center",
                                    HTML("
                           <img src='Juliana.png' width='100%'/>
                           <p style='text-align:center;'><strong>Juliana Zuluaga Serna</strong>
                            <br>Estadística
                           </p>
                           ")
                                    
                             ),
                             
                           ),
                           fluidRow(
                             column(12,
                                    HTML("<strong><h1 style='text-align: center'>Video Promocional</h1></strong>"),
                                    HTML("<iframe src='https://vimeo.com/463875912' height=480 style='margin: auto;width: 100%;border: 0px;padding: 10px;display: block;'></iframe>")
                             )
                           ),
                           
                  ),
                  tabPanel("Visualización de Datos",
                           
                           HTML("
                     <div style='text-align: center;margin-top:80px;'>
                        <img src='cero accidente.png' width='200px'/>  
                        <strong><p style='font-size: 40px;margin-top:-5px'>Accidentalidad en Medellín</p></strong>
                    </div>
                    <hr style='border-top: 1px solid rgba(0, 0, 0, 0.4);'/>
                    <strong><h1 style='text-align: center'>Visualización de datos </h1></strong>
                    <p style='margin-bottom: 30px;font-size: 20px'>En la siguiente tabla se podrá visualizar toda la información depurada acerca de la accidentalidad en Medellín, como también un mapa
                    de la ciudad de Medellín donde se puede visualizar la cantidad de accidentes que hay por barrio.
                    </p>
                    
                  "),
                           sidebarPanel(
                             tags$h3("Intervalo de fecha"),
                             dateInput("fechaInicio","Fecha Inicio",value = "2014-07-04",min="2014-07-04",max = "2020-08-31"),
                             dateInput("fechaFinal","Fecha Final",value = "2014-07-05",min="2014-07-04",max = "2020-08-31"),
                             selectInput("InputCLASE_ACCIDENTE", "Clase de accidente:", choices=c("Todos",unique(accidentes$CLASE_ACCIDENTE))),
                             selectInput("InputGRAVEDAD_ACCIDENTE", "Gravedad del accidente:", choices=c("Todos",unique(accidentes$GRAVEDAD_ACCIDENTE)))
                             
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
                              <img src='cero accidente.png' width='200px'/>
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
                      En el mapa de la izquierda se puede visualizar la ciudad de Medellín dividida 
                      por cada uno de los barrios que lo componen, cada uno se encuentra sombreado con 
                      un color el cual indica un agrupamiento según el nivel de riesgo respecto a 
                      accidentalidad. El nivel de riesgo se obtiene a través de la tasa de gravedad de 
                      los accidentes (fracción de accidentes con herido o fallecido) y el número total 
                      de accidentes para el periodo entre 2014 y 2020.
                      <br>
                      <br>
                      Se puede visualizar en color verde los de bajo riesgo, 
                      barrios que no necesitan ser intervenidos en temas de accidentalidad; 
                      en color amarillo los de riesgo medio, los cuales deben analizarse en 
                      detalle para determinar una posible intervención; en color rojo los barrios a 
                      los que urge intervenir en temas de accidentalidad y en color gris se encuentran 
                      algunos barrios no clasificados.
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
                        <img src='cero accidente.png' width='200px'/>
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
  grupos <- read.csv(file = 'Agrupamiento_barrios_ordenado3.csv' ) # Corregir
  colorMaker <- colorFactor(palette = c("#ABEBC6", "#F5B041", "#EC7063","#922B21"), 
                            levels = c("Bajo", "Medio", "Alto","Muy Alta"))
  pal <- colorFactor(
    palette = c("#EC7063","#ABEBC6", "#F5B041"),
    domain = c("Bajo", "Medio", "Alto")
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
                  fillColor =colorMaker(grupos$riesgo),
                  fillOpacity = 0.7,
                  
                  label=paste0("<p style='font-size:20px'> <strong>Barrio: </strong>",grupos$barrio_mapa,
                               "<br><strong>Riesgo: </strong>",grupos$riesgo,
                               "<br><strong>Cantidad de accidentes: </strong>",grupos$accidentes,
                               "<br><strong>Tasa de graves: </strong>",grupos$tasa_graves,"</p>"
                  )%>% lapply(htmltools::HTML),
                  
                  
      )%>%
      addTiles()%>%
      addLegend(position = "bottomright",pal=pal,values = c("Bajo", "Medio", "Alto"))
    
  })
  
  # Codigo Modelo--------------------------------------------------------------------------------------
  ################### FUNCIóN GEN DATOS #############
  
  ## Librerías necesarias para la función ------------
  library(readxl)
  library(lubridate)
  library(dplyr)
  library(ggplot2)
  
  ## Base de datos para la función ------------------
  fest_2021 <- read_xlsx("festivos2021.xlsx") # Pendiente
  fest_2021 <- fest_2021 %>% mutate(Fecha = as.Date(Fecha, format = "yyyy/mm/dd"))
  
  ## FUNCIóN ----------------------------------------------
  
  ## Entradas
  #fecha_ini <- as.Date("2021/01/01")
  #fecha_fin <- as.Date("2021/03/23")
  
  ## Núcleo
  
  gen_datos <- function(fecha_ini,
                        fecha_fin,
                        intervalo)
  {
    # INTERVALO: SEMANA ------------------
    if (intervalo == "Semana"){
      
      fecha_ini <- fecha_ini - yday(fecha_ini) %% 7 + 1
      
      if (yday(fecha_ini) %% 7 != 0){
        fecha_fin <- fecha_fin + (7 - yday(fecha_fin) %% 7)
      }
      
      # Se verifican inconsistencia
      if (fecha_ini < as.Date("2021-01-01")){
        fecha_ini <- as.Date("2021-01-01")
      }
      
      # Se verifican inconsistencia
      if (fecha_fin > as.Date("2021-12-31")){
        fecha_fin <- as.Date("2021-12-31")
      }     
      
      
    }
    
    # INTERVALO: SEMANA ------------------
    if (intervalo == "Mes"){
      fecha_ini <- floor_date(fecha_ini, unit = "month")  # Primer día del mes
      fecha_fin <- ceiling_date(fecha_fin, unit = "month") - 1 # último día del mes
    }
    
    # INTERVALO: DíA ---------------------
    if (intervalo == "Día"){
      # No se necesita hacer ninguna conversión
      fecha_ini <- fecha_ini
      fecha_fin <- fecha_fin
    }
    
    # Base de datos inicial
    df_fechas <- data.frame(fecha = seq(fecha_ini,fecha_fin, by = '1 day'))
    
    # Obtención de covariables necesarias en el modelo
    df_fechas <- df_fechas %>% mutate(semana = week(fecha),
                                      semana_dia = as.factor(wday(fecha, label = TRUE, abbr = FALSE)),
                                      fes_antes = ifelse(fecha %in% (fest_2021$Fecha[fest_2021$Festivo == 1] + 1), 1, 0),
                                      fes_despues = ifelse(fecha %in% (fest_2021$Fecha[fest_2021$Festivo == 1] - 1), 1, 0)
    )
    
    # Se añade la celebración
    for (i in 1:nrow(df_fechas)){
      df_fechas$celebracion[i] <- ifelse(df_fechas$fecha[i] %in% fest_2021$Fecha, fest_2021$Celebracion[fest_2021$Fecha == df_fechas$fecha[i]], "No")
    }
    
    # Conversión a factores
    cols <- c("semana_dia",
              "semana",
              "celebracion",
              "fes_antes",
              "fes_despues")
    
    df_fechas[cols] <- lapply(df_fechas[cols], as.factor)
    
    return(df_fechas)
  }
  
  ########### FUNCIÓN PREDICCIÓN ############
  
  ## Objetos requeridos
  load("BD_nueva/ModeloChoque.Rdata")
  load("BD_nueva/ModeloAtropello.Rdata")
  load("BD_nueva/ModeloCaida.Rdata")
  load("BD_nueva/ModeloVolcamiento.Rdata")
  load("BD_nueva/ModeloOtro.Rdata")
  load("BD_nueva/Modeloincendio.Rdata")
  
  prediccion <- function(clase, 
                         df_fechas,
                         intervalo){
    
    if (clase == "Atropello"){
      prediccion <- predict(modeloAtropello, newdata = df_fechas)
    } 
    
    else if (clase == "Caida de ocupante"){
      prediccion <- predict(modeloCaida, newdata = df_fechas)
    }
    
    else if (clase == "Choque"){
      prediccion <- predict(modeloChoque, newdata = df_fechas)
    }
    
    else if (clase == "Volcamiento"){
      prediccion <- predict(modeloVolcamiento, newdata = df_fechas)
    }
    
    else if (clase == "Otro"){
      prediccion <- round(predict(modeloOtro, newdata = df_fechas))
    }
    
    if (intervalo == "Dia"){
      df_prediccion <- data.frame(fecha = df_fechas$fecha, Total = round(prediccion))
    } 
    
    else if (intervalo == "Semana"){
      
      df_prediccion <- data.frame(fecha = df_fechas$fecha, Total = prediccion) %>% 
        mutate(fecha = week(fecha)) %>%    # DANGER: VERIFICAR GRÁFICO Y TRATAR DE CAMBIAR
        group_by(fecha) %>% summarise(Total = round(sum(Total))) # DANGER
      
    }
    
    else if (intervalo == "Mes"){
      
      df_prediccion <- data.frame(fecha = df_fechas$fecha, Total = prediccion) %>%
        mutate(fecha = month(fecha)) %>%  # DANGER: VERIFICAR GRÁFICO Y TRATAR DE CAMBIAR
        group_by(fecha) %>% summarise(Total = round(sum(Total))) # DANGER
      
    }
    
    return(df_prediccion)
  }
  
  ######### FUNCIÓN PARA CAMBIAR EL NOMBRE #####
  
  # Se necesita para que la tabla que se le muestra al usuario sea consistente con el intervalo
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
  
  ########### USO DE LAS FUNCIONES ############
  
  
  df_fechas <- reactive({
    gen_datos(input$fecha_ini, input$fecha_fin, input$intervalo)
  })
  
  df_prediccion <- reactive({
    prediccion(input$clase, df_fechas(), input$intervalo)
  })
  
  ############ GRÁFICO ##############
  
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
