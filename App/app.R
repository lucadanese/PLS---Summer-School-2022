library(shiny)
library(leaflet)
library(readxl)
library(raster)
library(ggplot2)
library(data.table)
library(curl)
library(sp)
library(rgdal)
library(dplyr)
library(shinythemes)

#setwd("C:\\Users\\Luca Danese\\Desktop\\PLS---Summer-School-2022")

#Aedes.gbif <- read_excel("C:\\Users\\Luca Danese\\Desktop\\PLS---Summer-School-2022\\Aedes albopictus_GBIF.xlsx")
Aedes.gbif <- fread("https://raw.githubusercontent.com/lucadanese/PLS---Summer-School-2022/main/aedes_albopictus_GBIF.csv",integer64 = "numeric")
Aedes.gbif <- as.data.frame(Aedes.gbif)

data_map <- readRDS(paste0("data/Maps_Worldbank.RDS"))

data <- Aedes.gbif[1:60849,c("decimalLatitude","decimalLongitude","year")]
data$decimalLatitude <- as.numeric(gsub(",", ".", data$decimalLatitude))
data$decimalLongitude <- as.numeric(gsub(",", ".", data$decimalLongitude))

vars <- c("Reale Nativo", "Mondo", "Manuale")
vars_bio <- data.frame(
  bio_name = c("Annual Mean Temperature",
              #"Mean Diurnal Range",
              #"Isothermality",
              #"Temperature Seasonality",
              #"Max Temperature of Warmest Month",
              #"Min Temperature of Coldest Month",
              "Temperature Annual Range (BIO5-BIO6)",
              #"Mean Temperature of Wettest Quarter",
              #"Mean Temperature of Driest Quarter",
              "Annual Precipitation",
              "Precipitation Seasonality",
              "Population Density"),
  bio_num = c(1,7,12,15,99))

indicatori <- c("imports",
                "exports",
                "container",
                "resource")

vars_demo_eco <- c("Indice_1","Indice_2")

ui <- navbarPage("Aedes", id = "nav", theme = shinytheme("united"),

                 tabPanel("Introduzione",

                    h1("Aedes albopictus: la zanzara tigre asiatica"),

                    #img(src = "aedes_img.png", height = 720, width = 1080)
                    #mainPanel(img(src = "aedes_img.png",height = 650, width = 720)),
                    #sidebarPanel(h2("la zanzara blabla"))

                    fluidRow(
                      column(
                        width = 7,

                        p("Il cambiamento climatico, la continua globalizzazione, non solo portano cambiamenti nelle nostre vite, ma anche in quelle degli esseri viventi che ci
                          circondano. Un esempio di questo fenomeno riguarda la zanzara tigre di orgine asiatica, scientificamente chiamata Aedes Albopictus.
                          Questa specie di zanzara nasce in oriente, in particolare nei tropici del sudest asiatico, le isole del pacifico e dell'oceano indiano, ma anche in China, Giappone e a ovest del
                          Madagascar.
                          Questa specie e ad oggi diffusa in tutto il mondo, ed è interessantea andare a studiare come avviene la sua diffusione e quali luoghi individua come più adatti alla sua riproduzione.
                          Per fare un esempio: la Pianura Padana presenta numerosissime colonie di questa zanzara, come mai? La zanzara tigre ha come metodo principale di diffusione i copertoni delle auto, o dei camion,
                          che vengono trasportati da una parte all'altra del mondo. Il motivo è molto semplice: i copertoni sono un ottimo raccoglitore di acqua piovana, al cui interno posson essere presenti uova o larve.
                          Lo scopo di questo lavoro è di provare a capire quali possono essere i fattori che hanno portato alla diffusione in alcune zone di questa zanzara, e chiedersi se altre zone possono essere potenzialmente
                          'invase' da questo insetto."),

                        p("La dashboard è suddivisa in tre pannelli: nel primo troverete una mappa con la diffusione negli anni della zanzara, potete concentrarvi sul reale nativo, oppure manualmente andare a studiare le regioni che più
                          vi interessano. La seconda sezione invece vi permette di confrontare le caratteristiche morfologiche e la densità di popolazione del reale nativo e di un territorio a vostra scelta. Questo vi permette di poter capire
                          se una zona è potenzialmente un luogo che può ospitare questo insetto. Infine, nella terza sezione, troverete alcuni indicatori economici e demografici con i quali studiare se certi fattori possono essere determinanti o meno
                          per la diffusione della zanzara."),

                        p("Vi riportiamo qua sotto una mappa con latitudine e longitudine che vi può permettere di navigare meglio sulle mappe del mondo."),

                        img(
                          height = 800,
                          width = 1280,

                          src = "world.jpg"
                        )


                      ),
                      column(
                        width = 2,

                        img(
                          height = 300,
                          width = 450,

                          src = "aedes_img.png"
                        ),
                      ), # end column
                    ), # end fluidRow

                 ),

                 tabPanel("Osservazioni Aedes",
                          div(class = "outer",
                              tags$head(
                                includeCSS("www/styles.css"),
                                #tags$link(rel = "stylesheet",  href = "www/style.css"),
                                includeScript("www/gomap.js")
                                #tags$script(src="www/gomap.js")
                              ),

                              leafletOutput("map", width = "100%", height = "100%"),

                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",

                                            h2("Explorer"),

                                            selectInput("area", "Area", vars),
                                            conditionalPanel("input.area == 'Manuale'",
                                                             numericInput("long_sx", "Longitudine Sinistra", 7),
                                                             numericInput("long_dx", "Longitudine Destra", 15),
                                                             numericInput("lat_down", "Latitudine Inferiore", 36),
                                                             numericInput("lat_upw", "Latitudine Superiore", 47)
                                            ),
                                            sliderInput("year","Year", min = 1920, max = 2022, value = 2022, step = 2L, animate = TRUE, label = "Anno"),

                              )
                          )

                 ),

                 tabPanel("Caratteristiche Morfologiche e Densità di Popolazione",
                          h4("In questa pagina potete mettere a confronto alcuni aspetti morfologici del territorio in analisi e del reale nativo della zanzara tigre. Inoltre potete confrontare le densità di popolazione."),
                          fluidPage(
                            fluidRow(
                              h1("Paese studio"),
                              sidebarPanel(
                                selectInput("bio1", "Bio", vars_bio$bio_name),
                                numericInput("long_sx_1", "Longitudine Sinistra", 7),
                                numericInput("long_dx_1", "Longitudine Destra", 15),
                                numericInput("lat_down_1", "Latitudine Inferiore", 36),
                                numericInput("lat_upw_1", "Latitudine Superiore", 47))
                              ,
                              mainPanel(
                                plotOutput(outputId = "rasterPlot")
                              )

                            ),
                            fluidRow(
                              h1("Reale Nativo"),
                              sidebarPanel(
                                selectInput("bio2", "Bio", vars_bio$bio_name),
                                numericInput("long_sx_2", "Longitudine Sinistra", 66),
                                numericInput("long_dx_2", "Longitudine Destra", 180),
                                numericInput("lat_down_2", "Latitudine Inferiore", -45),
                                numericInput("lat_upw_2", "Latitudine Superiore", 0))
                              ,
                              mainPanel(
                                plotOutput(outputId = "rasterPlot_realenativo")
                              )
                            )
                          )

                 ),

                 tabPanel("Caratteristiche Demografiche ed Economiche",
                          fluidPage(
                            h1("Indicatore di Riferimento"),
                            h4("In questa pagina potete trovare alcuni indicatori demografici ed economici dei paesi del mondo. I dati sono riferiti all'ultimo anno disponibile (2019), e possono essere reperiti sui siti di World Bank (https://www.worldbank.org/en/home) e del programma di sviluppo delle Nazioni Unite (https://hdr.undp.org/en)"),
                            sidebarPanel(
                              selectInput("indicatore", "Indicatore", indicatori),
                              numericInput("long_sx_3", "Longitudine Sinistra", 7),
                              numericInput("long_dx_3", "Longitudine Destra", 15),
                              numericInput("lat_down_3", "Latitudine Inferiore", 36),
                              numericInput("lat_upw_3", "Latitudine Superiore", 47))
                            ,
                            mainPanel(
                              plotOutput(outputId = "PlotDemoEco")
                            )
                          )

                 )

)

server <- function(input, output, session) {

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 93.85, lat = 37.45, zoom = 4)
  })




  observe({

    areaBy <- input$area

    if (areaBy == "Reale Nativo") {
      condition <- which(data$decimalLongitude > 66 & data$decimalLongitude < 180 & data$decimalLatitude < 90 & data$decimalLatitude > -90)
      data <- data[condition,]
    }

    if (areaBy == "Mondo") {
      data <- data
    }

    if (areaBy == "Manuale") {
      condition <- which(data$decimalLongitude > input$long_sx & data$decimalLongitude < input$long_dx & data$decimalLatitude < input$lat_upw & data$decimalLatitude > input$lat_down)
      data <- data[condition,]
    }


    condition_years <- which(data$year <= as.numeric(input$year))

    data <- data[condition_years,]

    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      addCircles(lat = ~decimalLatitude, lng = ~decimalLongitude)

  })


  observe({

    bio1Input <- input$bio1

    if(bio1Input != "Population Density"){
      bio_number <- vars_bio[which(vars_bio$bio_name == bio1Input),"bio_num"]
      #rasterDF <- readRDS(paste0("C:\\Users\\Luca Danese\\Desktop\\PLS---Summer-School-2022\\bio_",bio_number,".RDS"))
      #githubURL <- paste0("https://github.com/lucadanese/PLS---Summer-School-2022/raw/main/bio_",bio_number,".RDS")
      #rasterDF <- readRDS(url(githubURL))
      rasterDF <- readRDS(paste0("data/bio_",bio_number,".RDS"))

    } else {
      #rasterDF <- readRDS(paste0("C:\\Users\\Luca Danese\\Desktop\\PLS---Summer-School-2022\\bio_","pop",".RDS"))
      #githubURL <- paste0("https://github.com/lucadanese/PLS---Summer-School-2022/raw/main/bio_","pop",".RDS")
      #rasterDF <- readRDS(url(githubURL))
      rasterDF <- readRDS(paste0("data/bio_","pop",".RDS"))

      # take the log of the population density
      rasterDF$value <- log(rasterDF$value)
      #
    }

    rasterDF <- rasterDF[which(rasterDF$x < input$long_dx_1 & rasterDF$x > input$long_sx_1
                               & rasterDF$y < input$lat_upw_1 & rasterDF$y > input$lat_down_1),]

    output$rasterPlot <- renderPlot(
      ggplot() +
        geom_tile(data = dplyr::filter(rasterDF, !is.na(value)),
                  aes(x = x, y = y), fill = "coral3") +
        geom_tile(data = rasterDF,
                  aes(x = x, y = y, fill = value)) +
        scale_fill_gradient(paste0(bio1Input),
                            low = 'yellow', high = 'blue',
                            na.value = NA) +
        coord_quickmap()

    )

  })

  observe({

    bio1Input_2 <- input$bio2

    if(bio1Input_2 != "Population Density"){
      #bio_number_2 <- which(vars_bio == bio1Input_2)
      bio_number_2 <- vars_bio[which(vars_bio$bio_name == bio1Input_2),"bio_num"]
      #rasterDF_2 <- readRDS(paste0("C:\\Users\\Luca Danese\\Desktop\\PLS---Summer-School-2022\\bio_",bio_number_2,".RDS"))
      #githubURL <- paste0("https://github.com/lucadanese/PLS---Summer-School-2022/raw/main/bio_",bio_number_2,".RDS")
      #rasterDF_2 <- readRDS(url(githubURL))
      rasterDF_2 <- readRDS(paste0("data/bio_",bio_number_2,".RDS"))
    } else {
      #rasterDF_2 <- readRDS(paste0("C:\\Users\\Luca Danese\\Desktop\\PLS---Summer-School-2022\\bio_","pop",".RDS"))
      #githubURL <- paste0("https://github.com/lucadanese/PLS---Summer-School-2022/raw/main/bio_","pop",".RDS")
      #rasterDF_2 <- readRDS(url(githubURL))
      rasterDF_2 <- readRDS(paste0("data/bio_","pop",".RDS"))
      rasterDF_2$value <- log(rasterDF_2$value)
    }


    rasterDF_2 <- rasterDF_2[which(rasterDF_2$x < input$long_dx_2 & rasterDF_2$x > input$long_sx_2
                                   & rasterDF_2$y < input$lat_upw_2 & rasterDF_2$y > input$lat_down_2),]

    output$rasterPlot_realenativo <- renderPlot(
      ggplot() +
        geom_tile(data = dplyr::filter(rasterDF_2, !is.na(value)),
                  aes(x = x, y = y), fill = "coral3") +
        geom_tile(data = rasterDF_2,
                  aes(x = x, y = y, fill = value)) +
        scale_fill_gradient(paste0(bio1Input_2),
                            low = 'yellow', high = 'blue',
                            na.value = NA) +
        coord_quickmap()

    )

  })

  observe({

    data_map[,"var_plot"] = data_map[,input$indicatore]

    data_map <- data_map[which(data_map$long < input$long_dx_3 & data_map$long > input$long_sx_3
                                   & data_map$lat < input$lat_upw_3 & data_map$lat > input$lat_down_3),]

    output$PlotDemoEco <- renderPlot(

      ggplot(data_map, aes(long,lat, group=group, fill = var_plot)) +
      geom_polygon() +
      coord_quickmap() +
      guides(fill=guide_legend(title=input$indicatore)),
      width = 1000,
      height = 1000

    )



  })

}


# Run the application
shinyApp(ui = ui, server = server)
