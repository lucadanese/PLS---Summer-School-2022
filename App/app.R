library(shiny)
library(leaflet)
library(readxl)
library(raster)
library(ggplot2)
library(data.table)
library(curl)

#setwd("C:\\Users\\Luca Danese\\Desktop\\PLS---Summer-School-2022")

#Aedes.gbif <- read_excel("C:\\Users\\Luca Danese\\Desktop\\PLS---Summer-School-2022\\Aedes albopictus_GBIF.xlsx")
Aedes.gbif <- fread("https://raw.githubusercontent.com/lucadanese/PLS---Summer-School-2022/main/aedes_albopictus_GBIF.csv",integer64 = "numeric")
Aedes.gbif<-as.data.frame(Aedes.gbif)

data <- Aedes.gbif[1:60849,c("decimalLatitude","decimalLongitude")]
data$decimalLatitude <- as.numeric(gsub(",", ".", data$decimalLatitude))
data$decimalLongitude <- as.numeric(gsub(",", ".", data$decimalLongitude))


vars <- c("Reale Nativo", "Mondo", "Manuale")
vars_bio <- c("Annual Mean Temperature",
              "Mean Diurnal Range",
              "Isothermality",
              "Temperature Seasonality",
              "Max Temperature of Warmest Month",
              "Min Temperature of Coldest Month",
              "Temperature Annual Range (BIO5-BIO6)",
              "Mean Temperature of Wettest Quarter",
              "Mean Temperature of Driest Quarter",
              "Population Density")

ui <- navbarPage("Aedes", id = "nav",

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
                                            )
                              )
                          )

                 ),

                 tabPanel("Caratteristiche Morfologiche",

                          fluidPage(
                            fluidRow(
                              h1("Paese studio"),
                              sidebarPanel(
                                selectInput("bio1", "Bio", vars_bio),
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
                                selectInput("bio2", "Bio", vars_bio),
                                numericInput("long_sx_2", "Longitudine Sinistra", 66),
                                numericInput("long_dx_2", "Longitudine Destra", 180),
                                numericInput("lat_down_2", "Latitudine Inferiore", -90),
                                numericInput("lat_upw_2", "Latitudine Superiore", 90))
                              ,
                              mainPanel(
                                plotOutput(outputId = "rasterPlot_realenativo")
                              )
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

    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      addCircles(lat = ~decimalLatitude, lng = ~decimalLongitude)

  })


  observe({

    bio1Input <- input$bio1

    if(bio1Input != "Population Density"){
      bio_number <- which(vars_bio == bio1Input)
      #rasterDF <- readRDS(paste0("C:\\Users\\Luca Danese\\Desktop\\PLS---Summer-School-2022\\bio_",bio_number,".RDS"))
      #githubURL <- paste0("https://github.com/lucadanese/PLS---Summer-School-2022/raw/main/bio_",bio_number,".RDS")
      #rasterDF <- readRDS(url(githubURL))
      rasterDF <- readRDS(paste0("data/bio_",bio_number,".RDS"))

    } else {
      #rasterDF <- readRDS(paste0("C:\\Users\\Luca Danese\\Desktop\\PLS---Summer-School-2022\\bio_","pop",".RDS"))
      #githubURL <- paste0("https://github.com/lucadanese/PLS---Summer-School-2022/raw/main/bio_","pop",".RDS")
      #rasterDF <- readRDS(url(githubURL))
      rasterDF <- readRDS(paste0("data/bio_","pop",".RDS"))
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
      bio_number_2 <- which(vars_bio == bio1Input_2)
      #rasterDF_2 <- readRDS(paste0("C:\\Users\\Luca Danese\\Desktop\\PLS---Summer-School-2022\\bio_",bio_number_2,".RDS"))
      #githubURL <- paste0("https://github.com/lucadanese/PLS---Summer-School-2022/raw/main/bio_",bio_number_2,".RDS")
      #rasterDF_2 <- readRDS(url(githubURL))
      rasterDF_2 <- readRDS(paste0("data/bio_",bio_number_2,".RDS"))
    } else {
      #rasterDF_2 <- readRDS(paste0("C:\\Users\\Luca Danese\\Desktop\\PLS---Summer-School-2022\\bio_","pop",".RDS"))
      #githubURL <- paste0("https://github.com/lucadanese/PLS---Summer-School-2022/raw/main/bio_","pop",".RDS")
      #rasterDF_2 <- readRDS(url(githubURL))
      rasterDF_2 <- readRDS(paste0("data/bio_","pop",".RDS"))
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

}


# Run the application
shinyApp(ui = ui, server = server)
