library(shiny)
library(leaflet)
#library(maps)
#library(RColorBrewer)
library(readxl)

setwd("C:\\Users\\danes\\Desktop\\PLS - Summer School 2022\\PLS---Summer-School-2022")

Aedes.gbif <- read_excel("C:\\Users\\danes\\Desktop\\PLS - Summer School 2022\\PLS---Summer-School-2022\\Aedes albopictus_GBIF.xlsx")
Aedes.gbif<-as.data.frame(Aedes.gbif)
data <- Aedes.gbif[1:60849,c("decimalLatitude","decimalLongitude")]

vars <- c("Reale Nativo", "Mondo", "Manuale")

ui <- navbarPage("Aedes", id = "nav",

          tabPanel("Osservazioni Aedes",
                div(class = "outer",

                    tags$head(
                      includeCSS("styles.css"),
                      includeScript("gomap.js")
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


}


# Run the application
shinyApp(ui = ui, server = server)
