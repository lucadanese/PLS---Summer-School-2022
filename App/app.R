#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(maps)
library(RColorBrewer)
library(readxl)

Aedes.gbif <- read_excel("C:\\Users\\Luca Danese\\Desktop\\Uni\\Dottorato\\R4DS\\PLS---Summer-School-2022\\Aedes albopictus_GBIF.xlsx")
Aedes.gbif<-as.data.frame(Aedes.gbif)
data_prova <- Aedes.gbif[1:60849,c("decimalLatitude","decimalLongitude")]

# Define UI for application that draws a histogram
ui <- navbarPage(
    tabPanel(
    #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    #absolutePanel(top = 10, right = 10,
    #              selectInput("colors", "Color Scheme",
    #                          rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
    #             )
    #)
)
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data_prova) %>%
      addTiles() %>%
      addMarkers(lat = ~decimalLatitude, lng = ~decimalLongitude,
                 clusterOptions = markerClusterOptions(), icon = )
  })


}

# Run the application
shinyApp(ui = ui, server = server)
