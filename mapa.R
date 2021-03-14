library(shiny)
library(rgdal)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)


data <- read.csv("data/data.csv")

map <- readOGR("data/fe_2007_39_county/fe_2007_39_county.shp")


# ui object
ui <- fluidPage(
  titlePanel(p("Spatial app", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      p("Feito pela Paula", a("Shiny",
                       href = "http://shiny.rstudio.com"
      ), "."),
      img(
        src = "imageShiny.png",
        width = "70px", height = "70px"
      )
    ),
    mainPanel(
      leafletOutput(outputId = "map"),
      DTOutput(outputId = "table")
    )
  )
)

# server()
server <- function(input, output) {
  
  
  output$map <- renderLeaflet({
    
    # Add data to map
    datafiltered <- data[which(data$year == 1980), ]
    ordercounties <- match(map@data$NAME, datafiltered$county)
    map@data <- datafiltered[ordercounties, ]
    
    # Create leaflet
    pal <- colorBin("YlOrRd", domain = map$cases, bins = 7)
    
    labels <- sprintf("%s: %g", map$county, map$cases) %>%
      lapply(htmltools::HTML)
    
    l <- leaflet(map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(cases),
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~cases,
        opacity = 0.7, title = NULL
      )
  })
}

# shinyApp()
shinyApp(ui = ui, server = server)
