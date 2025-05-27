#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#      Leaflet Basics
#    
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(sf)
library(leaflet)
###-----------------------------------------------------
#    Leaflet Intro
###-----------------------------------------------------

m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=-82.41694569072655, 
                 lat=29.727245351471385,
                 popup="You are here")
m  # Print the map

###-----------------------------------------------------
#    Chloropleth
###-----------------------------------------------------

# From https://leafletjs.com/examples/choropleth/us-states.js
states <- sf::read_sf("https://rstudio.github.io/leaflet/json/us-states.geojson")

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
#pal can be one of
#colorBin
#colorFactor
#colorNumeric
#colorQuantile
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  states$name, states$density
) %>% lapply(htmltools::HTML)

leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      #Custom HTML styling
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
    position = "bottomright")
###-----------------------------------------------------
#    Markers
###-----------------------------------------------------
getColor <- function(quakes) {
  sapply(quakes$mag, function(mag) {
  if(mag <= 4) {
    "green"
  } else if(mag <= 5) {
    "orange"
  } else {
    "red"
  } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(quakes)
)

leaflet(quakes) %>% 
  addTiles() %>%
  addAwesomeMarkers(~long, ~lat,
                    icon=icons, #list of of custom icons
                    label=~as.character(mag), #simple label
                    clusterOptions = markerClusterOptions())