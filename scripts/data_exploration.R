library(tidyverse)
library(arrow)
library(sf)
library(leaflet)

# For cleaning final data in parquet format, see data_cleaning.R script
data <- read_parquet("./data/data_final.parquet")

data_sf <- 
  sf::st_as_sf(
    data,
    # Pretty sure x represents longitude (NOT latitude)
    coords = c("x", "y"), # coords takes longitude first
    # Set our coordinate reference system to EPSG:32188 ,
    # the standard WGS84 geodetic coordinate reference system
    crs = 32188
  )


coordinates_nad <- st_coordinates(data_sf) # extract coordinates

data_sf_wgs <- st_transform(data_sf, crs = 4326)

pal <- colorNumeric(
  palette = "YlGnBu",
  domain = data_sf$acc)

leaflet() |> 
  addTiles() |> 
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") |> 
  addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") |> 
  addCircleMarkers(data=st_transform(data_sf, crs = 4326), radius = 5, fillColor = ~pal(acc),
                   stroke = FALSE, fillOpacity = 0.8)