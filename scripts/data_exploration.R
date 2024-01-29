library(tidyverse)
library(arrow)
library(sf)
library(leaflet)

# For cleaning final data in parquet format, see data_cleaning.R script
data <- read_parquet("./data/data_final.parquet")

not_boroughs <- c("Kirkland", "Hampstead", "Côte-Saint-Luc", "Dorval", "Dollard-des-Ormeaux", "Beaconsfield", "Sud-Ouest", "Westmount", "Mont-Royal")

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
  palette = "viridis",
  domain = data_sf$acc)
leaflet(data = data_sf_wgs) |> 
  addTiles() |> 
  addProviderTiles("CartoDB.Positron") |> 
  addCircleMarkers(radius = 7,
                   fillColor = ~pal(acc), stroke = FALSE, fillOpacity = 0.8,
                   popup = ~str_glue("<b>Number of accidents: </b>{acc}<br/>
                                       <b>Intersection: </b>{rue_1} & {rue_2}<br/>
                                       <b>Borough: </b>{borough}"), 
                   label = ~str_glue("Intersection number: {int_no}")) %>%
  addLegend(pal = pal, values = ~acc, position = "bottomright", title = "Accidents")

# Some nice map palettes c("Stadia.AlidadeSmooth", "CartoDB.Positron")




# Land use data: https://donnees.montreal.ca/dataset/schema-affectation-densite

mat = st_intersects(data_sf, land_use, sparse = FALSE)
rownames(mat) <- data$int_no
colnames(mat) <- land_use$AFFECTATIO

aug_land_use <- 
  as_tibble(mat, .name_repair = "universal", rownames = NA) |> 
  rownames_to_column("int_no") |> 
  pivot_longer(!int_no) |> 
  mutate(name = 
           str_replace(name, "...\\d+", "") |> 
           str_replace_all("\\.", "_") |> str_to_lower()) |>
  group_by(int_no, name) |> 
  summarise(value = sum(value), .groups = "drop") |> 
  filter(value >= 1) |> 
  pivot_wider(names_from = name, values_from = value, values_fill = 0) |> 
  right_join(select(data, int_no), by = "int_no") |> 
  mutate(across(everything(), ~replace_na(., 0))) |> 
  pivot_longer(!int_no) |> 
  filter(value == 1) |>  
  # intersections only have one land use (checked)
  # However, 4 intersections missing
  select(int_no, land_use = name)

# Combine grand_espace_vert_ou_récréation and conservation with dominante_résidentielle
# grande_emprise_ou_grande_infrastructure_publique with industrie 
# NA with something

left_join(data_sf, aug_land_use, by = "int_no")

# if you want to use date, there are missing data points
# For months you may want to aggregate into seasons
# Weekday... no collection on weekends
# year... most of the data was collected 2009 (50.9%)... maybe add before_2009 var




