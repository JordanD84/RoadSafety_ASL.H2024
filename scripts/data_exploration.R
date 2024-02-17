library(tidyverse)
library(tidymodels)
library(arrow)
library(sf)
library(leaflet)

# For cleaning final data in parquet format, see data_cleaning.R script
data <- read_parquet("./processed_data/data_final.parquet")

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

factor_count <- 
  select(data, where(is.factor)) |> 
  pivot_longer(everything()) |> 
  count(name, value) |> 
  print(n = 30)

compare_graph <- function(var){
  select(data, acc, all_of(var)) |> 
    GGally::ggpairs()
}

# Comparing single covariate models to full
compare_estimates <- function(df){
  ind_var <- colnames(df)[colnames(df)!="acc"]
  
  
  nested <- 
    map(ind_var, \(x) 
        df |> 
          recipe() |> 
          update_role(acc, new_role = "outcome") |> 
          update_role(all_of(x), new_role = "predictor") |> 
          #step_normalize(all_predictors()) |>
          workflow(spec = linear_reg()) |> 
          fit(data = df) |> 
          tidy() |> 
          filter(term != "(Intercept)") 
        
    ) |> 
    set_names(ind_var) |> 
    list_rbind(names_to = "term") |> 
    rename_with(~str_glue("nested_{.x}"), .cols = !term)
  
  full <- 
    df |> 
    recipe() |> 
    update_role(acc, new_role = "outcome") |> 
    update_role(all_of(ind_var), new_role = "predictor") |> 
    #step_normalize(all_predictors()) |>
    workflow(spec = linear_reg()) |> 
    fit(data = df) |> 
    tidy() |> 
    filter(term != "(Intercept)") |> 
    rename_with(~str_glue("full_{.x}"), .cols = !term)
  
  compare_df <- left_join(nested, full, by = "term", unmatched = "error", relationship = "one-to-one")
  return(compare_df)
}
#test <- compare_estimates(select(data, acc, where(is.double), -x, -y, -date))
check <- compare_estimates(select(data, acc, starts_with("ln_")))

check |> 
  mutate(full = nested_estimate/full_estimate-1,
         nested= 0) |> 
  select(term, full, nested) |> 
  pivot_longer(!term, values_to = "change") |> 
  ggplot(aes(term, change)) +
  geom_line(aes(group = term), color = "darkgrey") +
  geom_point(aes(color = name)) +
  coord_flip() +
  scale_y_continuous(labels = label_percent())+
  theme_minimal() +
  labs(title = "Difference in coefficient estimate", y = "Change (%)", x = "Term", color = "Model")

# should probably have standardized coefficients

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


# if you want to use date, there are missing data points
# For months you may want to aggregate into seasons
# Weekday... no collection on weekends
# year... most of the data was collected 2009 (50.9%)... maybe add before_2009 var




