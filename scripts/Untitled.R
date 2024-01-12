data <- read_delim("/Users/Jordan/Documents/School/University/Master's degree/Winter 2024/MATH 60611A - Advanced Statistical Learning/project/data_final.csv", delim = ";")
data_sf <- 
  sf::st_as_sf(
    data,
    # "coords" is in x/y order -- so longitude goes first!
    coords = c("y", "x"),
    # Set our coordinate reference system to EPSG:4326,
    # the standard WGS84 geodetic coordinate reference system
    crs = 32188
  )
library(spatialsample)

set.seed(123)
cluster_folds <- spatial_clustering_cv(data_sf, v = 15)

autoplot(cluster_folds)
st_coordinates(data_sf)
st_transform(data_sf, 4326)