
library(dplyr)
library(ggplot2)
library(sf)
library(osmdata)
library(spData)

countries <- c("Vietnam", "Philippines", "Indonesia", "Thailand")

borders <- spData::world %>%
  dplyr::filter(name_long %in% countries)

for (i in countries){
  
  cat("\n", i, "...")
  
  # creating bounding box
  bb <- osmdata::getbb(i)
  
  # get country mask
  cb <- borders %>%
    dplyr::filter(name_long== i) %>%
    sf::st_transform(4326)  # Ensure correct CRS
  
  river <- bb %>%
    osmdata::opq(timeout = 180) %>%
    osmdata::add_osm_feature("water", "river") %>%
    osmdata::osmdata_sf()
  river <- sf::st_intersection(river$osm_polygons, cb) %>% # clip to country boundary
    dplyr::select(geometry)
  sf::st_write(river, paste0("data/osm/", i, ".gpkg"), layer = "river", append=FALSE)
  
  canal <- bb %>%
    osmdata::opq(timeout = 180) %>%
    osmdata::add_osm_feature("water", "canal") %>%
    osmdata::osmdata_sf()
  canal <- sf::st_intersection(canal$osm_polygons, cb) %>% # clip to country boundary
    dplyr::select(geometry)
  sf::st_write(canal, paste0("data/osm/", i, ".gpkg"), layer = "canal", append=FALSE)
  
  lake <- bb %>%
    osmdata::opq(timeout = 180) %>%
    osmdata::add_osm_feature("water", "lake") %>%
    osmdata::osmdata_sf()
  lake <- sf::st_intersection(lake$osm_polygons, cb) %>% # clip to country boundary
    dplyr::select(geometry)
  sf::st_write(lake, paste0("data/osm/", i, ".gpkg"), layer = "lake", append=FALSE)
  
  reservoir <- bb %>%
    osmdata::opq(timeout = 180) %>%
    osmdata::add_osm_feature("water", "reservoir") %>%
    osmdata::osmdata_sf()
  reservoir$osm_polygons <- reservoir$osm_polygons %>% sf::st_make_valid()
  reservoir <- sf::st_intersection(reservoir$osm_polygons, cb) %>% # clip to country boundary
    dplyr::select(geometry)
  sf::st_write(reservoir, paste0("data/osm/", i, ".gpkg"), layer = "reservoir", append=FALSE)
 
}


# maps --------------------------------------------------------------------

borders <- spData::world %>%
  dplyr::filter(name_long %in% c("Vietnam", "Philippines", "Indonesia", "Thailand"))

# Vietnam
river <- st_read("data/osm/Vietnam.gpkg", layer = "river")
canal <- st_read("data/osm/Vietnam.gpkg", layer = "canal")
lake <- st_read("data/osm/Vietnam.gpkg", layer = "lake")
reservoir <- st_read("data/osm/Vietnam.gpkg", layer = "reservoir")

mapview(
  borders %>% dplyr::filter(name_long == "Vietnam"),
  color = "white",
  col.regions = "white",
  alpha.regions = 0.05,
  layer.name = "Vietnam",
  popup = FALSE
) +
  mapview(
    river,
    color = "black",
    col.regions = "black",
    alpha.regions = 0.2,
    layer.name = "Rivers"
  ) +
  mapview(
    canal,
    color = "orange",
    col.regions = "orange",
    alpha.regions = 0.2,
    layer.name = "Canals"
  ) +
  mapview(
    lake,
    color = "lightblue",
    col.regions = "lightblue",
    alpha.regions = 0.2,
    layer.name = "Lakes"
  ) +
  mapview(
    reservoir,
    color = "blue",
    col.regions = "blue",
    alpha.regions = 0.2,
    layer.name = "Reservoirs"
  )
