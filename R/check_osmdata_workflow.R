
library(dplyr)
library(ggplot2)
library(sf)
library(osmdata)

available_features()
available_tags("highway")

# creating bounding box for vienna
vienna_bb <- getbb("vienna")

# retrieving data of streets in vienna
vienna_streets <- vienna_bb %>%
  opq() %>%
  add_osm_feature("highway", c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

# retrieving data of small streets in vienna
vienna_small_streets <- vienna_bb %>%
  opq() %>%
  add_osm_feature(key = "highway", value = c("residential", "living_street", "unclassified", "service", "footway")) %>%
  osmdata_sf()

# retrieving data of rivers in vienna
vienna_rivers <- vienna_bb %>%
  opq() %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

# retrieving data of hospitals in vienna
vienna_hospitals <- vienna_bb %>%
  opq() %>%
  add_osm_feature("amenity", "hospital") %>%
  osmdata_sf()

# retrieving data of power lines and plants in vienna
vienna_power_line <- vienna_bb %>%
  opq() %>%
  add_osm_feature("power", "line") %>%
  osmdata_sf()
vienna_power_plant <- vienna_bb %>%
  opq() %>%
  add_osm_feature("power", "plant") %>%
  osmdata_sf()

# retrieving data of railways in vienna
vienna_railways <- vienna_bb %>%
  opq() %>%
  add_osm_feature("railway", "rail") %>%
  osmdata_sf()

# map
ggplot() +
  geom_sf(data = vienna_small_streets$osm_lines, inherit.aes = FALSE, color = "#bfbfbf", size = .2, alpha = .8) +
  geom_sf(data = vienna_streets$osm_lines, inherit.aes = FALSE, color = "#ffbe7f", size = .4, alpha = .8) +
  geom_sf(data = vienna_railways$osm_lines, inherit.aes = FALSE, color = "red", size = .8, alpha = .5) +
  geom_sf(data = vienna_rivers$osm_lines, inherit.aes = FALSE, color = "#7fc0ff", size = .8, alpha = .5) +
  geom_sf(data = vienna_power_line$osm_lines, inherit.aes = FALSE, color = "black", size = .8, alpha = .5) +
  geom_sf(data = vienna_power_plant$osm_polygons, inherit.aes = FALSE, color = "black", fill = "black", size = 1, alpha = .6) +
  geom_sf(data = vienna_hospitals$osm_polygons, inherit.aes = FALSE, colour = "#08519c", fill = "#08306b", alpha = .6, size = 1) +
  coord_sf(xlim = c(vienna_bb[1], vienna_bb[3]), ylim = c(vienna_bb[2], vienna_bb[4])) +
  theme_bw() +
  labs(
    title = "Vienna infrastructure",
    x = "Latitude",
    y = "Longitude"
  )
