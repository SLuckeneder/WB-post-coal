
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

  # main roads --------------------------------------------------------------

  cat("roads...")

  motorway <- bb %>%
    osmdata::opq(timeout = 180) %>%
    osmdata::add_osm_feature("highway", "motorway") %>%
    osmdata::osmdata_sf()
  motorway <- sf::st_intersection(motorway$osm_lines, cb) %>% # clip to country boundary
    dplyr::select(geometry)
  sf::st_write(motorway, paste0("data/osm/", i, ".gpkg"), layer = "motorway")

  primary <- bb %>%
    osmdata::opq(timeout = 180) %>%
    osmdata::add_osm_feature("highway", "primary") %>%
    osmdata::osmdata_sf()
  primary <- sf::st_intersection(primary$osm_lines, cb) %>% # clip to country boundary
    dplyr::select(geometry)
  sf::st_write(primary, paste0("data/osm/", i, ".gpkg"), layer = "primary")

  # transmission lines ------------------------------------------------------

  cat("power lines...")

  power_line <- bb %>%
    osmdata::opq(timeout = 180) %>%
    osmdata::add_osm_feature("power", "line") %>%
    osmdata::osmdata_sf()
  power_line <- sf::st_intersection(power_line$osm_lines, cb) %>% # clip to country boundary
    dplyr::select(geometry)
  sf::st_write(power_line, paste0("data/osm/", i, ".gpkg"), layer = "power_line")


  # substations -------------------------------------------------------------

  cat("substations...")

  substation <- bb %>%
    osmdata::opq(timeout = 180) %>%
    osmdata::add_osm_feature("power", "substation") %>%
    osmdata::osmdata_sf()
  substation <- sf::st_intersection(substation$osm_points, cb) %>% # clip to country boundary
    dplyr::select(geometry)
  sf::st_write(substation, paste0("data/osm/", i, ".gpkg"), layer = "substation")

  cat("done.")
  
  # airports ----------------------------------------------------------------
  
  cat("airports...")
  #  aerodrome includes aerodrome, airport or airfield (OSM)
  
  airport <- bb %>%
    osmdata::opq(timeout = 180) %>%    
    osmdata::add_osm_feature("aeroway", "aerodrome") %>%
    osmdata::osmdata_sf()
  airport <- sf::st_intersection(airport$osm_points, cb) %>% # clip to country boundary
    dplyr::select(geometry)
  sf::st_write(airport, paste0("data/osm/", i, ".gpkg"), layer = "airport")
  
  cat("done.")
  
    
}


# maps --------------------------------------------------------------------

borders <- spData::world %>%
  dplyr::filter(name_long %in% c("Vietnam", "Philippines", "Indonesia", "Thailand"))

# Vietnam
motorway <- st_read("data/osm/Vietnam.gpkg", layer = "motorway")
primary <- st_read("data/osm/Vietnam.gpkg", layer = "primary")
power_line <- st_read("data/osm/Vietnam.gpkg", layer = "power_line")
substation <- st_read("data/osm/Vietnam.gpkg", layer = "substation")
airport <- st_read("data/osm/Vietnam.gpkg", layer = "airport")

bb <- osmdata::getbb("Vietnam")

p_VIE <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = borders %>% dplyr::filter(name_long == "Vietnam"), 
                   color = "black", fill = NA, size = .5) +
  ggplot2::geom_sf(data = power_line, inherit.aes = FALSE, 
                   aes(color = "Power Lines"), size = .5) +
  ggplot2::geom_sf(data = substation, inherit.aes = FALSE, 
                   aes(color = "Substations"), size = .5, shape = 0) +
  ggplot2::geom_sf(data = primary, inherit.aes = FALSE, 
                   aes(color = "Primary Roads"), size = .5) +
  ggplot2::geom_sf(data = motorway, inherit.aes = FALSE, 
                   aes(color = "Motorways"), size = .8) +
  ggplot2::geom_sf(data = airport, inherit.aes = FALSE, 
                   aes(color = "Airports"), size = .5, shape = 2) +
  ggplot2::coord_sf(datum = NA, xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4])) +
  ggplot2::labs(title = "Vietnam") +
  ggplot2::scale_color_manual(name = NULL, 
                              values = c("Motorways" = "red", 
                                         "Primary Roads" = "orange", 
                                         "Airports" = "blue",
                                         "Power Lines" = "darkgrey", 
                                         "Substations" = "black"),
                              breaks = c("Motorways", "Primary Roads", "Airports", "Power Lines", "Substations"),
                              labels = c("Motorways", "Primary Roads", "Airports", "Power Lines", "Substations")) +
  ggplot2::theme_minimal() +
  ggspatial::annotation_scale() +
  ggplot2::theme(legend.position = "bottom")


# Philippines
motorway <- st_read("data/osm/Philippines.gpkg", layer = "motorway")
primary <- st_read("data/osm/Philippines.gpkg", layer = "primary")
power_line <- st_read("data/osm/Philippines.gpkg", layer = "power_line")
substation <- st_read("data/osm/Philippines.gpkg", layer = "substation")
airport <- st_read("data/osm/Philippines.gpkg", layer = "airport")

bb <- osmdata::getbb("Philippines")

p_PHL <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = borders %>% dplyr::filter(name_long == "Philippines"), 
                   color = "black", fill = NA, size = .5) +
  ggplot2::geom_sf(data = power_line, inherit.aes = FALSE, 
                   aes(color = "Power Lines"), size = .5) +
  ggplot2::geom_sf(data = substation, inherit.aes = FALSE, 
                   aes(color = "Substations"), size = .5, shape = 0) +
  ggplot2::geom_sf(data = primary, inherit.aes = FALSE, 
                   aes(color = "Primary Roads"), size = .5) +
  ggplot2::geom_sf(data = motorway, inherit.aes = FALSE, 
                   aes(color = "Motorways"), size = .8) +
  ggplot2::geom_sf(data = airport, inherit.aes = FALSE, 
                   aes(color = "Airports"), size = .5, shape = 2) +
  ggplot2::coord_sf(datum = NA, xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4])) +
  ggplot2::labs(title = "Philippines") +
  ggplot2::scale_color_manual(name = NULL, 
                              values = c("Motorways" = "red", 
                                         "Primary Roads" = "orange", 
                                         "Airports" = "blue",
                                         "Power Lines" = "darkgrey", 
                                         "Substations" = "black"),
                              breaks = c("Motorways", "Primary Roads", "Airports", "Power Lines", "Substations"),
                              labels = c("Motorways", "Primary Roads", "Airports", "Power Lines", "Substations")) +
  ggplot2::theme_minimal() +
  ggspatial::annotation_scale() +
  ggplot2::theme(legend.position = "bottom")

# Indonesia
motorway <- st_read("data/osm/Indonesia.gpkg", layer = "motorway")
primary <- st_read("data/osm/Indonesia.gpkg", layer = "primary")
power_line <- st_read("data/osm/Indonesia.gpkg", layer = "power_line")
substation <- st_read("data/osm/Indonesia.gpkg", layer = "substation")
airport <- st_read("data/osm/Indonesia.gpkg", layer = "airport")

bb <- osmdata::getbb("Indonesia")

p_IDN <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = borders %>% dplyr::filter(name_long == "Indonesia"), 
                   color = "black", fill = NA, size = .5) +
  ggplot2::geom_sf(data = power_line, inherit.aes = FALSE, 
                   aes(color = "Power Lines"), size = .5) +
  ggplot2::geom_sf(data = substation, inherit.aes = FALSE, 
                   aes(color = "Substations"), size = .5, shape = 0) +
  ggplot2::geom_sf(data = primary, inherit.aes = FALSE, 
                   aes(color = "Primary Roads"), size = .5) +
  ggplot2::geom_sf(data = motorway, inherit.aes = FALSE, 
                   aes(color = "Motorways"), size = .8) +
  ggplot2::geom_sf(data = airport, inherit.aes = FALSE, 
                   aes(color = "Airports"), size = .5, shape = 2) +
  ggplot2::coord_sf(datum = NA, xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4])) +
  ggplot2::labs(title = "Indonesia") +
  ggplot2::scale_color_manual(name = NULL, 
                              values = c("Motorways" = "red", 
                                         "Primary Roads" = "orange", 
                                         "Airports" = "blue",
                                         "Power Lines" = "darkgrey", 
                                         "Substations" = "black"),
                              breaks = c("Motorways", "Primary Roads", "Airports", "Power Lines", "Substations"),
                              labels = c("Motorways", "Primary Roads", "Airports", "Power Lines", "Substations")) +
  ggplot2::theme_minimal() +
  ggspatial::annotation_scale() +
  ggplot2::theme(legend.position = "bottom")

# Thailand
motorway <- st_read("data/osm/Thailand.gpkg", layer = "motorway")
primary <- st_read("data/osm/Thailand.gpkg", layer = "primary")
power_line <- st_read("data/osm/Thailand.gpkg", layer = "power_line")
substation <- st_read("data/osm/Thailand.gpkg", layer = "substation")
airport <- st_read("data/osm/Thailand.gpkg", layer = "airport")

bb <- osmdata::getbb("Thailand")

p_THA <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = borders %>% dplyr::filter(name_long == "Thailand"), 
                   color = "black", fill = NA, size = .5) +
  ggplot2::geom_sf(data = power_line, inherit.aes = FALSE, 
                   aes(color = "Power Lines"), size = .5) +
  ggplot2::geom_sf(data = substation, inherit.aes = FALSE, 
                   aes(color = "Substations"), size = .5, shape = 0) +
  ggplot2::geom_sf(data = primary, inherit.aes = FALSE, 
                   aes(color = "Primary Roads"), size = .5) +
  ggplot2::geom_sf(data = motorway, inherit.aes = FALSE, 
                   aes(color = "Motorways"), size = .8) +
  ggplot2::geom_sf(data = airport, inherit.aes = FALSE, 
                   aes(color = "Airports"), size = .5, shape = 2) +
  ggplot2::coord_sf(datum = NA, xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4])) +
  ggplot2::labs(title = "Thailand") +
  ggplot2::scale_color_manual(name = NULL, 
                              values = c("Motorways" = "red", 
                                         "Primary Roads" = "orange", 
                                         "Airports" = "blue",
                                         "Power Lines" = "darkgrey", 
                                         "Substations" = "black"),
                              breaks = c("Motorways", "Primary Roads", "Airports", "Power Lines", "Substations"),
                              labels = c("Motorways", "Primary Roads", "Airports", "Power Lines", "Substations")) +
  ggplot2::theme_minimal() +
  ggspatial::annotation_scale() +
  ggplot2::theme(legend.position = "bottom")
# export
ggplot2::ggsave("osm-VIE.pdf",
                plot = p_VIE, device = "pdf",
                path = paste0("./output"),
                scale = 1, width = 150, height = 200, units = "mm")
ggplot2::ggsave("osm-PHL.pdf",
                plot = p_PHL, device = "pdf",
                path = paste0("./output"),
                scale = 1, width = 150, height = 200, units = "mm")
ggplot2::ggsave("osm-IDN.pdf",
                plot = p_IDN, device = "pdf",
                path = paste0("./output"),
                scale = 1, width = 400, height = 200, units = "mm")
ggplot2::ggsave("osm-THA.pdf",
                plot = p_THA, device = "pdf",
                path = paste0("./output"),
                scale = 1, width = 150, height = 200, units = "mm")



                   