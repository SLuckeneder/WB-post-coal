
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
  
  # res land use
  residential <- bb %>%
    osmdata::opq(timeout = 180) %>%
    osmdata::add_osm_feature(key = "landuse", value = "residential") %>%
    osmdata::osmdata_sf()
  residential$osm_polygons <- residential$osm_polygons %>% sf::st_make_valid()
  poly <- residential$osm_polygons
  poly <- sf::st_intersection(poly, cb) %>% # clip to country boundary
    dplyr::select(geometry)
  
  # combine with multipolygons
  residential$osm_multipolygons <- residential$osm_multipolygons %>% sf::st_make_valid()
  multi <- residential$osm_multipolygons
  multi <- multi[sf::st_is_valid(multi), ] # ind and remove invalid (3 observations in Vietnam) as nothing else helped
  multi <- sf::st_intersection(multi, cb) %>% # clip to country boundary
    dplyr::select(geometry)

  common_cols <- intersect(names(poly), names(multi)) # Identify shared columns
  poly_aligned <- poly[, common_cols] # Subset and reorder columns in both datasets
  multi_aligned <- multi[, common_cols]
  res_combined <- rbind(poly_aligned, multi_aligned)
  
  sf::st_write(res_combined, paste0("data/osm/", i, ".gpkg"), layer = "residential", append=FALSE)
  
}

# # map
# mapview(
#   borders %>% dplyr::filter(name_long == "Vietnam"),
#   color = "white",
#   col.regions = "white",
#   alpha.regions = 0.05,
#   layer.name = "Vietnam",
#   popup = FALSE
# ) +
#   mapview(
#     residential,
#     color = "magenta",
#     col.regions = "magenta",
#     alpha.regions = 0.2,
#     layer.name = "Residential Area"
#   ) +
#   mapview(
#     multi_aligned,
#     color = "red",
#     col.regions = "red",
#     alpha.regions = 0.2,
#     layer.name = "Residential Area Multi"
#   )
