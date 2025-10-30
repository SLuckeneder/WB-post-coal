

library(dplyr)
library(sf)
library(terra)

countries <- c("Vietnam", "Philippines", "Indonesia", "Thailand")


# coal mine polygons
cm_polygons <-  sf::st_read("data/coal_mine_polygons.gpkg")

# large data, hence spatial intersection by country and then merge
store <- list()
for (i in countries){
  
  cm_polygons_i <- cm_polygons %>% dplyr::filter(country_name == i)
  
  # OSM infrastructure ------------------------------------------------------
  
  # osm roads (merge motorway and primary roads)
  motorway <- st_read(paste0("data/osm/", i, ".gpkg"), layer = "motorway")
  primary <- st_read(paste0("data/osm/", i, ".gpkg"), layer = "primary")
  roads_combined <- bind_rows(
    motorway, primary
  )
  
  roads_combined <- sf::st_transform(roads_combined, sf::st_crs(cm_polygons_i)) # Ensure same CRS
  dist_road <- sf::st_distance(cm_polygons_i, roads_combined) # distances are in meters
  min_distances <- apply(dist_road, 1, min)
  cm_polygons_i$dist_road_m <- min_distances
  
  # osm power line
  power_line <- st_read(paste0("data/osm/", i, ".gpkg"), layer = "power_line")
  power_line <- sf::st_transform(power_line, sf::st_crs(cm_polygons_i)) # Ensure same CRS
  dist_power_line <- sf::st_distance(cm_polygons_i, power_line) # distances are in meters
  min_distances <- apply(dist_power_line, 1, min)
  cm_polygons_i$dist_power_line_m <- min_distances
  
  # osm substation
  substation <- st_read(paste0("data/osm/", i, ".gpkg"), layer = "substation")
  substation <- sf::st_transform(substation, sf::st_crs(cm_polygons_i)) # Ensure same CRS
  dist_substation <- sf::st_distance(cm_polygons_i, substation) # distances are in meters
  min_distances <- apply(dist_substation, 1, min)
  cm_polygons_i$dist_substation_m <- min_distances
  
  # osm airport
  airport <- st_read(paste0("data/osm/", i, ".gpkg"), layer = "airport")
  airport <- sf::st_transform(airport, sf::st_crs(cm_polygons_i)) # Ensure same CRS
  dist_airport <- sf::st_distance(cm_polygons_i, airport) # distances are in meters
  min_distances <- apply(dist_airport, 1, min)
  cm_polygons_i$dist_airport_m <- min_distances
  
  # osm waterbodies
  river <- st_read(paste0("data/osm/", i, ".gpkg"), layer = "river")
  canal <- st_read(paste0("data/osm/", i, ".gpkg"), layer = "canal")
  lake <- st_read(paste0("data/osm/", i, ".gpkg"), layer = "lake")
  reservoir <- st_read(paste0("data/osm/", i, ".gpkg"), layer = "reservoir")
  waterbodies_combined <- bind_rows(
    river, canal, lake, reservoir
  )
  
  waterbodies_combined <- sf::st_transform(waterbodies_combined, sf::st_crs(cm_polygons_i)) # Ensure same CRS
  dist_waterbody <- sf::st_distance(cm_polygons_i, waterbodies_combined) # distances are in meters
  min_distances <- apply(dist_waterbody, 1, min)
  cm_polygons_i$dist_waterbody_m <- min_distances
  
  # # osm residential area
  # residential <- st_read(paste0("data/osm/", i, ".gpkg"), layer = "residential")
  # residential <- sf::st_transform(residential, sf::st_crs(cm_polygons_i)) # Ensure same CRS
  # dist_residential <- sf::st_distance(cm_polygons_i, residential) # distances are in meters
  # min_distances <- apply(dist_residential, 1, min)
  # cm_polygons_i$dist_residential_m <- min_distances
  

  # Global Urban Areas ------------------------------------------------------

  # Global Urban Areas dataset available in Daylight https://daylightmap.org/2023/11/27/urban.html
  if (!exists("urban")) {
    borders <- spData::world %>%
      dplyr::filter(name_long %in% countries)
    
    urban <- sf::st_read("data/urban_areas_full.shp") 
    urban <- sf::st_transform(urban, st_crs(borders))
    urban <- urban %>%
      sf::st_filter(borders)
  }
  
  urban <- sf::st_transform(urban, st_crs(cm_polygons_i)) # make sure both datasets have the same CRS
  dist_urban <- sf::st_distance(cm_polygons_i, urban) # distances are in meters
  min_distances <- apply(dist_urban, 1, min)
  cm_polygons_i$dist_urban_m <- min_distances
  
  # # Populated places --------------------------------------------------------
  # 
  # # Natural Earth https://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-populated-places/
  # if (!exists("pop")) {
  #   pop_places <- sf::st_read("data/ne_10m_populated_places/ne_10m_populated_places.shp") %>%
  #     sf::st_transform(sf::st_crs(cm_polygons_i)) %>% # Ensure same CRS
  #     dplyr::select(NAME, FEATURECLA, POP_MAX, ADM0NAME) %>%
  #     dplyr::filter(ADM0NAME %in% countries) %>%
  #     dplyr::filter(POP_MAX > 1000)
  # }
  # pop_places %>%
  #   dplyr::group_by(ADM0NAME) %>% dplyr::summarise(n = n())
  # 
  # # Compute the distance matrix between polygons and populated places
  # dist_matrix <- sf::st_distance(cm_polygons_i, pop_places)  # result is a matrix [polygons x points]
  # 
  # # Find minimum distance to any populated place for each polygon
  # min_dist_m <- apply(dist_matrix, 1, min)  # in meters (assuming projected CRS)
  # 
  # # Add as new column to cm_polygons_i
  # cm_polygons_i <- cm_polygons_i %>%
  #   dplyr::mutate(dist_popplace_m = min_dist_m)
  

  # GlobalSolarAtlas --------------------------------------------------------

  # PVOUT - PV power output
  if (!exists("PVOUT")) {
    PVOUT <- terra::rast("data/GlobalSolarAtlas/World_PVOUT_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF/PVOUT.tif")
  }
  # terra::crs(PVOUT)
  # sf::st_crs(cm_polygons_i)  

  # subset to country
  cm_vect <- terra::vect(cm_polygons_i) # convert sf object to terra-compatible format
  expanded_extent <- ext(cm_vect) + 0.1 # expand the area of interest just a little more
  PVOUT_crop <- terra::crop(PVOUT, expanded_extent) # Crop to the extent of the polygons

  # compute average PVOUT per polygon
  extracted_values <- terra::extract(PVOUT_crop, cm_vect, fun = mean, na.rm = TRUE) # Extract raster values for each polygon
  cm_polygons_i$avg_PVOUT <- extracted_values$PVOUT # Match the average PVOUT values back to the original sf object
  
  # Global Horizontal Irradiation
  if (!exists("GHI")) {
    GHI <- terra::rast("data/GlobalSolarAtlas/World_GHI_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF/GHI.tif") 
  }
  
  # subset to country
  cm_vect <- terra::vect(cm_polygons_i) # convert sf object to terra-compatible format
  expanded_extent <- ext(cm_vect) + 0.1 # expand the area of interest just a little more
  GHI_crop <- terra::crop(GHI, expanded_extent) # Crop to the extent of the polygons
  
  # compute average GHI per polygon
  extracted_values <- terra::extract(GHI_crop, cm_vect, fun = mean, na.rm = TRUE) # Extract raster values for each polygon
  cm_polygons_i$avg_GHI <- extracted_values$GHI # Match the average GHI values back to the original sf object
  
  
  # TEMP - Air Temperature at 2 m above ground level
  if (!exists("TEMP")) {
    TEMP <- terra::rast("data/GlobalSolarAtlas/World_TEMP_GISdata_LTAy_GlobalSolarAtlas-v2_GEOTIFF/TEMP.tif")
  }
  
  # subset to country
  cm_vect <- terra::vect(cm_polygons_i) # convert sf object to terra-compatible format
  expanded_extent <- ext(cm_vect) + 0.1 # expand the area of interest just a little more
  TEMP_crop <- terra::crop(TEMP, expanded_extent) # Crop to the extent of the polygons

  # compute average TEMP per polygon
  extracted_values <- terra::extract(TEMP_crop, cm_vect, fun = mean, na.rm = TRUE) # Extract raster values for each polygon
  cm_polygons_i$avg_TEMP <- extracted_values$TEMP # Match the average TEMP values back to the original sf object

  

  # GlobalWindAtlas ---------------------------------------------------------

  # wind speed
  windspeed <- terra::rast(paste0("data/GlobalWindAtlas/", i, "_wind-speed_50m.tif"))
  
  # # check CRS
  # terra::crs(windspeed)
  # sf::st_crs(cm_polygons_i)
  
  # compute average PVOUT per polygon
  cm_vect <- terra::vect(cm_polygons_i) # convert sf object to terra-compatible format
  extracted_values <- terra::extract(windspeed, cm_vect, fun = mean, na.rm = TRUE) # Extract raster values for each polygon
  colnames(extracted_values) <- c("ID", "windspeed")
  cm_polygons_i$avg_wind_speed_ms_50m <- extracted_values$windspeed # Match the average PVOUT values back to the original sf object
  

  # wind power density
  windpowerdensity <- terra::rast(paste0("data/GlobalWindAtlas/", i, "_power-density_50m.tif"))
  
  # compute average PVOUT per polygon
  cm_vect <- terra::vect(cm_polygons_i) # convert sf object to terra-compatible format
  extracted_values <- terra::extract(windpowerdensity, cm_vect, fun = mean, na.rm = TRUE) # Extract raster values for each polygon
  colnames(extracted_values) <- c("ID", "windpowerdensity")
  cm_polygons_i$avg_wind_power_density_Wm2_50m <- extracted_values$windpowerdensity # Match the average PVOUT values back to the original sf object
  
  

  # Pumped hydro energy storage potential -----------------------------------
  
  # read data
  PHES_raw <- readxl::read_xlsx("data/weber-et-al-2024-PHES.xlsx", sheet = 3)
  PHES_sub <- PHES_raw %>%
    dplyr::filter(`Non-overlapping` == 1) %>%
    dplyr::select("Unique Identifier", "Pair Identifier", "Class", "Energy (GWh)", "Upper latitude", "Upper longitude", "Lower latitude", "Lower longitude")
  
  # make 2 spatial datasets (upper and lower coordinates)
  PHES_sf_upper <- PHES_sub %>%
    st_as_sf(coords = c("Upper longitude", "Upper latitude"), crs = 4326)  # WGS 84 coordinate reference system
  PHES_sf_upper <- sf::st_transform(PHES_sf_upper, crs = sf::st_crs(cm_polygons_i))
  PHES_sf_lower <- PHES_sub %>%
    st_as_sf(coords = c("Lower longitude", "Lower latitude"), crs = 4326)  # WGS 84 coordinate reference system
  PHES_sf_lower <- sf::st_transform(PHES_sf_lower, crs = sf::st_crs(cm_polygons_i))
  
  # distance to closest PHES site upper coordinates
  dist_PHES_upper <- sf::st_distance(cm_polygons_i, PHES_sf_upper) # distances are in meters
  min_distances <- apply(dist_PHES_upper, 1, min)
  cm_polygons_i$dist_PHES_upper_m <- min_distances
  
  # distance to closest PHES site lower coordinates
  dist_PHES_lower <- sf::st_distance(cm_polygons_i, PHES_sf_lower) # distances are in meters
  min_distances <- apply(dist_PHES_lower, 1, min)
  cm_polygons_i$dist_PHES_lower_m <- min_distances
  
  # biophysical -------------------------------------------------------------
  
  # elevation
  if (!exists("elevation")) {
    elevation <- terra::rast("data/elevation_aws_terrain_tiles_z6.tif")
  }
  # terra::crs(elevation)

  # subset to country
  cm_vect <- terra::vect(cm_polygons_i) # convert sf object to terra-compatible format
  expanded_extent <- ext(cm_vect) + 0.1 # expand the area of interest just a little more
  elevation_crop <- terra::crop(elevation, expanded_extent) # Crop to the extent of the polygons

  # compute average elevation per polygon
  extracted_values <- terra::extract(elevation_crop, cm_vect, fun = mean, na.rm = TRUE) # Extract raster values for each polygon
  cm_polygons_i$avg_elevation <- extracted_values$elevation # Match the average elevation values back to the original sf object
  
  # # investigate NaNs (Vietnam) - some mining polygons are in the sea
  # plot(elevation_crop)
  # plot(cm_vect, add = TRUE, border = "orange")
  # plot(cm_vect[which(is.nan(extracted_values[, 2])), ], add = TRUE, border = "red")
  
  # # slope
  # if (!exists("slope")) {
  #   slope <- terra::rast("data/slope_aws_terrain_tiles_z6.tif") 
  # }
  # # terra::crs(slope)
  # 
  # # subset to country
  # cm_vect <- terra::vect(cm_polygons_i) # convert sf object to terra-compatible format
  # expanded_extent <- ext(cm_vect) + 0.1 # expand the area of interest just a little more
  # slope_crop <- terra::crop(slope, expanded_extent) # Crop to the extent of the polygons
  # 
  # # compute average slope per polygon
  # extracted_values <- terra::extract(slope_crop, cm_vect, fun = mean, na.rm = TRUE) # Extract raster values for each polygon
  # cm_polygons_i$avg_slope <- extracted_values$slope # Match the average slope values back to the original sf object
  
  # set elevation and slope for missing value (= at sea level) to zero
  cm_polygons_i$avg_elevation[is.nan(cm_polygons_i$avg_elevation)] <- 0
  # cm_polygons_i$avg_slope[is.nan(cm_polygons_i$avg_slope)] <- 0


  # # OLD WDPA ----------------------------------------------------------------
  # 
  # # load WDPA version February 2025 as used in Nat Reviews paper
  # if (!exists("wdpa_clean")) {
  #   wdpa_clean <- sf::st_read("../GlobalMetalMining/data/wdpa/wdpa_clean.gpkg") %>%
  #     dplyr::filter(ISO3 %in% c("VNM", "PHL", "IDN", "THA", "MMR", "MYS", "KHM", "LAO")) # filter to focus countries an potentially relevant neighbours
  #     
  # }
  # 
  # # compute distances (ensures units are in metres)
  # nearest_indices <- st_nearest_feature(cm_polygons_i, wdpa_clean)
  # min_distances <- st_distance(cm_polygons_i, wdpa_clean[nearest_indices, ], by_element = TRUE)
  # 
  # # Convert to numeric
  # cm_polygons_i$dist_PA_m <- as.numeric(min_distances)

  # Key Biodiversity Areas (KBAs) -------------------------------------------

  KBA <- sf::st_read("data/IBAT/SE_Asia_KBAs_2025_April.shp") 
  
  # compute distances (ensures units are in metres)
  nearest_indices <- st_nearest_feature(cm_polygons_i, KBA)
  min_distances <- st_distance(cm_polygons_i, KBA[nearest_indices, ], by_element = TRUE)

  # Convert to numeric
  cm_polygons_i$dist_KBA_m <- as.numeric(min_distances)
  

  # Protected areas ---------------------------------------------------------
  
  PA <- sf::st_read("data/IBAT/SE_Asia_PAs_2025_April.shp") 
  
  # compute distances (ensures units are in metres)
  nearest_indices <- st_nearest_feature(cm_polygons_i, PA)
  min_distances <- st_distance(cm_polygons_i, PA[nearest_indices, ], by_element = TRUE)
  
  # Convert to numeric
  cm_polygons_i$dist_PA_m <- as.numeric(min_distances)
  
  # # # Indigenous peoples territories -------------------------------------------
  # 
  # # IP_point <- sf::st_read("data/LandMark/CommunityLevelData_pt_202411.shp") # No points in focus countries!
  # IP_poly <- sf::st_read("data/LandMark/CommunityLevelData_poly_202411.shp") %>%
  #   dplyr::filter(ISO_Code %in% c("VNM", "PHL", "IDN", "THA", "MMR", "MYS", "KHM", "LAO")) # filter to focus countries an potentially relevant neighbours
  # # No polygons in focus countries, only some in KHM and MYS
  
  # store -------------------------------------------------------------------
  
  store[[i]] <- cm_polygons_i
  
}

cm_polygons_extended <- do.call("rbind", store)
sf::st_write(cm_polygons_extended, "data/coal_mine_polygons_extended.gpkg")
write.csv(cm_polygons_extended %>% sf::st_drop_geometry(), "data/coal_mine_polygons_extended.csv", row.names = FALSE)

# export a shareable, clean version
cm_polygons_extended_clean <- cm_polygons_extended %>%
  dplyr::mutate(centroid = sf::st_centroid(geom)) %>% # Calculate centroids
  dplyr::mutate(centroid_x = sf::st_coordinates(centroid)[,1], # Extract X and Y coordinates
                centroid_y = sf::st_coordinates(centroid)[,2],
                area_mine_ha = area_mine / 10000) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(1, 26, 27, 3, 28, 7, 9:24) %>%
  dplyr::rename("polygon_source" = "data_source")
write.csv(cm_polygons_extended_clean, "data/coal_mine_polygons_extended_clean.csv", row.names = FALSE)

