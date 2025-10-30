

library(sf)
library(dplyr)
library(tidyr)
library(rnaturalearth)
library(mapview)
library(leaflet)
library(htmlwidgets)

# Get the country borders
countries <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>%
  filter(admin %in% c("Indonesia", "Vietnam", "Thailand", "Philippines"))

cm_polygons_extended <- sf::st_read("data/coal_mine_polygons_extended.gpkg")

# Convert area_mine  to hectares
cm_polygons_extended$area_mine_ha <- cm_polygons_extended$area_mine / 10000

# tidy data and rename variables
cm_polygons_extended <- cm_polygons_extended %>%
  dplyr::select(1, 7, 26, 9:12, 14, 16, 18, 20, 21, 23, 24) %>%
  dplyr::rename("ID" = "id", 
                "Country" = "country_name",
                "Polygon area (ha)" = "area_mine_ha",
                "Distance to primary road (m)" = "dist_road_m",
                "Distance to power line (m)" = "dist_power_line_m",
                "Distance to substation (m)" = "dist_substation_m",
                "Distance to airport (m)" = "dist_airport_m",
                "Distance to urban area (m)" = "dist_urban_m",
                "Avg. Global Horizontal Irradiation (GHI) (kWh/m²/day)" = "avg_GHI",
                "Avg. wind speed (m/s at 50m)" = "avg_wind_speed_ms_50m",
                "Distance to PHES upper (m)" = "dist_PHES_upper_m",
                "Distance to PHES lower (m)" = "dist_PHES_lower_m",
                "Distance to Key Biodiversity Area (m)" = "dist_KBA_m",
                "Distance to Protected Area (m)" = "dist_PA_m") %>%
  dplyr::mutate(
    # Round all numeric columns except ID, Country, and the two special ones
    dplyr::across(
      .cols = where(is.numeric) & 
        !dplyr::any_of(c("Polygon area (ha)", 
                         "Avg. Global Horizontal Irradiation (GHI) (kWh/m²/day)", 
                         "Avg. wind speed (m/s at 50m)")),
      .fns = ~ round(., 0)
    ),
    # Round the two selected columns to two decimals
    dplyr::across(
      .cols = c("Polygon area (ha)", "Avg. Global Horizontal Irradiation (GHI) (kWh/m²/day)", "Avg. wind speed (m/s at 50m)"),
      .fns = ~ round(., 2)
    )
  )


# Prep other spatial data
# osm roads (merge motorway and primary roads)
motorway_VNM <- st_read("data/osm/Vietnam.gpkg", layer = "motorway")
primary_VNM <- st_read("data/osm/Vietnam.gpkg", layer = "primary")
motorway_THA <- st_read("data/osm/Thailand.gpkg", layer = "motorway")
primary_THA <- st_read("data/osm/Thailand.gpkg", layer = "primary")
motorway_PHL <- st_read("data/osm/Philippines.gpkg", layer = "motorway")
primary_PHL <- st_read("data/osm/Philippines.gpkg", layer = "primary")
motorway_IDN <- st_read("data/osm/Indonesia.gpkg", layer = "motorway")
primary_IDN <- st_read("data/osm/Indonesia.gpkg", layer = "primary")

roads_combined <- bind_rows(
  motorway_VNM, primary_VNM,
  motorway_THA, primary_THA,
  motorway_PHL, primary_PHL,
  motorway_IDN, primary_IDN
)
rm(motorway_VNM, primary_VNM, motorway_THA, primary_THA, motorway_PHL, primary_PHL, motorway_IDN, primary_IDN); gc()


# osm energy infrastructure data
power_line_VNM <- st_read("data/osm/Vietnam.gpkg", layer = "power_line")
substation_VNM <- st_read("data/osm/Vietnam.gpkg", layer = "substation")
power_line_THA <- st_read("data/osm/Thailand.gpkg", layer = "power_line")
substation_THA <- st_read("data/osm/Thailand.gpkg", layer = "substation")
power_line_PHL <- st_read("data/osm/Philippines.gpkg", layer = "power_line")
substation_PHL <- st_read("data/osm/Philippines.gpkg", layer = "substation")
power_line_IDN <- st_read("data/osm/Indonesia.gpkg", layer = "power_line")
substation_IDN <- st_read("data/osm/Indonesia.gpkg", layer = "substation")

power_line <- bind_rows(
  power_line_VNM, 
  power_line_THA, 
  power_line_PHL, 
  power_line_IDN, 
)
rm(power_line_VNM, power_line_THA, power_line_PHL, power_line_IDN); gc()

substation <- bind_rows(
  substation_VNM, 
  substation_THA, 
  substation_PHL, 
  substation_IDN, 
)
rm(substation_VNM, substation_THA, substation_PHL, substation_IDN); gc()

# urban areas
urban <- sf::st_read("data/urban_areas_full.shp", quiet = TRUE)
urban <- sf::st_transform(urban, sf::st_crs(countries))
urban <- urban %>% sf::st_filter(countries) %>% dplyr::select(geometry)

# mapview
mapviewOptions(homebutton = FALSE)  # disables "zoom to layer" control

m <- mapview(
  countries,
  color = "white",
  col.regions = "white",
  alpha.regions = 0.05,
  layer.name = "Focus Countries",
  label = NA,
  popup = FALSE
) +
  mapview(
    urban,
    color = "lightgrey",
    col.regions = "lightgrey",
    alpha.regions = 0.2,
    layer.name = "Urban Area",
    label = NA,
    popup = FALSE,
    hide = TRUE
  ) +
  mapview(
    roads_combined,
    color = "#1f78b4",
    col.regions = "#1f78b4",
    alpha.regions = 0.2,
    layer.name = "Primary Road",
    popup = FALSE,
    hide = TRUE
  ) +
  mapview(
    power_line,
    color = "#e6ac00",
    col.regions = "#e6ac00",
    alpha.regions = 0.2,
    layer.name = "Power Line",
    label = NA,
    popup = FALSE,
    hide = TRUE
  ) +
  mapview(
    substation,
    color = "#009e73",
    col.regions = "#009e73",
    alpha.regions = 0.1,
    layer.name = "Substation",
    label = NA,
    popup = FALSE,
    hide = TRUE,
    cex = 2
  ) +
  mapview(
    cm_polygons_extended,
    color = "#FF00FF",
    col.regions = "#FF00FF",
    alpha.regions = 0.5,
    layer.name = "Coal Mining",
    label =  cm_polygons_extended$ID
  ) 


leaflet_map <- m@map

leaflet_map <- leaflet_map %>%
  addControl(
    html = "<button onclick=\"alert('Sources:\\n- Countries: Natural Earth\\n- Coal mining areas: WU Vienna, Maus et al. (2022), Tang and Werner (2023)\\n- Roads, power lines, substations, airports: OpenStreetMap (OSM)\\n- Urban areas: Daylight Map Distribution (OSM-based)\\n- Global Horizontal Irradiation: Global Solar Atlas\\n- Wind speed: Global Wind Atlas\\n- Potential pumped hydro energy storage (PHES): Weber et al. (2024)\\n- Key Biodiversity Areas: SOURCE MISSING\\n- Protected Areas: SOURCE MISSING');\">Sources</button>",
    position = "bottomright"
  )

# save as html
saveWidget(leaflet_map, "docs/coal_mining_map.html", selfcontained = FALSE)

# # self contained html
# saveWidget(leaflet_map, "docs/coal_mining_map_self.html", selfcontained = TRUE)



