
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(grid)

# ----------------------------------------
# Read & prepare data
# ----------------------------------------

cm_polygons_extended <- sf::st_read("data/coal_mine_polygons_extended.gpkg")

cm_polygons_extended <- cm_polygons_extended %>%
  dplyr::mutate(
    n_true = rowSums(across(starts_with("suitable"), ~ .x)),
    `Suitability` = case_when(
      n_true == 0 ~ "Not suitable",
      n_true > 1  ~ "Multiple use",
      `suitable_solar` ~ "Solar",
      `suitable_wind`  ~ "Wind",
      `suitable_phes`  ~ "PHES"
    )
  ) %>%
  dplyr::select(-n_true) %>%
  mutate(
    Suitability = factor(
      Suitability,
      levels = c(
        "Not suitable",
        "Solar",
        "Wind",
        "PHES",
        "Multiple use"
      )
    )
  )

# ----------------------------------------
# Zoom extent
# ----------------------------------------

zoom_bbox <- tibble::tribble(
  ~region,                    ~x_lim,            ~y_lim,
  "Kalimantan",         c(113.50, 120.20), c(-4.40, 1.50),
  "Sumatra",            c(101.10, 104.90), c(-4.00, -1.30),
  "Sumatra2",           c(100.50, 104.90), c(-4.00, -0.20),
  "N-Vietnam",          c(106.22, 107.70), c(20.6, 21.50),
) %>%
  dplyr::mutate(geometry = lapply(seq_along(region), function(i) sf::st_multipoint(matrix(c(x_lim[[i]], y_lim[[i]]), nrow = 2))),
                group = 1,
                geometry = lapply(geometry, sf::st_bbox),
                geometry = lapply(geometry, sf::st_as_sfc),
                geometry = lapply(geometry, sf::st_geometrycollection),
                geometry = sf::st_sfc(geometry)) %>%
  sf::st_sf() %>%
  sf::st_collection_extract()

# ----------------------------------------
# Suitability colours
# ----------------------------------------

suitability_cols <- c(
  "Not suitable" = "grey85",
  "Solar"        = "#FFD700",
  "Wind"         = "#1E90FF",
  "PHES"         = "#00FFFF",
  "Multiple use" = "#FF4500"
)

# ----------------------------------------
# BASELAYERS
# ----------------------------------------


# Countries (low resolution for clean outlines)
countries <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Populated places (cities)
cities <- rnaturalearth::ne_download(
  scale = "medium",
  type = "populated_places",
  category = "cultural",
  returnclass = "sf"
)

# Indonesia outline
IDN <- rnaturalearth::ne_countries(
  country = "Indonesia",
  scale = "medium",
  returnclass = "sf"
)

# Vietnam outline
VNM <- rnaturalearth::ne_countries(
  country = "Vietnam",
  scale = "medium",
  returnclass = "sf"
)

# ----------------------------------------
# FIGURE 1 – Kalimantan
# ----------------------------------------

lim <- zoom_bbox %>%
  dplyr::filter(region == "Kalimantan")

cities_in_extent <- cities %>%
  st_transform(st_crs(cm_polygons_extended)) %>%
  
  # Extract lon/lat into columns
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  
  # Filter to bounding box
  filter(
    lon >= lim$x_lim[[1]][1],
    lon <= lim$x_lim[[1]][2],
    lat >= lim$y_lim[[1]][1],
    lat <= lim$y_lim[[1]][2]
  )

polygons_to_label <- cm_polygons_extended %>%
  filter(id %in% c("A0190033", "A0208956", "A0189965", "A0190149"))

# Compute centroids for placing labels
polygons_to_label <- polygons_to_label %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_transform(st_crs(cm_polygons_extended)) %>%
  
  # Extract lon/lat into columns
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  
  # Filter to bounding box
  filter(
    lon >= lim$x_lim[[1]][1],
    lon <= lim$x_lim[[1]][2],
    lat >= lim$y_lim[[1]][1],
    lat <= lim$y_lim[[1]][2]
  )

p <- ggplot() +
  
  # Baselayer: country borders
  geom_sf(data = countries, fill = "grey98", colour = "grey70", size = 0.2) +
  
  # Main cities (only within the map extent)
  geom_sf(
    data = cities,
    colour = "black",
    size = 1.2
  ) +
  
  # Your polygons
  geom_sf(
    data = cm_polygons_extended %>% dplyr::filter(! Suitability %in% c("Wind")),
    aes(fill = Suitability),
    lwd = 0.15
  ) +
  
  # Extent
  coord_sf(
    xlim = lim$x_lim[[1]],
    ylim = lim$y_lim[[1]],
    expand = 0
  ) +
  
  # Suitability palette
  scale_fill_manual(
    values = c(
      "Not suitable" = "grey85",
      "Solar" = "#FFD700",
      "Wind" = "#1E90FF",
      "PHES" = "#00FFFF",
      "Multiple use" = "#FF4500"
    ),
    name = "Coal mining area"
  ) +
  
  # Legend inside
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.85),
    legend.key.size = unit(4, "mm"),
    legend.text = element_text(size = 9),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.7
  ) +
  
  ggrepel::geom_text_repel(
    data = cities_in_extent,
    aes(x = lon, y = lat, label = NAME),
    size = 4,
    nudge_y = -0.1,
    colour = "black",       # text colour
    segment.color = NA      # remove connecting line if you want
  ) +
  
  # Add labels to the map
  ggrepel::geom_text_repel(
    data = polygons_to_label,
    aes(x = lon, y = lat, label = id),
    size = 3,
    nudge_y = 0.1,        # small vertical nudge if needed
    max.overlaps = Inf
  ) +
  
  # North arrow
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(6, "mm"),
    pad_y = unit(6, "mm"),
    style = north_arrow_fancy_orienteering
  )


# add inset map
zoom_poly <- sf::st_as_sfc(
  st_bbox(
    c(
      xmin = lim$x_lim[[1]][1],
      xmax = lim$x_lim[[1]][2],
      ymin = lim$y_lim[[1]][1],
      ymax = lim$y_lim[[1]][2]
    ),
    crs = st_crs(IDN)
  )
)
inset_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = IDN, fill = "grey95", colour = "grey50", linewidth = 0.3) +
  ggplot2::geom_sf(data = zoom_poly, fill = NA, colour = "red", linewidth = 0.8) +
  ggplot2::theme_void() +
  ggplot2::theme(
    panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.4)
  )

inset_grob <- ggplotGrob(inset_map) # convert inset map to a grob

# p_final <- p +
#   ggplot2::annotation_custom(
#     grob = inset_grob,
#     xmin = lim$x_lim[[1]][1] + 1.2,    # adjust horizontal placement
#     xmax = lim$x_lim[[1]][1] + 3.2,    # adjust width of inset
#     ymin = lim$y_lim[[1]][1] + 4.5,    # adjust vertical placement
#     ymax = lim$y_lim[[1]][1] + 6.5     # adjust height of inset
#   )

# p_final <- p +
#   ggplot2::annotation_custom(
#     grob = inset_grob,
#     xmin = lim$x_lim[[1]][1] + 3.2,    # adjust horizontal placement
#     xmax = lim$x_lim[[1]][1] + 5.2,    # adjust width of inset
#     ymin = lim$y_lim[[1]][1] + 0.01,    # adjust vertical placement
#     ymax = lim$y_lim[[1]][1] + 1.01     # adjust height of inset
#   )

p_final <- p +
  ggplot2::annotation_custom(
    grob = inset_grob,
    xmin = lim$x_lim[[1]][1] + 4.5,    # adjust horizontal placement
    xmax = lim$x_lim[[1]][1] + 6.7,    # adjust width of inset
    ymin = lim$y_lim[[1]][1] + 0.4,    # adjust vertical placement
    ymax = lim$y_lim[[1]][1] + 1.8     # adjust height of inset
  )

ggsave(
  filename = "figures/figure-1_map_kalimantan.png",
  plot = p_final,
  width = 5,         # in inches
  height = 5 * diff(lim$y_lim[[1]]) / diff(lim$x_lim[[1]]),  # keep aspect ratio
  dpi = 300
)


# ----------------------------------------
# FIGURE 2 – Sumatra
# ----------------------------------------

lim <- zoom_bbox %>%
  dplyr::filter(region == "Sumatra")

cities_in_extent <- cities %>%
  st_transform(st_crs(cm_polygons_extended)) %>%
  
  # Extract lon/lat into columns
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  
  # Filter to bounding box
  filter(
    lon >= lim$x_lim[[1]][1],
    lon <= lim$x_lim[[1]][2],
    lat >= lim$y_lim[[1]][1],
    lat <= lim$y_lim[[1]][2]
  )

polygons_to_label <- cm_polygons_extended %>%
  filter(id %in% c("A0190033", "A0208956", "A0189965", "A0190149"))

# Compute centroids for placing labels
polygons_to_label <- polygons_to_label %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_transform(st_crs(cm_polygons_extended)) %>%
  
  # Extract lon/lat into columns
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  
  # Filter to bounding box
  filter(
    lon >= lim$x_lim[[1]][1],
    lon <= lim$x_lim[[1]][2],
    lat >= lim$y_lim[[1]][1],
    lat <= lim$y_lim[[1]][2]
  )

p <- ggplot() +
  
  # Baselayer: country borders
  geom_sf(data = countries, fill = "grey98", colour = "grey70", size = 0.2) +
  
  # Main cities (only within the map extent)
  geom_sf(
    data = cities,
    colour = "black",
    size = 1.2
  ) +
  
  # Your polygons
  geom_sf(
    data = cm_polygons_extended %>% dplyr::filter(! Suitability %in% c("Wind")),
    aes(fill = Suitability),
    lwd = 0.15
  ) +
  
  # Extent
  coord_sf(
    xlim = lim$x_lim[[1]],
    ylim = lim$y_lim[[1]],
    expand = 0
  ) +
  
  # scale_x_continuous(
  #   breaks = seq(
  #     floor(lim$x_lim[[1]][1]), 
  #     ceiling(lim$x_lim[[1]][2]), 
  #     by = 1
  #   ),
  #   name = "Longitude"
  # ) +
  
  # Suitability palette
  scale_fill_manual(
    values = c(
      "Not suitable" = "grey85",
      "Solar" = "#FFD700",
      "Wind" = "#1E90FF",
      "PHES" = "#00FFFF",
      "Multiple use" = "#FF4500"
    ),
    name = "Coal mining area"
  ) +
  
  # Legend inside
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.85),
    legend.key.size = unit(4, "mm"),
    legend.text = element_text(size = 9),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.7
  ) +
  
  ggrepel::geom_text_repel(
    data = cities_in_extent,
    aes(x = lon, y = lat, label = NAME),
    size = 4,
    nudge_y = -0.1,
    colour = "black",       # text colour
    segment.color = NA      # remove connecting line if you want
  ) +
  
  # Add labels to the map
  ggrepel::geom_text_repel(
    data = polygons_to_label,
    aes(x = lon, y = lat, label = id),
    size = 3,
    nudge_y = 0.2,        # small vertical nudge if needed
    max.overlaps = Inf
  ) +
  
  # North arrow
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(6, "mm"),
    pad_y = unit(6, "mm"),
    style = north_arrow_fancy_orienteering
  )

# add inset map
zoom_poly <- sf::st_as_sfc(
  st_bbox(
    c(
      xmin = lim$x_lim[[1]][1],
      xmax = lim$x_lim[[1]][2],
      ymin = lim$y_lim[[1]][1],
      ymax = lim$y_lim[[1]][2]
    ),
    crs = st_crs(IDN)
  )
)
inset_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = IDN, fill = "grey95", colour = "grey50", linewidth = 0.3) +
  ggplot2::geom_sf(data = zoom_poly, fill = NA, colour = "red", linewidth = 0.8) +
  ggplot2::theme_void() +
  ggplot2::theme(
    panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.4)
  )

inset_grob <- ggplotGrob(inset_map) # convert inset map to a grob

p_final <- p +
  ggplot2::annotation_custom(
    grob = inset_grob,
    xmin = lim$x_lim[[1]][1] + 0.05,    # adjust horizontal placement
    xmax = lim$x_lim[[1]][1] + 1.15,    # adjust width of inset
    ymin = lim$y_lim[[1]][1] + 0.5,    # adjust vertical placement
    ymax = lim$y_lim[[1]][1] + 1.7     # adjust height of inset
  )

ggsave(
  filename = "figures/figure-2_map_sumatra.png",
  plot = p_final,
  width = 5,         # in inches
  height = 5 * diff(lim$y_lim[[1]]) / diff(lim$x_lim[[1]]),  # keep aspect ratio
  dpi = 300
)

# ----------------------------------------
# ALTERNATIVE FIGURE 2 – Sumatra large
# ----------------------------------------

lim <- zoom_bbox %>%
  dplyr::filter(region == "Sumatra2")

cities_in_extent <- cities %>%
  st_transform(st_crs(cm_polygons_extended)) %>%
  
  # Extract lon/lat into columns
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  
  # Filter to bounding box
  filter(
    lon >= lim$x_lim[[1]][1],
    lon <= lim$x_lim[[1]][2],
    lat >= lim$y_lim[[1]][1],
    lat <= lim$y_lim[[1]][2]
  )

polygons_to_label <- cm_polygons_extended %>%
  filter(id %in% c("A0190149", "A0190033", "A0208956", "A0189965", "A0190149"))

# Compute centroids for placing labels
polygons_to_label <- polygons_to_label %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_transform(st_crs(cm_polygons_extended)) %>%
  
  # Extract lon/lat into columns
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  
  # Filter to bounding box
  filter(
    lon >= lim$x_lim[[1]][1],
    lon <= lim$x_lim[[1]][2],
    lat >= lim$y_lim[[1]][1],
    lat <= lim$y_lim[[1]][2]
  )

p <- ggplot() +
  
  # Baselayer: country borders
  geom_sf(data = countries, fill = "grey98", colour = "grey70", size = 0.2) +
  
  # Main cities (only within the map extent)
  geom_sf(
    data = cities,
    colour = "black",
    size = 1.2
  ) +
  
  # Your polygons
  geom_sf(
    data = cm_polygons_extended %>% dplyr::filter(! Suitability %in% c("Wind")),
    aes(fill = Suitability),
    lwd = 0.15
  ) +
  
  # Extent
  coord_sf(
    xlim = lim$x_lim[[1]],
    ylim = lim$y_lim[[1]],
    expand = 0
  ) +
  
  # scale_x_continuous(
  #   breaks = seq(
  #     floor(lim$x_lim[[1]][1]), 
  #     ceiling(lim$x_lim[[1]][2]), 
  #     by = 1
  #   ),
  #   name = "Longitude"
  # ) +
  
  # Suitability palette
  scale_fill_manual(
    values = c(
      "Not suitable" = "grey85",
      "Solar" = "#FFD700",
      "Wind" = "#1E90FF",
      "PHES" = "#00FFFF",
      "Multiple use" = "#FF4500"
    ),
    name = "Coal mining area"
  ) +
  
  # Legend inside
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.85),
    legend.key.size = unit(4, "mm"),
    legend.text = element_text(size = 9),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.7
  ) +
  
  ggrepel::geom_text_repel(
    data = cities_in_extent,
    aes(x = lon, y = lat, label = NAME),
    size = 4,
    nudge_y = -0.1,
    colour = "black",       # text colour
    segment.color = NA      # remove connecting line if you want
  ) +
  
  # Add labels to the map
  ggrepel::geom_text_repel(
    data = polygons_to_label,
    aes(x = lon, y = lat, label = id),
    size = 3,
    nudge_y = 0.2,        # small vertical nudge if needed
    max.overlaps = Inf
  ) +
  
  # North arrow
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(6, "mm"),
    pad_y = unit(6, "mm"),
    style = north_arrow_fancy_orienteering
  )

# add inset map
zoom_poly <- sf::st_as_sfc(
  st_bbox(
    c(
      xmin = lim$x_lim[[1]][1],
      xmax = lim$x_lim[[1]][2],
      ymin = lim$y_lim[[1]][1],
      ymax = lim$y_lim[[1]][2]
    ),
    crs = st_crs(IDN)
  )
)
inset_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = IDN, fill = "grey95", colour = "grey50", linewidth = 0.3) +
  ggplot2::geom_sf(data = zoom_poly, fill = NA, colour = "red", linewidth = 0.8) +
  ggplot2::theme_void() +
  ggplot2::theme(
    panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.4)
  )

inset_grob <- ggplotGrob(inset_map) # convert inset map to a grob

p_final <- p +
  ggplot2::annotation_custom(
    grob = inset_grob,
    xmin = lim$x_lim[[1]][1] + 0.05,    # adjust horizontal placement
    xmax = lim$x_lim[[1]][1] + 1.35,    # adjust width of inset
    ymin = lim$y_lim[[1]][1] + 0.5,    # adjust vertical placement
    ymax = lim$y_lim[[1]][1] + 1.9     # adjust height of inset
  )

ggsave(
  filename = "figures/figure-2_map_sumatra_alternative.png",
  plot = p_final,
  width = 5,         # in inches
  height = 5 * diff(lim$y_lim[[1]]) / diff(lim$x_lim[[1]]),  # keep aspect ratio
  dpi = 300
)


# ----------------------------------------
# FIGURE 3 – Northern Vietnam
# ----------------------------------------

lim <- zoom_bbox %>%
  dplyr::filter(region == "N-Vietnam")

cities_in_extent <- cities %>%
  st_transform(st_crs(cm_polygons_extended)) %>%
  
  # Extract lon/lat into columns
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  
  # Filter to bounding box
  filter(
    lon >= lim$x_lim[[1]][1],
    lon <= lim$x_lim[[1]][2],
    lat >= lim$y_lim[[1]][1],
    lat <= lim$y_lim[[1]][2]
  )

polygons_to_label <- cm_polygons_extended %>%
  filter(id %in% c("A0191997", "A0191979", "A0191970"))

# Compute centroids for placing labels
polygons_to_label <- polygons_to_label %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_transform(st_crs(cm_polygons_extended)) %>%
  
  # Extract lon/lat into columns
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  
  # Filter to bounding box
  filter(
    lon >= lim$x_lim[[1]][1],
    lon <= lim$x_lim[[1]][2],
    lat >= lim$y_lim[[1]][1],
    lat <= lim$y_lim[[1]][2]
  )

p <- ggplot() +
  
  # Baselayer: country borders
  geom_sf(data = countries, fill = "grey98", colour = "grey70", size = 0.2) +
  
  # Main cities (only within the map extent)
  geom_sf(
    data = cities,
    colour = "black",
    size = 1.2
  ) +
  
  # Your polygons
  geom_sf(
    data = cm_polygons_extended %>% dplyr::filter(! Suitability %in% c("PHES")),
    aes(fill = Suitability),
    lwd = 0.15
  ) +
  
  # Extent
  coord_sf(
    xlim = lim$x_lim[[1]],
    ylim = lim$y_lim[[1]],
    expand = 0
  ) +
  
  # Suitability palette
  scale_fill_manual(
    values = c(
      "Not suitable" = "grey85",
      "Solar" = "#FFD700",
      "Wind" = "#1E90FF",
      "PHES" = "#00FFFF",
      "Multiple use" = "#FF4500"
    ),
    name = "Coal mining area"
  ) +
  
  # Legend inside
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.82),
    legend.key.size = unit(4, "mm"),
    legend.text = element_text(size = 9),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.7
  ) +
  
  ggrepel::geom_text_repel(
    data = cities_in_extent,
    aes(x = lon, y = lat, label = NAME),
    size = 4,
    nudge_y = -0.02,
    colour = "black",       # text colour
    segment.color = NA      # remove connecting line if you want
  ) +
  
  # Add labels to the map
  ggrepel::geom_text_repel(
    data = polygons_to_label,
    aes(x = lon, y = lat, label = id),
    size = 3,
    nudge_y = 0.05,        # small vertical nudge if needed
    max.overlaps = Inf
  ) +
  
  # North arrow
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(6, "mm"),
    pad_y = unit(6, "mm"),
    style = north_arrow_fancy_orienteering
  )

# add inset map
zoom_poly <- sf::st_as_sfc(
  st_bbox(
    c(
      xmin = lim$x_lim[[1]][1],
      xmax = lim$x_lim[[1]][2],
      ymin = lim$y_lim[[1]][1],
      ymax = lim$y_lim[[1]][2]
    ),
    crs = st_crs(VNM)
  )
)
inset_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = VNM, fill = "grey95", colour = "grey50", linewidth = 0.3) +
  ggplot2::geom_sf(data = zoom_poly, fill = NA, colour = "red", linewidth = 0.8) +
  ggplot2::theme_void() +
  ggplot2::theme(
    panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.4)
  )

inset_grob <- ggplotGrob(inset_map) # convert inset map to a grob

p_final <- p +
  ggplot2::annotation_custom(
    grob = inset_grob,
    xmin = lim$x_lim[[1]][1] + 1.2,    # adjust horizontal placement
    xmax = lim$x_lim[[1]][1] + 1.6,    # adjust width of inset
    ymin = lim$y_lim[[1]][1] + 0.02,    # adjust vertical placement
    ymax = lim$y_lim[[1]][1] + 0.42     # adjust height of inset
  )

ggsave(
  filename = "figures/figure-3_map_vietnam.png",
  plot = p_final,
  width = 5,         # in inches
  height = 5 * diff(lim$y_lim[[1]]) / diff(lim$x_lim[[1]]),  # keep aspect ratio
  dpi = 300
)

