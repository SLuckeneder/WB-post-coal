
# Mining sites with Pumped hydro energy storage (PHES) potential
# Weber et al 2024
# https://doi.org/10.1016/j.renene.2024.120113

library(dplyr)
library(readxl)
library(sf)
library(ggplot2)


# read data
dat_raw <- readxl::read_xlsx("data/weber-et-al-2024-PHES.xlsx", sheet = 3)
dat_sub <- dat_raw %>%
  dplyr::filter(`Non-overlapping` == 1) %>%
  dplyr::select("Unique Identifier", "Pair Identifier", "Class", "Energy (GWh)", "Upper latitude", "Upper longitude", "Lower latitude", "Lower longitude")
  

# make spatial
dat_sf <- dat_sub %>%
  st_as_sf(coords = c("Upper longitude", "Upper latitude"), crs = 4326)  # WGS 84 coordinate reference system

# get world map
world <- spData::world

# draw map
xlim <- c(-180, 180)  # Covers entire longitude range
ylim <- c(-60, 85)    # Crops south of Argentina (-60) and north of Norway (85)

m <- ggplot2::ggplot(data = world) +
  ggplot2::geom_sf(fill = "grey90", colour = "white") +  # World map background
  ggplot2::geom_sf(data = dat_sf, aes(colour = Class)) +
  ggplot2::scale_color_viridis_d(option = "rocket", begin = 0.3, end = 0.9) +  
  ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +  # Crop to region
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "bottom", 
                 legend.justification = "center",
                 legend.box.margin=unit(c(-0.5,0,0,0), "cm"))


ggplot2::ggsave("test_PHES.pdf",
                plot = m, device = "pdf",
                path = paste0("./output/"),
                scale = 1, width = 300, height = 150, units = "mm")



