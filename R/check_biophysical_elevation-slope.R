


library(terra)    # For raster processing
library(elevatr)  # For downloading elevation data
library(sf)       # For spatial objects
library(spData)

# set elevatr zoom for resolution
zz <- 6

# Define global range
range <- spData::world

# Download elevation data using the bounding box
elevation_raster <- get_elev_raster(range, z = zz, src = "aws")

# Convert world map to an sf object for masking
world_sf <- st_as_sf(range)

# Convert world map to raster (matching the resolution of the elevation raster)
world_raster <- raster::rasterize(world_sf, elevation_raster, field = 1)

# Create a mask: Land areas will be kept, ocean areas (NA) will be excluded
masked_elevation_raster <- mask(elevation_raster, world_raster)

# Convert to terra format for processing
elevation <- rast(masked_elevation_raster)

# Compute slope (in degrees)
slope <- terrain(elevation, v = "slope", unit = "degrees")

# # Plot to verify
# plot(elevation, main = "Global Elevation")
# plot(slope, main = "Global Slope")

# Save the files locally
writeRaster(masked_elevation_raster, paste0("data/elevation_aws_terrain_tiles_z", zz, ".tif"), overwrite = TRUE)
writeRaster(slope, paste0("data/slope_aws_terrain_tiles_z", zz, ".tif"), overwrite = TRUE)



