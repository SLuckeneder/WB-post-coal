
library(terra)

# DNI - Direct Normal Irradiation -----------------------------------------

DNI <- terra::rast("data/GlobalSolarAtlas/World_DNI_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF/DNI.tif")
# plot(DNI)

# GHI - Global Horizontal Irradiation -------------------------------------

GHI <- terra::rast("data/GlobalSolarAtlas/World_GHI_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF/GHI.tif")
# plot(GHI)

# DIF - Diffuse Horizontal Irradiation ------------------------------------

DIF <- terra::rast("data/GlobalSolarAtlas/World_DIF_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF/DIF.tif")
# plot(DIF)

# PVOUT - PV power output -------------------------------------------------

PVOUT <- terra::rast("data/GlobalSolarAtlas/World_PVOUT_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF/PVOUT.tif")
# plot(PVOUT)

# TEMP - Air Temperature at 2 m above ground level  -----------------------

TEMP <- terra::rast("data/GlobalSolarAtlas/World_TEMP_GISdata_LTAy_GlobalSolarAtlas-v2_GEOTIFF/TEMP.tif")
# plot(TEMP)

