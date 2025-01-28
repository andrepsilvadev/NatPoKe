## Name: libraries ##
## Authors: Andre P. Silva ##
## Description: Loads land-use scenarios ##

# Import data -------------------------------------------------------
# 2017 baseline
LULC_ESA_2017 <- rast("data/pnas_lulc_dist20240207a/lulc_esa_2017.tif")
# Business as usual (BAU) scenario
BAU_PNAS <- rast("data/pnas_lulc_dist20240207a/pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_BAU.tif")

# for testing only -------------------------------------------------------
# Select german boundary
germany_sf <- ne_countries(scale = "medium", country = "Germany", returnclass = "sf")
# Ensure CRS consistency
germany_sf <- st_transform(germany_sf,crs=crs(LULC_ESA_2017))
# Convert the sf to a spatial object
germany_sp <- vect(germany_sf)
# Crop and mask the raster to Germany's boundary
LULC_ESA_2017_germany <- mask(crop(LULC_ESA_2017,germany_sp),germany_sp)
BAU_PNAS_2030_germany <- mask(crop(BAU_PNAS,germany_sp),germany_sp)

