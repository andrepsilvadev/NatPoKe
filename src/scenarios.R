## Name: libraries ##
## Authors: Andre P. Silva & Jorinde-M. Rieger ##
## Description: Loads land-use scenarios ##

# Import data -------------------------------------------------------
# 2017 baseline
LULC_ESA_2017 <- rast("data/pnas_lulc_dist20240207a/lulc_esa_2017.tif")
# Business as usual (BAU) scenario
BAU_PNAS <- rast("data/pnas_lulc_dist20240207a/pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_BAU.tif")
# Business as usual, Economic Rigidities (BAU rigid) scenario
BAU_rigid <- rast("data/pnas_lulc_dist20240207a/pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_BAU_rigid.tif")
# Payment of Ecosystem Services - global carbon forest (PESGC) scenario
PESGC <- rast("data/pnas_lulc_dist20240207a/pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_PESGC.tif")
# Payment of Ecosystem Services - domestic (local) carbon forest (PESLC) scenario
PESLC <- rast("data/pnas_lulc_dist20240207a/pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_PESLC.tif")
# Subsidy Repurposing through Land Payments (SR_Land) scenario
SR_Land <- rast("data/pnas_lulc_dist20240207a/pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_SR_Land.tif")
# Subsidy Repurposing on Agricultural R&D 20 percent (SR_RnD_20p) scenario
SR_RnD_20p <- rast("data/pnas_lulc_dist20240207a/pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_SR_RnD_20p.tif")
# Subsidy Repurposing through land Payments + Payment of Ecosystem Services - global carbon forest (SR_Land_PESGC) scenario
SR_Land_PESGC <- rast("data/pnas_lulc_dist20240207a/pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_SR_Land_PESGC.tif")
# Subsidy Repurposing through land Payments + Local Payment of Ecosystem Services - domestic (local) carbon forest (SR_PESLC) scenario
SR_PESLC <- rast("data/pnas_lulc_dist20240207a/pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_SR_PESLC.tif")
# Subsidy Repurposing on Agricultural R&D 20 percent + Payment of Ecosystem Services - global carbon forest (SR_RnD_20p_PESGC) scenario
SR_RnD_20p_PESGC <- rast("data/pnas_lulc_dist20240207a/pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_SR_RnD_20p_PESGC.tif")
# Subsidy Repurposing on Agricultural R&D 20 percent + Payment of Ecosystem Services - global carbon forest, 30 % conservation by 2030 (SR_RnD_20p_PESGC_30) scenario
SR_RnD_20p_PESGC_30 <- rast("data/pnas_lulc_dist20240207a/pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_SR_RnD_20p_PESGC_30.tif")
# Subsidy Repurposing on Agricultural R&D 20 percent + LocalPayment of Ecosystem Services - domestic carbon forest (SR_RnD_PESLC) scenario
SR_RnD_PESLC <- rast("data/pnas_lulc_dist20240207a/pnas_lulc_seals5_gtap1_rcp45_ssp2_2030_SR_RnD_PESLC.tif")

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
BAU_rigid_2030_germany_2030_germany <- mask(crop(BAU_rigid,germany_sp),germany_sp)
PESGC_2030_germany <- mask(crop(PESGC,germany_sp),germany_sp)
PESLC_2030_germany <- mask(crop(PESLC,germany_sp),germany_sp)
SR_Land_2030_germany <- mask(crop(SR_Land,germany_sp),germany_sp)
SR_RnD_20p_2030_germany <- mask(crop(SR_RnD_20p,germany_sp),germany_sp)
SR_Land_PESGC_2030_germany <- mask(crop(SR_Land_PESGC,germany_sp),germany_sp)
SR_PESLC_2030_germany <- mask(crop(SR_PESLC,germany_sp),germany_sp)
SR_RnD_20p_PESGC_2030_germany <- mask(crop(SR_RnD_20p_PESGC,germany_sp),germany_sp)
SR_RnD_20p_PESGC_30_2030_germany <- mask(crop(SR_RnD_20p_PESGC_30,germany_sp),germany_sp)
SR_RnD_PESLC_2030_germany <- mask(crop(SR_RnD_PESLC,germany_sp),germany_sp)

# Select Central African Republic boundary
CAR_sf <- ne_countries(scale = "medium", country = "Central African Republic", returnclass = "sf")
# Ensure CRS consistency
CAR_sf <- st_transform(CAR_sf,crs=crs(LULC_ESA_2017))
# Convert the sf to a spatial object
CAR_sp <- vect(CAR_sf)
# Crop and mask the raster to Germany's boundary
LULC_ESA_2017_CAR <- mask(crop(LULC_ESA_2017,CAR_sp),CAR_sp)
BAU_PNAS_2030_CAR <- mask(crop(BAU_PNAS,CAR_sp),CAR_sp)
BAU_rigid_2030_CAR <- mask(crop(BAU_rigid,CAR_sp),CAR_sp)
PESGC_2030_CAR <- mask(crop(PESGC,CAR_sp),CAR_sp)
PESLC_2030_CAR <- mask(crop(PESLC,CAR_sp),CAR_sp)
SR_Land_2030_CAR <- mask(crop(SR_Land,CAR_sp),CAR_sp)
SR_RnD_20p_2030_CAR <- mask(crop(SR_RnD_20p,CAR_sp),CAR_sp)
SR_Land_PESGC_2030_CAR <- mask(crop(SR_Land_PESGC,CAR_sp),CAR_sp)
SR_PESLC_2030_CAR <- mask(crop(SR_PESLC,CAR_sp),CAR_sp)
SR_RnD_20p_PESGC_CAR <- mask(crop(SR_RnD_20p_PESGC,CAR_sp),CAR_sp)
SR_RnD_20p_PESGC_30_2030_CAR <- mask(crop(SR_RnD_20p_PESGC_30,CAR_sp),CAR_sp)
SR_RnD_PESLC_2030_CAR <- mask(crop(SR_RnD_PESLC,CAR_sp),CAR_sp)

