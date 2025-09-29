## Name: Creating SDM-based environmental layer ##
## Author: Inês Silva ##
## Description: stack .tif outputs from SDMs, for multiple species and multiple environmental scenarios ##
## Date: September 10th 2025 ##


source("./src/libraries.R")
source("./src/customFunctions2.R")

##########
# STEP 1 # Define parameters 
##########

target_species <- gsub(" ", ".", target_species)

# biome
if (target_biome == "Tropical & Subtropical Moist Broadleaf Forests") {
  biome <- "tropical"
} else if (target_biome == "Boreal Forests/Taiga") {
  biome <- "boreal"
}

# biomes <- c("tropical", "boreal")  
# 
# # path for each biomes' SDM outputs
# biome_paths <- list(                      
#   tropical = "C:/Users/maria/OneDrive - Universidade de Lisboa/NatPokeTropical",
#   boreal   = "C:/Users/maria/OneDrive - Universidade de Lisboa/NatPokeBoreal")
# 
# # # years to repeat each period
#  rep_scheme <- c("current" = 15, "2030" = 20, "2050" = 50, "2100" = 50)
#  future_scenarios <- c("ssp126", "ssp585")
# # 
# # ##########
# # # STEP 2 # Stack projections per scenario and species. Save that output
# # ##########
# # 
# for (sp in target_species) {
#   for (biom in biomes) {
# 
#     ## NAVIGATION WARNING ##
#     ### Warnings will appear in the end it's totally fine.
#     ### we just intiated with an empty raster in line 48
# 
#     path <- biome_paths[[biom]]
# 
#     # Current raster
#     current_file <- file.path(path, paste0("proj_Current_EM_", sp, "_continuous.tif"))
#     if (!file.exists(current_file)) next
#     r_current <- rast(current_file)
# 
#     for (sc in future_scenarios) {
# 
#       # Future rasters for this scenario
#       future_files <- list.files(
#         path,
#         pattern = paste0("proj_", sc, "_.*_", biom, "_", sp, "_continuous.tif$"),
#         full.names = TRUE
#       )
#       if (length(future_files) == 0) next
# 
#       # start empty stack
#       r_stack <- rast()
#       year_counter <- 2015
#       layer_names <- character()
# 
#       # repeat current raster n times
#       for (i in 1:rep_scheme["current"]) {
#         r_stack <- c(r_stack, r_current)
#         layer_names <- c(layer_names, as.character(year_counter))
#         year_counter <- year_counter + 1
#       }
# 
#       # repeat each future raster n times
#       for (f in future_files) {
#         r <- rast(f)
#         yr <- strsplit(basename(f), "_")[[1]][3]  # 2030, 2050, 2100
#         if (!yr %in% names(rep_scheme)) next
# 
#         for (i in 1:rep_scheme[[yr]]) {
#           r_stack <- c(r_stack, r)
#           layer_names <- c(layer_names, as.character(year_counter))
#           year_counter <- year_counter + 1
#         }
#       }
# 
#       # rename layers for years
#       names(r_stack) <- layer_names
# 
#       # save a stack per species & scenario
#       out_file <- file.path("./data/SDMlandscapes/biome", paste0(sp, "_", biom, "_", sc, ".tif"))
#       writeRaster(r_stack, out_file, overwrite = TRUE)
#       message("Saved: ", out_file, " with ", nlyr(r_stack), " layers")
#       rm(r, r_stack)
#     }
#   }
# }
# plot(r_stack$`2015`)
# plot(r_stack$`2030`)
# plot(r_stack$`2050`)
# plot(r_stack$`2120`)


##########
# STEP 3 # Rescale, crop & mask for each region. Save output again
##########

message("Start Rescaling, Cropping and Masking Output Rasters")

# list of all our target sps, biome & scenario stacked rasters
raster_files <- list.files(
  path = "data/SDMlandscapes/biome/",
  pattern = paste0("^(", paste(target_species, collapse = "|"), ")_", biome, "_", scenario, ".*\\.tif$"),
  full.names = TRUE)

#r_file <- "data/SDMlandscapes/biome/Panthera.onca_tropical_ssp126.tif"
#target_biome <- "Tropical & Subtropical Moist Broadleaf Forests"
#target_region <- "South America"
#target_species<- "Panthera onca"

for (r_file in raster_files) {
  sp_raster <- rast(r_file)
  
  # RESCALE 0-1000 → 0-1
  sp_raster <- sp_raster / 1000
  invisible(gc())
  
  # load biome and continent geometries
  biome_sf <- load_biome(biome_name = target_biome)
  continent_sf <- load_select_continents(continent_names = target_region)
  
  # Crop biome to continents
  sf_use_s2(FALSE)
  target_geom <- crop_biome_to_continent(biome_sf, continent_sf)
  invisible(gc())
  
  # CROP and MASK raster
  sp_raster <- terra::crop(sp_raster, target_geom)
  sp_raster <- terra::mask(sp_raster, target_geom)
  invisible(gc())
  #plot(sp_raster)
  
  # Save processed raster
  output_path <- file.path(dirinput, paste0(tools::file_path_sans_ext(basename(r_file)), "_cropped.tif"))
  writeRaster(sp_raster, output_path, overwrite = TRUE)
  
  message("Processed and saved: ", basename(output_path))
  
  rm(sp_raster)
  gc()
}

##########
# STEP 4 # Reprojected Rasters & save output
##########

# define target CRS to reproject landscapes
targetRegionCRS <- ifelse(target_region == "Europe", "ESRI:54030",
                          ifelse(target_region == "North America", "ESRI:54030",
                                 ifelse(target_region == "Africa", "ESRI:54030",
                                        ifelse(target_region == "South America", "ESRI:54030",
                                               ifelse(target_region == "Asia", "ESRI:54030",
                                                      NA)))))


# list all cropped rasters
landscapes <- list.files(path = dirinput,
                       pattern = ".*_cropped\\.tif$",
                       full.names = TRUE)
gc()
message("Start Reprojecting and Converting to km...")

#landscape <- landscapes[[1]]
for (landscape in landscapes) {
  message("Processing: ", basename(landscape))
  
  ## WARNINGS may appear in the end!! It might be ok, but still check
  ## GDAL couldn’t compute the outer bounds reliably when using the Robinson projection system
  
  # load raster stack
  r <- terra::rast(landscape)

  # Reproject to target CRS
  r_utm <- terra::project(r, targetRegionCRS)
  
  # convert to rasterStack (note: here we changed packages because terra was removing the rasters' values when changing the crs)
  r_raster <- raster::stack(r_utm)
  
  # get original CRS
  orig_crs <- crs(r_utm)
  
  # rescale extent (divide by 1000 to convert meters to kilometers)
  extent(r_raster) <- extent(r_raster) / 1000
  
  # extract species name from filename
  species_name <- gsub("_.*", "", file_path_sans_ext(basename(landscape)))
  
  # select target species
  species_traits <- read.csv(file.path(dirinput,"metaRangeSpeciesDataframe.csv"))
  
  # get an aggregation factor from species traits
  species_fact <- ceiling(species_traits$ModellingRes[species_traits$Species == species_name] / sqrt(species_traits$CellResolution[species_traits$Species == species_name]))
  
  # aggregate raster using terra
  r_agg <- aggregate(r_utm, fact = species_fact, fun = mean, na.rm = TRUE)

  # build an output filename
  output_filename <- gsub("\\.tif$", "_reprojectedKm.tif", landscape)
  
  # save aggregated raster
  writeRaster(r_agg, output_filename, overwrite = TRUE)
  
  # clean memory & save space
  rm(r, r_utm, r_agg)
  gc()
}

message("✅ Environmental layers prepared successfully! ")
