## Name: Creating SDM-based environmental layer ##
## Author: Inês Silva & Sarina ##
## Description: stack .tif outputs from SDMs, for multiple species and multiple environmental scenarios ##
## Date: December 27th 2025 ##

source("./src/libraries.R")
source("./src/customFunctions2.R")

##########
# STEP 1 # Define parameters 
##########

# target_species <- c( "Dama dama", "Alces alces", "Bison bonasus", "Lynx lynx")
# 
# target_region <- "Europe+Asia"
# target_biome <- "Boreal Forests/Taiga"
# future_scenario <- "ssp126"
target_species <- gsub(" ", ".", target_species)
#target_biome <- "Boreal Forests/Taiga"

# biome
if (target_biome == "Tropical & Subtropical Moist Broadleaf Forests") {
  biome <- "tropical"
} else if (target_biome == "Boreal Forests/Taiga") {
  biome <- "boreal"
}

# correction for region
if (target_region == "Europe+Asia") {
  target_region <- "Europe"
  }

#biomes <- c("tropical", "boreal")  
basePathSDM <- "./data/SDMlandscapes_October25"
# path for each biomes' SDM outputs
biome_paths <- list(                      
  tropical = "./data/NatPoke_October25_tropical",
  boreal   = "./data/NatPoke_October25_boreal"
)

##########
# STEP 2 # Stack projections per species & Interpolate (Save intermeadiate output)
##########

for (sp in target_species) {
  
  path <- biome_paths[[biome]]
  # fetch current raster for sp in biome
  current_file <- file.path(path, paste0("proj_Current_EM_", sp, "_continuous.tif"))
  if (!file.exists(current_file)) {
    message("SKIPPING ", sp, ": current raster not found")
    next
  }
  
  # fetch future rasters for sp in biome
  future_files <- list.files(
    path,
    pattern = paste0("proj_", future_scenario, "_.*_", biome, "_", sp, "_continuous.tif$"),
    full.names = TRUE
  )
  if (length(future_files) == 0) {
    message("SKIPPING ", sp, ": no future rasters found for scenario ", future_scenario)
    next
  }
  
  message("Processing ", sp)
  message("Placing SDM scenarios in correct layer")
  # build raster stack
  interpolated_raster <- rast(current_file) # template
  # set SDM scenarios in the correct layer order
  nlyr(interpolated_raster) <- 111
  interpolated_raster[[1]] <- rast(current_file)
  interpolated_raster[[15]] <- rast(future_files[[1]])
  interpolated_raster[[35]] <- rast(future_files[[2]])
  interpolated_raster[[85]] <- rast(future_files[[3]])
  
  message("Interpolating raster layers across years")
  
  # do linear interpolation between them
  r_interp <- approximate(interpolated_raster, method = "linear")
  # find last non-NA layer (2100)
  last_known <- max(which(!is.na(global(interpolated_raster, "sum", na.rm=TRUE)[,1])))
  # fill remaining layers by repeating the last SDM scenario
  r_interp[[ (last_known + 1):nlyr(r_interp) ]] <- r_interp[[ last_known ]]
  # fix names to match years
  names(r_interp) <- as.character(2015:2125)
  #plot(r_interp$`2125`)
  
  out_dir <- file.path(basePathSDM, "biome")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, paste0(sp, "_", biome, "_", future_scenario, ".tif"))
  # write biome-wide raster
  writeRaster(r_interp, out_file, overwrite = TRUE)
  message("Saved interpolated raster: ", out_file)
  
  # remove unecessary objects
  rm(r, r_interp, interpolated_raster, current_file, future_files, path, last_known)
  gc()
}

##########
# STEP 3 # Rescale, crop & mask for each region. Save output again
##########

message("Start Rescaling, Cropping and Masking Output Rasters")

# # list of all our target sps, biome & scenario stacked rasters
# raster_files <- list.files(
#   path = "./data/SDMlandscapes_October25/biome",
#   pattern = paste0("^(", paste(gsub(" ", ".", target_species), collapse = "|"), ")_", biome, "_", scenario, ".*\\.tif$"),
#   full.names = TRUE)

output_cropped <- file.path(basePathSDM, "biome_cropped")

for (sp in target_species) {
  
  # expected input raster
  raster_file <- file.path(
    out_dir,
    paste0(sp, "_", biome, "_", future_scenario, ".tif")
  )
  
  # skip if file does not exist
  if (!file.exists(raster_file)) next
  
  # load raster
  sp_raster <- rast(raster_file)
  
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
  
  # output filename
  output_path <- file.path(
    dirinput,
    paste0(sp, "_", biome, "_", future_scenario, "_cropped.tif")
  )
  
  # save raster
  writeRaster(sp_raster, output_path, overwrite = TRUE)
  message("Processed and saved: ", basename(output_path))
  
  # cleanup
  rm(raster_file, biome_sf, continent_sf, target_geom, sp_raster, output_path)
  invisible(gc())
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
invisible(gc())
message("Start Reprojecting and Converting to km...")

for (landscape in landscapes) {
  message("Processing: ", basename(landscape))
  
  ## WARNINGS may appear in the end!! It might be ok, but still check
  ## GDAL couldn’t compute the outer bounds reliably when using the Robinson projection system
  
  # load raster stack
  r <- terra::rast(landscape)
  #plot(r)
  
  # Reproject to target CRS
  r_utm <- terra::project(r, targetRegionCRS)
  
  # crop and mask - to ensure no dead pixels in the corners
  
  # load biome and continent geometries
  biome_sf <- load_biome(biome_name = target_biome)
  continent_sf <- load_select_continents(continent_names = target_region)
  # crop biome to continents
  sf_use_s2(FALSE)
  target_geom <- crop_biome_to_continent(biome_sf, continent_sf)
  # project target region to ronbinson
  target_geom_robinson <- st_transform(target_geom, crs = "ESRI:54030")
  invisible(gc())
  # CROP and MASK raster
  r_utm_masked <- terra::crop(r_utm, target_geom_robinson)
  r_utm_masked <- terra::mask(r_utm_masked, target_geom_robinson)
  invisible(gc(rm(biome_sf, continent_sf, target_geom, target_geom_robinson)))
  
  # convert to rasterStack (note: here we changed packages because terra was removing the rasters' values when changing the crs)
  r_raster <- raster::stack(r_utm_masked)
  
  # get original CRS
  orig_crs <- crs(r_utm_masked)
  
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
  #file.path(dirinput, paste0(gsub("\\.", "", species_name), "_", biome, "_", scenario, "_cropped_reprojectedKm.tif"))
  
  # save aggregated raster
  writeRaster(r_agg, output_filename, overwrite = TRUE)
  
  # clean memory & save space
  #rm(r, r_utm, r_agg)
  gc()
}

message("✅ Environmental layers prepared successfully! ")