#########################################
# INPUT SUITABILITY FILES FOR METARANGE #
#########################################
# Inês Silva
# 12 Feb 2025


##########
# Step 1 # Define area and species
##########

# select Target biome (only one)
target_biome <- "Boreal Forests/Taiga" # Tropical & Subtropical Moist Broadleaf Forests OR Boreal Forests/Taiga

# select target region (only one)
target_continent <- "Europe" # "North America" OR "South America" OR "Europe" OR "Asia" OR "Antarctica" OR "Africa" OR "Australia" OR "Oceania"     

# select target species
target_species <- read.csv(file.path(dirinput,"metaRangeSpeciesDataframe.csv")) %>% 
  dplyr::pull(Species)

##########
# Step 2 # Retrieve Global Suitability Landscapes from Google drive 
##########
# 
# download_matching_files <- function(drive_path, species_list, local_folder) {
#   ## drive path = path inside your google drive folder
#   ## species_list = vector with species names
#   ## local_folder = path to the folder where to save raster files
#   
#   # authenticate into your google drive
#   drive_auth()
#   
#   # create folder to save global suitability rasters
#   if (!dir_exists(local_folder)) {
#     dir_create(local_folder)
#   }
#   
#   # get folder path to folder ID
#   folder_ids <- Reduce(function(parent_id, folder) {
#     query <- sprintf("name = '%s' and mimeType = 'application/vnd.google-apps.folder'", folder)
#     result <- if (is.null(parent_id)) drive_ls(q = query) else drive_ls(as_id(parent_id), q = query)
#     if (nrow(result) == 0) stop(paste("Folder not found:", folder)) # the function stops if the folder is not found
#     result$id # get the folder's ID
#   }, unlist(strsplit(drive_path, "/")), init = NULL)
#   
#   # list files inside that specific folder
#   files <- drive_ls(as_id(folder_ids))
#   
#   # filter files to match wanted species
#   matching_files <- files[grep(paste(species_list, collapse = "|"), files$name, ignore.case = TRUE), ]
#   
#   # download matching files to a local folder
#   for (i in seq_len(nrow(matching_files))) {
#     drive_download(as_id(matching_files$id[i]),
#                    path = file.path(local_folder, matching_files$name[i]), overwrite = TRUE)
#     message(paste("Downloaded:", matching_files$name[i]))
#   }
# }
# 
# print("Retrieving global suitability rasters")
# download_matching_files(drive_path = "SRIT-database/user/global_suitability_landscapes",
#                         species_list = target_species,
#                         local_folder = file.path(dirinput, "global_suitability_landscapes"))
# 

##########
# Step 3 # Crop, Reproject & Convert to km all landscapes
##########

print("Retrieving global suitability rasters")

# list rasters
raster_files <- list.files(here("data/global_suitability_landscapes"),
                           pattern = paste0(target_species, "_suitability\\.tif$", collapse = "|"),
                           full.names = TRUE)

# function to duplicate raster layers as we see fit
duplicate_layers <- function(raster, times) {
  replicated <- list()
  
  # layer 1: Original raster
  replicated[[1]] <- raster
  
  # layer 2: Exact copy of original raster
  replicated[[2]] <- raster
  
  # layers 3 to end - suitability decreases progressivly by 1%
  new_layer <- raster
  for (i in 3:times) {
    new_layer <- new_layer * 0.99  # Reduce by 1% each time
    replicated[[i]] <- new_layer
  }
  return(rast(replicated))
}


# Getting region to model shapefile --------------------------------------------

# work on flat earth
sf_use_s2(FALSE) 
# function to load and select the biome shapefile
load_select_biome <- function(biome_name) {
  biome_sf <- st_read(here("data/Ecoregions2017", "Ecoregions2017.shp"))
  biome_sf[biome_sf$BIOME_NAME == biome_name, ] %>% 
    group_by(BIOME_NAME) %>% 
    summarise(geometry = st_union(geometry))
}

# function to load and select continents
load_select_continents <- function(continent_names) {
  continents <- ne_countries(scale = "medium", returnclass = "sf") %>%
    dplyr::filter(continent %in% continent_names) %>% 
    group_by(continent) %>%
    summarise(geometry = st_union(geometry))
}

# function to crop the biome boundaries to the continents
crop_biome_to_continent <- function(biome, continent_geom) {
  st_intersection(biome, continent_geom)
}


# Step 3 - crop the target region to model
biome_to_model <- crop_biome_to_continent(biome = load_select_biome(biome_name = target_biome),
                                          continent_geom = load_select_continents(continent_names = target_continent))
#SW <- ne_countries(scale ="medium", country = "Sweden", returnclass = "sv")

# Cropping ---------------------------------------------------------------------

print("Creating a dynamic landscape")

# loop through each raster file
for (r in raster_files) {
  # read the raster
  sp_raster <- rast(r)
  
  # transform values from 0-100 to 0-1
  r_rescaled <- sp_raster/100
  
  # CROP the raster to the bounding box
  cropped_raster <- terra::crop(r_rescaled, biome_to_model)
  
  # MASK the raster to the bounding box (to avoid weird finland land masses)
  masked_raster <- terra::mask(cropped_raster, biome_to_model)
  
  # DUPLICATE the layers 25 times
  duplicated_raster <- duplicate_layers(masked_raster, times = 25)
  
  # SAVE processed raster
  output_path <- file.path(dirinput, tools::file_path_sans_ext(basename(r)))
  writeRaster(duplicated_raster, paste0(output_path, "_cropped_modified.tif"), overwrite = TRUE)
  
  # remove unecessary objects
  rm(sp_raster, r_rescaled, cropped_raster, masked_raster, duplicated_raster, output_path)
}


# remove unecessary objects
rm(raster_files)
invisible(gc())

# Reprojecting & Converting to km ----------------------------------------------

landscapes <- list.files(path = dirinput,
                            pattern = paste0(target_species, "_suitability_cropped_modified\\.tif$", collapse = "|"),
                            full.names = TRUE)

print("Reprojecting and converting meters to km")

for (landscape in landscapes) {
  
  # load raster
  r <- rast(landscape)
  
  # reproject to SWEREF99 TM (EPSG:3006) 
  r_utm <- project(r, "EPSG:3035")
  
  # convert to rasterStack
  r_raster <- stack(r_utm)
  
  # get original CRS
  orig_crs <- crs(r_utm)
  
  # Rescale extent (divide by 1000 to convert meters to kilometers)
  extent(r_raster) <- extent(r_raster) / 1000
  
  # Modify CRS to indicate the new unit is kilometers
  #new_crs <- gsub("UNIT\\[\"metre\",1\\]", "UNIT[\"kilometre\",1000]", orig_crs)
  
  # Apply modified CRS
  #crs(r_raster) <- new_crs
  
  # set target resolution
  target_resolution <- 10 # km
  # # aggregate raster by Modelling resolution to match species
  agregated_raster <- raster::aggregate(x = r_raster, fact = ceiling(target_resolution/res(r_raster)[1]), fun = mean)
  extent(agregated_raster) <- extent(r_raster)
  
  # Convert back to SpatRaster while keeping all layers
  #r_km <- rast(agregated_raster)
  #r_km <- rast(agregated_raster)
  
  #r_km[is.na(r_km)] <- 0
  
  # output filename
  output_filename <- gsub("\\.tif$", "_reprojectedKm.tif", landscape)
  
  # save reprojected raster
  writeRaster(agregated_raster, output_filename, overwrite = TRUE)
  
  # remove unecessary objects
  #rm(r, r_utm, r_raster, orig_crs, new_crs, r_km, output_filename)
}
rm(landscape, landscape_SW)


##########
# Step 4 # Quick Landscape checkup 
##########

## checking dynamic landscape and cropping
species1 <- rast(file.path(dirinput, paste0(target_species[1], "_suitability_cropped_modified.tif")))
plot(species1)
res(species1) # checking initial resolution
## at this stage all species shoudl still have the same landscape resolution

## checking reprojection & conversion to km
species1_reprojected <- rast(file.path(dirinput, paste0(target_species[5], "_suitability_cropped_modified_reprojectedKm.tif")))
plot(species1_reprojected) 
res(species1_reprojected) # checking new resolution


## OLD STUFF ##


################################# DELETE LATER #################################
#target_region <- ext(6.299125, 17.2476, 59.28353, 62.78255) # regional-extent bbox
#bbox_SW <- ext(6.020508, 26.411133, 55.002826, 69.395783) # sweden bbox
#bbox_SW <- ext(12.774353, 15.526428, 61.796497, 62.595869) # micro-extent bbox
################################################################################
