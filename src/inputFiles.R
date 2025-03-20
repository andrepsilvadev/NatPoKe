#########################################
# INPUT SUITABILITY FILES FOR metaRange #
#########################################
# Inês Silva
# 12 Feb 2025

# GOAL: Dowoad & modify suitability rasters for all the species we want to model
# with metaRange


##########
# STEP 1 # Import Species Trait Dataframe 
##########

species_traits <- read.csv(file.path(dirinput,"metaRangeSpeciesDataframe.csv"))

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
# download_matching_files(drive_path = "SRIT-database/user/global_suitability_landscapes",
#                         species_list = species_traits$Species,
#                         local_folder = file.path(dirinput, "global_suitability_landscapes"))
# 

##########
# STEP 3 # (just for testing the model) - Cropping & reprojecting for Sweden
##########

library(here)
library(terra)

###########################
## cropping & new layers ##
###########################

print("Retrieving global suitability rasters")

# list rasters
raster_files <- list.files(here("data/global_suitability_landscapes"),
                           pattern = paste0(paste(species_traits$Species, collapse = "|"), "_suitability.tif$"),
                           full.names = TRUE)
# micro-extent bbox
#bbox_SW <- ext(12.774353, 15.526428, 61.796497, 62.595869)

# regional-extent bbox
bbox_SW <- ext(6.299125, 17.2476, 59.28353, 62.78255)

# sweden bbox
#bbox_SW <- ext(6.020508, 26.411133, 55.002826, 69.395783)


SW <- ne_countries(scale = "large", country = "Sweden", returnclass = "sv")


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

print("Creating a dynamic landscape")
# Loop through each raster file
for (r in raster_files) {
  # read the raster
  sp_raster <- rast(r)
  
  # transform values from 0-100 to 0-1
  r_rescaled <- sp_raster/100
  
  # crop the raster to the bounding box
  cropped_raster <- terra::crop(r_rescaled, SW)
  # mask the raster to the bounding box (to avoid weird finland land masses)
  masked_raster <- terra::mask(cropped_raster, SW)
  
  # duplicate the layers 25 times
  duplicated_raster <- duplicate_layers(masked_raster, times = 25)
  
  # save processed raster
  output_path <- file.path(dirinput, tools::file_path_sans_ext(basename(r)))
  writeRaster(duplicated_raster, paste0(output_path, "_cropped_modified.tif"), overwrite = TRUE)
  
  # remove unecessary objects
  rm(sp_raster, r_rescaled, cropped_raster, masked_raster, duplicated_raster, output_path)
}


# checking new layers
#plot(rast(file.path(dirinput, "Alcesalces_suitability_cropped_modified.tif")))
#alce <- rast(file.path(dirinput, "Alcesalces_suitability_cropped_modified.tif"))
#res(alce)
# # 0.04999306 0.04998611
# plot(rast(file.path(dirinput, "Cervuselaphus_suitability_cropped_modified.tif")))
# plot(rast(file.path(dirinput, "Lynxlynx_suitability_cropped_modified.tif")))
# plot(rast(file.path(dirinput, "Rangifertarandus_suitability_cropped_modified.tif")))

rm(raster_files, bbox_SW)
invisible(gc())


##################
## reprojecting ##
##################

# https://gis.stackexchange.com/questions/226170/rescaling-coordinates-of-rasters-shapefiles-and-spatial-objects-from-meters-to


landscape_SW <- list.files(path = dirinput,
                            pattern = "_suitability_cropped_modified.tif",
                            full.names = TRUE)
# alcesalces <- rast(landscape_SW[[1]])
# alcesalces_utm <- terra::project(alcesalces, "EPSG:3006")
# alcesalces_stack <- stack(alcesalces_utm)
# orig_crs <- crs(alcesalces_utm)
# extent(alcesalces_stack) <- extent(alcesalces_stack)/1000
# 
# factor <- ceiling(species_traits$ModellingRes[species_traits$Species == "Alcesalces"]/sqrt(species_traits$CellResolution[species_traits$Species == "Alcesalces"]))
# 
# 
# agregated_alces <- raster::aggregate(x = alcesalces_stack, fact = factor, fun = mean)
# extent(agregated_alces) <- extent(alcesalces_stack)
# agregated_alces_km <- rast(agregated_alces)
# plot(agregated_alces_km)

print("Reprojecting and converting meters to km")

SW <- ne_countries(scale = "large", country = "Sweden", returnclass = "sv")
plot(SW)
#newbbox <- ext(73553.2070,6132048.9058,945927.7058,7740232.3194)

for (landscape in landscape_SW) {
  
  # load raster
  r <- rast(landscape)
  
  # reproject to SWEREF99 TM (EPSG:3006) 
  r_utm <- project(r, "EPSG:3006")
  
  # convert to rasterStack
  r_raster <- stack(r_utm)
  
  # get original CRS
  orig_crs <- crs(r_utm)
  
  # Rescale extent (divide by 1000 to convert meters to kilometers)
  extent(r_raster) <- extent(r_raster) / 1000
  
  # Modify CRS to indicate the new unit is kilometers
  new_crs <- gsub("UNIT\\[\"metre\",1\\]", "UNIT[\"kilometre\",1000]", orig_crs)
  
  # Apply modified CRS
  crs(r_raster) <- new_crs
  
  # Extract species name from file name (assuming it's before the first underscore or period)
  species_name <- tools::file_path_sans_ext(basename(landscape)) # Remove extension
  species_name <- gsub("_.*", "", species_name) # Remove everything after the first underscore
  
  # Get the corresponding modeling resolution
  species_fact <- ceiling(species_traits$ModellingRes[species_traits$Species == species_name]/sqrt(species_traits$CellResolution[species_traits$Species == species_name]))
  
  # aggregate raster by Modelling resolution to match species
  agregated_raster <- raster::aggregate(x = r_raster, fact = species_fact, fun = mean)
  extent(agregated_raster) <- extent(r_raster)
  
  # Convert back to SpatRaster while keeping all layers
  r_km <- rast(agregated_raster)
  
  #r_km[is.na(r_km)] <- 0
  
  # output filename
  output_filename <- gsub("\\.tif$", "_reprojectedKm.tif", landscape)
  
  # save reprojected raster
  writeRaster(r_km, output_filename, overwrite = TRUE)
  
  # remove unecessary objects
  #rm(r, r_utm, r_raster, orig_crs, new_crs, r_km, output_filename)
}
rm(landscape, landscape_SW)



###################################################################
# CHECKING THE RESOLUTION OF RASTER BEFORE AND AFTER AGGREGATIONS #
###################################################################

#rangifer_before <- rast(file.path(dirinput, "Rangifertarandus_suitability_cropped_modified.tif"))
#rangifer_after <- rast(file.path(dirinput, "Rangifertarandus_suitability_cropped_modified_reprojectedKm.tif"))

#plot(rangifer_after)

 
#cervus_before <- rast(file.path(dirinput, "cervuselaphus_suitability_cropped_modified.tif"))
#cervus_after <- rast(file.path(dirinput, "cervuselaphus_suitability_cropped_modified_reprojectedKm.tif"))
# 
#lynx_before <- rast(file.path(dirinput, "Lynxlynx_suitability_cropped_modified.tif"))
#lynx_after <- rast(file.path(dirinput, "Lynxlynx_suitability_cropped_modified_reprojectedKm.tif"))
# 
#alces_before <- rast(file.path(dirinput, "Alcesalces_suitability_cropped_modified.tif"))
#alces_after <- rast(file.path(dirinput, "Alcesalces_suitability_cropped_modified_reprojectedKm.tif"))

#plot(alces_before, main = "Before reprojection")
#plot(alces_after)

#######################3 FROM HERE DOWN IS LIXO ################################
############################# SIT DOWN AND READ BEFORE DELETING ################
# 
# library(terra)
# library(raster)
# 
# # Load the SpatRaster
# r <- rast(file.path(dirinput, "Alcesalces_suitability_cropped_modified_reprojected.tif"))
# 
# # Convert SpatRaster to RasterStack (CRS is preserved)
# r_raster <- stack(r)
# 
# # Get original CRS (EPSG:3006)
# orig_crs <- crs(r)  # This is still in meters
# 
# # Convert extent from meters to kilometers (scale coordinates properly)
# r_km <- terra::project(r, orig_crs, scale = 0.001)  # Proper unit conversion
# 
# # Save the transformed raster
# writeRaster(r_km, file.path(dirinput, "Alcesalces_suitability_km.tif"), overwrite=TRUE)
# 
# # Compare extents
# ext(r)   # Before (meters)
# ext(r_km) # After (kilometers)
# 
# # Compare resolutions
# res(r)   # Before (meters)
# res(r_km) # After (kilometers)
# 
# # Compare coordinate values
# xy_meters <- crds(r)  # Before
# xy_km <- crds(r_km)  # After
# head(xy_meters)
# head(xy_km)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 





