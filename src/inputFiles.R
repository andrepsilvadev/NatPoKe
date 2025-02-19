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

download_matching_files <- function(drive_path, species_list, local_folder) {
  ## drive path = path inside your google drive folder
  ## species_list = vector with species names
  ## local_folder = path to the folder where to save raster files
  
  # authenticate into your google drive
  drive_auth()
  
  # create folder to save global suitability rasters
  if (!dir_exists(local_folder)) {
    dir_create(local_folder)
  }
  
  # get folder path to folder ID
  folder_ids <- Reduce(function(parent_id, folder) {
    query <- sprintf("name = '%s' and mimeType = 'application/vnd.google-apps.folder'", folder)
    result <- if (is.null(parent_id)) drive_ls(q = query) else drive_ls(as_id(parent_id), q = query)
    if (nrow(result) == 0) stop(paste("Folder not found:", folder)) # the function stops if the folder is not found
    result$id # get the folder's ID
  }, unlist(strsplit(drive_path, "/")), init = NULL)
  
  # list files inside that specific folder
  files <- drive_ls(as_id(folder_ids))
  
  # filter files to match wanted species
  matching_files <- files[grep(paste(species_list, collapse = "|"), files$name, ignore.case = TRUE), ]
  
  # download matching files to a local folder
  for (i in seq_len(nrow(matching_files))) {
    drive_download(as_id(matching_files$id[i]),
                   path = file.path(local_folder, matching_files$name[i]), overwrite = TRUE)
    message(paste("Downloaded:", matching_files$name[i]))
  }
}

download_matching_files(drive_path = "SRIT-database/user/global_suitability_landscapes",
                        species_list = species_traits$Species,
                        local_folder = file.path(dirinput, "global_suitability_landscapes"))


##########
# STEP 3 # (just for testing the model) - Cropping & reprojecting for Sweden
##########
# 
# ## cropping
# 
# # load Sweden boundary shapefile
# #st_read("C:/Users/User/OneDrive - Universidade de Lisboa/Ambiente de Trabalho/gadm41_SWE_shp/gadm41_SWE_0.shp")
# sweden <- st_read("https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_SWE.gpkg")
# 
# # list rasters
# raster_files <- list.files(file.path(dirinput, "global_suitability_landscapes"),
#                            pattern = "_suitability.tif$", full.names = TRUE)
# # define the bounding box
# bbox_SW <- ext(6.306152, 17.248535, 59.288332, 62.769811)
# 
# duplicate_layers <- function(raster, times) {
#   replicated <- rast(rep(list(raster), times))
#   return(replicated)
# }
# 
# # Loop through each raster file
# for (r in raster_files) {
#   # read the raster
#   sp_raster <- rast(r)
#   
#   # crop the raster to the bounding box
#   cropped_raster <- terra::crop(sp_raster, bbox_SW)
#   #cropped_raster <- terra::crop(sp_raster, extent(sweden))
#   
#   # duplicate the layers 25 times
#   duplicated_raster <- duplicate_layers(cropped_raster, times = 25)
#   
#   # save processed raster

#   output_path <- file.path(dirinput, "temp_mammals_landscapes"), tools::file_path_sans_ext(basename(r)))
#   writeRaster(duplicated_raster, paste0(output_path, "_cropped_modified.tif"), overwrite = TRUE)
# }
# 
# ## reprojecting
# 
# landscape_SW <- list.files(path = file.path(dirinput, "temp_mammals_landscapes")),
#                            pattern = "_suitability_cropped_modified.tif",
#                            full.names = TRUE)
# 
# for (landscape in landscape_SW) {
#   
#   # load raster
#   r <- rast(landscape)
#   
#   # reproject to SWEREF99 TM (EPSG:3006)
#   r_utm <- project(r, "EPSG:3006", res = 1000)
#   
#   # output filename
#   output_filename <- gsub("\\.tif$", "_reprojected.tif", landscape)
#   
#   # save reprojected raster
#   writeRaster(r_utm, output_filename, overwrite = TRUE)
# }



##Step 1 - Extract tropical moist forest and boreal forest shp
#Import ecoregions shapefile



ecoregions_2017 <- sf::st_read("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/SRIT_ANDRE/external_data/Ecoregions2017/Ecoregions2017.shp")

library(dplyr)
unique(ecoregions_2017$BIOME_NAME)
sf_use_s2(FALSE) #info about this on RMarkdown links
#subset only ropical Moist and Boreal Forests
forests_2017 <- ecoregions_2017 %>%
  subset(BIOME_NAME %in% "Boreal Forests/Taiga") %>%
  group_by(BIOME_NAME) %>%
  summarize(geometry = st_union(geometry))
plot(forests_2017)

# list rasters
raster_files <- list.files(file.path(here("data/global_suitability_landscapes")),
                            pattern = "_suitability.tif$", full.names = TRUE)

duplicate_layers <- function(raster, times) {
  replicated <- rast(rep(list(raster), times))
 return(replicated)
}

# Loop through each raster file
for (r in raster_files) {
 # read the raster
 sp_raster <- rast(r)

 # crop the raster to the bounding box
 cropped_raster <- terra::crop(sp_raster, ext(forests_2017))
 cropped_raster <- mask(cropped_raster, forests_2017)
 #cropped_raster <- terra::crop(sp_raster, extent(sweden))
   # duplicate the layers 25 times
 duplicated_raster <- duplicate_layers(cropped_raster, times = 25)

 # save processed raster

 output_path <- file.path(here("data/boreal_forests"), tools::file_path_sans_ext(basename(r)))
 writeRaster(duplicated_raster, paste0(output_path, "_cropped_modified.tif"), overwrite = TRUE)
}
plot(rast(here("data/boreal_forests", "Alcesalces_suitability_cropped_modified_reprojected.tif")))

## reprojecting

landscape_SW <- list.files(path = file.path(here("data/boreal_forests")),
                            pattern = "_suitability_cropped_modified.tif",
                            full.names = TRUE)

for (landscape in landscape_SW) {

 # load raster
 r <- rast(landscape)
 # reproject to SWEREF99 TM (EPSG:3006)
 r_utm <- project(r, "EPSG:3006", res = 1000)

 # output filename
 output_filename <- gsub("\\.tif$", "_reprojected.tif", landscape)
 # save reprojected raster
 writeRaster(r_utm, output_filename, overwrite = TRUE)
}






# 
# ## changing from meters to km DEPRECATED SEE IF WE CAN DELETELATER
# rena <- rast(file.path(dirinput,"temp_mammals_landscapes"), "Rangifertarandus_suitability_cropped_modified_reprojected.tif"))
# crs(rena)
# extent(rena) <- extent(c(xmin(rena), xmax(rena), ymin(rena), ymax(rena))/1000)
# projection(rena) <- gsub("units=m", "units=km", projection(rena))
# 
