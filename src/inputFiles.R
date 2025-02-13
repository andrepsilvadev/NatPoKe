#########################################
# INPUT SUITABILITY FILES FOR metaRange #
#########################################
# Inês Silva
# 12 Feb 2025

# GOAL: Get suitability rasters for all the species we want to model with metaRange
## rasters will be saved in a new folder called global suitabilities


# packages
library(here)
library(googledrive)
library(fs) # for creating directories
library(terra)
library(sf)
library(raster)

##########
# STEP 1 # Import Species Trait Dataframe --------------------------------------
##########

species_traits <- read.csv(here("data","metaRangeSpeciesDataframe.csv"))

##########
# Step 2 # Retrieve Global Suitability Landscapes from Google drive ------------
##########
### (available on SRIT DATABASE Google drive)

## Iistall and load google drive package
if (!require(googledrive)) install.packages("googledrive", dependencies = TRUE)
library(googledrive)

## log in to your own Google Drive 
#drive_auth() # this goes to the browser and asks if you wnat to allow access to files (yes)


list_files_by_path <- function(path) {
  # this function to list files in a given Google Drive folder path
  folder_ids <- Reduce(function(parent_id, folder) {
    query <- sprintf("name = '%s' and mimeType = 'application/vnd.google-apps.folder'", folder)
    result <- if (is.null(parent_id)) drive_ls(q = query) # search inside parent folder
    else drive_ls(as_id(parent_id), q = query) # search in root directory
    if (nrow(result) == 0)
      stop(paste("Folder not found:", folder))
    result$id # update parent_id for the next one
  }, unlist(strsplit(path, "/")), init = NULL)
  drive_ls(as_id(folder_ids))
}

# Aathenticate Google Drive (prompts user to authenticate teh first time)
## do not forget to check the box about viewing, editing, creating and deleting files
drive_auth()

# Set the folder path for saving downloaded files
suitabilities_folder <- here("data", "global_suitability_landscapes")
if (!dir_exists(suitabilities_folder)) {
  dir_create(suitabilities_folder)
}

# define Google Drive path
files <- list_files_by_path("SRIT-database/user/global_suitability_landscapes")

# filter files that match species names
matching_files <- files[grep(paste(species_traits$Species, collapse = "|"), files$name, ignore.case = TRUE), ]

# download each matching file
for (i in seq_len(nrow(matching_files))) {
  drive_download(as_id(matching_files$id[i]),
                 path = file.path(suitabilities_folder, matching_files$name[i]), overwrite = TRUE)
  message(paste("Downloaded:", matching_files$name[i]))
}

##########
# STEP 3 # (just for testing the model) - Cropping & reprojecting for Sweden
##########

## cropping

# load Sweden boundary shapefile
#st_read("C:/Users/User/OneDrive - Universidade de Lisboa/Ambiente de Trabalho/gadm41_SWE_shp/gadm41_SWE_0.shp")
sweden <- st_read("https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_SWE.gpkg")

# list rasters
raster_files <- list.files(here("data/global_suitability_landscapes"),
                           pattern = "_suitability.tif$", full.names = TRUE)
# define the bounding box
bbox_SW <- ext(6.306152, 17.248535, 59.288332, 62.769811)

duplicate_layers <- function(raster, times) {
  replicated <- rast(rep(list(raster), times))
  return(replicated)
}

# Loop through each raster file
for (r in raster_files) {
  # read the raster
  sp_raster <- rast(r)
  
  # crop the raster to the bounding box
  cropped_raster <- terra::crop(sp_raster, bbox_SW)
  #cropped_raster <- terra::crop(sp_raster, extent(sweden))
  
  # duplicate the layers 25 times
  duplicated_raster <- duplicate_layers(cropped_raster, times = 25)
  
  # save processed raster
  output_path <- file.path(here("data/temp_mammals_landscapes"), tools::file_path_sans_ext(basename(r)))
  writeRaster(duplicated_raster, paste0(output_path, "_cropped_modified.tif"), overwrite = TRUE)
}

## reprojecting

landscape_SW <- list.files(path = here("data/temp_mammals_landscapes"),
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
# ## changing from meters to km
# rena <- rast(here("data/temp_mammals_landscapes", "Rangifertarandus_suitability_cropped_modified_reprojected.tif"))
# crs(rena)
# extent(rena) <- extent(c(xmin(rena), xmax(rena), ymin(rena), ymax(rena))/1000)
# projection(rena) <- gsub("units=m", "units=km", projection(rena))
# 
