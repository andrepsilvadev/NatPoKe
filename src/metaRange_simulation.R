####################################
# HOW TO RUN metaRange FOR MAMMALS #
####################################
# MIS
# 04 Feb 20025

# GOAL: Running the model for mammals species

#######################
# NAVIGATION WARNINGS #
#######################

# Model input files
    ## (1) Global Suitability Landscapes - for each species to model
    ## (2) Species Trait Database

# Model Output files
## should follow this structure:
    ## SCENARIO_BIOME_REGION_TIME_SPECIES_VARIABLE.tif
    ## in "save_results" process change in the prefix line to accomodate this


# RANDOM DUMMY MISTAKES TO AVOID
    ## 1 - species name CANNOT have spaces or "_"
    ## 2 - species for which we do not have a suitability raster cannot be in the .csv file
    ## 3 - max_dispersal_dist HAS to be an INTEGRER! So I added as.integer() into that line 
    ## 4 - when this "self$sim$environment$current[[species_suitability_name]]" appears make sure species_suitability is the EXACT same name as the name of the raster imported with sds()


###############
# Input files #
###############

## Global Suitability Landscapes - for each species ----------------------------
### (available on SRIT DATABASE Google drive)

# Install and load googledrive package
if (!require(googledrive)) install.packages("googledrive", dependencies = TRUE)
library(googledrive)

# log in to your own Google Drive 
drive_auth() # this goes to the browser and asks if you wnat to allow access to files (yes)


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

# INSERT HERE YOUR OWN PATH INSIDE YOUR GOOGLE DRIVE #
files <- list_files_by_path("SRIT-database/user/global_suitability_landscapes")
# Check files
print(files)

suitabilities_folder <- "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/Newfolder"

# DOWNLOAD each file
for (i in seq_len(nrow(files))) {
  drive_download(as_id(files$id[i]), # selec files by id column
                 path = file.path(suitabilities_folder, files$name[i]), overwrite = TRUE)
  message(paste("Downloaded:", files$name[i])) 
}

## Species Trait Dataframe -----------------------------------------------------

species_traits <- read.csv(here("example/mammals_try2/clean_data_2species/target_metarange_mammals20250110.csv"))

### Check if we have landscapes for all sps in trait dataframe
# list species in trait data
species_list<- unique(species_traits$species)
existing_files <- file.exists(here(file.path(suitabilities_folder,
                                             paste0(species_list, "_suitability.tif"))))
# Display a WARNING with just the species names
if (length(species_list[!existing_files]) > 0) {
  warning("No suitability files found for the following species: ", paste(species_list[!existing_files], collapse = ", "))
}

