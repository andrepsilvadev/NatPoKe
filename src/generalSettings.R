####################
# GENERAL SETTINGS #
####################
# Inês Silva
# 14 Feb 2025

# create a working directory for each simulation run ---------------------------
runpath <- file.path("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs", runname)
dir.create(runpath, showWarnings = TRUE)
dir.create(file.path(runpath, "Inputs"), showWarnings = TRUE)
dir.create(file.path(runpath, "Outputs"), showWarnings = TRUE)
#dir.create(file.path(runpath, "Output_Maps"), showWarnings = TRUE)
dirinput <- file.path(runpath, "Inputs")
dirout <- file.path(runpath, "Ouputs")


# creating working directories in the drive -------(WORK IN PROGRESS) ----------
# base_folder <- drive_get("modelRuns")
# run_folder <- drive_mkdir(runname, path = as_id(base_folder$id)) # create the main run folder in Google Drive
# inputs_folder <- drive_mkdir("Inputs", path = as_id(run_folder$id)) # create subfolders: Inputs & Outputs
# outputs_folder <- drive_mkdir("Outputs", path = as_id(run_folder$id))
# dirinput <-  inputs_folder$id # OR - drive_mkdir("Inputs", path = as_id(run_folder$id))[[1]]
# dirout <- outputs_folder$id
# rm(base_folder, run_folder, inputs_folder, outputs_folder)
# AFTER THIS EVERYTIME WE WANT TO SAVE SOMETHING WE NEED TO DO IT FIRST INTO A 
# TEMPORARY FILE AND ONLY AFTER THAT DO WE UPLOAD IT TO THE DRIVE, LIKE THIS
########### CHECK WITH ANDRE IF IT IS WORTH IT ############ DON'T THINK SO #####

# Write species_traits to a temp CSV file
# temp_csv <- tempfile(fileext = ".csv")
# write_csv(species_traits, temp_csv)
# 
# # upload the file to the Google Drive "Inputs" folder
# drive_upload(
#   media = temp_csv,
#   name = "metaRangeSpeciesDataframe.csv",
#   path = as_id(dirinput_id),  # Uploads to the "Inputs" folder
#   overwrite = TRUE
# )
# 
# # Delete temporary file after upload
# unlink(temp_csv)


# spatial projections ----------------------------------------------------------
# UTM North-Sweden minimizes local distortion
# Tried this before. Generates the following:
# "unequal horizontal and vertical resolutions. Such data cannot be stored in arc-ascii format"
#utm34 <- "+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs"

# Lambert Azimuthal Equal Area - preserves the relative sizes of areas
# throughout the projection. This property makes it unsuitable for
# large regions
#crs.laea <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# wgs84
#wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"
#wgs84_crs <- "EPSG:4326"

# country crs
#sweden_crs <- "EPSG:3006"