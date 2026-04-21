###########################################
# AVERAGE TRAIT RASTERS ACROSS REPLICATES #
###########################################
# Stefan Fallert. & Inês Silva
# 02 Jan 2026

##########
# Step 1 # Check directories and get reference objects to locate rasters
##########
source(file.path("src", "libraries.R"))
source(file.path("src", "customFunctions2.R"))

message("Starting to average rasters across replicates")

# directory with output rasters
#dirout  
# species to find
#target_species
# specify the number of timesteps 
timesteps <- seq(from = 26, to = 136)
# specify which traits to average
traits_of_interrest <- c("abundance", "reproductionRate", "dispersalChange")
#dirout <- "D:/metaRange_April26/Africa_ssp126_20260405/Outputs"
##########
# Step 2 # Average rasters specified above per species and timestep 
##########

for (sp in target_species) {
  
  sp <- gsub(" ", ".", sp)
  message("Averaging for ", sp)
  
  for (i in timesteps) {
    for (trait in traits_of_interrest) {
      
      filepath <- list.files(dirout,
                             pattern = paste0(sprintf("%03d", i), '_', sp, '_', trait, ".tif"),
                             full.names = TRUE)
      
      if (length(filepath) > 0) {
        # Load the raster
        r <- rast(filepath)
        meanTraitValue <- mean(r, na.rm = TRUE)
        
        # Save the raster to a new file
        output_file <- file.path(dirout, paste0(sp, "_", trait, "_meanAcrossReplicates", "_", sprintf("%03d", i), ".tif"))
        writeRaster(meanTraitValue, filename = output_file, overwrite = TRUE)
      } else {
        print(paste("No file found for", sp, "at timestep", i, "for trait", trait))
      }
    }
  }
}

message("Finished averaging rasters!")

# # Import all abundance rasters for Alces alces
# alces_abundance_files <- list.files(
#   path = dirout,
#   pattern = "Alcesalces_abundance_meanAcrossReplicates.*\\.tif$",
#   full.names = TRUE
# )
# 
# alces_abundance_rasters_list <- lapply(alces_abundance_files, rast)
# 
# # Set layer names as the base filename (without extension)
# names(alces_abundance_rasters_list) <- tools::file_path_sans_ext(basename(alces_abundance_files))
# 
# # Combine all rasters into a single SpatRaster
# alces_abundance_rasters <- rast(alces_abundance_rasters_list)
# 
# plot(alces_abundance_rasters[[100:125]])
# # Check the layer names
# print(names(alces_abundance_rasters))