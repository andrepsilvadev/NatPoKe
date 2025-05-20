###########################################
# AVERAGE TRAIT RASTERS ACROSS REPLICATES #
###########################################
# Stefan Fallert. & Inês Silva
# 20 May 2025

##########
# Step 1 # Check directories and get reference objects to locate rasters
##########

# directory with output rasters
dirout 
# species to find
species_names
# specify the number of timesteps 
timesteps <- seq_len(125)
# specify which traits to average
traits_of_interrest <- c("abundance", "reproductionRate", "dispersalChange")

##########
# Step 2 # Average rasters specified above per species and timestep 
##########

for (sp in species_names) {
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
