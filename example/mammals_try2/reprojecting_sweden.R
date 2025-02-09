############################
# PROJECT SWEDEN LANDSCAPE #
############################

# packages
library(raster)
library(terra)
library(here)


landscape_SW <- list.files(path = here("example/mammals_try2/clean_data_2species"),
                          pattern = "_suitability_cropped_modified.tif",
                          full.names = TRUE)
for (landscape in landscape_SW) {
  
  # Load a raster with a defined projection
  r <- rast(landscape)
  
  # Reproject to SWEREF99 TM (EPSG:3006)
  r_utm <- project(r, "EPSG:3006")
  
  # Create output filename by appending "_UTM33"
  output_filename <- gsub("\\.tif$", "_reprojected.tif", landscape)
  
  # Save the reprojected raster
  writeRaster(r_utm, output_filename, overwrite = TRUE)
  
}

plot(rast(here("example/mammals_try2/clean_data_2species", "Rangifertarandus_suitability_cropped_modified_reprojected.tif")))
