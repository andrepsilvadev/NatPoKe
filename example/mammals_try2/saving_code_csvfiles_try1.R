library(Rcpp)
library(checkmate)
library(raster)  # For handling raster files
library(tools)   # For working with file paths

# Define the C++ function
output_values_cpp <- cppFunction('
#include <Rcpp.h>
#include <fstream>
using namespace Rcpp;

// [[Rcpp::export]]
void output_values_cpp(NumericMatrix mat, std::string path) {
  std::ofstream file(path);
  if (!file.is_open()) {
    Rcpp::stop("Unable to open file");
  }

  file << "x,y,value\\n";
  for (int i = 0; i < mat.nrow(); ++i) {
    for (int j = 0; j < mat.ncol(); ++j) {
      file << (i + 1) << "," << (j + 1) << "," << mat(i, j) << "\\n";
    }
  }

  file.close();
}
')

# Function to save a matrix as a CSV file
save_matrix_as_csv <- function(mat, path) {
  if (!is.numeric(mat) || !is.matrix(mat)) {
    stop("Input must be a numeric matrix")
  }
  
  if (!checkmate::checkPathForOutput(path)) {
    stop("Output path invalid")
  }
  
  file.create(path)
  
  output_values_cpp(mat, path)
}

###########################################
# testing Stefans function for one raster #
###########################################

library(terra)
alce <- rast("~/NatPoKe/example/mammals_try2/001-Alcesalces_abundance.tif")
plot(alce)

test_output_file <- tempfile("matrix_output", fileext = ".csv")

save_matrix_as_csv(mat, test_output_file)

save_matrix_as_csv(mat = terra::as.matrix(alce),
                   path = test_output_file)
reimported_mat <- read.csv(test_output_file)



# Main function to process multiple raster files
process_rasters_to_csv <- function(input_files, output_dir) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Loop through each input file
  for (raster_file in input_files) {
    tryCatch({
      # Load the raster
      raster_data <- terra::rast(raster_file)
      
      # Convert raster to a matrix
      raster_matrix <- as.matrix(raster_data)
      
      # Define output file path
      output_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(raster_file)), ".csv"))
      
      # Save matrix to CSV
      save_matrix_as_csv(raster_matrix, output_file)
      
      message(paste("Processed:", raster_file, "->", output_file))
    }, error = function(e) {
      message(paste("Error processing", raster_file, ":", e$message))
    })
  }
}

# Example usage
# Define your input raster files and output directory
input_rasters <- list.files("~/NatPoKe/example/mammals_try2", pattern = "\\.tif$", full.names = TRUE)  # Adjust file extension if needed
output_directory <- "~/NatPoKe/example/mammals_try2/outputs"

# Call the function
process_rasters_to_csv(input_rasters, output_directory)
