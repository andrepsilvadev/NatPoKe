#########################
# SAVING RASTERS AS CSV #
########## MIS ##########
# 22 Jan 2025


library(Rcpp)
library(checkmate)
library(raster)  # For handling raster files
library(tools)   # For working with file paths
library(here)

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
alce <- rast("C:/Users/maria/Documents/NatPoKe/example/mammals_try2/012-Alcesalces_abundance.tif")
plot(alce)

test_output_file <- tempfile("matrix_output", fileext = ".csv")

save_matrix_as_csv(mat = terra::as.matrix(alce),
                   path = test_output_file)

reimported_mat <- read.csv(test_output_file)

#####################################################
# automating function to loop over all raster files #
#####################################################


# define input and output folders
input_folder <- here("example/mammals_try2/results/")
output_folder <- here("example/mammals_try2/results")


# list raster files in the input folder
raster_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

# loop through each raster file
for (raster_file in raster_files) {
  # Step 1 - read raster file
  raster_obj <- terra::rast(raster_file)
  
  # Step 2 - convert raster to matrix
  raster_matrix <- terra::as.matrix(raster_obj)
  
  # Step 3 - extract raster name (without the .tif)
  raster_name <- tools::file_path_sans_ext(basename(raster_file))
  
  # Step 4 - create a temp file with the raster name included
  output_file <- tempfile(paste0(raster_name, "_output_"), tmpdir = output_folder, fileext = ".csv")
  
  # Step 5 - save the matrix to the temporary CSV
  save_matrix_as_csv(mat = raster_matrix, path = output_file)
  
  # check progress
  message("Processed: ", raster_file, " -> ", output_file)
}

###############################
# JUST TRYING TO SEE THE DATA #
###############################

library(ggplot2)
ggplot() +
  # plot data for the index in question (here Shannon wiener = sum just because these are dummydata)
  geom_raster(data = X008_Alcesalces_abundance_output_5c5439791090, aes(x = x, y = y, fill = value))
