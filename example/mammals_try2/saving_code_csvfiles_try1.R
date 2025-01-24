#########################
# SAVING RASTERS AS CSV #
########## MIS ##########
# 22 Jan 2025


library(Rcpp) # for C++
library(checkmate)
library(raster)  # for raster files
library(terra)
library(tools)   # for file paths
library(here)
library(dplyr)
library(purrr) # for map_dfr()
library(data.table) # for fread()
library(tidyr)

#########################
# STEFAN's C++ FUNCTION #
#########################

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


alce <- rast("C:/Users/maria/Documents/NatPoKe/example/mammals_try2/012-Alcesalces_abundance.tif")
plot(alce)

test_output_file <- tempfile("matrix_output", fileext = ".csv")

save_matrix_as_csv(mat = terra::as.matrix(alce),
                   path = test_output_file)

reimported_mat <- read.csv(test_output_file)

#########################################################
# AUTOMATING THE FUNCTION TO LOOP OVER ALL RASTER FILES #
#########################################################

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

# seeing the raster
alce009 <- rast("C:/Users/maria/Documents/NatPoKe/example/mammals_try2/results/009-Alcesalces_abundance.tif")
plot(alce009)

# seeing the csv
ggplot() +
  # plot data for the index in question (here Shannon wiener = sum just because these are dummydata)
  geom_raster(data = X009_Alcesalces_abundance_output_5c542eaf32e0,
              aes(x = x, y = y, fill = value))

################################
# COMBINING ALL .CSVs INTO ONE #
################################


# IDEAL IMAGINED DATAFRAME THAT WOULD COME OUT OF METARANGE

# | Scenario | Time | x   | y   | Species     | Taxa   | Abundance | ReproductionRate |
# |----------|------|-----|-----|-------------|--------|-----------|------------------|
# | A        | 1    | 1   | 1   | Alces alces | Mammal | 59        | 2                |
# | A        | 1    | 1   | 2   | Alces alces | Mammal | 62        | 2                |
# | A        | 1    | 1   | 1   | Lynx lynx   | Mammal | 30        | 1                |
# | A        | 1    | 1   | 2   | Lynx lynx   | Mammal | 15        | 1                |
# | A        | 1    | 1   | 1   | bird 1      | Bird   | 30        | 1                |
# | A        | 1    | 1   | 2   | bird 1      | Bird   | 30        | 1                |

# set working directory for the map_dfr function (CHECK IF I CAN FIND A CLEANER ALTERNATIVE HERE)
setwd("~/NatPoKe/example/mammals_try2/results")

all_data <- list.files(pattern = "\\.csv$", full.names = TRUE) %>%
  map_dfr(function(path) {
    # remove file extention
    new_file_name <- tools::file_path_sans_ext(path)
    
    # remove the leading "./"
    new_file_name <- sub("^\\./", "", new_file_name)
    
    fread(path) %>% # import the files faster
      mutate(file_name = new_file_name) # add a new collumn with the file name
  })

# at this point in the script I have a dataframe with x, y, value and file name
# next I want the file name to be split into multiple word so I can get the sps name
# the timestep and the variable those values correspond to


all_data <- all_data %>%
  separate(file_name, c("time_species", "variable", "trash1", "trash2"), sep = "_", remove = FALSE) %>% # this step here will takea VERY long time
  dplyr::select(-c("file_name", "trash1", "trash2")) %>% 
  separate(time_species, c("time", "species"), sep = "-", remove = FALSE) %>% 
  dplyr::select(-"time_species") %>% 
  pivot_wider(names_from = variable, values_from = value)

# in the next simulation run, change filenames to have only "_" and not "-" and "_"
#write.csv(all_data, "all_data_together_22Jan.csv")


######################
# PLOTTING OVER TIME #
######################

# calculate mean value per time step
all_data_mean <- all_data %>%
  dplyr::group_by(species, time) %>% # in the future this will also be grouped per scenario and not by species but per taxa probably
  dplyr::summarise(across(c(abundance, reproduction), .fns = list(mean = mean, sd = sd), na.rm = TRUE), .groups = 'drop') %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

# plot abundace over time
abundance_overtime <- ggplot(data = all_data_mean,
       aes(x = as.numeric(time), y = abundance_mean , group = species)) +
  scale_x_continuous(limits = c(1, 25)) +
  geom_line(color="red") +
  xlab("Time") +
  ylab("Mean abundance (nº indiv.)") +
  theme_minimal() +
  theme(
    # remove gridlines 
    panel.grid = element_blank(),
    # add subtle horizontal lines 
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    # modify facet labels
    strip.text = element_text(face = "bold", size = rel(1)),
    strip.placement = "outside",
    # adjust legend
    legend.position = "bottom",
    # modify x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    # remove panel borders
    panel.border = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(2, "lines"),
    plot.margin = unit(c(0, 0.5, 0, 0.5), "cm"))

ggsave(plot = abundance_overtime, file = "~/NatPoKe/example/mammals_try2/results/Abundance_through_time_23jan2025.tiff", 
       bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200,
       compression = "lzw")
# Here we need to imagine that there will at least three lines representing how 
# does taxa abundance over time
  # one for mammals
  # one for birds
  # one for insects (or other taxa not sure about insects)


