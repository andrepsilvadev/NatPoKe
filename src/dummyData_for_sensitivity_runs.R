#######################################
# DUMMY DATA FOR SENSITIVITY RUN PLOT #
################# MIS #################
# 25 Jan 2025

# packages
library(terra)
library(here)
library(metaRange)
library(readr)
library(raster)
library(sf)

# Check if environment data is available
#stopifnot(file.exists(here("example/mammals_try2/Lynxlynx_suitability.tif")))
stopifnot(file.exists(here("example/mammals_try2/Alcesalces_suitability.tif")))
stopifnot(file.exists(here("example/mammals_try2/target_metarange_mammals20250110.csv")))

###########################
# model simulation SR 105 # increasing reproduction_rate by +5%
###########################

# setting up the simulation ----------------------------------------------------

# setup
set_verbosity(2L) # 0L for no output, 1L for progress updates, 2L for debug
options(scipen = 999)
set.seed(1)
save_string <- here("example/mammals_try2/")

# simulation parameters
sim_name <- "example_01"

species_traits <- read.csv(here("example/mammals_try2/target_metarange_mammals20250110.csv"))

# add random number for this until ANDRE decides what to do
species_traits$optimum_forest_cover <- c(70, 50)

# increase reproduction rate by 5%
species_traits$reproduction_rate <- species_traits$reproduction_rate * 1.05

# image parameters
wid <- 2000
hgt <- 1400
unt <- "px"
ppi <- 300

# Landscape --------------------------------------------------------------------

# load the environment
sim_env <- sds(list.files(here("example/mammals_try2"), pattern = "species_suitability", full.names = TRUE))


# create a simulation object ---------------------------------------------------

sim <- create_simulation(sim_env)

# We have already created our suitability layer, so we can just add them to the simulation
# in the order they are in the SDS.
# To have an aditional burn-in period, we set the time layer mapping to 1 for the first 5 time steps
# > c(rep(1, 5), seq_len(min(nlyr(sim_env))))
# >  1  1  1  1  1  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

sim$set_time_layer_mapping(c(rep(1, 5), seq_len(min(nlyr(sim_env)))))

# add species ------------------------------------------------------------------

# we loop over the species_traits data frame and add the species to the simulation
for (i in seq_len(nrow(species_traits))) {
  this_species <- species_traits[["species"]][i]
  
  # "register" the species with the simulation
  sim$add_species(this_species)
  
  # add traits that need to bes strored at the population level
  sim$add_traits(
    species = this_species,
    population_level = TRUE,
    
    "abundance" = species_traits[["initial_abundance"]][i],
    "juvenile_abundance" = 0, # we only need this for one species in the example, but we can just add the trait to all species
    "reproduction_rate" = species_traits[["reproduction_rate"]][i],
    "carrying_capacity" = species_traits[["carrying_capacity"]][i]
  )
  
  # add traits that are the same for all populations of a species
  sim$add_traits(
    species = this_species,
    population_level = FALSE,
    "dispersal_distance" =  species_traits[["dispersal_max_distance"]][i],
    "max_reproduction_rate" = species_traits[["reproduction_rate"]][i],
    "max_carrying_capacity" = species_traits[["carrying_capacity"]][i],
    
    # the following is just a simple kernel, but you can use any function / dispersal kernel you like
    "dispersal_kernel" = calculate_dispersal_kernel(
      max_dispersal_dist = as.integer(species_traits[["dispersal_max_distance"]][i]),
      kfun = negative_exponential_function,
      mean_dispersal_dist = species_traits[["dispersal_max_distance"]][i] / 2,
    )
  )
}
# check what we've done
# sim$Lynxlynx$traits
# sim$Alcesalces$traits

# NOTAS 20250117
# species name CANNOT have spaces or "_"
# species for which we do not have a suitability raster cannot be in the .csv file
# max_dispersal_dist HAS to be an INTEGRER! So I added as.integer() into that line 

species_names <- sim$species_names()
sim$add_globals(
  # keep track of the species that are still alive
  "alive_species" = species_names
)
# add global variables ---------------------------------------------------------

# add some global variables to track stats
# i.e. we want to know:
# - the total abundance of each species
# - the total number of suitable cells for each species ????? WHERE IS THIS ?????
# - the total number of occupied cells for each species
# Note: this is mainly for debugging purposes, to save data, there are better ways
species_sum_abundance <- vector("list", length(species_names))
names(species_sum_abundance) <- species_names
for (i in species_names) {
  species_sum_abundance[[i]] <- list(
    "n_abundance" = vector("numeric", sim$number_time_steps),
    "n_occupied" = vector("numeric", sim$number_time_steps),
    "n_juveniles" = vector("numeric", sim$number_time_steps)
  )
}
do.call(sim$add_globals, species_sum_abundance)

# check which globals exist
sim$globals

# add processes ----------------------------------------------------------------

# To simplify the example, we just add the same processes for all species here
sim$add_process(
  species = species_names,
  process_name = "suitability_influence_population_parameter",
  process_fun = function() {
    species_suitability_name <- paste0("species_suitability_", self$name)
    
    self$traits[["carrying_capacity"]] <-
      self$traits[["max_carrying_capacity"]] * self$sim$environment$current[[species_suitability_name]]
    
    self$traits[["reproduction_rate"]] <-
      self$traits[["max_reproduction_rate"]] * self$sim$environment$current[[species_suitability_name]]
  },
  execution_priority = 2
)


sim$add_process(
  species = species_names,
  process_name = "reproduction",
  process_fun = function() {
    self$traits[["abundance"]] <-
      ricker_reproduction_model(
        self$traits[["abundance"]],
        self$traits[["reproduction_rate"]],
        self$traits[["carrying_capacity"]]
      )
  },
  execution_priority = 3
)


sim$add_process(
  species = species_names,
  process_name = "reproduction_age_structured",
  process_fun = function() {
    
    # calculate how many juveniles are produced
    self$traits[["juvenile_abundance"]] <- self$traits[["abundance"]] * self$traits[["reproduction_rate"]]
    
    # how many of the adult population survive, based on the environment suitability
    self$traits[["abundance"]] <-
      self$traits[["abundance"]] * self$sim$environment$current[[paste0("species_suitability_", self$name)]]
    
    # how many of the juveniles grow up and become adults
    self$traits[["abundance"]] <-
      self$traits[["abundance"]] +
      self$traits[["juvenile_abundance"]] *
      (1 - self$traits[["juvenile_abundance"]] / self$traits[["carrying_capacity"]])
    
    # make sure we don't exceed the carrying capacity
    exeed_populations <- self$traits[["abundance"]] > self$traits[["carrying_capacity"]]
    exeed_populations[is.na(exeed_populations)] <- FALSE
    self$traits[["abundance"]][exeed_populations] <-
      self$traits[["carrying_capacity"]][exeed_populations]
    
  },
  execution_priority = 3
)


sim$add_process(
  species = species_names,
  process_name = "dispersal_process",
  process_fun = function() {
    # weighted dispersal
    # i.e. individuals disperse more likely into more suitable cells
    self$traits[["abundance"]] <- dispersal(
      abundance = self$traits[["abundance"]],
      weights = self$sim$environment$current[[paste0("species_suitability_", self$name)]],
      dispersal_kernel = self$traits[["dispersal_kernel"]])
  },
  execution_priority = 4
)


sim$add_process(
  process_name = "truncate",
  process_fun = function() {
    # just because there is nothing in reality like 0.5 individuals
    for (i in self$globals[["alive_species"]]) {
      self[[i]]$traits[["abundance"]] <- trunc(self[[i]]$traits[["abundance"]])
    }
  },
  execution_priority = 5
)


sim$add_process(
  process_name = "track_stats",
  process_fun = function() {
    for (i in self$globals[["alive_species"]]) {
      current_abu <- sum(self[[i]]$traits[["abundance"]], na.rm = TRUE)
      # remove species from future process queue if extinct
      # i.e. we don't need to calculate suitability for extinct species
      if (current_abu <= 1) {
        current_abu <- 0
        for (p in self[[i]]$processes) {
          self$queue$dequeue(p$get_PID())
        }
        
        self$globals[["alive_species"]] <-
          self$globals[["alive_species"]][
            self$globals[["alive_species"]] != i
          ]
        if (length(self$globals[["alive_species"]]) == 0) {
          print("all species extinct")
          self$exit()
        }
      }
      self$globals[[i]][["n_abundance"]][[self$get_current_time_step()]] <-
        current_abu
      
      self$globals[[i]][["n_occupied"]][[self$get_current_time_step()]] <-
        sum(self[[i]]$traits[["abundance"]] > 1, na.rm = TRUE)
      
      self$globals[[i]][["n_juveniles"]][[self$get_current_time_step()]] <- # I ADDED THIS TO SEE IF I COULD GET ANOTHER VARIABLE BY MYSELF
        sum(self[[i]]$traits[["juvenile_abundance"]] > 1, na.rm = TRUE)
    }
  },
  execution_priority = 6
)

# saving the results -----------------------------------------------------------

save_string <- here("example/mammals_try2/results_sensitivityRuns")
# Note: Saving the results is a process that takes the longest time
# because writing a raster to disk is slow
# So think about when you want to save results (each time step vs jsut the last one)
sim$add_process(
  process_name = "save_results",
  process_fun = function() {
    
    for (species in self$globals[["alive_species"]]) {
      # define a suffix string for sensitivity run
      suffix <- "SR105_" 
      
      save_species(
        # pass the species object
        self[[species]],
        # specify traits we want to save
        traits = c("abundance", "reproduction_rate"),
        # a prefix for each time step
        prefix = paste0(suffix, sprintf("%03d", self$get_current_time_step()), "_"),
        # where should it be saved
        path = save_string,
        overwrite = TRUE
      )
    }
  },
  execution_priority = 7
)

# check what I have done defore running the simulation
# sim$Lynxlynx$traits
# sim$Alcesalces$traits

# run simulation ---------------------------------------------------------------

set_verbosity(1L)
print("starting simulation")
sim$begin()
print("simulation finished")


###########################
# model simulation SR 095 # increasing reproduction_rate by -5%
###########################

# setting up the simulation ----------------------------------------------------

# setup
set_verbosity(2L) # 0L for no output, 1L for progress updates, 2L for debug
options(scipen = 999)
set.seed(1)
save_string <- here("example/mammals_try2/")

# simulation parameters
sim_name <- "example_01"

species_traits <- read.csv(here("example/mammals_try2/target_metarange_mammals20250110.csv"))

# add random number for this until ANDRE decides what to do
species_traits$optimum_forest_cover <- c(70, 50)

# increase reproduction rate by 5%
species_traits$reproduction_rate <- species_traits$reproduction_rate * 0.95

# image parameters
wid <- 2000
hgt <- 1400
unt <- "px"
ppi <- 300

# Landscape --------------------------------------------------------------------

# load the environment
sim_env <- sds(list.files(here("example/mammals_try2"), pattern = "species_suitability", full.names = TRUE))


# create a simulation object ---------------------------------------------------

sim <- create_simulation(sim_env)

# We have already created our suitability layer, so we can just add them to the simulation
# in the order they are in the SDS.
# To have an aditional burn-in period, we set the time layer mapping to 1 for the first 5 time steps
# > c(rep(1, 5), seq_len(min(nlyr(sim_env))))
# >  1  1  1  1  1  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

sim$set_time_layer_mapping(c(rep(1, 5), seq_len(min(nlyr(sim_env)))))

# add species ------------------------------------------------------------------

# we loop over the species_traits data frame and add the species to the simulation
for (i in seq_len(nrow(species_traits))) {
  this_species <- species_traits[["species"]][i]
  
  # "register" the species with the simulation
  sim$add_species(this_species)
  
  # add traits that need to bes strored at the population level
  sim$add_traits(
    species = this_species,
    population_level = TRUE,
    
    "abundance" = species_traits[["initial_abundance"]][i],
    "juvenile_abundance" = 0, # we only need this for one species in the example, but we can just add the trait to all species
    "reproduction_rate" = species_traits[["reproduction_rate"]][i],
    "carrying_capacity" = species_traits[["carrying_capacity"]][i]
  )
  
  # add traits that are the same for all populations of a species
  sim$add_traits(
    species = this_species,
    population_level = FALSE,
    "dispersal_distance" =  species_traits[["dispersal_max_distance"]][i],
    "max_reproduction_rate" = species_traits[["reproduction_rate"]][i],
    "max_carrying_capacity" = species_traits[["carrying_capacity"]][i],
    
    # the following is just a simple kernel, but you can use any function / dispersal kernel you like
    "dispersal_kernel" = calculate_dispersal_kernel(
      max_dispersal_dist = as.integer(species_traits[["dispersal_max_distance"]][i]),
      kfun = negative_exponential_function,
      mean_dispersal_dist = species_traits[["dispersal_max_distance"]][i] / 2,
    )
  )
}
# check what we've done
# sim$Lynxlynx$traits
# sim$Alcesalces$traits

# NOTAS 20250117
# species name CANNOT have spaces or "_"
# species for which we do not have a suitability raster cannot be in the .csv file
# max_dispersal_dist HAS to be an INTEGRER! So I added as.integer() into that line 

species_names <- sim$species_names()
sim$add_globals(
  # keep track of the species that are still alive
  "alive_species" = species_names
)
# add global variables ---------------------------------------------------------

# add some global variables to track stats
# i.e. we want to know:
# - the total abundance of each species
# - the total number of suitable cells for each species ????? WHERE IS THIS ?????
# - the total number of occupied cells for each species
# Note: this is mainly for debugging purposes, to save data, there are better ways
species_sum_abundance <- vector("list", length(species_names))
names(species_sum_abundance) <- species_names
for (i in species_names) {
  species_sum_abundance[[i]] <- list(
    "n_abundance" = vector("numeric", sim$number_time_steps),
    "n_occupied" = vector("numeric", sim$number_time_steps),
    "n_juveniles" = vector("numeric", sim$number_time_steps)
  )
}
do.call(sim$add_globals, species_sum_abundance)

# check which globals exist
sim$globals

# add processes ----------------------------------------------------------------

# To simplify the example, we just add the same processes for all species here
sim$add_process(
  species = species_names,
  process_name = "suitability_influence_population_parameter",
  process_fun = function() {
    species_suitability_name <- paste0("species_suitability_", self$name)
    
    self$traits[["carrying_capacity"]] <-
      self$traits[["max_carrying_capacity"]] * self$sim$environment$current[[species_suitability_name]]
    
    self$traits[["reproduction_rate"]] <-
      self$traits[["max_reproduction_rate"]] * self$sim$environment$current[[species_suitability_name]]
  },
  execution_priority = 2
)


sim$add_process(
  species = species_names,
  process_name = "reproduction",
  process_fun = function() {
    self$traits[["abundance"]] <-
      ricker_reproduction_model(
        self$traits[["abundance"]],
        self$traits[["reproduction_rate"]],
        self$traits[["carrying_capacity"]]
      )
  },
  execution_priority = 3
)


sim$add_process(
  species = species_names,
  process_name = "reproduction_age_structured",
  process_fun = function() {
    
    # calculate how many juveniles are produced
    self$traits[["juvenile_abundance"]] <- self$traits[["abundance"]] * self$traits[["reproduction_rate"]]
    
    # how many of the adult population survive, based on the environment suitability
    self$traits[["abundance"]] <-
      self$traits[["abundance"]] * self$sim$environment$current[[paste0("species_suitability_", self$name)]]
    
    # how many of the juveniles grow up and become adults
    self$traits[["abundance"]] <-
      self$traits[["abundance"]] +
      self$traits[["juvenile_abundance"]] *
      (1 - self$traits[["juvenile_abundance"]] / self$traits[["carrying_capacity"]])
    
    # make sure we don't exceed the carrying capacity
    exeed_populations <- self$traits[["abundance"]] > self$traits[["carrying_capacity"]]
    exeed_populations[is.na(exeed_populations)] <- FALSE
    self$traits[["abundance"]][exeed_populations] <-
      self$traits[["carrying_capacity"]][exeed_populations]
    
  },
  execution_priority = 3
)


sim$add_process(
  species = species_names,
  process_name = "dispersal_process",
  process_fun = function() {
    # weighted dispersal
    # i.e. individuals disperse more likely into more suitable cells
    self$traits[["abundance"]] <- dispersal(
      abundance = self$traits[["abundance"]],
      weights = self$sim$environment$current[[paste0("species_suitability_", self$name)]],
      dispersal_kernel = self$traits[["dispersal_kernel"]])
  },
  execution_priority = 4
)


sim$add_process(
  process_name = "truncate",
  process_fun = function() {
    # just because there is nothing in reality like 0.5 individuals
    for (i in self$globals[["alive_species"]]) {
      self[[i]]$traits[["abundance"]] <- trunc(self[[i]]$traits[["abundance"]])
    }
  },
  execution_priority = 5
)


sim$add_process(
  process_name = "track_stats",
  process_fun = function() {
    for (i in self$globals[["alive_species"]]) {
      current_abu <- sum(self[[i]]$traits[["abundance"]], na.rm = TRUE)
      # remove species from future process queue if extinct
      # i.e. we don't need to calculate suitability for extinct species
      if (current_abu <= 1) {
        current_abu <- 0
        for (p in self[[i]]$processes) {
          self$queue$dequeue(p$get_PID())
        }
        
        self$globals[["alive_species"]] <-
          self$globals[["alive_species"]][
            self$globals[["alive_species"]] != i
          ]
        if (length(self$globals[["alive_species"]]) == 0) {
          print("all species extinct")
          self$exit()
        }
      }
      self$globals[[i]][["n_abundance"]][[self$get_current_time_step()]] <-
        current_abu
      
      self$globals[[i]][["n_occupied"]][[self$get_current_time_step()]] <-
        sum(self[[i]]$traits[["abundance"]] > 1, na.rm = TRUE)
      
      self$globals[[i]][["n_juveniles"]][[self$get_current_time_step()]] <- # I ADDED THIS TO SEE IF I COULD GET ANOTHER VARIABLE BY MYSELF
        sum(self[[i]]$traits[["juvenile_abundance"]] > 1, na.rm = TRUE)
    }
  },
  execution_priority = 6
)

# saving the results -----------------------------------------------------------

save_string <- here("example/mammals_try2/results_sensitivityRuns")
# Note: Saving the results is a process that takes the longest time
# because writing a raster to disk is slow
# So think about when you want to save results (each time step vs jsut the last one)
sim$add_process(
  process_name = "save_results",
  process_fun = function() {
    
    for (species in self$globals[["alive_species"]]) {
      # define a suffix string for sensitivity run
      suffix <- "SR095_" 
      
      save_species(
        # pass the species object
        self[[species]],
        # specify traits we want to save
        traits = c("abundance", "reproduction_rate"),
        # a prefix for each time step
        prefix = paste0(suffix, sprintf("%03d", self$get_current_time_step()), "_"),
        # where should it be saved
        path = save_string,
        overwrite = TRUE
      )
    }
  },
  execution_priority = 7
)

# check what I have done defore running the simulation
# sim$Lynxlynx$traits
# sim$Alcesalces$traits

# run simulation ---------------------------------------------------------------

set_verbosity(1L)
print("starting simulation")
sim$begin()
print("simulation finished")


####################################
# CONVERTING RASTERS OUTPUT TO CSV #
####################################

# STEFAN's C++ FUNCTION --------------------------------------------------------
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


# define input and output folders
input_folder <- here("example/mammals_try2/results_sensitivityRuns")
output_folder <- here("example/mammals_try2/results_sensitivityRuns")


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

