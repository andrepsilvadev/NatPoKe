####################################
# HOW TO RUN metaRange FOR MAMMALS #
####################################
# Ines Silva
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

# packages
library(terra)
library(here)
library(metaRange)
library(readr)
library(raster)
library(sf)
library(tools) # for file without paths

###############
# Input files #
###############

## Species Trait Dataframe -----------------------------------------------------

species_traits <- read.csv(here("data","metaRangeSpeciesDataframe.csv"))

## Global Suitability Landscapes - for each species ----------------------------
### (available on SRIT DATABASE Google drive)

# Install and load googledrive package
if (!require(googledrive)) install.packages("googledrive", dependencies = TRUE)
library(googledrive)

# log in to your own Google Drive 
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

# INSERT HERE YOUR OWN PATH INSIDE YOUR GOOGLE DRIVE #
files <- list_files_by_path("SRIT-database/user/global_suitability_landscapes")

# Filter files that contain any species name in their filename
matching_files <- files[grep(paste(species_traits$species, collapse = "|"), files$name, ignore.case = TRUE), ]
# Check files with landscape and traits
#print(matching_files)


# folder to save downloaded landscapes
suitabilities_folder <- "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/Newfolder"

# DOWNLOAD each file
for (i in seq_len(nrow(matching_files))) {
  drive_download(as_id(matching_files$id[i]), # selec matching_files by id column
                 path = file.path(suitabilities_folder, matching_files$name[i]), overwrite = TRUE)
  message(paste("Downloaded:", matching_files$name[i])) 
}

###################
# metaRange MODEL #
###################

# setting up the simulation ----------------------------------------------------

# setup
set_verbosity(2L) # 0L for no output, 1L for progress updates, 2L for debug
options(scipen = 999)
set.seed(1)

# simulation parameters
sim_name <- "example_01"

# Landscape --------------------------------------------------------------------

# load the environment
sim_env <- sds(list.files(here("example/mammals_try2/clean_data_2species"), pattern = "_cropped_modified.tif", full.names = TRUE))
#####################
#### HERE THE PATH TO THE ENVIRONMENT FILES SHOULD BE THE suitabilities folder

#plot(rast(here("example/mammals_try2/clean_data_2species", "Rangifertarandus_suitability_cropped_modified.tif")))

# create a simulation object ---------------------------------------------------

sim <- create_simulation(sim_env)

# We have already created our suitability layer, so we can just add them to the simulation
# in the order they are in the SDS.
# To have an aditional burn-in period, we set the time layer mapping to 1 for the first 5 time steps
# > c(rep(1, 5), seq_len(min(nlyr(sim_env))))
# >  1  1  1  1  1  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

sim$set_time_layer_mapping(c(rep(1, 5), seq_len(min(nlyr(sim_env)))))

plot(sim_env$Rangifertarandus_suitability_cropped_modified)

# add species ------------------------------------------------------------------

# we loop over the species_traits data frame and add the species to the simulation
for (i in seq_len(nrow(species_traits))) {
  this_species <- species_traits[["Species"]][i]
  
  # "register" the species with the simulation
  sim$add_species(this_species)
  
  # add traits that need to bes strored at the population level
  sim$add_traits(
    species = this_species,
    population_level = TRUE,
    
    "abundance" = species_traits[["initialAbundance"]][i],
    "juvenileAbundance" = 0, # we only need this for one species in the example, but we can just add the trait to all species
    "reproductionRate" = species_traits[["reproductionRate"]][i],
    "carryingCapacity" = species_traits[["carryingCapacity"]][i],
    "yearlySurvivalRate" = species_traits[["yearlySurvivalRate"]]
  )
  
  # add traits that are the same for all populations of a species
  sim$add_traits(
    species = this_species,
    population_level = FALSE,
    "dispersalDistance" =  species_traits[["dispersalDistance"]][i],
    "maxReproductionRate" = species_traits[["reproductionRate"]][i],
    "maxCarryingCapacity" = species_traits[["carryingCapacity"]][i],
    
    # the following is just a simple kernel, but you can use any function / dispersal kernel you like
    "dispersalKernel" = calculate_dispersal_kernel(
      max_dispersal_dist = as.integer(species_traits[["dispersalMaxDistance"]][i]),
      kfun = negative_exponential_function,
      mean_dispersal_dist = species_traits[["dispersalDistance"]][i] / 2,
    )
  )
}


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

sim$Rangifertarandus$traits$abundance
plot(sim$Rangifertarandus, "abundance")
plot(sim$Lynxlynx, "abundance")
# add processes ----------------------------------------------------------------

# To simplify the example, we just add the same processes for all species here
sim$add_process(
  species = species_names,
  process_name = "suitability_influence_population_parameter",
  process_fun = function() {
    species_suitability_name <- paste0(self$name, "_suitability_cropped_modified")
    
    self$traits[["carryingCapacity"]] <-
      self$traits[["maxCarryingCapacity"]] * self$sim$environment$current[[species_suitability_name]]
    
    self$traits[["reproductionRate"]] <-
      self$traits[["maxReproductionRate"]] * self$sim$environment$current[[species_suitability_name]]
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
        self$traits[["reproductionRate"]],
        self$traits[["carryingCapacity"]]
      )
  },
  execution_priority = 3
)
# 
# sim$add_process(
#   species = species_names,
#   process_name = "adult_mortality",
#   process_fun = function() {
#     
#     self$traits[["overpopulation"]] <-
#       1 / (
#         self$traits[["abundance"]] /
#           self$traits[["carryingCapacity"]]
#       )
#     self$traits[["overpopulation"]][self$traits[["overpopulation"]] > 1] <- 1
#     
#     self$traits[["abundance"]] <-
#       matrix(
#         rbinom(
#           ncell(self$traits[["abundance"]]),
#           self$traits[["abundance"]],
#           self$traits[["yearlySurvivalProbability"]] *
#             self$traits[["overpopulation"]] *
#             self$sim$environment$current[[paste0(self$name, "_suitability_cropped_modified")]]
#         ),
#         nrow = nrow(self$traits[["abundance"]]),
#         ncol = ncol(self$traits[["abundance"]])
#       )
#   },
#   execution_priority = 4
# )

sim$add_process(
  species = species_names,
  process_name = "reproduction_age_structured",
  process_fun = function() {
    
    # calculate how many juveniles are produced
    self$traits[["juvenileAbundance"]] <- self$traits[["abundance"]] * self$traits[["reproductionRate"]]
    
    # how many of the adult population survive, based on the environment suitability
    self$traits[["abundance"]] <-
      self$traits[["abundance"]] * self$sim$environment$current[[paste0(self$name, "_suitability_cropped_modified")]]
    
    # how many of the juveniles grow up and become adults
    self$traits[["abundance"]] <-
      self$traits[["abundance"]] +
      self$traits[["juvenileAbundance"]] *
      (1 - self$traits[["juvenileAbundance"]] / self$traits[["carryingCapacity"]])
    
    # make sure we don't exceed the carrying capacity
    exeed_populations <- self$traits[["abundance"]] > self$traits[["carryingCapacity"]] #check where abundance exceeds the carrying capacity - result is TRUE or FALSE
    exeed_populations[is.na(exeed_populations)] <- FALSE # handle missing values
    self$traits[["abundance"]][exeed_populations] <-
      self$traits[["carryingCapacity"]][exeed_populations] # where populations exceed carrying capacity, the abundance is changed to match carrying capacity
    
  },
  execution_priority = 5
)


sim$add_process(
  species = species_names,
  process_name = "dispersal_process",
  process_fun = function() {
    # weighted dispersal
    # i.e. individuals disperse more likely into more suitable cells
    self$traits[["abundance"]] <- dispersal(
      abundance = self$traits[["abundance"]],
      weights = self$sim$environment$current[[paste0(self$name, "_suitability_cropped_modified")]],
      dispersal_kernel = self$traits[["dispersalKernel"]])
  },
  execution_priority = 6
)


sim$add_process(
  process_name = "truncate",
  process_fun = function() {
    # just because there is nothing in reality like 0.5 individuals
    for (i in self$globals[["alive_species"]]) {
      self[[i]]$traits[["abundance"]] <- trunc(self[[i]]$traits[["abundance"]])
    }
  },
  execution_priority = 7
)


sim$add_process(
  process_name = "track_stats",
  process_fun = function() {
    for (i in self$globals[["alive_species"]]) {
      current_abu <- sum(self[[i]]$traits[["abundance"]], na.rm = TRUE) # sum up all abundances across cells
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
        current_abu # for sps still alive save total abundance for each time step
      
      self$globals[[i]][["n_occupied"]][[self$get_current_time_step()]] <-
        sum(self[[i]]$traits[["abundance"]] > 1, na.rm = TRUE) # sum up all cells occupied by each alive sps
      
      self$globals[[i]][["n_juveniles"]][[self$get_current_time_step()]] <- # sum up all juveniles (INSERTED BY ME COULD BE WRING IT SHOULD BE SIMILIAR TO n_abundance THINK ITS MISSING A STEP)
        sum(self[[i]]$traits[["juvenileAbundance"]] > 1, na.rm = TRUE)
    }
  },
  execution_priority = 8
)

# saving the results -----------------------------------------------------------
# OUTPUT FILE NAME STRUCTURE = SCENARIO_BIOME_REGION_TIME_SPECIES_VARIABLE.tif

save_string <- here("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/results06Feb2025v2")
# Note: Saving the results is a process that takes the longest time
# because writing a raster to disk is slow
# So think about when you want to save results (each time step vs jsut the last one)
sim$add_process(
  process_name = "save_results",
  process_fun = function() {
    
    for (species in species_names) {
      # suffix with SCENARIO, BIOME, REGION <- THIS SHOULD BE CHNAGED EACH TIME WE RUN THE MODEL !!!!!!!!!!!
      suffix <- "BAU_Tropical_Asia_"
      save_species(
        # pass the species object
        self[[species]],
        # specify traits we want to save
        traits = c("abundance","reproductionRate"),
        # a prefix for each time step
        prefix = paste0(suffix, sprintf("%03d", self$get_current_time_step()), "_"),
        # where should it be saved
        path = save_string,
        overwrite = TRUE
      )
    }
  },
  execution_priority = 9
)

# run simulation ---------------------------------------------------------------

set_verbosity(1L)
print("starting simulation")
sim$begin()
print("simulation finished")

################################
# PRINTING SIMULATION SETTINGS #
################################

# output file path
output_file <- here("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/results06Feb2025v2", 
                    "metaRangeSimulationSettings.txt")

# define the species names
species_names <- species_traits$Species
sink(output_file)

# write overall summary of simumlation
cat("### Overall Simulation Summary ###\n")
print(summary(sim))
cat("\n========================================\n\n")

# go through each species and print a summary for each
for (species in species_names) {
  cat("\nSummary for:", species, "\n")  # add a header
  print(summary(sim[[species]]))  
  cat("\n--------------------------------\n")  
}
sink()