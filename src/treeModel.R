###################################
# RUNNING THE metaRange FOR TREES #
###################################
# Ines Silva
# 03 Mar 2025

# GOAL: Running the model for Tree species

# packages
library(terra)
library(here)
library(metaRange)
library(readr)
library(raster)
library(sf)
library(tools) # for file without paths

###################
# metaRange MODEL #
###################

# import Species Trait Dataframe -----------------------------------------------
tree_species_traits <- read.csv(file.path(dirinput, "tree_metarangeSpeciesDataframe.csv"))

# setting up the simulation ----------------------------------------------------

# setup
set_verbosity(2L) # 0L for no output, 1L for progress updates, 2L for debug
options(scipen = 999)
set.seed(1)

# simulation parameters
sim_name <- "example_01"

# Landscape --------------------------------------------------------------------

# load the environment
sim_env <- sds(list.files(dirinput,
                          pattern = "_cropped_modified_reprojectedKm.tif", full.names = TRUE))

invisible(gc())
##################### HERE THE PATH TO THE ENVIRONMENT FILES SHOULD BE THE suitabilities folder

# create a simulation object ---------------------------------------------------

sim <- create_simulation(sim_env)
invisible(gc())

# We have already created our suitability layer, so we can just add them to the simulation
# in the order they are in the SDS.
# To have an aditional burn-in period, we set the time layer mapping to 1 for the first 5 time steps
# > c(rep(1, 5), seq_len(min(nlyr(sim_env))))
# >  1  1  1  1  1  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

sim$set_time_layer_mapping(c(rep(1, 5), seq_len(min(nlyr(sim_env)))))

# add species ------------------------------------------------------------------

# we loop over the species_traits data frame and add the species to the simulation
for (i in seq_len(nrow(tree_species_traits))) {
  this_species <- tree_species_traits[["Species"]][i]
  
  # "register" the species with the simulation
  sim$add_species(this_species)
  
  # add traits that need to bes strored at the population level
  sim$add_traits(
    species = this_species,
    population_level = TRUE,
    
    "abundance" = tree_species_traits[["initialAbundance"]][i],
    "reproductionRate" = tree_species_traits[["reproductionRate"]][i],
    "carryingCapacity" = tree_species_traits[["carryingCapacity"]][i],
    "yearlySurvivalRate" = tree_species_traits[["yearlySurvivalRate"]]
  )
  
  # add traits that are the same for all populations of a species
  sim$add_traits(
    species = this_species,
    population_level = FALSE,
    "dispersalDistance" =  tree_species_traits[["dispersalDistance"]][i],
    "maxReproductionRate" = tree_species_traits[["reproductionRate"]][i],
    "maxCarryingCapacity" = tree_species_traits[["carryingCapacity"]][i],
    
    # the following is just a simple kernel, but you can use any function / dispersal kernel you like
    "dispersalKernel" = calculate_dispersal_kernel(
      max_dispersal_dist = as.integer(tree_species_traits[["dispersalMaxDistance"]][i]),
      kfun = negative_exponential_function,
      mean_dispersal_dist = tree_species_traits[["dispersalDistance"]][i] / 2,
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
    "mean_abundance" = vector("numeric", sim$number_time_steps),
    "mean_rrate" = vector("numeric", sim$number_time_steps),
    "mean_ccap" = vector("numeric", sim$number_time_steps),
    "n_occupied" = vector("numeric", sim$number_time_steps)
  )
}
do.call(sim$add_globals, species_sum_abundance)

# add processes ----------------------------------------------------------------

sim$add_process(
  species = species_names,
  process_name = "suitability_influence_population_parameter",
  process_fun = function() {
    species_suitability_name <- paste0(self$name, "_suitability_cropped_modified_reprojectedKm")
    
    self$traits[["carryingCapacity"]] <-
      self$traits[["maxCarryingCapacity"]] * self$sim$environment$current[[species_suitability_name]]
    
    self$traits[["reproductionRate"]] <-
      self$traits[["maxReproductionRate"]] * self$sim$environment$current[[species_suitability_name]]
  },
  execution_priority = 1
)

# Beverton & Holt function #

beverton_holt <- function(abundance, reproduction_rate, carrying_capacity, survival_rate) {
  # Safeguarding the input
  # you may remove this part if you are sure that the input is correct
  survival_rate <- ifelse(survival_rate > 1, 1, survival_rate)
  survival_rate <- ifelse(survival_rate < 0, 0, survival_rate)
  reproduction_rate <- ifelse(reproduction_rate < 0, 0, reproduction_rate)
  
  
  abundance <- abundance * survival_rate
  abundance_t1 <- (reproduction_rate * abundance) /
    (1 + ((reproduction_rate - 1) / carrying_capacity) * abundance)
  abundance_t1[abundance_t1 < 0] <- 0
  return(abundance_t1)
}

sim$add_process(
  species = species_names,
  process_name = "demography_BevertonHolt",
  process_fun = function(){
    self$traits[["abundance"]] <- beverton_holt(abundance = self$traits[["abundance"]],
                                                reproduction_rate = self$traits[["reproductionRate"]],
                                                carrying_capacity = self$traits[["carryingCapacity"]],
                                                survival_rate = self$traits[["yearlySurvivalRate"]] )
  },
  execution_priority = 2
)


# sim$add_process(
#   species = species_names,
#   process_name = "reproduction",
#   process_fun = function() {
#     self$traits[["abundance"]] <-
#       ricker_reproduction_model(
#         self$traits[["abundance"]],
#         self$traits[["reproductionRate"]],
#         self$traits[["carryingCapacity"]]
#       )
#   },
#   execution_priority = 2
# )

sim$add_process(
  species = species_names,
  process_name = "dispersal_process",
  process_fun = function() {
    # weighted dispersal
    # i.e. individuals disperse more likely into more suitable cells
    self$traits[["abundance"]] <- dispersal(
      abundance = self$traits[["abundance"]],
      weights = self$sim$environment$current[[paste0(self$name, "_suitability_cropped_modified_reprojectedKm")]],
      dispersal_kernel = self$traits[["dispersalKernel"]])
  },
  execution_priority = 3
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
      
      self$globals[[i]][["mean_abundance"]][[self$get_current_time_step()]] <-
        mean(self[[i]]$traits[["abundance"]], na.rm = TRUE) # mean abundance of species in the landscape
      
      self$globals[[i]][["mean_rrate"]][[self$get_current_time_step()]] <-
        mean(self[[i]]$traits[["reproductionRate"]], na.rm = TRUE) # mean reproduction rate
      
      self$globals[[i]][["mean_ccap"]][[self$get_current_time_step()]] <-
        mean(self[[i]]$traits[["carryingCapacity"]], na.rm = TRUE) # mean carrying capacity
      
      self$globals[[i]][["n_occupied"]][[self$get_current_time_step()]] <-
        sum(self[[i]]$traits[["abundance"]] > 1, na.rm = TRUE) # sum up all cells occupied by each alive sps
      
    }
  },
  execution_priority = 6
)

# saving the results -----------------------------------------------------------

# OUTPUT FILE NAME STRUCTURE = SCENARIO_BIOME_REGION_TIME_SPECIES_VARIABLE.tif

# Note: Saving the results is a process that takes the longest time
# because writing a raster to disk is slow
# So think about when you want to save results (each time step vs jsut the last one)

sim$add_process(
  process_name = "save_results",
  process_fun = function() {
    
    for (species in species_names) {
      # suffix with SCENARIO, BIOME, REGION <- THIS SHOULD BE CHNAGED EACH TIME WE RUN THE MODEL !!!!!!!!!!!
      suffix <- "SSP1_Boreal_SMALL_"
      save_species(
        # pass the species object
        self[[species]],
        # specify traits we want to save
        traits = "abundance",
        # a prefix for each time step
        prefix = paste0(suffix, sprintf("%03d", self$get_current_time_step()), "_"),
        # where should it be saved
        path = dirout,
        overwrite = TRUE
      )
    }
  },
  execution_priority = 7
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
output_file <- file.path(dirout, "simulationSettings.txt")

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

# remove unecessary objects
rm(i, output_file, species, species_names, species_sum_abundance, this_species)

# CHECKING MODEL RESULTS (deleteLater when working)

tiff(file.path(dirout, "MeanAbundancePerCell_plots.tiff"),width = 300, height = 230, units = "mm", res = 1200, compression = "lzw")

par(mfrow=c(2,2))

plot(
  sim$globals[["Alcesalces"]][["mean_abundance"]],
  type = "l",
  xlab = "Time",
  ylab = "Abundance",
  main = "Alcesalces"
)
plot(
  sim$globals[["Cervuselaphus"]][["mean_abundance"]],
  type = "l",
  xlab = "Time",
  ylab = "Abundance",
  main = "Cervuselaphus"
)
plot(
  sim$globals[["Lynxlynx"]][["mean_abundance"]],
  type = "l",
  xlab = "Time",
  ylab = "Abundance",
  main = "Lynxlynx"
)
plot(
  sim$globals[["Rangifertarandus"]][["mean_abundance"]],
  type = "l",
  xlab = "Time",
  ylab = "Abundance",
  main = "Rangifertarandus"
)
mtext(paste0("Mean Abundance Per Cell Over Time", runname), side = 3, line = - 2, outer = TRUE)
dev.off()

