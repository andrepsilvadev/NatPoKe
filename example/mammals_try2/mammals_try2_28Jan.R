######################################
# 4th TRY WITH metaRange FOR MAMMALS #
################ MIS #################
# 28 Jan 2025


# GOAL: Running the model for a couple of mammals species
# use global suitability landscape and repeat layers one 25 time so in the end
# the rasters have 25 layers, one for eahc time step needed

# Output files of the model should follow this structure:
  # SCENARIO_BIOME_REGION_TIME_SPECIES_VARIABLE.tif
  # change in "save_results" process the prefix line to accomodate this


# RANDOM DUMMY MISTAKES TO AVOID
  # 1 - species name CANNOT have spaces or "_"
  # 2 - species for which we do not have a suitability raster cannot be in the .csv file
  # 3 - max_dispersal_dist HAS to be an INTEGRER! So I added as.integer() into that line 
  # 4 - when this "self$sim$environment$current[[species_suitability_name]]" appears make sure species_suitability is the EXACT same name as the name of the raster imported with sds()


# packages
library(terra)
library(here)
library(metaRange)
library(readr)
library(raster)
library(sf)
library(tools) # for file without paths

# Check if environment data is available
stopifnot(file.exists(here("example/mammals_try2/clean_data_2species/Lynxlynx_suitability.tif")))
stopifnot(file.exists(here("example/mammals_try2/clean_data_2species/Alcesalces_suitability.tif")))
stopifnot(file.exists(here("example/mammals_try2/clean_data_2species/target_metarange_mammals20250110.csv")))

##########################################################
# CREATE DUMMY RASTER BASED ON SRIT GLOBAL SUITABILITIES #
##########################################################

# Here SRTIT Databases Global suitability rasters for 2 species were imported,
# cropped to a smaller extent in sweden and them duplicated 25 times so that the
# landscape to import to metaRange is "dynamic"

# # list of raster files
# raster_files <- list.files(here("example/mammals_try2/clean_data_2species"),
#                             pattern = "_suitability.tif$", full.names = TRUE)
# 
# # function to duplicate layers in a raster
# duplicate_layers <- function(raster, times) {
#    # duplicate layers
#    new_r <- do.call(c, rep(list(raster), times))
#    return(new_r)
#  }
# 
# # output folder
# output_folder <- here("example/mammals_try2/clean_data_2species")
# 
# # loop through each raster file
# for (r in raster_files) {
#    # read the raster
#    sp_raster <- rast(r)
# 
#    # define the bounding box
#    bbox_SW <- ext(6.306152, 17.248535, 59.288332, 62.769811)
# 
#    # crop the raster to the bounding box
#    cropped_raster <- terra::crop(sp_raster, bbox_SW)
# 
#    # duplicate the layers 25 times
#    duplicated_raster <- duplicate_layers(cropped_raster, times = 25)
# 
#    # save modified raster
#    file_name <- file_path_sans_ext(basename(r))
#    output_path <- file.path(output_folder, paste0(file_name, "_cropped_modified.tif"))
#    writeRaster(duplicated_raster, output_path, overwrite = TRUE)
#    invisible(gc())
#  }
# 
# # check rasters
# alce <- rast(here("example/mammals_try2/clean_data_2species/Alcesalces_suitability_cropped_modified.tif"))
# plot(alce)


####################
# model simulation #
####################

# setting up the simulation ----------------------------------------------------

# setup
set_verbosity(2L) # 0L for no output, 1L for progress updates, 2L for debug
options(scipen = 999)
set.seed(1)
save_string <- here("example/mammals_try2/")

# simulation parameters
sim_name <- "example_01"

species_traits <- read.csv(here("example/mammals_try2/clean_data_2species/target_metarange_mammals20250110.csv"))
species_traits[2,5] <- 1000
species_traits[1,5] <- 1000
species_traits[1,3] <- 10000
species_traits[2,3] <- 10000

# image parameters
wid <- 2000
hgt <- 1400
unt <- "px"
ppi <- 300

# Landscape --------------------------------------------------------------------

# load the environment
sim_env <- sds(list.files(here("example/mammals_try2/clean_data_2species"), pattern = "_cropped_modified.tif", full.names = TRUE))


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
    "reproductionRate" = species_traits[["reproduction_rate"]][i],
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

# add processes ----------------------------------------------------------------

# To simplify the example, we just add the same processes for all species here
sim$add_process(
  species = species_names,
  process_name = "suitability_influence_population_parameter",
  process_fun = function() {
    species_suitability_name <- paste0(self$name, "_suitability_cropped_modified")
    
    self$traits[["carrying_capacity"]] <-
      self$traits[["max_carrying_capacity"]] * self$sim$environment$current[[species_suitability_name]]
    
    self$traits[["reproductionRate"]] <-
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
        self$traits[["reproductionRate"]],
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
    self$traits[["juvenile_abundance"]] <- self$traits[["abundance"]] * self$traits[["reproductionRate"]]
    
    # how many of the adult population survive, based on the environment suitability
    self$traits[["abundance"]] <-
      self$traits[["abundance"]] * self$sim$environment$current[[paste0(self$name, "_suitability_cropped_modified")]]
    
    # how many of the juveniles grow up and become adults
    self$traits[["abundance"]] <-
      self$traits[["abundance"]] +
      self$traits[["juvenile_abundance"]] *
      (1 - self$traits[["juvenile_abundance"]] / self$traits[["carrying_capacity"]])
    
    # make sure we don't exceed the carrying capacity
    exeed_populations <- self$traits[["abundance"]] > self$traits[["carrying_capacity"]] #check where abundance exceeds the carrying capacity - result is TRUE or FALSE
    exeed_populations[is.na(exeed_populations)] <- FALSE # handle missing values
    self$traits[["abundance"]][exeed_populations] <-
      self$traits[["carrying_capacity"]][exeed_populations] # where populations exceed carrying capacity, the abundance is changed to match carrying capacity
    
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
      weights = self$sim$environment$current[[paste0(self$name, "_suitability_cropped_modified")]],
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
        sum(self[[i]]$traits[["juvenile_abundance"]] > 1, na.rm = TRUE)
    }
  },
  execution_priority = 6
)

# saving the results -----------------------------------------------------------
# OUTPUT FILE NAME STRUCTURE = SCENARIO_BIOME_REGION_TIME_SPECIES_VARIABLE.tif

save_string <- here("example/mammals_try2/results_28Jan")
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
        traits = c("abundance", "reproductionRate"),
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

# save results in .csv file ----------------------------------------------------
# 
# # optionally save some results as csv
# res_df <- data.frame()
# for (i in species_names) {
#   res_df <- rbind(
#     res_df,
#     data.frame(
#       species = i,
#       time = 1:sim$number_time_steps,
#       alive = i %in% sim$globals[["alive_species"]],
#       n_abundance = sim$globals[[i]][["n_abundance"]],
#       n_occupied = sim$globals[[i]][["n_occupied"]],
#       n_juveniles = sim$globals[[i]]["n_juveniles"]
#     )
#   )
# }
# write.csv(res_df, paste0(save_string, "/", sim_name, "_res_df.csv"), row.names = FALSE)
# 



