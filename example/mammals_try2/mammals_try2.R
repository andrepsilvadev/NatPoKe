######################################
# 2nd TRY WITH metaRange FOR MAMMALS #
################ MIS #################
# 22 Jan 2025


# GOAL: Running the model for a couple of mammals species

# Output files of the model should follow this structure:
  # SCENARIO_BIOME_REGION_TIME_SPECIES_VARIABLE.tif
  # change in "save_results" process the prefix line to accomodate this

# packages
library(terra)
library(here)
library(metaRange)
library(readr)
library(raster)
library(sf)

# Check if environment data is available
stopifnot(file.exists(here("example/mammals_try2/Lynxlynx_suitability.tif")))
stopifnot(file.exists(here("example/mammals_try2/Alcesalces_suitability.tif")))
stopifnot(file.exists(here("example/mammals_try2/target_metarange_mammals20250110.csv")))

###############################################
# crop suitability raster by a smaller extent #
###############################################

# rast_alces <- rast("example/mammals_try2/Alcesalces_suitability.tif")
# 
# bbox_SW <- ext(12.8, 17, 59.3, 62.4)  # coordinates for somewhere in Sweden (above Upsalla)
# rast_alces_cropped <- terra::crop(rast_alces, bbox_SW)
# 
# # write new small raster
# writeRaster(rast_alces_cropped, "example/mammals_try2/alces_suitability_cropped.tif")

# # remove full raster
# rm(rast_alces)

###############################################################
# creating suitability rasters with multiple layers (dynamic) #
###############################################################

r <- terra::rast(
  here::here("example/mammals_try2", "alces_suitability_cropped.tif")
)

for (i in seq_len(nrow(species_traits))) {
  species_suitability <- 1 - abs(r - species_traits$optimum_forest_cover[i])
  species_suitability <- rep(species_suitability, 20)

  for (j in seq_len(nlyr(species_suitability))) {
    species_suitability[[j]] <- species_suitability[[j]] + runif(1, -0.25, 0.25)
  }
  species_suitability <- clamp(species_suitability, 0, 100)

  writeRaster(
    species_suitability,
    here::here("example/mammals_try2", paste0("species_suitability_", species_traits$species[i], ".tif")),
    overwrite = TRUE
  )
}

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

species_traits <- read.csv(here("example/mammals_try2/target_metarange_mammals20250110.csv"))

# add random number for this until ANDRE decides what to do
species_traits$optimum_forest_cover <- c(70, 50)

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

save_string <- here("example/mammals_try2")
# Note: Saving the results is a process that takes the longest time
# because writing a raster to disk is slow
# So think about when you want to save results (each time step vs jsut the last one)
sim$add_process(
  process_name = "save_results",
  process_fun = function() {
    
    for (species in self$globals[["alive_species"]]) {
      save_species(
        # pass the species object
        self[[species]],
        # specify traits we want to save
        traits = c("abundance", "reproduction_rate"),
        # a prefix for each time step
        prefix = paste0(sprintf("%03d", self$get_current_time_step()), "-"),
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

# optionally save some results as csv
res_df <- data.frame()
for (i in species_names) {
  res_df <- rbind(
    res_df,
    data.frame(
      species = i,
      time = 1:sim$number_time_steps,
      alive = i %in% sim$globals[["alive_species"]],
      n_abundance = sim$globals[[i]][["n_abundance"]],
      n_occupied = sim$globals[[i]][["n_occupied"]],
      n_juveniles = sim$globals[[i]]["n_juveniles"]
    )
  )
}
write.csv(res_df, paste0(save_string, "/", sim_name, "_res_df.csv"), row.names = FALSE)




