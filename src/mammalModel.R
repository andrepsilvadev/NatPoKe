#####################################
# RUNNING THE metaRange FOR MAMMALS #
#####################################
# Ines Silva
# 11 Feb 2025

# GOAL: Running the model for mammals species

#######################
# NAVIGATION WARNINGS #
#######################

# Model input files
    ## (1) Global Suitability Landscapes - for each species to model
    ## (2) Species Trait Dataframe
          ## if for some reason we want to change the initial values for target
          ## sps we need to re-writre the csv in input folder 
          ## write_csv(species_traits, file = file.path(dirinput,"metaRangeSpeciesDataframe.csv"))

# Model Output files
## should follow this structure:
    ## SCENARIO_BIOME_REGION_TIME_SPECIES_VARIABLE.tif
    ## in "save_results" process change in the prefix line to accomodate this


# RANDOM DUMMY MISTAKES TO AVOID
    ## 1 - species names CANNOT have spaces or "_"
    ## 2 - species for which we do not have a suitability raster cannot be in the .csv file
    ## 3 - max_dispersal_dist HAS to be an INTEGRER! So I added as.integer() into that line 
    ## 4 - when this "self$sim$environment$current[[species_suitability_name]]" appears make sure species_suitability is the EXACT same name as the name of the raster imported with sds()


##########
# SET UP #
##########

# import trait dataframe
species_traits <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv"))

# select target biome (only one)
target_biome <- "Boreal Forests/Taiga" # Tropical & Subtropical Moist Broadleaf Forests OR Boreal Forests/Taiga

# select target region (only one)
#target_region <- "Europe" # "North America" OR "South America" OR "Europe" OR "Asia" OR "Antarctica" OR "Africa" OR "Australia" OR "Oceania"     
target_region <- "Sweden"

# select target species (multiple sps are allowed)
target_species <- species_traits$Species

# setup 
set_verbosity(2L) # 0L for no output, 1L for progress updates, 2L for debug
options(scipen = 999)
set.seed(1)

##########################
# RUNNING THE SIMULATION #
##########################

for (species in target_species) {
  
  ## Step 1 # Subset trait data for target species 
  
  species_traits <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv")) %>% 
    dplyr::filter(Species == species)
  
  sim_name <- paste0("Mammals_", species, "_", Sys.Date())

  ## Step 2 - Add landscape
  
  sps_landscape <- rast(file.path(dirinput, paste0(species,"_suitability_cropped_modified_reprojectedKm.tif")))
  sim_env <- sds(sps_landscape)
  invisible(gc())

  ## Step 3 - Create a simulation object 
  
  sim <- create_simulation(sim_env)
  invisible(gc())

  ## Step 4  - Add Timesteps layer
  
  # to have a burn-in period, we set the time layer mapping to 1 for the first x time steps
  sim$set_time_layer_mapping(c(rep(1, 100), seq_len(min(nlyr(sim_env)))))

  ## Step 5 - Add species & traits 
  
  for (i in seq_len(nrow(species_traits))) {
    this_species <- species_traits[["Species"]][i]
  
    # "register" the species with the simulation
    sim$add_species(species)
  
    # add traits that need to be stored at the population level
    sim$add_traits(
      species = this_species,
      population_level = TRUE,
    
        "abundance" = species_traits[["initialAbundance"]][i],
        "dispersal_change" = 0,
        "reproductionRate" = species_traits[["reproductionRate"]][i],
        "carryingCapacity" = species_traits[["carryingCapacity"]][i],
        "yearlySurvivalRate" = species_traits[["yearlySurvivalRate"]][i])
  
    # add traits that are the same for all populations of a species
    sim$add_traits(
      species = this_species,
      population_level = FALSE,
        "dispersalDistance" =  species_traits[["dispersalDistance"]][i],
        "maxReproductionRate" = species_traits[["reproductionRate"]][i],
        "maxCarryingCapacity" = species_traits[["carryingCapacity"]][i],
    
    # simple kernel, but you can use any function / dispersal kernel
    "dispersalKernel" = calculate_dispersal_kernel(
      max_dispersal_dist = as.integer(species_traits[["dispersalMaxDistance"]][i]),
      kfun = negative_exponential_function,
      mean_dispersal_dist = species_traits[["dispersalDistance"]][i],
    )
  )
}

  species_names <- sim$species_names()
    
  ## keep track of the species that are still alive 
  sim$add_globals("alive_species" = species_names)

  # Step 6 - Add global variables
  
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


  # Step 7 - Add process 

  # Suitability influence on the environment  
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
  
  # Demographic processes (Beverton & Holt)
  sim$add_process(
    species = species_names,
    process_name = "demography_BevertonHolt",
    process_fun = function(){
      self$traits[["abundance"]] <- beverton_holt(abundance = self$traits[["abundance"]],
                                                  reproduction_rate = self$traits[["reproductionRate"]],
                                                  carrying_capacity = self$traits[["carryingCapacity"]],
                                                  survival_rate = self$traits[["yearlySurvivalRate"]])
    },
    execution_priority = 2
  )
  
  # Dispersal
  sim$add_process(
    species = species_names,
    process_name = "dispersal_process",
    process_fun = function() {
      self$traits[["dispersal_change"]] <- trunc(self$traits[["abundance"]])
      # weighted dispersal
      # i.e. individuals disperse more likely into more suitable cells
      self$traits[["abundance"]] <- dispersal(
        abundance = self$traits[["abundance"]],
        weights = self$sim$environment$current[[paste0(self$name, "_suitability_cropped_modified_reprojectedKm")]],
        dispersal_kernel = self$traits[["dispersalKernel"]])
      self$traits[["dispersal_change"]] <- self$traits[["abundance"]] - self$traits[["dispersal_change"]]
    },
    execution_priority = 3
  )

  # Ensure whole individuals
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
  
  # Tracking statistics
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

  # Saving results
  sim$add_process(
    process_name = "save_results",
    process_fun = function() {
  
      for (species in species_names) {
        suffix <- paste0(str_replace_all(target_biome, " ", ""), "_", target_region, "_")
        save_species(
          # pass the species object
          self[[species]],
          # specify traits we want to save
          traits = c("abundance"
                     #, "reproductionRate", "dispersal_change"
                     ),
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
  
  # Step 8 - STARTING THE SIMULATION
  
  set_verbosity(1L)
  print(paste0("starting simulation for ", species))
  sim$begin()
  print(paste0("simulation finished for ", species))
  
  # Step 9 - Save a mean abundance per cell plot (for easy diagnostics)
  
  tiff(file.path(dirout, paste0("MeanAbundancePerCell", species, ".tiff")),
       width = 300, height = 230, units = "mm", res = 1200, compression = "lzw")
  # plotting mean abundance per cell
  plot(sim$globals[[species]][["mean_abundance"]],
       type = "l",
       xlab = "Time", ylab = "Mean Abundance Per Cell", main = species)
  dev.off()

  # Step 10 - Save a settings file
  
  sink(file.path(dirout, paste0("simulationSettings", species, ".txt")))
  # write overall summary of simulation
  cat("### Overall Simulation Summary ###\n")
  print(summary(sim))
  cat("\n========================================\n\n")
  # write a specific summary for the species
  cat("\nSummary for:", species, "\n")  
  print(summary(sim[[species]]))  
  cat("\n--------------------------------\n")  
  sink()
  
  # remove unecessary objects
  rm(i, species, species_names, species_sum_abundance, this_species)
}