## Name: mammalModel.R ##
## Author: Inês Silva & Stefan Fallert ##
## Date: 11 Feb 2025
## Description: Build population dynamics model for mammals species using the 
## metaRange and including the influence of suitbaility on species, beverton& holt
## demography and dispersal. It initiates species within the IUCN range (needs
## IUCN spatial file). Runs 3 replicates. Saves outputs as .tif for abundance,
## reproductionRate and dispersalChange, plus a .csv file with TNIND, MNIND, 
## mean_repRate, mean_carrCap and occupancy, all of them per species, timestep and replicate.


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
## 1 - species names CANNOT have spaces or "_", but "." is ok!
## 2 - species for which we do not have a suitability raster cannot be in the .csv file
## 3 - max_dispersal_dist HAS to be an INTEGRER! So I added as.integer() into that line 
## 4 - when this "self$sim$environment$current[[species_suitability_name]]" appears make sure species_suitability is the EXACT same name as the name of the raster imported with sds()



##########
# Step 1 # Configure Targets and Model Parameters
##########

# import species traits df
species_traits <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv"))

# correction for biome
if (target_biome == "Tropical & Subtropical Moist Broadleaf Forests") {
  biome <- "tropical"
} else if (target_biome == "Boreal Forests/Taiga") {
  biome <- "boreal"
}
# clean target biome and region names (removes special characters like /, &, and space)
target_biome <- gsub("[/& ]", "", target_biome)
target_region <- gsub("[/& ]", "", target_region)

# correction for region
if (target_region == "Europe") {
  target_region <- "Europe+Asia"
}

# select target species from dataframe to avoid errors
target_species <- species_traits$Species # by default, this selects *all* species in the traits df

# set number of replicates
n_replicates <- 3 # total nº of replicates
all_reps_list <- list() # list to save the .csv

# burn-in
burnin_t <- 25 # nº of years the model should consider

# global setup options
set_verbosity(2L) # 0L = silent, 1L = progress updates, 2L =  debug
options(scipen = 999) # prevents scientific notation for large numbers
set.seed(1) # reproducibility

##########
# Step 2 # RUN THE METARANGE MODEL FOR MAMMAL SPECIES
##########

# load iucn's species ranges (to initiate species only within their range)
iucn <- vect("./data/externaldata/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
iucn$sci_name <- gsub(" ", ".", iucn$sci_name)
invisible(gc())

for (replicateN in 1:n_replicates) {
  
  # Step 1 # Add landscape for all target species 
  
  sim_name <- paste0(replicateN, "_", str_replace_all(target_biome, " ", ""), "_", target_region, "_Mammals")
  sim_env <- sds(list.files(dirinput,
                            pattern = paste0("_", biome, "_", future_scenario, "_cropped_reprojectedm.tif"), full.names = TRUE))
  invisible(gc())
  
  # Step 2 # Create a simulation object 
  
  sim <- create_simulation(sim_env,
                           ID = sim_name)
  invisible(gc())
  
  # Step 3 # Add a timesteps "layer"
  
  sim$set_time_layer_mapping(c(rep(1, burnin_t), seq_len(min(nlyr(sim_env)))))
  
  # Step 4 # Add species & traits 
  
  for (i in seq_len(nrow(species_traits))) {
    this_species <- species_traits[["Species"]][i]
    
    # "register" the species with the simulation
    sim$add_species(this_species)
    
    iucn_sps <- iucn[iucn$sci_name == species_traits$Species[i]]
    iucn_sps <- project(iucn_sps, crs(sim_env[[i]]))
    #plot(iucn_sps)
    range_raster <- rasterize(iucn_sps, sim_env[[i]], values = 1)
    range_raster <- terra::subst(range_raster, NA, 0)
    
    # traits that need to be stored at the population level
    sim$add_traits(
      species = this_species,
      population_level = TRUE,
      "abundance" = as.matrix(range_raster * species_traits[["initialAbundance"]][i], wide = TRUE),
      "abundance_before" = 0,
      "reproductionRate" = species_traits[["reproductionRate"]][i],
      "carryingCapacity" = species_traits[["carryingCapacity"]][i],
      "yearlySurvivalRate" = species_traits[["yearlySurvivalRate"]][i])
    
    # traits that are the same for all populations of a species
    sim$add_traits(
      species = this_species,
      population_level = FALSE,
      "dispersalDistance" =  species_traits[["dispersalDistance"]][i],
      "maxReproductionRate" = species_traits[["reproductionRate"]][i],
      "maxCarryingCapacity" = species_traits[["carryingCapacity"]][i],
      
      # simple dispersal kernel
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
  
  # Step 5 # Add global variables
  
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
  
  # Step 6 # Add process 
  
  # Suitability influence on the environment  
  sim$add_process(
    species = species_names,
    process_name = "suitability_influence_population_parameter",
    process_fun = function() {
      species_suitability_name <- paste0(self$name, "_", biome, "_", future_scenario, "_cropped_reprojectedm")
      
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
      # save the number of individuals before dispersing
      self$traits[["abundance_before"]] <- trunc(self$traits[["abundance"]])
      # weighted dispersal
      # i.e. individuals disperse more likely into more suitable cells
      abundance_after <- dispersal(
        abundance = self$traits[["abundance"]],
        weights = self$sim$environment$current[[paste0(self$name, "_", biome, "_", future_scenario, "_cropped_reprojectedm")]],
        dispersal_kernel = self$traits[["dispersalKernel"]])
      
      # adding randomness?
      abundance_after <- matrix(rpois(ncell(abundance_after), abundance_after),
                                nrow = nrow(self$traits[["abundance"]]),
                                ncol = ncol(self$traits[["abundance"]]))
      self$traits[["abundance"]] <- abundance_after
      # calculate the dispersal change
      self$traits[["dispersalChange"]] <- self$traits[["abundance"]] - self$traits[["abundance_before"]]
    },
    execution_priority = 3
  )
  
  # Ensure whole individuals & randomness
  sim$add_process(
    process_name = "truncate",
    process_fun = function() {
      # keep individuals whole, because there is nothing in like 0.5 individual
      for (i in self$globals[["alive_species"]]) {
        self[[i]]$traits[["abundance"]] <- trunc(self[[i]]$traits[["abundance"]])
      }
    },
    execution_priority = 4
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
    execution_priority = 5
  )
  
  
  # saving results
  sim$add_process(
    process_name = "saving_traits",
    process_fun = function() {
      for (species in species_names) {
        
        # save all replicates
        results_paths <- save_species(
          # pass the species object
          self[[species]],
          # specify traits we want to save
          traits = c("abundance", "reproductionRate", "dispersalChange"),
          # a prefix for each time step
          prefix = paste0(sim$ID, sprintf("%03d", self$get_current_time_step()), "_"),
          # where should it be saved
          path = dirout,
          overwrite = TRUE
        )
        
        self$globals$results_paths <- c(self$globals$results_paths, results_paths)
      }
    },
    execution_priority = 6
  )
  
  
  # Step 7 # STARTING THE SIMULATION
  
  set_verbosity(1L)
  print("Simulation begin")
  sim$begin()
  print("Simulation finished")
  
  # Step 8 # Save additional outputs (for easy diagnostics & plotting)
  
  ## .csv file with global variables ##
  
  # create a list to store dfs
  df_list <- list()
  # save total number of individuals (TNIND) in the landscape for each species
  for (species in target_species) {
    df_list[[species]] <-  data.frame(sim$globals[[species]]$n_abundance,
                                      sim$globals[[species]]$mean_abundance,
                                      sim$globals[[species]]$mean_rrate,
                                      sim$globals[[species]]$mean_ccap,
                                      sim$globals[[species]]$n_occupied) %>% 
      mutate(Scenario = future_scenario,
             biome = target_biome,
             region = target_region,
             species = species,
             timestep = row_number(),
             rep = sim$ID)
  }
  
  # combine all species together
  TNIND_yr <- do.call(rbind, df_list)
  colnames(TNIND_yr) <- c("TNIND", "MNIND", "mean_repRate", "mean_carrCap",
                          "occupancy", "future_scenario", "biome", "region",
                          "species", "timestep", "rep")
  
  # append the current replicate's data to the full list
  all_reps_list[[length(all_reps_list) + 1]] <- TNIND_yr
  
  
  ## settings file (currently saving the last replicate) ##
  
  sink(file.path(dirout, "simulationSettings.txt"))
  # write overall summary of simulation
  cat("### Overall Simulation Summary ###\n")
  print(summary(sim))
  cat("\n========================================\n\n")
  # write a specific summary for the species
  for (species in target_species) {
    cat("\nSummary for:", species, "\n")  
    print(summary(sim[[species]]))  
    cat("\n--------------------------------\n")
  }
  
  sink()
}

# Step 9 # Finish saving the .csv file (this is done outside the loop purposefully)

all_TNIND_data <- do.call(rbind, all_reps_list) # combine all replicates into one big data frame
# write to .csv
write.csv(all_TNIND_data,
          file = file.path(dirout, paste0("TNIND_yr_", runname, ".csv")),
          row.names = FALSE)
