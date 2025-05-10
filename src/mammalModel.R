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
# Step 1 # Configure Targets and Parameters
##########

# import species traits df
species_traits <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv"))

# clean target biome and region names (removes special characters like /, &, and space)
target_biome <- gsub("[/& ]", "", target_biome)
target_region <- gsub("[/& ]", "", target_region)

# select target species 
# by default, this selects *all* species in the traits df
target_species <- species_traits$Species

# set number of replicates
n_replicates <- 3 # total number of replicates to run
random_number <- 1 # should be random but i am impatient (change later)

# global setup options
set_verbosity(2L) # 0L = silent, 1L = progress updates, 2L =  debug
options(scipen = 999) # prevents use of scientific notation for large numbers
set.seed(1) # reproducibility

# # For each species, create an empty raster and store in a named list
cumulative_abundance <- lapply(target_species, function(sp) mean_abundance <- matrix(0, 763, 671)) # this needs to be with ifs because it need to match each landscape based on the region's name definied in the run.R
names(cumulative_abundance) <- target_species

##########
# Step 2 # RUN THE METARANGE MODEL FOR MAMMAL SPECIES
##########

for (replicateN in 1:n_replicates) {
  # select a random number inside the replicates to save
  current_replicate_n <- 1
  
  # Step 1 # Add landscape for all target species 

  sim_env <- sds(list.files(dirinput,
                            pattern = "_cropped_modified_reprojectedKm.tif", full.names = TRUE))
  invisible(gc())

  # Step 2 # Create a simulation object 
  
  sim <- create_simulation(sim_env,
                           ID = as.character(current_replicate_n))
  invisible(gc())
  
  # Step 3 # Add a timesteps "layer"
  
  sim$set_time_layer_mapping(c(rep(1, 100), seq_len(min(nlyr(sim_env)))))
  
  # Step 4 # Add species & traits 
  
  for (i in seq_len(nrow(species_traits))) {
    this_species <- species_traits[["Species"]][i]
    
    # "register" the species with the simulation
    sim$add_species(this_species)
    
    # add traits that need to be stored at the population level
    sim$add_traits(
      species = this_species,
      population_level = TRUE,
    
        "abundance" = species_traits[["initialAbundance"]][i],
        "abundance_before" = 0,
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
          mean_dispersal_dist = species_traits[["dispersalDistance"]][i] / 2,
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
  
  # Step 7 # Add process 
  
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
        # save the number of individuals before dispersing
        self$traits[["abundance_before"]] <- trunc(self$traits[["abundance"]])
        # weighted dispersal
        # i.e. individuals disperse more likely into more suitable cells
        self$traits[["abundance"]] <- dispersal(
          abundance = self$traits[["abundance"]],
          weights = self$sim$environment$current[[paste0(self$name, "_suitability_cropped_modified_reprojectedKm")]],
          dispersal_kernel = self$traits[["dispersalKernel"]])
        # calculate the dispersal change
        self$traits[["dispersal_change"]] <- self$traits[["abundance"]] - self$traits[["abundance_before"]]
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
    
    # setting a simulation name
    sim_name <- paste0(str_replace_all(target_biome, " ", ""), "_", target_region, "_Mammals_")
    
    # saving results of one random run (when current replicate number is the same as the random number defined in the beggining)
    sim$add_process(
      process_name = "saving_traits",
      process_fun = function() {
        if (as.integer(replicateN) == as.integer(sim$ID)){
          for (species in species_names) {
            results_paths <- save_species(
              # pass the species object
              self[[species]],
              # specify traits we want to save
              traits = c("abundance", "reproductionRate", "dispersal_change"),
              # a prefix for each time step
              prefix = paste0(sim_name, sprintf("%03d", self$get_current_time_step()), "_"),
              # where should it be saved
              path = dirout,
              overwrite = TRUE
            )
            self$globals$results_paths <- c(self$globals$results_paths, results_paths)
          }
        }
      },
      execution_priority = 7
    )
    
    
    sim$add_process(
      process_name = "accumulate_abundance",
      process_fun = function() {
        for (species in species_names) {
        #sp <- self$species_names  # Gets current species inside simulation
        cumulative_abundance[[species]] <- cumulative_abundance[[species]] + self$traits[["abundance"]]
      }
    },
      execution_priority = 7
    )

    # Step 8 # STARTING THE SIMULATION
    
    set_verbosity(1L)
    print("Simulation begin")
    sim$begin()
    print("Simulation finished")
    
    # Step 9 # Save additional outputs (for easy diagnostics & plotting)
    
    ## mean abundance per cell plots ##
    
    for (species in target_species) {
      tiff(file.path(dirout, paste0("MeanAbundancePerCell", species, ".tiff")),
           width = 300, height = 230, units = "mm", res = 1200, compression = "lzw")
      # plotting mean abundance per cell
      plot(sim$globals[[species]][["mean_abundance"]],
           type = "l",
           xlab = "Time", ylab = "Mean Abundance Per Cell", main = species)
      dev.off()
    }
    
    ## total number of individuals ##
    
    # create a list to store dfs
    df_list <- list()
    # save total number of individuals (TNIND) in the landscape for each species
    for (species in target_species) {
      df_list[[species]] <-  data.frame(sim$globals[[species]]$n_abundance,
                                           sim$globals[[species]]$mean_abundance,
                                           sim$globals[[species]]$mean_rrate,
                                           sim$globals[[species]]$mean_ccap,
                                           sim$globals[[species]]$n_occupied) %>% 
        mutate(Scenario = scenario,
          biome = target_biome,
          region = target_region,
          species = species,
          timestep = row_number())
    }
    # combine all species together
    TNIND_yr <- do.call(rbind, df_list)
    colnames(TNIND_yr) <- c("TNIND", "MNIND", "mean_repRate", "mean_carrCap", "occupancy", "scenario", "biome", "region", "species", "timestep")
    # save csv file with TNIND
    write.csv(TNIND_yr, file = file.path(dirout, paste0("TNIND_yr_", runname, ".csv")), row.names = FALSE)
    
    ###########
    # Step 10 # Save a settings file
    ###########
    
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
    
################
# DELETE LATER #
################

# for (sp in species_names) {
#   avg_abundance <- cumulative_abundance[[sp]] / n_replicates
#   
#   # Save the result to disk
#   writeRaster(
#     avg_abundance,
#     filename = file.path(dirout, paste0("avg_abundance_", sp, ".tif")),
#     format = "GTiff",
#     overwrite = TRUE
#   )
# }
  
    
###########
# Step 2  # 
###########
# replace line 69 with 65-67 if modelling just one species 
# r <- rast(list.files(dirinput,
#            pattern = "_cropped_modified_reprojectedKm.tif", full.names = TRUE))
# sim_env <- sds(r)




