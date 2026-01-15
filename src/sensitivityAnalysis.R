## Name: SensitivityAnalysis.R ##
## Author: Inês Silva ##
## Date: January 10th, 2026 ##
## Description: Run metarange sensitivity runs and produce plot showing model's sensitivity to certain parameters ##

##########
# STEP 0 # Load libraries and custom functions
##########

source(file.path("src", "libraries.R"))
source(file.path("src", "customFunctions.R"))
source(file.path("src", "customFunctions2.R"))

##########
# STEP 1 # Read runs table
##########

# csv file specifying which sensitivity runs to run
sens_runs <- read.csv("data/sensrun_table.csv", stringsAsFactors = FALSE)

# loop over the sensitivity runs table
for(i in seq_len(nrow(sens_runs))){
  while (sink.number() > 0) sink()
  
    # metadata
    target_region <- sens_runs$region[i]
    target_biome  <- sens_runs$biome[i]
    sensitivity_label <- sens_runs$label[i]
    
    # build runmane
    runname <- paste(
      target_region,
      sensitivity_label,
      format(Sys.time(), "%Y%m%d"),
      sep = "_"
    )
    
    tryCatch({
      
      # --------------------------------------------------------
      # General Setup
      # --------------------------------------------------------
      
      # get project root
      project_root <- getwd()
      # data paths (shared across runs)
      data_dir <- file.path(project_root, "data")
      # output root (all runs live here)
      output_root <- file.path(project_root, "outputs")
      # sensitivity root
      dir.create(file.path(output_root, "sensitivity_runs"), showWarnings = FALSE)
      sens_output_root <- file.path(output_root, "sensitivity_runs")
      # create run-specific directory (inside sensitivity root)
      runpath <- file.path(sens_output_root, runname)
      dir.create(runpath, recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(runpath, "Inputs"), showWarnings = FALSE)
      dir.create(file.path(runpath, "Outputs"), showWarnings = FALSE)
      # shortcuts used by all scripts
      dirinput <- file.path(runpath, "Inputs")
      dirout   <- file.path(runpath, "Outputs")
      # Logging
      logfile <- file.path(runpath, "run.log")
      sink(logfile, split = TRUE)
      on.exit(sink(), add = TRUE)
      cat("Started run name:", runname, "\n")
      cat("Region:", target_region, "\n")
      cat("Biome:", target_biome, "\n")
      
      # --------------------------------------------------------
      # Load species list
      # --------------------------------------------------------
      species_table <- read.csv(
        file.path(data_dir, "species_by_region.csv"),
        stringsAsFactors = FALSE
      )
      
      target_species <- species_table |>
        dplyr::filter(
          BIOME_NAME == target_biome,
          CONTINENT  == target_region
        ) |>
        dplyr::pull(sci_name) |>
        unique()
      
      if (length(target_species) == 0) {
        stop("No species found for this region/biome.")
      }
      
      # --------------------------------------------------------
      # Load MetaRange Trait Dataframe & modify accordingly
      # --------------------------------------------------------
      
      # build "normal" metaRange formatted trait dataframe
      source(file.path("src", "mammalMetaRangeSpeciesDataframe.R"))
      
      # see which parameter this run is changing 
      sens_param <- sens_runs$parameter[i]
      # add change for sensitivity run (multiply by x%)
      species_traits <- species_traits |>
        dplyr::mutate(
          !!sens_param := .data[[sens_param]] * sens_runs$multiplier[i]
        )
      
      # overwrite table to .csv file in the same input folder
      write_csv(species_traits, file = file.path(dirinput,"metaRangeSpeciesDataframe.csv"), append = FALSE)
      
      # --------------------------------------------------------
      # Species Suitability Landscapes
      # --------------------------------------------------------

      # OR REPLACE WITH OWN CODE FOR BUILDING METARANGE INPUT LANDSCAPES
      
      # sps names correction
      target_species <- gsub(" ", ".", target_species)
      # biome correction
      if (target_biome == "Tropical & Subtropical Moist Broadleaf Forests") {
        biome <- "tropical"
      } else if (target_biome == "Boreal Forests/Taiga") {
        biome <- "boreal"
      }
      # region correction
      if (target_region == "Europe+Asia") {
        target_region <- "Europe"
      }

      basePathSDM <- "./data/sdm/SDMlandscapes_October25"
      # path for each biomes' SDM outputs
      biome_paths <- list(
        tropical = "./data/sdm/tropical_SDMS",
        boreal   = "./data/sdm/boreal_SDMS"
      )

      output_cropped <- file.path(basePathSDM, "biome_cropped")

      for (sp in target_species) {
        
        path <- biome_paths[[biome]]
        # fetch current raster for sp in biome
        current_file <- file.path(path, paste0("proj_Current_EM_", sp, "_continuous.tif"))
        if (!file.exists(current_file)) {
          message("SKIPPING ", sp, ": current raster not found")
          next
        }

        # load raster
        r1 <- terra::rast(current_file)
        # repeat the 1st layers 25 times to run metaRange after
        sp_raster <- rep(r1, 25)
        names(sp_raster) <- rep(as.character(2015), 25)
        # RESCALE 0-1000 → 0-1
        sp_raster <- sp_raster / 1000
        invisible(gc())
        # load biome and continent geometries
        biome_sf <- load_biome(biome_name = target_biome)
        continent_sf <- load_select_continents(continent_names = target_region)
        # Crop biome to continents
        sf_use_s2(FALSE)
        target_geom <- crop_biome_to_continent(biome_sf, continent_sf)
        invisible(gc())
        # CROP and MASK raster
        sp_raster <- terra::crop(sp_raster, target_geom)
        sp_raster <- terra::mask(sp_raster, target_geom)
        invisible(gc())
        # output filename
        output_path <- file.path(
          dirinput,
          paste0(sp, "_", biome, "_", target_region, "_cropped.tif")
        )
        # save raster
        writeRaster(sp_raster, output_path, overwrite = TRUE)
        message("Processed and saved: ", basename(output_path))
        # cleanup
        rm(raster_file, biome_sf, continent_sf, target_geom, sp_raster, output_path)
        invisible(gc())
      }

      # list all cropped rasters
      landscapes <- list.files(path = dirinput,
                               pattern = ".*_cropped\\.tif$",
                               full.names = TRUE)
      invisible(gc())
      message("Start Reprojecting and Converting to km...")

      for (landscape in landscapes) {
        message("Processing: ", basename(landscape))
        ## WARNINGS may appear in the end!! It might be ok, but still check
        ## GDAL couldn’t compute the outer bounds reliably when using the Robinson projection system
        # load raster stack
        r <- terra::rast(landscape)
        # Reproject to target CRS
        r_utm <- terra::project(r, "ESRI:54030")
        # crop and mask - to ensure no dead pixels in the corners
        # load biome and continent geometries
        biome_sf <- load_biome(biome_name = target_biome)
        continent_sf <- load_select_continents(continent_names = target_region)
        # crop biome to continents
        sf_use_s2(FALSE)
        target_geom <- crop_biome_to_continent(biome_sf, continent_sf)
        # project target region to ronbinson
        target_geom_robinson <- st_transform(target_geom, crs = "ESRI:54030")
        invisible(gc())
        # CROP and MASK raster
        r_utm_masked <- terra::crop(r_utm, target_geom_robinson)
        r_utm_masked <- terra::mask(r_utm_masked, target_geom_robinson)
        invisible(gc(rm(biome_sf, continent_sf, target_geom, target_geom_robinson)))
        # convert to rasterStack (note: here we changed packages because terra was removing the rasters' values when changing the crs)
        r_raster <- raster::stack(r_utm_masked)
        # get original CRS
        orig_crs <- crs(r_utm_masked)
        # rescale extent (divide by 1000 to convert meters to kilometers)
        extent(r_raster) <- extent(r_raster) / 1000
        # extract species name from filename
        species_name <- gsub("_.*", "", file_path_sans_ext(basename(landscape)))
        # select target species
        species_traits <- read.csv(file.path(dirinput,"metaRangeSpeciesDataframe.csv"))
        # get an aggregation factor from species traits
        species_fact <- ceiling(species_traits$ModellingRes[species_traits$Species == species_name] / sqrt(species_traits$CellResolution[species_traits$Species == species_name]))
        # aggregate raster using terra
        r_agg <- aggregate(r_utm, fact = species_fact, fun = mean, na.rm = TRUE)
        # build an output filename
        output_filename <- gsub("\\.tif$", "_reprojectedKm.tif", landscape)
        # save aggregated raster
        writeRaster(r_agg, output_filename, overwrite = TRUE)
        # clean memory & save space
        rm(r, r_utm, r_agg, biome_sf, continent_sf, target_geom, r_utm_masked, r_raster, orig_crs, species_name, species_traits, species_fact, output_filename)
        gc()
      }


      
      # --------------------------------------------------------
      # Run MetaRange Model for each
      # --------------------------------------------------------
      
      ## Step 1 ## Configure Targets and Model Parameters
      
      # import species traits df
      species_traits <- read.csv(file.path(dirinput, "metaRangeSpeciesDataframe.csv"))
      # clean target biome and region names (removes special characters like /, &, and space)
      target_biome <- gsub("[/& ]", "", target_biome)
      target_region <- gsub("[/& ]", "", target_region)
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
    
      ## Step 2 ## RUN THE METARANGE MODEL FOR MAMMAL SPECIES
      
      # load iucn's species ranges (to initiate species only within their range)
      iucn <- vect("./data/externaldata/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
      invisible(gc())
      
      for (replicateN in 1:n_replicates) {
        
        # Step 1 # Add landscape for all target species 
        
        sim_name <- paste0(replicateN, "_", str_replace_all(target_biome, " ", ""), "_", target_region, "_Mammals")
        sim_env <- sds(list.files(dirinput,
                                  pattern = paste0("_", biome, "_", target_region, "_cropped_reprojectedKm.tif"), full.names = TRUE))
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
          
          iucn_sps <- iucn[iucn$sci_name == gsub("[.]", " ", this_species)]
          iucn_sps <- project(iucn_sps, crs(sim_env[[i]]))
          #plot(iucn_sps)
          range_raster <- rasterize(iucn_sps, sim_env[[i]], values = 1)
          range_raster <- subst(range_raster, NA, 0)
          
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
        
        # Step 6 # Add process 
        
        # Suitability influence on the environment  
        sim$add_process(
          species = species_names,
          process_name = "suitability_influence_population_parameter",
          process_fun = function() {
            species_suitability_name <- paste0(self$name, "_", biome, "_", target_region, "_cropped_reprojectedKm")
            
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
              weights = self$sim$environment$current[[paste0(self$name, "_", biome, "_", target_region, "_cropped_reprojectedKm")]],
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
        
        
        # # saving results
        # sim$add_process(
        #   process_name = "saving_traits",
        #   process_fun = function() {
        #     for (species in species_names) {
        #       
        #       # save all replicates
        #       results_paths <- save_species(
        #         # pass the species object
        #         self[[species]],
        #         # specify traits we want to save
        #         traits = c("abundance"
        #                    #, "reproductionRate", "dispersalChange"
        #                    ),
        #         # a prefix for each time step
        #         prefix = paste0(sim$ID, sprintf("%03d", self$get_current_time_step()), "_"),
        #         # where should it be saved
        #         path = dirout,
        #         overwrite = TRUE
        #       )
        #       
        #       self$globals$results_paths <- c(self$globals$results_paths, results_paths)
        #     }
        #   },
        #   execution_priority = 6
        # )
        
        
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
            mutate(#Scenario = future_scenario,
                   biome = target_biome,
                   region = target_region,
                   species = species,
                   timestep = row_number(),
                   rep = sim$ID)
        }
        
        # combine all species together
        TNIND_yr <- do.call(rbind, df_list)
        colnames(TNIND_yr) <- c("TNIND", "MNIND", "mean_repRate", "mean_carrCap",
                                "occupancy", "biome", "region",
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
      
      
      
    }, error = function(e) {
      message("Run failed: ", runname)
      message(e)
   })
}

# --------------------------------------------------------
# Sensitivity Analysis
# --------------------------------------------------------

# list all run directories
run_dirs <- list.dirs(sens_output_root, recursive = FALSE, full.names = TRUE)

# empty df for all runs data
TNIND_all <- data.frame()

for (run_dir in run_dirs) {
  # get run names
  run_name <- basename(run_dir)
  # get summary file with TNIND
  csv_file <- file.path(
    run_dir,
    "Outputs",
    paste0("TNIND_yr_", run_name, ".csv"))
  
  # skip if missing
  if (!file.exists(csv_file)) next
  
  TNIND_yr <- read.csv(csv_file)
  
  TNIND_yr <- TNIND_yr %>%
    # select needed columns with TNIND per species
    dplyr::select(biome, region, species, timestep, rep, TNIND) %>% 
    # remove  burn in values
    dplyr::filter(timestep > 25) %>% 
    # add run name
    dplyr::mutate(run_name = run_name)
  
  # bind evruthing together
  TNIND_all <- rbind(TNIND_all, TNIND_yr)
}

# summarise values across replicates
TNIND_sp <- TNIND_all %>% 
  group_by(run_name, biome, region, species, timestep) %>% 
  summarise(TNIND_yr = mean(TNIND, na.rm = TRUE),
            .groups = "drop") 

# sumarise across timesteps
TNIND_sp2 <- TNIND_sp %>% 
  group_by(run_name, biome, region, species) %>% 
  summarise(
    TNIND = mean(TNIND_yr, na.rm = TRUE),
    .groups = "drop"
  )

# separate baseline values
baseline_df <- TNIND_sp2 %>% 
  filter(grepl("baseline", run_name)) %>% 
  mutate(
    region = sub("_baseline.*", "", run_name)
  ) %>% 
  select(region, species, baseline_TNIND = TNIND)

# combine baseline values with sensitivity 
sens_df <- TNIND_sp2 %>% 
  filter(!grepl("baseline", run_name)) %>% 
  mutate(
    region = sub("_(repro|mort).*", "", run_name),
    label  = stringr::str_extract(run_name, "(repro|mort)[0-9]+")) %>% 
  left_join(
    baseline_df,
    by = c("region", "species")) %>% 
  filter(baseline_TNIND > 0) %>% 
  # calculate proportion value between each sensitivity & baseline value per region
  mutate(
    proportion = TNIND / baseline_TNIND)

write.csv(sens_df, file = file.path(sens_output_root, "sensitivity_proportions_values.csv"))

# plot the values per sensitivity run
ggplot(sens_df, aes(x = label, y = proportion)) +
  geom_boxplot() +
  facet_wrap(~region) +
  # add dashed line for lower treshold of 20% variation
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  # add dashed line for upper treshold of 20% variation
  geom_hline(yintercept = 1.2, linetype = "dashed", color = "red") +
  labs(x = "Sensitivity run", y = "Value") +
  theme_minimal()
