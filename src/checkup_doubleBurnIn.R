## Name: checkup_doubleBurnIn.R ##
## Authors: Inês Silva ##
## Date: 30 Sept 2026
## Description: Plot suitability landscapes for selected species within just the 
## IUCN range and afterwards with landscape expansion to check if there is a very
## large increase in the number of suitable cells ##

##########
# STEP 0 # Selected species
##########

# selected species
selected_sps <- c(# tropical africa
                  "Aepyceros melampus",
                  "Loxodonta africana",
                  # tropicla south America
                  "Odocoileus virginianus",
                  # boreal North America
                  "Canis latrans",
                  "Vulpes Vulpes",
                  # boreal europe+asia
                  "Canis lupus",
                  "Meles meles")


timestep_1 <- 1
timestep_2 <- 15   # <-- change this to your desired timestep


runs <- read.csv("./data/run_table.csv", stringsAsFactors = FALSE)

inputFolder_paths <- character(0)

for (i in seq_len(nrow(runs))) {
  
  target_region <- runs$region[i]
  target_biome <- runs$biome[i]
  future_scenario <- runs$scenario[i]
  
  runname <- paste(target_region, future_scenario, "20260517", sep = "_")
  
  input_folder <- file.path("E:/metaRange_May26/outputs", runname, "Inputs")
  
  if (!file.exists(input_folder)) {
    warning(
      "Input folder not found (skipping): ",
      input_folder
    )
    next
  }
  
  inputFolder_paths[runname] <- input_folder
}


# load iucn's species ranges (to initiate species only within their range)
iucn <- vect("./data/externaldata/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
iucn$sci_name <- gsub(" ", ".", iucn$sci_name)
invisible(gc())



suitability_summary <- data.frame()

threshold <- 0.6

for (dir in inputFolder_paths) {
  
  files <- list.files(dir, pattern = "_reprojectedm\\.tif$", full.names = TRUE)
  
  if (length(files) == 0) {
    next
  }
  
  # Keep ONLY selected species
  files_selected <- files[
    sapply(files, function(f) {
      
      fname <- basename(f)
      
      species_name <- str_extract(fname, "^(.*?)_(?=(boreal|tropical))")
      
      species_name <- gsub("_$", "", species_name)
      
      species_name %in% gsub(" ", ".", selected_sps)
    })
  ]
  
  
  if (length(files_selected) == 0) {
    
    message("No selected species found in: ", dir)
    
    next
  }
  
  # loop through selected species
  for (f in files_selected) {
    
    fname <- basename(f)
    
    species_name <- str_extract(fname, "^(.*?)_(?=(boreal|tropical))")
    
    species_name <- gsub("_$", "", species_name)
    
    species_label <- suppressMessages(pretty_species_names(species_name))
    
    message("Processing: ", species_label, " | ", basename(dirname(dir)))
    
    ##########
    # STEP 1 # Load suitability raster
    ##########
    
    r <- rast(f)
  
    if (max(timestep_1, timestep_2) > nlyr(r)) {
      
      warning(species_label, ": requested timestep exceeds number of raster layers.")
      
      next
    }
    
    # extract timestep within IUCN range and after introduction of new landscape
    ## 2015
    r1 <- r[[timestep_1]]
    # 2030
    r2 <- r[[timestep_2]]
    
    names(r1) <- paste0("Timestep ", timestep_1)
    names(r2) <- paste0("Timestep ", timestep_2)
    
    # crop first time by iucn range
    species_range <- iucn[
      iucn$sci_name == species_name,
    ]
    
    if (nrow(species_range) == 0) {
      
      warning("No IUCN range found for: ", species_name)
      
      next
    }
    
    species_range <- project(species_range, crs(r1))
    r1 <- crop(r1, species_range)
    r1 <- mask(r1, species_range)
    
    # convert into df for plotting
    df1 <- as.data.frame(r1, xy = TRUE, na.rm = TRUE)
    
    names(df1)[3] <- "suitability"
    
    df1$timestep <- paste0("Timestep ", timestep_1)
    df2 <- as.data.frame(r2, xy = TRUE, na.rm = TRUE)
    
    names(df2)[3] <- "suitability"
    
    df2$timestep <- paste0("Timestep ", timestep_2)

    df <- bind_rows(df1, df2)
    
    # plot two timesteps side by side
    p <- ggplot(df, aes(x = x, y = y, fill = suitability)) +
      geom_raster() +
      facet_wrap(~ timestep, ncol = 2) +
      coord_equal() +
      scale_fill_viridis_c(option = "viridis", limits = c(0, 1), na.value = "transparent", name = "Suitability") +
      theme_bw() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "right") +
      labs(title = species_label)
    
    #print(p)
    ggsave(p, 
           bg = 'white', width = 180, height = 100, units = "mm", dpi = 1200,
           filename = file.path("E:/metaRange_May26/outputs/checkup_doubleBurnIn", paste0(species_label,"_", basename(dirname(dir)), ".png")))
    
    ##########
    # STEP 2 # Quantify suitable cells
    ##########
    
    v1 <- values(r1)
    v2 <- values(r2)
    
    # Remove NA cells separately
    v1 <- v1[!is.na(v1)]
    v2 <- v2[!is.na(v2)]
    
    # Number of cells
    n_cells_1 <- length(v1)
    n_cells_2 <- length(v2)
    
    # Number of cells above suitability threshold
    n_suitable_1 <- sum(v1 > threshold)
    n_suitable_2 <- sum(v2 > threshold)
    
    # Percentage of cells suitable
    pct_suitable_1 <- (100 * n_suitable_1 / n_cells_1)
    
    pct_suitable_2 <- (100 * n_suitable_2 / n_cells_2)
    
    # Percentage change
    if (pct_suitable_1 > 0) {
      
      pct_change <- ((pct_suitable_2 - pct_suitable_1) / pct_suitable_1) * 100
      
    } else {
      
      pct_change <- NA_real_
    }
    
    # Store results
    suitability_summary <- bind_rows(
      suitability_summary,
      data.frame(
        species = species_label,
        run = basename(dirname(dir)),
        timestep_1 = 25,
        timestep_2 = 40,
        threshold = threshold,
        n_cells_1 = n_cells_1,
        n_cells_2 = n_cells_2,
        suitable_cells_1 = n_suitable_1,
        suitable_cells_2 = n_suitable_2,
        pct_suitable_1 = pct_suitable_1,
        pct_suitable_2 = pct_suitable_2,
        pct_change = pct_change
      )
    )
    
    
    invisible(gc())
  }
}


# Round results
suitability_summary <- suitability_summary %>%
  mutate(across(c(pct_suitable_1, pct_suitable_2, pct_change),
      ~ round(.x, 2))) %>% 
  separate()

##########
# STEP 3 # Add other model variables
##########

TNIND_yr <- read.csv("E:/metaRange_May26/outputs/completeMetaRangeRun_20260517.csv")

TNIND_yr <- TNIND_yr %>% 
  # average across metaRange replicates
  group_by(future_scenario, biome, region, species, trophic_level, timestep) %>%
  summarise(across(c(TNIND, MNIND, mean_repRate, mean_carrCap, occupancy),
                   ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>% 
  dplyr::filter(timestep %in% c(25, 40))%>%
  pivot_wider(
    names_from = timestep,
    values_from = c(TNIND, MNIND, mean_repRate, mean_carrCap, occupancy),
    names_glue = "{.value}_{timestep}")%>%
  mutate(TNIND_change = TNIND_40 - TNIND_25,
    MNIND_change = MNIND_40 - MNIND_25,
    repRate_change = mean_repRate_40 - mean_repRate_25,
    carrCap_change = mean_carrCap_40 - mean_carrCap_25,
    occupancy_change = occupancy_40 - occupancy_25) %>%
  dplyr::select(future_scenario, biome, region, species, trophic_level, TNIND_change,
    MNIND_change, repRate_change, carrCap_change, occupancy_change)

# join two tables together
table_checkup <- suitability_summary %>%
  separate(run, into = c("region", "future_scenario", "date"), sep = "_",
           remove = FALSE) %>%
  left_join(TNIND_yr, by = c("region", "future_scenario", "species")) %>% 
  select(future_scenario, biome, region, species, threshold, n_cells_1, n_cells_2,
         pct_suitable_1, pct_suitable_2, pct_change, TNIND_change, MNIND_change, repRate_change,
         carrCap_change, occupancy_change)

# save metrics per species as .csv
write.csv(table_checkup,
          file = file.path("E:/metaRange_May26/outputs/checkup_doubleBurnIn", paste0("checkUpSuitability_", Sys.Date(), ".csv")),
          row.names = FALSE)



















