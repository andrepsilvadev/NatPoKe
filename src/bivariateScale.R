## Name: bivariateScaleMaps.R ##
## Author: Inês Silva ##
## Date: 10 Sept 2026 ##
## Goal? Combine the Shannon-Wiener Index Change and teh Bray Curtis Dissimilarity
## spatially explicit into one map with a bivariate scale.

library(here)
source(here("src", "libraries.R"))
source(here("src", "customFunctions2.R"))

##########
# STEP 1 # Calculate both indexes spatially explicit
##########

# list all directories with outputs to map SSP1 and SSP5
outputFolder_paths <- character(0)
runs <- read.csv("./data/run_table.csv", stringsAsFactors = FALSE)

for (i in seq_len(nrow(runs))) {
  
  target_region <- runs$region[i]
  target_biome <- runs$biome[i]
  future_scenario <- runs$scenario[i]
  
  runname <- paste(
    target_region,
    future_scenario,
    "20260517",
    #format(Sys.time(), "%Y%m%d"),
    sep = "_"
  )
  
  output_folder <- file.path(
    "E:/metaRange_May26/outputs", 
    runname, "Outputs")
  
  
  if (!file.exists(output_folder)) {
    warning("Output folder not found (skipping): ", output_folder)
    next
  }
  
  outputFolder_paths[runname] <- output_folder
}

# clean up
rm(output_folder)
invisible(gc())

# get target species
TNIND_yr <- read.csv("E:/metaRange_May26/outputs/completeMetaRangeRun_20260517.csv")
target_species <- unique(TNIND_yr$species)
target_species <- gsub(" ", ".", target_species)


### Shannon-Wiener -------------------------------------------------------------

# Initialize an empty list to store final dataframes
all_final_data <- list()

# Loop through each directory
for (dir in outputFolder_paths) {
  
  dir_data <- list()
  
  # Loop through each species
  for (target_sps in target_species) {
    
    message(paste0("Working on ", target_sps))
    
    for (timestep in c(26, 136)) {
      
      # list all rasters for this species & timestep (handles replicates)
      sps_files <- list.files(path = dir,
                              pattern = paste0(".*", timestep, "_", target_sps, "_abundance\\.tif"),
                              full.names = TRUE)
      
      if (length(sps_files) > 0) {
        # import all replicates as a SpatRaster
        sps_stack <- rast(sps_files)
        
        # compute mean across replicates
        sps_mean <- app(sps_stack, mean, na.rm = TRUE)
        
        # convert to dataframe with xy coordinates
        sps_df <- as.data.frame(sps_mean, xy = TRUE) %>%
          mutate(timestep = timestep,
                 species = target_sps)
        
        # store in the directory list
        dir_data[[paste(target_sps, timestep, sep = "_")]] <- sps_df
      }
      
    }
    
  }
  
  # Combine all species & timesteps into a single dataframe
  final_df <- bind_rows(dir_data)
  
  # Extract second-to-last folder name as key
  folder_names <- strsplit(dir, "/")[[1]]
  short_dir_name <- folder_names[length(folder_names) - 1]
  
  # Store in the master list
  all_final_data[[short_dir_name]] <- final_df
}


# Calculate Shannon index change **per functional group**
Shannon_indexes <- list()

# call combined trait data to get trophic levels
combined_traits_data <- read_csv(here("data", "externaldata", "mammalTraits_2025-12-11.csv")) %>% 
  mutate(sci_name = gsub("[/& ]", ".",sci_name)) %>% 
  dplyr::filter(BIOME_NAME %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga")) %>% 
  mutate(
    CONTINENT = case_when(
      BIOME_NAME == "Boreal Forests/Taiga" & CONTINENT == "Europe" ~ "Europe+Asia",
      TRUE ~ CONTINENT),
    trophic_level = case_when(
      trophic_level == 1 ~ "Herbivore",
      trophic_level == 2 ~ "Omnivore",
      trophic_level == 3 ~ "Carnivore",
      TRUE ~ as.character(trophic_level))
  )

# going trhough each scenario+region
for (dir_name in names(all_final_data)) { 
  df <- all_final_data[[dir_name]]
  
  # join with combined triats for trophic levels
  df <- df %>%
    left_join(combined_traits_data, by = c("species" = "sci_name"), relationship = "many-to-many") %>% # to silence warning, careful if it does not apply in the future!!
    dplyr::filter(!is.na(trophic_level))  # keep only species with known trophic level
  invisible(gc())
  
  # start a sublist for the scenario+region
  Shannon_indexes[[dir_name]] <- list()
  
  # go through each trophic level in each scenario+region
  for (troph in unique(df$trophic_level)) {
    troph_df <- df %>%
      dplyr::filter(trophic_level == troph, mean != 0)
    
    # get the sps names used in that specific trophic group
    species_used <- unique(troph_df$species)
    
    # print a message saying which species are being used
    message("Scenario: ", dir_name, " | Trophic level: ", troph, " | Species: ", paste(species_used, collapse = ", "))
    
    # calculate Shannon Wiener index
    troph_df <- troph_df %>%
      group_by(timestep, x, y) %>%
      dplyr::mutate(
        p_i = mean / sum(mean),
        ln_p_i = ifelse(p_i > 0, log(p_i), 0)
      ) %>%
      dplyr::summarize(
        Shannon_Wiener_Index = -sum(p_i * ln_p_i),
        .groups = "drop"
      ) %>%
      # keep only what you need
      select(x, y, timestep, Shannon_Wiener_Index) %>%
      # reshape wide by timestep
      pivot_wider(names_from = timestep, values_from = Shannon_Wiener_Index, names_prefix = "t") %>%
      # compute change (t235 - t101), automatically NA if one is missing
      mutate(Shannon_change = t136 - t26)
    
    invisible(gc())
    
    # save the df into a nested list (per scenario+region and trophic level)
    Shannon_indexes[[dir_name]][[troph]] <- troph_df
    invisible(gc())
  }
}


### Bray Curtis ----------------------------------------------------------------

all_results <- list()

for(dir in outputFolder_paths){
  
  message("Processing: ", dir)
  
  run_name <- basename(dirname(dir))
  
  # read species list
  files_all <- list.files(dir,
                          pattern = "abundance\\.tif$",
                          full.names = TRUE)
  
  species <- unique(
    sub(".*_(.*)_abundance\\.tif$", "\\1", basename(files_all))
  )
  
  ##########
  # Step 1 # Join species names with trophic levels
  ##########
  
  species_df <- data.frame(species = species)
  
  species_df <- species_df %>%
    left_join(combined_traits_data,
              by = c("species" = "sci_name")) %>%
    dplyr::filter(!is.na(trophic_level))
  
  troph_groups <- unique(species_df$trophic_level)
  
  all_results[[run_name]] <- list()
  
  ##########
  # Step 2 # Loop over trophic groups
  ##########
  
  for(troph in troph_groups){
    
    message("  Trophic level: ", troph)
    
    species_used <- species_df %>%
      dplyr::filter(trophic_level == troph) %>%
      pull(species) %>%
      unique()
    
    message("  Species: ", paste(species_used, collapse = ", "))
    
    ##########
    # Step 3 # Build t26 stack
    ##########
    
    t1_list <- list()
    
    for(sp in species_used){
      
      f <- list.files(dir,
                      pattern = paste0("26_", sp, "_abundance\\.tif"),
                      full.names = TRUE)
      
      if(length(f) > 0){
        
        r <- rast(f)
        
        # average across replicates
        r_mean <- app(r, mean, na.rm = TRUE)
        
        t1_list[[sp]] <- r_mean
      }
    }
    
    if(length(t1_list) == 0){
      message("    No species found at t26")
      next
    }
    
    t1 <- rast(t1_list)
    
    ##########
    # Step 4 # Build t136 stack
    ##########
    
    t136_list <- list()
    
    for(sp in species_used){
      
      f <- list.files(dir,
                      pattern = paste0("136_", sp, "_abundance\\.tif"),
                      full.names = TRUE)
      if(length(f) > 0){
        
        r <- rast(f)
        
        # average across replicates
        r_mean <- app(r, mean, na.rm = TRUE)
        
        t136_list[[sp]] <- r_mean
      }
    }
    
    if(length(t136_list) == 0){
      message("    No species found at t136")
      next
    }
    
    t136 <- rast(t136_list)
    
    invisible(gc())
    
    ##########
    # Step 5 # Convert raster stacks to community matrices
    ##########
    
    # build a community matrix for t26 with pixelID as rows and species as columns
    comm_t1 <- as.data.frame(t1, xy = TRUE, na.rm = FALSE)
    
    # build a community matrix for t136 with pixelID as rows and species as columns
    comm_t136 <- as.data.frame(t136, xy = TRUE, na.rm = FALSE)
    
    coords <- comm_t1[, c("x", "y")]
    
    comm_t1_sp <- comm_t1[, -c(1,2)]
    comm_t136_sp <- comm_t136[, -c(1,2)]
    
    ##########
    # Step 6 # Remove completely empty cells
    ##########
    
    # Here we are removing pixels where no species exist in t26 or in t136
    # Because Bray-Curtis is meaningless if no species exist at either time
    # It would create a falsly similar community
    keep <- rowSums(
      replace(comm_t1_sp, is.na(comm_t1_sp), 0) +
        replace(comm_t136_sp, is.na(comm_t136_sp), 0) ) > 0
    
    coords <- coords[keep, ]
    
    comm_t1_sp <- comm_t1_sp[keep, ]
    comm_t136_sp <- comm_t136_sp[keep, ]
    
    invisible(gc())
    
    ##########
    # Step 7 # Bray-Curtis per pixel
    ##########
    
    # the top portion where you subtract the two communities (timesteps)
    numerator <- rowSums(abs(comm_t1_sp - comm_t136_sp), na.rm = TRUE)
    
    # the bottom portion where you add both communities
    denominator <- rowSums(comm_t1_sp + comm_t136_sp, na.rm = TRUE)
    
    # computing the real index (per pixel)
    bray_pixel <- numerator / denominator
    
    # avoid division-by-zero issues
    bray_pixel[denominator == 0] <- NA
    
    ##########
    # Step 8 # Convert back to raster
    ##########
    
    results <- data.frame(x = coords$x, y = coords$y, bray = bray_pixel)
    
    pts <- vect(results, geom = c("x", "y"), crs = crs(t1))
    
    # put it back in a raster
    out_bray <- rasterize(pts, t1[[1]], field = "bray")
    #writeRaster(out_bray,
     #           filename = file.path("E:/metaRange_May26/BrayCurtis", paste0("bray_", run_name, "_", troph, ".tif")),
      #          overwrite = TRUE)
    
    invisible(gc())
    
    ##########
    # Step 9 # Store results
    ##########
    
    all_results[[run_name]][[troph]] <- list(bray = out_bray)
    
    invisible(gc())
  }
}

##########
# STEP 2 # Define a bivariate scale for each trophic group in each scenario
##########

# get regions, trophic groups and scenarios into vectors
regions <- c("North America", "Europe+Asia", "South America", "Africa", "Asia")

trophic_groups <- c("Herbivore", "Carnivore", "Omnivore")

scenarios <- c("ssp126", "ssp585")

bivariate_data <- list()

for (trophic_group in trophic_groups) {
  
  for (scenario in scenarios) {
    
    data_list <- list()
    
    for (region in regions) {
      
      name <- paste0(region, "_", scenario, "_20260517")
      
      shannon <- Shannon_indexes[[name]][[trophic_group]]
      
      bray <- as.data.frame(all_results[[name]][[trophic_group]]$bray,
                            xy = TRUE,
                            na.rm = FALSE)
      
      names(bray)[3] <- "bray"
      
      data_list[[region]] <- shannon %>%
        left_join(bray, by = c("x", "y"))
    }
    
    bivariate_data[[paste0(trophic_group, "_", scenario)]] <- data_list
  }
}


bivariate_classified <- list()

for (trophic_group in trophic_groups) {
  
  for (scenario in scenarios) {
    
    name <- paste0(trophic_group, "_", scenario)
    
    # Combine the five regions
    combined <- bind_rows(bivariate_data[[name]], .id = "region")
    
    # Keep complete observations for calculating classes
    complete_data <- combined %>%
      dplyr::filter(!is.na(Shannon_change),!is.na(bray))
    
    # Calculate bivariate classes using all five regions together
    complete_data <- bi_class(complete_data,
                              x = Shannon_change,
                              y = bray,
                              style = "quantile",
                              dim = 3)
    
    # Put the classified data back into regions
    bivariate_classified[[name]] <- split(complete_data,
                                          complete_data$region)
  }
}

##########
# STEP 3 # Build the plots
##########

world <- ne_countries(scale = "medium", returnclass = "sf")

# get icons for each taxonomic group plot
uuid_carnivores <- get_uuid(name = "Panthera leo", n = 5)[[5]]
uuid_herbivores <- get_uuid(name = "Cervus elaphus", n = 1)
uuid_omnivores <- get_uuid(name = "Sus scrofa", n = 5)[[2]]


# main bivariate colour legend
legend <- bi_legend(pal = "BlueYl", flip_axes = FALSE, rotate_pal = FALSE,
                    dim = 4, xlab = "Change in Shannon diversity",
                    ylab = "Bray–Curtis dissimilarity", size = 6)  +
  theme(
    # no background
    plot.background = element_rect(fill='transparent', color=NA),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)))

 
### Carnivores SSP1-2.6 --------------------------------------------------------

carn_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80",color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = bivariate_classified$Carnivore_ssp126$`North America`, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Carnivore_ssp126$`Europe+Asia`, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Carnivore_ssp126$`South America`, aes(x = x, y = y, fill =  bi_class)) +
  geom_tile(data = bivariate_classified$Carnivore_ssp126$Africa, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Carnivore_ssp126$Asia, aes(x = x, y = y, fill = bi_class))+
  add_phylopic(uuid = uuid_carnivores, x = -14500000, y = 6500000, height = 1200000) +
  annotate("text", x = -14500000, y = 5500000, label = "SSP1–2.6", fontface = "bold", size = 3) +
  # bivariate colour scale
  bi_scale_fill(pal = "BlueYl", dim = 4, flip_axes = FALSE, rotate_pal = FALSE) +
  labs(x = "", y = "") +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7))

### Carnivores SSP1-2.6 LEGEND
carn_ssp126_leg <- bind_rows(bind_rows(bivariate_data[["Carnivore_ssp126"]], .id = "region"),
                         bind_rows(bivariate_data[["Carnivore_ssp585"]], .id = "region"),
                         .id = "scenario") %>%
  dplyr::filter(!is.na(Shannon_change), !is.na(bray))

shannon_breaks <- classInt::classIntervals(carn_ssp126_leg$Shannon_change, n = 4, style = "quantile")$brks
bray_breaks <- classInt::classIntervals(carn_ssp126_leg$bray, n = 4, style = "quantile")$brks


legend_carn <- ggdraw() +
  draw_plot(legend, x = 0, y = 0, width = 1, height = 1) +
  # shannon min
  draw_label(round(shannon_breaks[1], 2), x = 0.43, y = 0.21, size = 6, fontface = "bold") +
  # shannon max
  draw_label(round(shannon_breaks[5], 2),  x = 0.66, y = 0.21, size = 6, fontface = "bold") +
  # bray curtis min
  draw_label(round(bray_breaks[1], 2),  x = 0.35, y = 0.34, size = 6, fontface = "bold") +
  # bray curtis max
  draw_label(round(bray_breaks[5], 2),  x = 0.35, y = 0.75, size = 6, fontface = "bold")

### final carnivores ssp1-2.6 
carn_ssp126 <- ggdraw() +
  draw_plot(carn_ssp126, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend_carn, x = 0.000001, y = 0.15, width = 0.40, height = 0.40)

### Herbivores SSP1-2.6 --------------------------------------------------------

herb_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80",color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = bivariate_classified$Herbivore_ssp126$`North America`, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Herbivore_ssp126$`Europe+Asia`, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Herbivore_ssp126$`South America`, aes(x = x, y = y, fill =  bi_class)) +
  geom_tile(data = bivariate_classified$Herbivore_ssp126$Africa, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Herbivore_ssp126$Asia, aes(x = x, y = y, fill = bi_class))+
  add_phylopic(uuid = uuid_herbivores, x = -14500000, y = 6500000, height = 2500000) +
  annotate("text", x = -14500000, y = 4700000, label = "SSP1–2.6", fontface = "bold", size = 3) +
  # bivariate colour scale
  bi_scale_fill(pal = "BlueYl", dim = 4, flip_axes = FALSE, rotate_pal = FALSE) +
  labs(x = "", y = "") +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7))

### herbivore ssp126 legend
herb_leg <- bind_rows(bind_rows(bivariate_data[["Herbivore_ssp126"]], .id = "region"),
                             bind_rows(bivariate_data[["Herbivore_ssp585"]], .id = "region"),
                             .id = "scenario") %>%
  dplyr::filter(!is.na(Shannon_change), !is.na(bray))

shannon_breaks <- classInt::classIntervals(herb_leg$Shannon_change, n = 4, style = "quantile")$brks
bray_breaks <- classInt::classIntervals(herb_leg$bray, n = 4, style = "quantile")$brks

shannon_breaks
bray_breaks

legend_herb <- ggdraw() +
  draw_plot(legend, x = 0, y = 0, width = 1, height = 1) +
  # shannon min
  draw_label(round(shannon_breaks[1], 2), x = 0.43, y = 0.22, size = 5, fontface = "bold") +
  # shannon max
  draw_label(round(shannon_breaks[5], 2),  x = 0.66, y = 0.22, size = 5, fontface = "bold") +
  # bray curtis min
  draw_label(round(bray_breaks[1], 2),  x = 0.37, y = 0.34, size = 5, fontface = "bold") +
  # bray curtis max
  draw_label(round(bray_breaks[5], 2),  x = 0.37, y = 0.75, size = 5, fontface = "bold")

herb_ssp126 <- ggdraw() +
  draw_plot(herb_ssp126, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend_herb, x = 0.000001, y = 0.15, width = 0.40, height = 0.40)

### Omnivores SSP1-2.6 ---------------------------------------------------------

omni_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80",color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = bivariate_classified$Omnivore_ssp126$`North America`, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Omnivore_ssp126$`Europe+Asia`, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Omnivore_ssp126$`South America`, aes(x = x, y = y, fill =  bi_class)) +
  geom_tile(data = bivariate_classified$Omnivore_ssp126$Africa, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Omnivore_ssp126$Asia, aes(x = x, y = y, fill = bi_class))+
  add_phylopic(uuid = uuid_omnivores, x = -14500000, y = 6500000, height = 1500000) +
  annotate("text", x = -14500000, y = 5300000, label = "SSP1–2.6", fontface = "bold", size = 3) +
  # bivariate colour scale
  bi_scale_fill(pal = "BlueYl", dim = 4, flip_axes = FALSE, rotate_pal = FALSE) +
  labs(x = "", y = "") +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7))

### Omnivore legend
### herbivore ssp126 legend
omni_leg <- bind_rows(bind_rows(bivariate_data[["Omnivore_ssp126"]], .id = "region"),
                      bind_rows(bivariate_data[["Omnivore_ssp585"]], .id = "region"),
                      .id = "scenario") %>%
  dplyr::filter(!is.na(Shannon_change), !is.na(bray))

shannon_breaks <- classInt::classIntervals(omni_leg$Shannon_change, n = 4, style = "quantile")$brks
bray_breaks <- classInt::classIntervals(omni_leg$bray, n = 4, style = "quantile")$brks

shannon_breaks
bray_breaks

legend_omni <- ggdraw() +
  draw_plot(legend, x = 0, y = 0, width = 1, height = 1) +
  # shannon min
  draw_label(round(shannon_breaks[1], 2), x = 0.43, y = 0.22, size = 5, fontface = "bold") +
  # shannon max
  draw_label(round(shannon_breaks[5], 2),  x = 0.66, y = 0.22, size = 5, fontface = "bold") +
  # bray curtis min
  draw_label(round(bray_breaks[1], 2),  x = 0.37, y = 0.34, size = 5, fontface = "bold") +
  # bray curtis max
  draw_label(round(bray_breaks[5], 2),  x = 0.37, y = 0.75, size = 5, fontface = "bold")

omni_ssp126 <- ggdraw() +
  draw_plot(omni_ssp126, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend_omni, x = 0.000001, y = 0.15, width = 0.40, height = 0.40)

### Carnivores SSP5-8.5 --------------------------------------------------------

carn_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80",color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = bivariate_classified$Carnivore_ssp585$`North America`, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Carnivore_ssp585$`Europe+Asia`, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Carnivore_ssp585$`South America`, aes(x = x, y = y, fill =  bi_class)) +
  geom_tile(data = bivariate_classified$Carnivore_ssp585$Africa, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Carnivore_ssp585$Asia, aes(x = x, y = y, fill = bi_class))+
  add_phylopic(uuid = uuid_carnivores, x = -14500000, y = 6500000, height = 1200000) +
  annotate("text", x = -14500000, y = 5500000, label = "SSP5–8.5", fontface = "bold", size = 3) +
  # bivariate colour scale
  bi_scale_fill(pal = "BlueYl", dim = 4, flip_axes = FALSE, rotate_pal = FALSE) +
  labs(x = "", y = "") +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7))

carn_ssp585 <- ggdraw() +
  draw_plot(carn_ssp585, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend_carn, x = 0.000001, y = 0.15, width = 0.40, height = 0.40)

### Herbivores SSP5-8.5 --------------------------------------------------------

herb_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80",color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = bivariate_classified$Herbivore_ssp585$`North America`, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Herbivore_ssp585$`Europe+Asia`, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Herbivore_ssp585$`South America`, aes(x = x, y = y, fill =  bi_class)) +
  geom_tile(data = bivariate_classified$Herbivore_ssp585$Africa, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Herbivore_ssp585$Asia, aes(x = x, y = y, fill = bi_class))+
  add_phylopic(uuid = uuid_herbivores, x = -14500000, y = 6500000, height = 2500000) +
  annotate("text", x = -14500000, y = 4700000, label = "SSP5–8.5", fontface = "bold", size = 3) +
  # bivariate colour scale
  bi_scale_fill(pal = "BlueYl", dim = 4, flip_axes = FALSE, rotate_pal = FALSE) +
  labs(x = "", y = "") +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7))

herb_ssp585 <- ggdraw() +
  draw_plot(herb_ssp585, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend_herb, x = 0.000001, y = 0.15, width = 0.40, height = 0.40)


### Omnivores SSP5-8.5 ---------------------------------------------------------

omni_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80",color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = bivariate_classified$Omnivore_ssp585$`North America`, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Omnivore_ssp585$`Europe+Asia`, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Omnivore_ssp585$`South America`, aes(x = x, y = y, fill =  bi_class)) +
  geom_tile(data = bivariate_classified$Omnivore_ssp585$Africa, aes(x = x, y = y, fill = bi_class)) +
  geom_tile(data = bivariate_classified$Omnivore_ssp585$Asia, aes(x = x, y = y, fill = bi_class))+
  add_phylopic(uuid = uuid_omnivores, x = -14500000, y = 6500000, height = 1500000) +
  annotate("text", x = -14500000, y = 5300000, label = "SSP5–8.5", fontface = "bold", size = 3) +
  # bivariate colour scale
  bi_scale_fill(pal = "BlueYl", dim = 4, flip_axes = FALSE, rotate_pal = FALSE) +
  labs(x = "", y = "") +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7))

omni_ssp585 <- ggdraw() +
  draw_plot(omni_ssp585, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend_omni, x = 0.000001, y = 0.15, width = 0.40, height = 0.40)

### ASSEMBLING FINAL FIGURE ----------------------------------------------------

# carnivores
row1 <- carn_ssp126 + carn_ssp585
# herbivores
row2 <- herb_ssp126 + herb_ssp585
# omnivores
row3 <- omni_ssp126 + omni_ssp585

final_plot2 <-
  row1 /
  plot_spacer() /
  row2 /
  plot_spacer() /
  row3 +
  plot_layout(heights = c(5, 0.03, 5, 0.03, 5))

final_plot2 <-
  final_plot2 + 
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 12),
    #plot.tag = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = ggplot2::margin(0.2,0.2,0,0,"cm"),
    panel.spacing = grid::unit(0, "cm")
  )


# save final plot
ggsave("Figure3_BivariateMaps.png",
  #"E:/metaRange_May26/outputs/FigureAndMetrics/Figure3_BivariateMaps.png",
       final_plot2,
       bg = "transparent", 
       width = 300,
       height = 247,
       units = "mm",
       dpi = 600)
