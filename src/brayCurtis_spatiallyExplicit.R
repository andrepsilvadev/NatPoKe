## Name: brayCurtis_spatiallyExplicit.R ##
## Author: Inês Silva ##
## Date: 20th May 2025 ##
## Description: Calculate the Bray Curtis Dissimilarity Index per cell for each
## scenario, biome, region and trophic level. Dissimilarity is calculated between
## 2100 (t=136) and 2015 (t=26). Output is plotted as world maps to visually detected
## hotspots of chnage in community similarity

source(file.path("src", "libraries.R"))
source(file.path("src", "customFunctions2.R"))

##########
# STEP 1 # list all directories with abundance outputs
##########

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
      TRUE ~ as.character(trophic_level)))


###################################
## BRAY-CURTIS PER TROPHIC LEVEL ##
###################################

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

bc_df <- map_dfr(all_results,
  ~ map_dfr(.x, ~ as.data.frame(.x$bray, xy = TRUE, na.rm = TRUE) %>%
      rename(bray = last), .id = "trophic_group"), .id = "scenario_region") %>%
  mutate(scenario = stringr::str_extract(scenario_region, "ssp\\d+"),
         region = stringr::str_remove(scenario_region, "_ssp\\d+_\\d+$")) %>%
  dplyr::select(region, scenario, trophic_group, x, y, bray)


####################################
## PLOTTING BRAY CURTIS PER PIXEL ##
####################################

world <- ne_countries(scale = "medium", returnclass = "sf")

# get icons for each taxonomic group plot
uuid_carnivores <- get_uuid(name = "Panthera leo", n = 5)[[5]]
uuid_herbivores <- get_uuid(name = "Cervus elaphus", n = 1)
uuid_omnivores <- get_uuid(name = "Sus scrofa", n = 5)[[2]]


## Herbivores - SSP5-8.5 -------------------------------------------------------

BC_herb_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = all_results$`North America_ssp585_20260517`$Herbivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Europe+Asia_ssp585_20260517`$Herbivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`South America_ssp585_20260517`$Herbivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Africa_ssp585_20260517`$Herbivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Asia_ssp585_20260517`$Herbivore$bray, aes(x = x, y = y, fill = last))+
  # scale with proper NAs
  scale_fill_viridis_c(#option = "mako",
    na.value = "transparent", name = "Bray Curtis\nDissimilarity Index") +
  # projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  # annotation
  add_phylopic(uuid = uuid_herbivores, x = -15325223, y = 0.25, height = 2000000) +
  annotate("text", x = -12325215, y = 11, label = "Herbivores", size = 3) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        # legend inside plot (bottom left corner)
        legend.position = c(0.15, 0.25),
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 6),
        
        # clean transparent background (publication safe)
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        
        # subtle frame
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

## Carnivores - SSP5-8.5 -------------------------------------------------------

BC_carn_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = all_results$`North America_ssp585_20260517`$Carnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Europe+Asia_ssp585_20260517`$Carnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`South America_ssp585_20260517`$Carnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Africa_ssp585_20260517`$Carnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Asia_ssp585_20260517`$Carnivore$bray, aes(x = x, y = y, fill = last))+
  # scale with proper NAs
  scale_fill_viridis_c(#option = "mako",
    na.value = "transparent", name = "Bray Curtis\nDissimilarity Index") +
  # projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  # annotation
  add_phylopic(uuid = uuid_carnivores, x = -15325223, y = 0.25, height = 1200000) +
  annotate("text", x = -12325223, y = -223267, label = "Carnivores", size = 3) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        # legend inside plot (bottom left corner)
        legend.position = c(0.15, 0.25),
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 6),
        
        # clean transparent background (publication safe)
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        
        # subtle frame
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

## Omnivores - SSP5-8.5 --------------------------------------------------------

BC_omni_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = all_results$`North America_ssp585_20260517`$Omnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Europe+Asia_ssp585_20260517`$Omnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`South America_ssp585_20260517`$Omnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Africa_ssp585_20260517`$Omnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Asia_ssp585_20260517`$Omnivore$bray, aes(x = x, y = y, fill = last))+
  # scale with proper NAs
  scale_fill_viridis_c(#option = "mako",
    na.value = "transparent", name = "Bray Curtis\nDissimilarity Index") +
  # projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  # annotation
  add_phylopic(uuid = uuid_omnivores, x = -15325223, y = 0.25, height = 1500000) +
  annotate("text", x = -12325223, y = 12, label = "Omnivores", size = 3) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        # legend inside plot (bottom left corner)
        legend.position = c(0.15, 0.25),
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 6),
        
        # clean transparent background (publication safe)
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        
        # subtle frame
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

## Herbivores - SSP1-2.6 -------------------------------------------------------

BC_herb_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = all_results$`North America_ssp126_20260517`$Herbivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Europe+Asia_ssp126_20260517`$Herbivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`South America_ssp126_20260517`$Herbivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Africa_ssp126_20260517`$Herbivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Asia_ssp126_20260517`$Herbivore$bray, aes(x = x, y = y, fill = last))+
  # scale with proper NAs
  scale_fill_viridis_c(#option = "mako",
    na.value = "transparent", name = "Bray Curtis\nDissimilarity Index") +
  # projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  # annotation
  add_phylopic(uuid = uuid_herbivores, x = -15325223, y = 0.25, height = 2000000) +
  annotate("text", x = -12325215, y = 11, label = "Herbivores", size = 3) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        # legend inside plot (bottom left corner)
        legend.position = c(0.15, 0.25),
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 6),
        
        # clean transparent background (publication safe)
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        
        # subtle frame
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

## Carnivores - SSP1-2.6 -------------------------------------------------------

BC_carn_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = all_results$`North America_ssp126_20260517`$Carnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Europe+Asia_ssp126_20260517`$Carnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`South America_ssp126_20260517`$Carnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Africa_ssp126_20260517`$Carnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Asia_ssp126_20260517`$Carnivore$bray, aes(x = x, y = y, fill = last))+
  # scale with proper NAs
  scale_fill_viridis_c(#option = "mako",
    na.value = "transparent", name = "Bray Curtis\nDissimilarity Index") +
  # projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  # annotation
  add_phylopic(uuid = uuid_carnivores, x = -15325223, y = 0.25, height = 1200000) +
  annotate("text", x = -12325223, y = -223267, label = "Carnivores", size = 3) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        # legend inside plot (bottom left corner)
        legend.position = c(0.1, 0.25),
        legend.direction = "vertical",
        legend.key.height = unit(0.7, 'cm'),
        legend.key.width = unit(0.7, 'cm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 6),
        
        # clean transparent background (publication safe)
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        
        # subtle frame
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

## Omnivores - SSP1-2.6 --------------------------------------------------------

BC_omni_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "gray80", color = "gray80", linewidth = 0.2) +
  # raster layer
  geom_tile(data = all_results$`North America_ssp126_20260517`$Omnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Europe+Asia_ssp126_20260517`$Omnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`South America_ssp126_20260517`$Omnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Africa_ssp126_20260517`$Omnivore$bray, aes(x = x, y = y, fill = last)) +
  geom_tile(data = all_results$`Asia_ssp126_20260517`$Omnivore$bray, aes(x = x, y = y, fill = last))+
  # scale with proper NAs
  scale_fill_viridis_c(#option = "mako",
    na.value = "transparent", name = "Bray Curtis\nDissimilarity Index") +
  # projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  # annotation
  add_phylopic(uuid = uuid_omnivores, x = -15325223, y = 0.25, height = 1500000) +
  annotate("text", x = -12325223, y = 12, label = "Omnivores", size = 3) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        # legend inside plot (bottom left corner)
        legend.position = c(0.15, 0.25),
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.text = element_text(size = 6),
        
        # clean transparent background (publication safe)
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        
        # subtle frame
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))


## Final plot ------------------------------------------------------------------


row1 <- BC_carn_ssp126 + BC_carn_ssp585
row2 <- BC_herb_ssp126 + BC_herb_ssp585
row3 <- BC_omni_ssp126 + BC_omni_ssp585

final_plot2 <-
  row1 /
  plot_spacer() /
  row2 /
  plot_spacer() /
  row3 +
  plot_layout(heights = c(4, -0.2, 4, -0.2, 4))

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
    plot.margin = ggplot2::margin(0,0.2,0,0,"cm"),
    panel.spacing = grid::unit(0, "cm")
  )
#+
#plot_layout(heights = c(0.08, 1, 0.08, 1)) #& theme(plot.margin = margin(0,0,0,0))

ggsave("E:/metaRange_May26/FigureAndMetrics/Figure3_BrayCurtisMaps.png",
       final_plot2,
       bg = "transparent", width = 320, height = 320, units = "mm", dpi = 1200)


#########################################
## BRAY CURTIS DISSIMILARITY OVER TIME ##
#########################################


##############
#### LIXO ####
##############

# ##########
# # STEP 2 # Compute the Bray-Curtis Dissimilarity & Jaccard Indeces
# ##########
# 
# # start empty list
# all_results <- list()
# 
# # loop through each run's results
# 
# for(dir in outputFolder_paths){
#   
#   message("Processing: ", dir)
#   
#   files_all <- list.files(dir, pattern = "abundance\\.tif$", full.names = TRUE)
#   
#   species <- unique(sub(".*_(.*)_abundance\\.tif$", "\\1", basename(files_all)))
#   
#   # stack abundance rasters for timestep 26 (first after burn-in)
#   
#   t1_list <- list()
#   
#   for(sp in species){
#     
#     f <- list.files(dir,
#                     pattern = paste0("26_", sp, "_abundance\\.tif"),
#                     full.names = TRUE)
#     
#     if(length(f) > 0){
#       
#       r <- rast(f)
#       # average rasters across replicates
#       r_mean <- app(r, mean, na.rm = TRUE)
#       
#       # add to stack
#       t1_list[[length(t1_list) + 1]] <- r_mean
#       names(t1_list)[length(t1_list)] <- sp
#     }
#   }
#   
#   t1 <- rast(t1_list)
#   
#   # stack abundance rasters for timestep 136 (last timestep)
#   t136_list <- list()
#   
#   for(sp in species){
#     
#     f <- list.files(dir,
#                     pattern = paste0("136_", sp, "_abundance\\.tif"),
#                     full.names = TRUE)
#     
#     if(length(f) > 0){
#       
#       r <- rast(f)
#       # average rasters across replicates
#       r_mean <- app(r, mean, na.rm = TRUE)
#       
#       # add to stack
#       t136_list[[length(t136_list) + 1]] <- r_mean
#       names(t136_list)[length(t136_list)] <- sp
#     }
#   }
#   
#   t136 <- rast(t136_list)
#   
#   #############################
#   # BRAY-CURTIS DISSIMILARITY #
#   #############################
#   ## USES ABUNDANCE DATA
#   
#   # transform raster stacks into dataframes (rows = pixel, columns = species abund)
#   comm_t1   <- as.data.frame(t1, na.rm = FALSE, xy = TRUE)
#   comm_t136 <- as.data.frame(t136, na.rm = FALSE, xy = TRUE)
#   
#   # remove coordinates
#   # (distances estimates here cannot have more than biological data attached)
#   coords <- comm_t1[, c("x","y")]
#   
#   comm_t1_sp   <- comm_t1[, -c(1,2)]
#   comm_t136_sp <- comm_t136[, -c(1,2)]
#   
#   # remove empty cells (if start is 0 and end is 0 similiary is 1,
#   # but this is not a real similar community as no one is inside it)
#   keep <- rowSums(comm_t1_sp + comm_t136_sp, na.rm = TRUE) > 0
#   
#   # remove rows that correspond to empty cells
#   coords <- coords[keep, ]
#   comm_t1_sp <- comm_t1_sp[keep, ]
#   comm_t136_sp <- comm_t136_sp[keep, ]
#   
#   # compute the bray curtis dissimilarity index (vegan package)
#   bray <- vegan::vegdist(rbind(comm_t1_sp, comm_t136_sp),
#                          method = "bray")
#   invisible(gc())
#   
#   n <- nrow(comm_t1_sp)
#   
#   # Extract Bray-Curtis dissimilarity between the SAME pixel
#   # at timestep 26 and timestep 136.
#   # After rbind, rows 1:n = t26 communities
#   # and rows (n+1):(2*n) = t136 communities.
#   # The diagonal therefore compares each pixel with itself through time
#   bray_pixel <- diag(as.matrix(bray)[1:n, (n+1):(2*n)])
#   invisible(gc())
#   
#   # convert to dataframe and add coordinates
#   results <- data.frame(x = coords$x, 
#                         y = coords$y,
#                         bray = bray_pixel)
#   invisible(gc())
#   
#   # convert to a vector object
#   pts <- vect(results, geom = c("x","y"), crs = crs(t1))
#   
#   # transfomr back into a raster
#   out_bray <- rasterize(pts, t1[[1]], field = "bray")
#   invisible(gc())
#   
#   ######################
#   # JACCARD SIMILARITY #
#   ######################
#   ## USES PRESENCE/ABSENCE DATA
#   
#   # convert abundance into presence/absence
#   pa_t1 <- decostand(comm_t1_sp, method = "pa")
#   pa_t136 <- decostand(comm_t136_sp, method = "pa")
#   
#   # compute Jaccard dissimilarity
#   jac <- vegan::vegdist(rbind(pa_t1, pa_t136), method = "jaccard")
#   invisible(gc())
#   
#   # Extract Jaccard dissimilarity between the SAME pixel
#   # at timestep 26 and timestep 136.
#   # The diagonal compares each pixel with itself through time.
#   jaccard_pixel <- diag(as.matrix(jac)[1:n, (n+1):(2*n)])
#   invisible(gc())
#   
#   # convert to dataframe and add coordinates
#   results_jac <- data.frame(x = coords$x,
#                             y = coords$y,
#                             jaccard = jaccard_pixel)
#   invisible(gc())
#   
#   # convert to vector object
#   pts_jac <- vect(results_jac,
#                   geom = c("x","y"),
#                   crs = crs(t1))
#   
#   # convert back into raster
#   out_jaccard <- rasterize(pts_jac, t1[[1]], field = "jaccard")
#   
#   run_name <- basename(dirname(dir))
#   
#   # save both Bray Curtis and Jaccard indeces into a list
#   all_results[[run_name]] <- list(
#     bray = out_bray,
#     jaccard = out_jaccard
#   )
# }
