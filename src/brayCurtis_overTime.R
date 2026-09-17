## Name: brayCurtis_overTime.R ##
## Author: Inês Silva ##
## Date: 20th May 2025 ##
## Description: Calculate the Bray Curtis Dissimilarity Index per timestep
## (after doing it per cell, values are averaged across the whole landscape) cell for each
## for each scenario, biome, region and trophic level. Output is plotted as
## line plots showing evolution over time of community similarity. Dissimilarity
## is always calculated aginst 2015 (t=26).


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

##########
# STEP 2 # Import rasters and calculate Bray Curtis Dissimilarity
##########

bc_time_series <- list()

outputFolder_paths <- outputFolder_paths

for(dir in outputFolder_paths){
  
  run_name <- basename(dirname(dir))
  
  bc_time_series[[run_name]] <- list()
  
  # species + trophic setup 
  files_all <- list.files(dir, pattern = "abundance\\.tif$", full.names = TRUE)
  species <- unique(sub(".*_(.*)_abundance\\.tif$", "\\1", basename(files_all)))
  
  species_df <- data.frame(species = species) %>%
    left_join(combined_traits_data, by = c("species" = "sci_name")) %>%
    filter(!is.na(trophic_level))
  
  troph_groups <- unique(species_df$trophic_level)
  
  for(troph in troph_groups){
    
    message("Starting for ", troph)
    
    species_used <- species_df %>%
      filter(trophic_level == troph) %>%
      pull(species)
    
    bc_vals <- c()
    
    time_steps <- 26:136
    
    # import baseline rasters (t26)
    t_base_list <- list()
    
    for(sp in species_used){
      f <- list.files(dir,
                      pattern = paste0("26_", sp, "_abundance\\.tif"),
                      full.names = TRUE)
      
      if(length(f) > 0){
        r <- rast(f)
        t_base_list[[sp]] <- app(r, mean, na.rm = TRUE)
      }
    }
    
    t_base <- rast(t_base_list)
    # convert baselnie stack to dataframe
    base_df <- as.data.frame(t_base, xy = FALSE, na.rm = FALSE)
    # convert to matrix
    base_mat <- as.matrix(base_df)
    
    # repeat the same procedure to all other timesteps
    for(t in time_steps){
      
      message("Timestep:", t)
      
      t_list <- list()
      for(sp in species_used){
        
        f <- list.files(dir,
                        pattern = paste0(t, "_", sp, "_abundance\\.tif"),
                        full.names = TRUE)
        
        if(length(f) > 0){
          r <- rast(f)
          # do the mean across replicates
          t_list[[sp]] <- app(r, mean, na.rm = TRUE)
        }
      }
      
      if(length(t_list) == 0){
        bc_vals <- c(bc_vals, NA)
        next
      }
      
      # read raster
      t_rast <- rast(t_list)
      # convert raster to dataframe
      df_t <- as.data.frame(t_rast, na.rm = FALSE)
      # convert to matrix
      mat_t <- as.matrix(df_t)
      
      # estimate the Bray-Curtis per pixel (see formula online to understand the numerator, num, and the denominator, den)
      num <- rowSums(abs(base_mat - mat_t), na.rm = TRUE)
      den <- rowSums(base_mat + mat_t, na.rm = TRUE)
      
      # do the ratio
      bc_pixel <- num / den
      # whenever the denominator is zero replace with NA
      bc_pixel[den == 0] <- NA
      
      # average across pixels (average value for the whole landscape)
      bc_vals <- c(bc_vals, mean(bc_pixel, na.rm = TRUE))
    }
    
    # store result for each timestep per region (here as run_name) and trophic group
    bc_time_series[[run_name]][[troph]] <- data.frame(
      timestep = time_steps,
      bray = bc_vals,
      trophic = troph)
  }
}


# save outputs as one excel file
bc_output <- imap_dfr(bc_time_series, ~ imap_dfr(.x,
                                                 ~ mutate(.x, run = .y)) %>%
                        mutate(run_name = .y))

write_xlsx(bc_output, "E:/metaRange_May26/outputs/BrayCurtis/BrayCurtis_TimeSeries.xlsx")

##########
# STEP 3 # Plot Index over time 
##########

BrayCurtis_TimeSeries <- read_excel("E:/metaRange_May26/outputs/BrayCurtis/BrayCurtis_TimeSeries.xlsx")

# split column into region, scenario and run date to plot
BrayCurtis_TimeSeries <- BrayCurtis_TimeSeries %>%
  tidyr::separate(run_name, c("region", "future_scenario", "runDate"), "_")


# biome labels
biome_names <- c("Boreal Forests/Taiga" = "Boreal Forests/\nTaiga",
                 "Tropical & Subtropical Moist Broadleaf Forests" = "Tropical & Subtropical\nMoist Broadleaf Forests")

# scenario labels
scenario_names <- c("ssp126" = "SSP1-2.6",
                    "ssp585" = "SSP5-8.5")

# region labels
region_names <- c("Asia" = "Asia",
                  "Africa" = "Africa",
                  "Europe+Asia" = "Europe &\nAsia",
                  "North America" = "North\nAmerica",
                  "South America" = "South\nAmerica")

trophic_cols <- c("Herbivore" = "#6A8F52",
                  "Carnivore" = "#F2A65A",
                  "Omnivore"  = "#5B8FA8")

# all runs were previously compiled into one .csv file stored in the outputs folder
TNIND_yr <- read.csv("E:/metaRange_May26/outputs/completeMetaRangeRun_20260517.csv")


# clean up erroneous species
TNIND_yr <- TNIND_yr %>%
  filter(
    !(species == "Lynx lynx" & biome == "Tropical & Subtropical Moist Broadleaf Forests" & region == "Asia"),
    !(species == "Ursus arctos" & biome == "Tropical & Subtropical Moist Broadleaf Forests" & region == "Asia"))

# count number of unique species per biome and trophic level
species_count <- TNIND_yr %>%
  group_by(biome, region, trophic_level) %>%
  summarise(
    n_species = n_distinct(species),
    .groups = "drop")


icon_positions_shannon2 <- BrayCurtis_TimeSeries %>%
  group_by(future_scenario, region, trophic, timestep) %>%
  summarise(bray = mean(bray, na.rm = TRUE)) %>% 
  arrange(timestep) %>%
  slice_tail(n = 5) %>%   # last 5 timesteps
  slice_head(n = 1) %>%   # max - 4
  transmute(
    x = timestep,
    y = bray + 0.02) %>%
  ungroup() %>% 
  left_join(species_count  %>%
              group_by(biome, region, trophic_level) %>% 
              dplyr::rename("trophic" = "trophic_level") %>% 
              summarise(n_species = sum(n_species)), by = c("region", "trophic"))

# order region more logically
region_levels <- c("North America", "Europe+Asia", "South America", "Africa", "Asia")

BrayCurtis_TimeSeries$region <- factor(BrayCurtis_TimeSeries$region, levels = region_levels)
icon_positions_shannon2$region <- factor(icon_positions_shannon2$region, levels = region_levels)

# icon_positions_shannon2$y <- c(0.2, 0.104, 0.04,
#                                0.112, 0.168, 0.112,
#                                0.213, 0.145, 0.213,
#                                0.166, 0.157, 0.167,
#                                0.0780, 0.253, 0.040,
#                                0.153, 0.149, 0.080,
#                                0.120, 0.181, 0.120,
#                                0.190, 0.300, 0.300,
#                                0.302, 0.190, 0.302,
#                                0.108, 0.269, 0.0545
#                                )
# icon_positions_shannon2$x <- c(130, 128, 125,
#                                130, 130, 128,
#                                130, 130, 128,
#                                130, 130, 130,
#                                125, 130, 130,
#                                130, 128, 125,
#                                130, 130, 128,
#                                130,130, 128,
#                                130, 130, 128,
#                                130, 130, 130)
# print(icon_positions_shannon2, n=Inf)


# same yy axis scale for all regions #

BrayCurtis_perRegion <- ggplot(data = BrayCurtis_TimeSeries,
                               aes(x = timestep, y = bray, colour = trophic, fill = trophic)) +
  # ribbon (mean ± SE)
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.25, colour = NA, show.legend = FALSE) +
  # mean line
  stat_summary(fun = mean, geom = "line", linewidth = 0.6) +
  # to deal with axis more freely (add axis on top row)
  ggh4x::facet_grid2(region ~ future_scenario,
                     scales = "free_x", #axes = "x",
                     axes = "all",
                     #remove_labels = "y",
                     switch = "y",
                     labeller = labeller(biome = as_labeller(biome_names),
                                         future_scenario = as_labeller(scenario_names),
                                         region = as_labeller(region_names))) +
  labs(x = "Time", y = "Bray Curtis Dissimilarity Index"
       #, caption = "Dotted line = Start of future scenarios"
  ) +
  scale_color_manual("Trophic levels", values = trophic_cols) +
  scale_fill_manual(values = trophic_cols) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = c(26, 40, 60, 110),
    labels = c("2015", "2030", "2050", "2100")) +
  # line limiting intro of new scenario
  geom_vline(xintercept = c(40, 60, 110), linetype = "dashed", color = "gray50", size = 0.5) +
  # add label with number of species
  #geom_text(data = icon_positions_shannon2,
  #         aes(x = x + c(-2,0,2)[as.numeric(factor(trophic))], y = y + 0.02,
  #           label = paste0("n=", n_species),
  #         colour = trophic), inherit.aes = FALSE, size = 3) +
  theme_minimal(base_size = 10) +
  theme(
    # background  -----
    panel.border = element_blank(),
    
    # grids -----
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed", size = 0.2),
    
    # axes -----
    axis.line = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    #axis.text.x = element_text(vjust = 1, hjust = 1),
    
    # facets -----
    strip.background.x = element_rect(fill = "grey90", colour = NA),
    strip.background.y = element_blank(),
    strip.text = element_text(face = "bold", size = rel(1)),
    strip.placement = "outside",
    
    # legend -----
    legend.position = "bottom",
    legend.justification = "right",
    
    # spacing ------
    plot.margin = unit(c(0, 0.5, 0, 0.5), "cm"),
    panel.spacing.x = unit(1, "lines"),
    #panel.spacing.y = unit(2, "lines")
  )


# save plot
ggsave(filename = "E:/metaRange_May26/FigureAndMetrics/Figure2_BrayCurtisTroughTime_perREGION.png", # path
       BrayCurtis_perRegion, # plot
       bg = 'transparent', width = 180, height = 240, units = "mm", dpi = 1200,
       #compression = "lzw"
) # image parameters


##########
# STEP 4 # Produce Bray-Curtis change Supplementary Table 
##########

BrayCurtis_Change_perRegion <- BrayCurtis_TimeSeries %>%
  dplyr::filter(timestep %in% c(26, 40, 60, 110)) %>%
  mutate(biome = case_when(region %in% c("Asia", "Africa", "South America") ~ "Tropical & Subtropical Moist Broadleaf Forests",
                           region %in% c("Europe+Asia", "North America") ~ "Boreal Forests/Taiga",
                           TRUE ~ NA_character_)) %>% 
  group_by(future_scenario, biome, region, trophic) %>%
  arrange(timestep) %>%
  mutate(brayCurtis = round(bray * 100,3)) %>%
  ungroup() %>%
  dplyr::select(future_scenario, biome, region, trophic,
                timestep, #Shannon_Wiener_Index, 
                brayCurtis) %>%
  tidyr::pivot_wider(names_from = timestep, values_from = c(#Shannon_Wiener_Index,
    brayCurtis),
    names_glue = "{.value}_{timestep}") %>% 
  dplyr::select(-brayCurtis_26) %>% 
  arrange(future_scenario, biome, region) %>% 
  rename("Scenario" = "future_scenario", 
         "Biome" = "biome",
         "Region" = "region",
         "Trophic Level" = "trophic",
         "Change 2030-2015" = "brayCurtis_40",
         "Change 2050-2015" = "brayCurtis_60",
         "Change 2100-2015" = "brayCurtis_110") 

#write.csv(BrayCurtis_Change_perRegion,
#          file = "E:/metaRange_May26/FigureAndMetrics/BrayCurtis_Change_perRegion.csv",
#         row.names = FALSE)         

# save as a formatted word document
gt::gtsave(gt(BrayCurtis_Change_perRegion) %>%
             tab_header(title = "Percentage change in the Bray-Curtis Dissimilarity Index relative to the baseline (2015, timestep 25)"),
           file.path("E:/metaRange_May26/outputs/FigureAndMetrics",
                     paste0("BrayCurtis_Change_perRegion", Sys.Date(), ".docx")))

