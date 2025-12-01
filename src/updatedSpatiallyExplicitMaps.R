## Name: UpdatedSpatiallyExplicitMaps.R ##
## Authors: Inês Silva ##
## Description: calculate Shannon-Index Change and build maps per scenario and trophic group ##
## Date: March 28th 2025 updated on November 25th 2025


# Set up, load needed packages & functions
library(here)
source(here("src", "libraries.R"))
source(here("src", "customFunctions2.R"))

##########
# STEP 1 # list all directories with outputs to map SSP5
##########

# Boreal Forests ---------------------------------------------------------------

## Europe SSP5
europe_SSP1 <- "./output/metaRangeRuns/Europe_ssp126_31Oct25/Outputs"
## Europe SSP1
europe_SSP5 <- "./output/metaRangeRuns/Europe_ssp585_31Oct25/Outputs"

## North America SSP5
northamerica_SSP1 <- "./output/metaRangeRuns/NorthAmerica_ssp126_31Oct25/Outputs" 

## North America SSP1
northamerica_SSP5 <- "./output/metaRangeRuns/NorthAmerica_ssp585_31Oct25/Outputs"


# Tropical Moist Forests -------------------------------------------------------

## South America SSP5
southamerica_SSP1 <- "./output/metaRangeRuns/SouthAmerica_ssp126_31Oct25/Outputs"
## South America SSP1
southamerica_SSP5 <- "./output/metaRangeRuns/SouthAmerica_ssp585_31Oct25/Outputs"

## Africa SSP5
africa_SSP1 <- "./output/metaRangeRuns/Africa_ssp126_31Oct25/Outputs"
## Africa SSP1
africa_SSP5 <- "./output/metaRangeRuns/Africa_ssp585_31Oct25/Outputs"

## Asia SSP5
asia_SSP1 <- "./output/metaRangeRuns/Asia_ssp126_31Oct25/Outputs"
## Asia SSP1
asia_SSP5 <- "./output/metaRangeRuns/Asia_ssp585_31Oct25/Outputs"

invisible(gc())

# all directories
directories <- c(europe_SSP5, asia_SSP5, africa_SSP5, southamerica_SSP5, northamerica_SSP5,
                 europe_SSP1, asia_SSP1, africa_SSP1, southamerica_SSP1, northamerica_SSP1)

# get every species that was modeled for the outputs
## Select Target Species (multiple allowed with spaces)
target_species <- c(
  ##############
  # BOREAL SPS #
  ##############
  
  # Europe
  "Alces alces", "Bison bonasus", "Cervus elaphus", "Sus scrofa", "Vulpes vulpes",
  "Panthera tigris", "Lynx lynx", "Ursus arctos", "Canis lupus", "Rangifer tarandus",
  
  # North America
  "Alces alces", "Canis latrans", "Lynx rufus", "Martes americana", "Taxidea taxus",
  "Ursus americanus", "Vulpes vulpes", "Puma concolor", "Bison bison", "Ursus arctos",
  "Canis lupus", "Rangifer tarandus",
  
  # South America
  "Leontopithecus caissara", 
  "Leopardus pardalis", "Nasua nasua", "Panthera onca",
  "Puma concolor",
  
  # Africa
  "Aepyceros melampus", "Colobus angolensis", #"Daubentonia madagascariensis",
  "Diceros bicornis", "Erythrocebus patas", "Gorilla beringei", "Gorilla gorilla",
  "Orycteropus afer", #"Pan paniscus",
  "Pan troglodytes", "Papio anubis", "Papio ursinus", 
  "Crocuta crocuta", "Mandrillus sphinx", "Panthera pardus", "Syncerus caffer",
  "Acinonyx jubatus", "Panthera leo", "Connochaetes taurinus", "Loxodonta africana",
  
  # Asia
  "Cervus nippon", "Cuon alpinus", "Felis chaus", "Macaca fuscata", "Pongo abelii",
  "Pongo pygmaeus", "Sus scrofa", "Vulpes vulpes", "Panthera pardus", "Acinonyx jubatus",
  "Lynx lynx", "Panthera leo", "Panthera tigris", "Ursus arctos",
  "Canis lupus"
)
target_species <- gsub(" ", ".", target_species)

##########
# STEP 2 # Transform rasters
##########

# Initialize an empty list to store final dataframes
all_final_data <- list()

# Loop through each directory
for (dir in directories) {
  
  dir_data <- list()
  
  # Loop through each species
  for (target_sps in target_species) {
    
    for (timestep in c(101, 235)) {
      
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

#all_final_data$`13Sep_Europe_ssp585`
#summary(all_final_data$`13Sep_Europe_ssp585`)
#unique(all_final_data$`13Sep_Europe_ssp585`$species)

##########
# STEP 3 # Calculate Shannon index change **per functional group**
##########

Shannon_indexes <- list()

# call combined trait data to get trophic levels
combined_traits_data <- read_csv(here("data", "mammalTraits_2025-03-17.csv")) %>% 
  mutate(sci_name = gsub("[/& ]", ".",sci_name))

#dir_name <- "13Sep_Europe_ssp585"

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
      mutate(Shannon_change = t235 - t101)
    
    invisible(gc())
    
    # save the df into a nested list (per scenario+region and trophic level)
    Shannon_indexes[[dir_name]][[troph]] <- troph_df
    invisible(gc())
  }
}


# check results
#Shannon_indexes$`28Mar2025_EuropeRobinson`$Herbivore

##########
# STEP 4 # Build actual SHANNON'S INDEX change maps
##########

world <- ne_countries(scale = "medium", returnclass = "sf")

## SSP5-8.5 --------------------------------------------------------------------

###################################
# Global Plot - SSP585 Herbivores #
###################################

herb_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "#606060",color = "#606060", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$NorthAmerica_ssp585_31Oct25$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Europe_ssp585_31Oct25$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Africa_ssp585_31Oct25$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Asia_ssp585_31Oct25$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL) +
  
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
       title = "Herbivores") +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        # legend inside, left bottom corner (adjust as needed)
        legend.position = c(0.09, 0.25),
        # keep vertical color scale
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'),
        # smaller box
        legend.background = element_rect(fill = alpha("white", 0.7),
                                         color = NA),
        # draw a rectangle frame around the map
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )


###################################
# Global Plot - SSP585 Carnivores #
###################################

carn_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "#606060", color = "#606060", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$NorthAmerica_ssp585_31Oct25$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Europe_ssp585_31Oct25$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Africa_ssp585_31Oct25$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Asia_ssp585_31Oct25$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$SouthAmerica_ssp585_31Oct25$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL) +
  
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
       title = "Carnivores") +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        # legend inside, left bottom corner (adjust as needed)
        legend.position = c(0.09, 0.25),
        # keep vertical color scale
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'),
        # smaller box
        legend.background = element_rect(fill = alpha("white", 0.7),
                                         color = NA),
        # draw a rectangle frame around the map
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

###################################
# Global Plot - SSP585 Omnivores #
###################################

omni_ssp585 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "#606060", color = "#606060", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$NorthAmerica_ssp585_31Oct25$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Europe_ssp585_31Oct25$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Africa_ssp585_31Oct25$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Asia_ssp585_31Oct25$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$SouthAmerica_ssp585_31Oct25$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL) +
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
       title = "Carnivores") +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        # legend inside, left bottom corner (adjust as needed)
        legend.position = c(0.09, 0.25),
        # keep vertical color scale
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'),
        # smaller box
        legend.background = element_rect(fill = alpha("white", 0.7),
                                         color = NA),
        # draw a rectangle frame around the map
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

## SSP1-2.6 --------------------------------------------------------------------

###################################
# Global Plot - SSP585 Herbivores #
###################################

herb_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "#606060", color = "#606060", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$NorthAmerica_ssp126_31Oct25$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Europe_ssp126_31Oct25$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Africa_ssp126_31Oct25$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Asia_ssp126_31Oct25$Herbivore, aes(x = x, y = y, fill = Shannon_change)) +
  # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL) +
  
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
       title = "Herbivores") +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        # legend inside, left bottom corner (adjust as needed)
        legend.position = c(0.09, 0.25),
        # keep vertical color scale
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'),
        # smaller box
        legend.background = element_rect(fill = alpha("white", 0.7),
                                         color = NA),
        # draw a rectangle frame around the map
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

###################################
# Global Plot - SSP126 Carnivores #
###################################

carn_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "#606060", color = "#606060", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$NorthAmerica_ssp126_31Oct25$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Europe_ssp126_31Oct25$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Africa_ssp126_31Oct25$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Asia_ssp126_31Oct25$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$SouthAmerica_ssp126_31Oct25$Carnivore, aes(x = x, y = y, fill = Shannon_change)) +
  # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL) +
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
       title = "Carnivores") +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        # legend inside, left bottom corner (adjust as needed)
        legend.position = c(0.09, 0.25),
        # keep vertical color scale
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'),
        # smaller box
        legend.background = element_rect(fill = alpha("white", 0.7),
                                         color = NA),
        # draw a rectangle frame around the map
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

###################################
# Global Plot - SSP126 Omnivores #
###################################

omni_ssp126 <- ggplot() +
  # borders on top
  geom_sf(data = world, fill = "#606060", color = "#606060", linewidth = 0.2) +
  # raster layer
  geom_tile(data = Shannon_indexes$NorthAmerica_ssp126_31Oct25$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Europe_ssp126_31Oct25$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Africa_ssp126_31Oct25$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$Asia_ssp126_31Oct25$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  geom_tile(data = Shannon_indexes$SouthAmerica_ssp126_31Oct25$Omnivore, aes(x = x, y = y, fill = Shannon_change)) +
  # color scale
  scale_fill_gradientn(
    colors = c("#8F0D14",  # strong negative
               "#D13C16",  # moderate negative
               "#F7DDA0",  # neutral
               "#5CA4B3",  # moderate positive
               "#1E4E79"   # strong positive
    ), name = NULL) +
  #scale_fill_scico(palette = "lapaz") +
  #scale_fill_viridis_c(name = "Shannon's Index\nChange") +
  labs(x = "", y = "",
       title = "Omnivores") +
  # Robinson projection
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        # legend inside, left bottom corner (adjust as needed)
        legend.position = c(0.09, 0.25),
        # keep vertical color scale
        legend.direction = "vertical",
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'),
        # smaller box
        legend.background = element_rect(fill = alpha("white", 0.7),
                                         color = NA),
        # draw a rectangle frame around the map
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

# row headers 
ssp126_header <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "SSP126",
           fontface = "bold", size = 5) +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey90", color = NA))

ssp585_header <- ggplot() +
  annotate("text", x = 0.5, y = 0.5, label = "SSP585",
           fontface = "bold", size = 5) +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey90", color = NA))

# Convert to patchwork elements so they can span all columns
ssp126_ribbon <- wrap_elements(full = ssp126_header)
ssp585_ribbon <- wrap_elements(full = ssp585_header)

final_plot <-
  ssp126_ribbon /
  (carn_ssp126 + herb_ssp126 + omni_ssp126) /
  ssp585_ribbon /
  (carn_ssp585 + herb_ssp585 + omni_ssp585) +
  plot_layout(heights = c(0.08, 1, 0.08, 1)) & theme(plot.margin = margin(0,0,0,0))

ggsave("./output/Figure4_ShannonIndexChangeMaps.png",
       final_plot,
       width = 320, height = 20, units = "mm", dpi = 900)







#### LIXO #####






# start empty list for plots
all_plots <- list()

# go through each scenario+region
for (region in names(Shannon_indexes)) {
  
  # go through the trophic levels (functional groups) in the scenario+region
  for (troph in names(Shannon_indexes[[region]])) {
    
    df <- Shannon_indexes[[region]][[troph]]
    
    # make the plot
    p <- ggplot() +
      geom_tile(data = df, aes(x = x, y = y, fill = Shannon_change)) +
      scale_fill_viridis_c(name = "Shannon's Index\nChange",
                           #limits = c(-0.5, 0.5), na.value = "transparent"
      ) +
      labs(x = "Longitude", y = "Latitude", 
           title = paste(region, "-", troph)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    # save to list
    all_plots[[paste(region, troph, sep = "_")]] <- p
    
    # save directly to a .tiff file
    #ggsave(filename = paste0("./output/", region, "_", troph, "_ShannonChange.tif"),
    #   plot = p,
    #  bg = 'white', width = 250, height = 300, units = "mm", dpi = 1200, compression = "lzw")
  }
}

df <- Shannon_indexes[["13Sep_Africa_ssp585"]][["Herbivore"]]

df <- Shannon_indexes$Africa_ssp585_31Oct25$Herbivore
# get African country polygons
africa <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf")

ggplot() +
  # raster layer
  geom_tile(data = df, aes(x = x, y = y, fill = Shannon_change)) +
  # country borders
  geom_sf(data = africa, fill = "#505050", color = "#505050", linewidth = 0.3) +
  # raster layer
  geom_tile(data = df, aes(x = x, y = y, fill = Shannon_change)) +
  # color scale
  scale_fill_viridis_c(
    name = "Shannon's Index\nChange"
    #, limits = c(-0.5, 0.5), na.value = "transparent"
  ) +
  labs(x = "Longitude", y = "Latitude",
       title = paste(region, "-", troph)) +
  # Robinson projection (EPSG:54030)
  coord_sf(crs = "+proj=robin") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


all_plots$`13Sep_Africa_ssp585_Herbivore`
all_plots$`13Sep_Europe_ssp585_Herbivore`
all_plots$`13Sep_Asia_ssp585_Omnivore`
all_plots$`13Sep_Asia_ssp126_Omnivore`
all_plots$`13Sep_Asia_ssp585_Carnivore`
all_plots$`13Sep_Africa_ssp126_Herbivore`
# check results
#all_plots$`28Mar2025_EuropeRobinson_Herbivore`

##############
# EXTRA STEP # Organise plots in a figure
##############


# The idea, as of 10 May 2025, is to have one figure per trophic group showing
# both scenarios (SSP1 and SSP5), beacuse in total using only mammals we will
# have 30 different maps (3 functional groups, 2 scenarios and 5 continents)


# aa <- ((southamerica_shannon_SSP5 + southamerica_shannon_SSP5 + southamerica_plot) /(africa_shannon_SSP5 + africa_shannon_SSP5 + africa_plot)/ (asia_shannon_SSP5 + asia_shannon_SSP5 + asia_plot)) +
#   plot_layout(guides = 'collect', widths = 1) +
#   plot_annotation(tag_levels = list(c("A", "B", "", "C", "D", "", "E", "F", ""))) & theme(legend.position = 'bottom', plot.tag = element_text(size = 12))
# 
# ggsave(plot = aa,
#        file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/figures_20250427/figure3_test.png",
#        bg = 'white', width = 400, height = 500, units = "mm", dpi = 1200, #compression = "lzw"
#        )