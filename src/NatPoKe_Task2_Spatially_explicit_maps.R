## NatPoKe Figure 3 #
######## MIS ########
##### 13 JAN 25 #####

#CHECK THIS https://stackoverflow.com/questions/78425576/specify-which-legend-to-keep-in-wrap-plots

# Packages
library(readr)
library(dplyr)
library(tidyr) # for pivot_wider()
library(ggplot2)
library(viridis)
#library(ggsci)
library(rnaturalearth) # for world maps
library(rnaturalearthdata) # for world maps
library(sf)
library(patchwork)

# import dummy community metrics
community_df <- read_csv("data/community_df_Jan2025.csv")


# specify burn in timestep & policy start
t_burnin <- 2
t_policy <- 5

######################
# CONTINENTS OUTLINE #
######################

# load continent outlines
continents <- ne_countries(scale = "medium", returnclass = "sf")

# filter for regions of interest
north_america <- continents %>%
  filter(continent == "North America") %>%
  select(continent, geometry) %>% 
  st_crop(xmin = -180, ymin = -3, xmax = -45, ymax = 79)
europe <- continents %>% filter(continent == "Europe") %>%
  select(continent, geometry) %>%
  st_crop(xmin = -16, ymin = 30, xmax = 50, ymax = 69)
south_america <- continents %>%
  filter(continent == "South America") %>%
  select(continent, geometry) %>% 
  st_crop(xmin = -90, ymin = -70, xmax = 8, ymax = 34)
africa <- continents %>%
  filter(continent == "Africa") %>%
  select(continent, geometry) %>% 
  st_crop(xmin = -52, ymin = -40, xmax = 81, ymax = 36)
south_asia <- continents %>%
  filter(region_un == "Asia") %>%
  select(continent, geometry) %>% 
  st_crop(xmin = 62, ymin = -21, xmax = 129, ymax = 42)

# create a list of regions for easier access 
region_outlines <- rbind(
  "North America" = north_america,
  "Europe" = europe,
  "South America" = south_america,
  "Africa" = africa,
  "South Asia" = south_asia) %>% 
  group_by(continent) %>% 
  summarise(geometry = st_union(geometry))


##############################
# CALCULATE CHANGE VARIABLES #
##############################

dummy_map_dataset <- community_df %>%
  group_by(biome, scenario, region, taxa) %>% 
  mutate(
    Shannon_change = Shannon_Wiener_Index  - Shannon_Wiener_Index [time == 1],
    Richness_change = Sps_richness - Sps_richness[time == 1],
    Funct_Div_change = Funct_diversity_Index - Funct_diversity_Index[time == 1]) %>%
  ungroup() %>% 
  dplyr::filter(time == 20) # filter last time step for plotting


###########################
# CREATE FAKE COORDINATES # DeleteLater with real data
###########################

set.seed(123)  # Ensure reproducibility

# Define coordinate ranges for each region
region_coords <- list(
  "North America" = data.frame(
    x = seq(-180, -45, length.out = 5),
    y = seq(-3, 79, length.out = 5)
  ),
  "Europe" = data.frame(
    x = seq(0, 20, length.out = 5),
    y = seq(50, 70, length.out = 5)
  ),
  "South America" = data.frame(
    x = seq(-80, -60, length.out = 5),
    y = seq(-10, -30, length.out = 5)
  ),
  "Africa" = data.frame(
    x = seq(10, 30, length.out = 5),
    y = seq(0, -20, length.out = 5)
  ),
  "South Asia" = data.frame(
    x = seq(62, 129, length.out = 5),
    y = seq(10, 30, length.out = 5)
  )
)

# Add coordinates to dummy data
dummy_map_dataset_coords <- dummy_map_dataset %>%
  group_by(region) %>%
  mutate(
    x = rep(region_coords[[unique(region)]]$x, times = n() / 5),
    y = rep(region_coords[[unique(region)]]$y, times = n() / 5)) %>%
  ungroup()

############
# FIGURE 3 # Spatially explicit maps with all policies per regions within each biome
############

# list all regions
regions <- unique(dummy_map_dataset$region)


generate_shannon_plots <- function(
    # THIS FUNCTION PRODUCES CHANGE MAPS PER BIOME, TAXA AND REGION WITH FACETS PER POLICY 
  # TO CHOOSE WHICH VARIABLE TO PLOT CHANGE THE FILL VARIABLE IN STEP 6 WITHIN THIS FUNCTION
  
  data,
  region_outlines, # sf object with regions outlines (it has to have a geometry column)
  output_dir = NULL) # directory to save figures
  
{
  # Step 1 - split the data by biome
  biome_data <- split(data, data$biome)
  
  # Step 2 - create an empty list to store all plots
  all_plots <- list()
  
  # Step 3 - go through each biome
  for (biome_name in names(biome_data)) {
    biome_subset <- biome_data[[biome_name]]
    
    # Step 4 - split the biome-specific data by taxa
    taxa_data <- split(biome_subset, biome_subset$taxa)
    
    # create an empty list to store biome-specific plots
    biome_plots <- list()
    
    # iterate through each taxa
    for (taxa_name in names(taxa_data)) {
      taxa_subset <- taxa_data[[taxa_name]]
      
      # Step 5 - split the taxa-specific data by region
      regions <- unique(taxa_subset$region)
      
      # create an empty list to store region-specific plots
      region_plots <- list()
      
      for (region in regions) {
        # filter data for the current region
        region_data <- taxa_subset[taxa_subset$region == region, ]
        
        # get the matching outline for the region
        region_outline <- region_outlines %>%
          filter(continent == region)
        
        # Step 6 - create the plot with the corresponding region outline
        shannon_plot <- ggplot() +
          geom_sf(data = region_outline, fill = "lightgray", color = "black") + # Add the region outline
          geom_raster(data = region_data, aes(x = x, y = y, fill = Shannon_change)) +
          facet_wrap(~scenario, ncol = 3) +
          scale_fill_viridis() +
          labs(x = "Longitude", y = "Latitude", fill = "Shannon Wiener \nIndex Change") +
          theme(
            legend.position = "bottom",
            panel.background = element_blank(), # no background grids
            # Facets identification customization
            strip.background = element_rect(fill = "grey94", color = "black"),
            strip.text = element_text(face = "italic", size = rel(0.8)),
            panel.border = element_rect(color = "black", fill = NA),
            # Axis and legend font type
            legend.title = element_text(face = "bold"),
            legend.text = element_text(size = 6),
            axis.title = element_text(face = "bold")
          )
        
        # Step 7 - store the plot
        region_plots[[region]] <- shannon_plot
        
        # Step 8 - save the plot as an image if an output directory is provided
        if (!is.null(output_dir)) {
          file_name <- paste0(output_dir, "/", biome_name, "_", taxa_name, "_", region,"Shannon Wiener Index change_plot.tiff")
          ggsave(file_name, shannon_plot, bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, compression = "lzw")
        }
      }
      
      # store region-specific plots in the biome-specific list
      biome_plots[[taxa_name]] <- region_plots
    }
    
    # Store biome-specific plots in the global list
    all_plots[[biome_name]] <- biome_plots
  }
  
  # return the list of all plots
  return(all_plots)
}

# apply the function
all_plots <- generate_shannon_plots(dummy_map_dataset_coords,
                                    region_outlines,
                                    output_dir = "~/NatPoKe/output/dummy_figures")

# get each created plot into a object to call with wrap_plot()
mammals_bf_europe <- all_plots$`Boreal forests`$Mammal$Europe
mammals_bf_northamerica <- all_plots$`Boreal forests`$Mammal$`North America`
mammals_tf_southamerica <- all_plots$`Tropical forests`$Mammal$`South America`
mammals_tf_africa <- all_plots$`Tropical forests`$Mammal$Africa
mammal_tf_southasia <- all_plots$`Tropical forests`$Mammal$`South Asia`


# plots organisation
design <- "AB#
           CDE"

mammals_plots <- wrap_plots(A = mammals_bf_northamerica,
                          B = mammals_bf_europe,
                          C = mammals_tf_southamerica,
                          D = mammals_tf_africa,
                          E = mammal_tf_southasia,
                          design = design) + 
                plot_layout(guides = 'collect') &
                theme(legend.position = "bottom")

# save mammals plot
#ggsave(paste0("~/NatPoKe/output/dummy_figures/","Figure3_SpatiallyExplicitMapsMammals", ".tiff"), mammals_plots, bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, compression = "lzw")





