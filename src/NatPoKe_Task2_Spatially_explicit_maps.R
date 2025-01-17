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
library(terra)
library(raster)
library(patchwork) # to mix and match different plots in a grid


# import dummy community metrics
community_df <- read_csv("data/community_df_peryear_Jan2025.csv")


# specify burn in timestep & policy start
t_burnin <- 2
t_policy <- 5

##########################################
# GET CONTINENTS AND FORESTS SHAPE FILES #
##########################################


# get continents from rnaturalearth
continents <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::filter(continent %in% c("Africa", "Asia", "Europe", "North America", "South America")) %>% 
  group_by(continent) %>%
  summarise(geometry = st_union(geometry))

southamerica <- continents %>% 
  dplyr::filter(continent %in% "Europe")
extent(southamerica)

# import ecoregions shapefile
ecoregions_2017 <- sf::st_read(
  "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/SRIT_ANDRE/external_data/Ecoregions2017/Ecoregions2017.shp")

sf_use_s2(FALSE)
# subset only ropical Moist and Boreal Forests
forests_2017 <- ecoregions_2017 %>%
  subset(
    BIOME_NAME %in% c(
      "Tropical & Subtropical Moist Broadleaf Forests",
      "Boreal Forests/Taiga"
    )
  ) %>%
  group_by(BIOME_NAME) %>%
  summarize(geometry = st_union(geometry))

# import raster ----------------------------------------------------------------
rast_alces <- rast("~/NatPoKe/data/Alcesalces_suitability.tif")
#plot(rast_alces)

# transform continent CRS to match the raster CRS
continents <- st_transform(continents, crs(rast_alces))

# transform forests to match raster CRS
forests_2017 <- st_transform(forests_2017, crs(rast_alces))

##############################
# CALCULATE CHANGE VARIABLES #
##############################

# dummy_map_dataset <- community_df %>%
#   group_by(biome, scenario, region, taxa) %>% 
#   mutate(
#     Shannon_change = Shannon_Wiener_Index  - Shannon_Wiener_Index [time == 1],
#     Richness_change = Sps_richness - Sps_richness[time == 1],
#     Funct_Div_change = Funct_diversity_Index - Funct_diversity_Index[time == 1]) %>%
#   ungroup() %>% 
#   dplyr::filter(time == 20) # filter last time step for plotting
# 
# # write this dataframe into a .csv to 
# write.csv(dummy_map_dataset, "~/NatPoKe/data/community_dfchange_variables_Jan2025.csv")
# 

################
# MOOSE RASTER # Just as an example to avoid creating fake pixels in water
################

# crop moose raster per continents and then per type of forest -----------------

# Initialize list to store results
cropped_rasters <- list()

# loop through each continent
for (continent in unique(continents$continent)) {
  # filter continent geometry
  continent_geom <- continents %>% filter(continent == !!continent)
  
  # convert to SpatVector (to compatible with terra pck objects)
  continent_vect <- vect(continent_geom)
  
  # crop the continent raster
  continent_raster <- mask(crop(rast_alces, continent_vect), continent_vect)
  
  # subset forests for each forest type based on the continent
  relevant_forests <- forests_2017 %>%
    filter((BIOME_NAME == "Tropical & Subtropical Moist Broadleaf Forests" & 
              continent %in% c("Africa", "South America", "Asia")) |
             (BIOME_NAME == "Boreal Forests/Taiga" &
                continent %in% c("North America", "Europe")))
  
  # loop through relevant forests for the continent
  for (forest_type in unique(relevant_forests$BIOME_NAME)) {
    # filter forest geometry for the current forest type
    forest_geom <- relevant_forests %>% filter(BIOME_NAME == forest_type)
    
    # convert to SpatVector (terra-compatible)
    forest_vect <- vect(forest_geom)
    
    # crop the continent raster by the forest type
    forest_raster <- mask(crop(continent_raster, forest_vect), forest_vect)
    
    # save the cropped raster combining continent and forest type in the name
    raster_key <- paste0(continent, "_", forest_type)
    cropped_rasters[[raster_key]] <- forest_raster
    
    # IF NEEDED save each cropped raster to a file
    #writeRaster(forest_raster, paste0("cropped_", raster_key, ".tif"), overwrite = TRUE)
  }
}


# convert all cropped rasters to data frames
raster_df_list <- lapply(names(cropped_rasters), function(key) {
  raster_df <- as.data.frame(cropped_rasters[[key]], xy = TRUE, na.rm = TRUE)
  
  # extract continent and forest typenames from the key
  parts <- strsplit(key, "_")[[1]]
  raster_df$continent <- parts[1]
  raster_df$forest_type <- parts[2]
  
  return(raster_df)
})

# combine everything
raster_df <- bind_rows(raster_df_list) %>% 
  rename("region" = "continent",
         "biome" = "forest_type") %>%
  # Expand for each combination of taxa and scenarios
  expand_grid(
    taxa = c("taxa1", "taxa2", "taxa3"),
    scenario = c("BAU", "policy1", "policy2", "policy3", "policy4", "policy5")
    
  )


############
# FIGURE 3 # Spatially explicit maps with all policies per regions within each biome
############

# list all regions
regions <- unique(raster_df$region)

region_extents <- list(
  "Africa" = c(xmin = -25.34155, ymin = -40, xmax = 51.39023, ymax = 25),
  "Asia" = c(xmin = 70, ymin = -12.1998, xmax = 145.833, ymax = 55.3896),
  "Europe" = c(xmin = -25, ymin = 0, xmax = 180, ymax = 81.85),
  "North America" = c(xmin = -160, ymin = 20, xmax = -20, ymax = 83.59961),
  "South America" = c(xmin = -90, ymin = -55.8917, xmax = -34.80547, ymax = 12.43437)
)

generate_shannon_plots <- function(
    # THIS FUNCTION PRODUCES CHANGE MAPS PER BIOME, TAXA AND REGION WITH FACETS PER POLICY 
    # TO CHOOSE WHICH VARIABLE TO PLOT CHANGE THE FILL VARIABLE IN STEP 6 WITHIN THIS FUNCTION
  
  data,
  continents, # sf object with regions outlines (it has to have a geometry column)
  output_dir) #directory to save images
  
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
    invisible(gc())
    
    # create an empty list to store biome-specific plots
    biome_plots <- list()
    
    # iterate through each taxa
    for (taxa_name in names(taxa_data)) {
      taxa_subset <- taxa_data[[taxa_name]]
      invisible(gc())
      
      # Step 5 - split the taxa-specific data by region
      regions <- unique(taxa_subset$region)
      
      # create an empty list to store region-specific plots
      region_plots <- list()
      
      for (region in regions) {
        # filter data for the current region
        region_data <- taxa_subset[taxa_subset$region == region, ]
        invisible(gc())
        
        # get the matching outline for the region
        region_outline <- continents %>%
          filter(continent == region) 
        invisible(gc())
        
        # Get the cropping extent for the current region
        extent <- region_extents[[region]]
        
        # Crop the region outline using the corresponding extent
        if (!is.null(extent)) {
          region_outline <- st_crop(region_outline, 
                                    st_sfc(st_polygon(list(rbind(
                                      c(extent["xmin"], extent["ymin"]),
                                      c(extent["xmax"], extent["ymin"]),
                                      c(extent["xmax"], extent["ymax"]),
                                      c(extent["xmin"], extent["ymax"]),
                                      c(extent["xmin"], extent["ymin"])
                                    ))), crs = st_crs(region_outline)))}
        
        # Step 6 - create the plot with the corresponding region outline
        shannon_plot <- ggplot() +
          # plot data for the index in question (here Shannon wiener = sum just because these are dummydata)
          geom_raster(data = region_data, aes(x = x, y = y, fill = sum)) +
          
          # plot a region outline if needed
          geom_sf(data = region_outline, fill = "lightgray", color = "black") + 
          
          # plot the data for the index again over the regions outline
          geom_raster(data = region_data, aes(x = x, y = y, fill = sum)) +
          
          # facet over the existing policies
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
        invisible(gc())
        
        # Step 7 - store the plot
        region_plots[[region]] <- shannon_plot
        
        # Step 8 - save the plot as an image if an output directory is provided
        file_name <- paste0(output_dir, taxa_name, "_", region,"Shannon Wiener Index change_plot.tiff")
        ggsave(file_name, shannon_plot, bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, compression = "lzw")
        invisible(gc())
      }
      
      # store region-specific plots in the biome-specific list
      biome_plots[[taxa_name]] <- region_plots
      invisible(gc())
    }
    
    # store biome-specific plots in the global list
    all_plots[[biome_name]] <- biome_plots
    invisible(gc())
  }
  
  # return the list of all plots
  return(all_plots)
}

# apply function to complete dataset
plots <- generate_shannon_plots(data = raster_df,
                               continents = continents,
                               output_dir = "~/NatPoKe/output/dummy_figures/")
invisible(gc())

#######################################
# PUT ALL PLOTS TOGETHER FOR ONE TAXA #
#######################################

# get each created plot into a object to call with wrap_plot()
mammals_bf_europe <- plots$`Boreal Forests/Taiga`$taxa1$Europe
mammals_bf_northamerica <- plots$`Boreal Forests/Taiga`$taxa1$`North America`
mammals_tf_southamerica <- plots$`Tropical & Subtropical Moist Broadleaf Forests`$taxa1$`South America`
mammals_tf_africa <- plots$`Tropical & Subtropical Moist Broadleaf Forests`$taxa1$Africa
mammal_tf_southasia <- plots$`Tropical & Subtropical Moist Broadleaf Forests`$taxa1$Asia
invisible(gc())


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
ggsave(paste0("~/NatPoKe/output/dummy_figures/","Figure3_SpatiallyExplicitMapsMammals", ".tiff"),
       mammals_plots,
       bg = 'white', width = 297, height = 210, units = "mm", dpi = 1200, compression = "lzw")





