## Figures for FinBio Meeting ##
## Inês Silva ##
## 16 Sept 2025 ##

setwd("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/NatPoKe")

source("./src/libraries.R")
source("./src/customFunctions.R")  


##########
## SDMs ## 
##########

library(terra)
library(ggplot2)
library(patchwork)

species_layouts <- list()

# load rasters
alces_ssp126 <- rast("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_NorthAmerica_ssp126/Inputs/Alces.alces_boreal_ssp126_cropped_reprojectedKm.tif")
alces_ssp585 <- rast("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/13Sep_NorthAmerica_ssp585/Inputs/Alces.alces_boreal_ssp585_cropped_reprojectedKm.tif")

# get north America map 
na_map <- ne_countries(continent = "North America", scale = "medium", returnclass = "sf")


# convert raster to ggplot with dark theme 
plot_raster <- function(r, year, scenario) {
  # data as dataframe
  r_df <- as.data.frame(r[[year]], xy = TRUE, na.rm = TRUE)
  names(r_df)[3] <- "Suitability"
  
  ggplot() +
    # plot background map
    geom_sf(data = na_map, fill = "#525e68ff", color = "white", linewidth = 0.3) +
    # plot sps suitability data
    geom_raster(data = r_df, aes(x = x, y = y, fill = Suitability)) +
    # colour scale
    scale_fill_viridis_c(na.value = "transparent", limits = c(0,1)) +
    coord_sf(crs = "+proj=robin",  xlim = c(-13696602.639126,-3236173.981084 )) +
    labs(title = paste(scenario, year)) +
    theme_void(base_size = 10) +
    # dark theme
    theme(
      plot.background = element_rect(fill = "#525e68ff", color = NA),
      panel.background = element_rect(fill = "#525e68ff", color = NA),
      plot.title = element_text(size = 10, hjust = 0.5, color = "white"),
      legend.position = "none"
    )
}

# format and extract plots with dark theme
p_current   <- plot_raster(alces_ssp126, "2015", "Current") # 2015 baseline
p_126_2030  <- plot_raster(alces_ssp126, "2030", "ssp126")
p_126_2050  <- plot_raster(alces_ssp126, "2050", "ssp126")
p_126_2100  <- plot_raster(alces_ssp126, "2100", "ssp126")

p_585_2030  <- plot_raster(alces_ssp585, "2030", "ssp585")
p_585_2050  <- plot_raster(alces_ssp585, "2050", "ssp585")
p_585_2100  <- plot_raster(alces_ssp585, "2100", "ssp585")

# arrange all plots with patchwork package
species_plot <- 
  (p_current | (p_126_2030 / p_585_2030) | (p_126_2050 / p_585_2050) | (p_126_2100 / p_585_2100)) +
  plot_layout(widths = c(1, 1, 1, 1), guides = "collect") +
  plot_annotation(title = expression(italic("Alces alces") ~ "-" ~ "North America")) &
  theme(
    plot.background = element_rect(fill = "#525e68ff", color = NA),
panel.background = element_rect(fill = "#525e68ff", color = NA),
legend.position = "bottom",
plot.title = element_text(color = "white"))
  
# save fig
  ggsave(
    plot = species_plot,
    file = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/output/SDMlandscapes_Alces_alces_NorthAmerica.png",
    bg = "#525e68ff", width = 400, height = 150, units = "mm", dpi = 1200
  )
  
###########################  
# POPULATION TRENDS PLOTS #
###########################

# target sps whose results make some sense
target_sps <- c("Rangifer tarandus", "Canis lupus", "Alces alces",
                "Lynx rufus", "Panthera leo", "Crocuta crocuta",
                "Loxodonta africana", "Orycteropus afer")

# all runs were previously compiled into one .csv file stored in the outputs folder
TNIND_yr <- fread("./output/13Sept_FinBioMeeting/completeRun13Sep2025.csv") %>% 
  filter(species %in% c("Rangifer tarandus", "Canis lupus", "Alces alces",
                        "Lynx rufus", "Panthera leo", "Crocuta crocuta",
                        "Loxodonta africana", "Orycteropus afer"))

# filter for target regions for figures
df <- TNIND_yr %>%
  filter(region %in% c("NorthAmerica", "Africa")) 


plot_list <- list()

for (sc in unique(df$scenario)) {
  for (bm in unique(df$biome)) {
    for (rg in unique(df$region)) {
      
      subdf <- df %>%
        filter(timestep >= 100,
               scenario == sc, biome == bm, region == rg)
      
      if (nrow(subdf) == 0) next  # skip empty combos
      
      p <- ggplot(subdf, aes(x = timestep, y = TNIND)) +
        facet_wrap(~species, scales = "free_y", nrow = 7) +
        geom_line(color = "white", size = 0.8) +
        labs(x = "Time",
             y = "Total number of individuals",
             title = sc) +
        scale_x_continuous(
          breaks = c(100, 115, 135, 185),
          labels = c("2015", "2030", "2050", "2100")
        ) +
        geom_vline(xintercept = c(115, 135, 185),
                   linetype = "dotted",
                   color = "red", size = 0.8) +
        theme_minimal(base_size = 14) +
        theme(
          plot.background = element_rect(fill = "#525e68ff", color = NA),
          panel.background = element_rect(fill = "#525e68ff", color = NA),
          strip.background = element_rect(fill = "#444c55", color = NA),
          strip.text = element_text(face = "italic", color = "white"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white"),
          axis.line = element_line(color = "white"),
          axis.ticks = element_line(color = "white"),
          panel.grid = element_line(color = "grey50"),
          plot.title = element_text(color = "white", hjust = 0.5)
        )
      
      plot_list[[paste(sc, bm, rg, sep = "_")]] <- p
      
      # Save (transparent bg replaced with custom background)
      # ggsave(paste0("./output/TNIND_", sc, "_", bm, "_", rg, ".png"),
      #        plot = p, width = 7, height = 5, dpi = 1200, bg = "#525e68ff")
    }
  }
}

# north America
plot_list$ssp585_BorealForestsTaiga_NorthAmerica
plot_list$ssp126_BorealForestsTaiga_NorthAmerica

# Africa
plot_list$ssp585_TropicalSubtropicalMoistBroadleafForests_Africa
plot_list$ssp126_TropicalSubtropicalMoistBroadleafForests_Africa


###############################
# COMMUNITY METRICS OVER TIME #
###############################

source("./src/CommunityMetrics.R")

ShannonOverTime <- ggplot(data = Shannon_index,
                          aes(x = timestep, y = Shannon_Wiener_Index, color = scenario)) +
  geom_line(size = 0.8) +
  # facet grid
  ggh4x::facet_grid2(
    biome ~ trophic_level,
    scales = "free", axes = "x", switch = "y",
    labeller = labeller(biome = as_labeller(biome_names))) +
  labs(x = "Time", 
       y = "Shannon-Wiener index", 
       caption = "Dotted line = Start of future scenarios") +
  scale_color_manual("Socio-economic\nscenario", values = custom_colors) +
  ylim(0, 0.45) +
  scale_x_continuous(
    breaks = c(100, 115, 135, 185),
    labels = c("2015", "2030", "2050", "2100")) +
  # add PhyloPic icons
  geom_phylopic(data = icon_positions_shannon,
                aes(x = x, y = y, uuid = phylopic), 
                size = 0.06, color = "#B6CF23",      # outline
                fill  = "#B6CF23",      # fill
                inherit.aes = FALSE) +  
  # add label with number of species
  geom_text(data = icon_positions_shannon, 
            aes(x = x, y = 0.4, label = paste0("n = ", n_species)), 
            inherit.aes = FALSE, size = 2.5, color = "white") +
  # theme modifications
  theme_minimal(base_size = 14) +
  theme(
    plot.background   = element_rect(fill = "#525e68ff", color = NA),
    panel.background  = element_rect(fill = "#525e68ff", color = NA),
    panel.grid        = element_line(color = "grey50"),
    panel.grid.major.y = element_line(color = "grey60", linetype = "dashed"),
    panel.grid.minor  = element_blank(),
    
    axis.text         = element_text(color = "white"),
    axis.title        = element_text(color = "white"),
    axis.line.x       = element_line(color = "white"),
    axis.line.y       = element_line(color = "white"),
    axis.ticks        = element_line(color = "white"),
    axis.text.x       = element_text(angle = 45, vjust = 1, hjust = 1, color = "white"),
    
    strip.text        = element_text(face = "bold", size = rel(1), color = "white"),
    strip.background  = element_rect(fill = "#444c55", color = NA),
    strip.placement   = "outside",
    
    legend.position   = "bottom",
    legend.text       = element_text(color = "white"),
    legend.title      = element_text(color = "white"),
    plot.caption      = element_text(color = "white"),
    
    panel.border      = element_blank(),
    panel.spacing.x   = unit(3, "lines"),
    panel.spacing.y   = unit(2, "lines"),
    plot.margin       = unit(c(0, 0.5, 0, 0.5), "cm")
  ) 

ShannonOverTime

###########################
# SPATIALLY EXPLICIT MAPS #
###########################

source("./src/updatedSpatiallyExplicitMaps.R")

all_plots <- list()

# loop through each scenario+region
for (region in names(Shannon_indexes)) {
  
  # loop through trophic levels (functional groups)
  for (troph in names(Shannon_indexes[[region]])) {
    
    df <- Shannon_indexes[[region]][[troph]]
    
    # assign continent based on region name
    reg <- case_when(
      grepl("Europe", region) ~ "Europe",
      grepl("NorthAmerica", region) ~ "North America",
      grepl("Africa", region) ~ "Africa",
      TRUE ~ "Africa"  # default fallback
    )
    
    # get country polygons
    map <- ne_countries(continent = reg, scale = "medium", returnclass = "sf")
    
    # make the plot
    p <- ggplot() +
      geom_tile(data = df, aes(x = x, y = y, fill = Shannon_change)) +
      # country borders
      geom_sf(data = map, fill = NA, color = "white", linewidth = 0.3) +
      # color scale
      scale_fill_viridis_c(
        name = "Shannon's Index\nChange",
        #option = "C"  # good contrast on dark background
        #, limits = c(-0.5, 0.5), na.value = "transparent"
      ) +
      # plot labels
      labs(x = "Longitude", y = "Latitude", title = paste(region, "-", troph)) +
      # Robinson projection
      coord_sf(crs = "+proj=robin") +
      # dark theme
      theme_minimal(base_size = 14) +
      theme(
        plot.background   = element_rect(fill = "#525e68ff", color = NA),
        panel.background  = element_rect(fill = "#525e68ff", color = NA),
        panel.grid        = element_line(color = "grey50"),
        axis.text         = element_blank(),
        axis.ticks        = element_blank(),
        axis.title        = element_text(color = "white"),
        plot.title        = element_text(color = "white", hjust = 0.5),
        legend.text       = element_text(color = "white"),
        legend.title      = element_text(color = "white"))
    
    # save to list
    all_plots[[paste(region, troph, sep = "_")]] <- p
    
    # optional: save as .tiff directly
    # ggsave(
    #   filename = paste0("./output/", region, "_", troph, "_ShannonChange.tif"),
    #   plot = p,
    #   bg = "#525e68ff", width = 250, height = 300, units = "mm",
    #   dpi = 1200, compression = "lzw"
    # )
  }
}
all_plots$`13Sep_Europe_ssp585_Herbivore`
all_plots$`13Sep_Europe_ssp585_Omnivore`
all_plots$`13Sep_Europe_ssp585_Carnivore`
all_plots$`13Sep_Europe_ssp126_Herbivore`
#all_plots$`13Sep_Europe_ssp585_Herbivore`
df <- Shannon_indexes[["13Sep_NorthAmerica_ssp126"]][["Carnivore"]]

# get country polygons
northAmerica <- ne_countries(continent = "North America", scale = "medium", returnclass = "sf")
# make the plot
carnivoreNA <- ggplot() +
  geom_tile(data = df, aes(x = x, y = y, fill = Shannon_change)) +
  # country borders
  geom_sf(data = northAmerica, fill = NA, color = "white", linewidth = 0.3) +
  # color scale
  scale_fill_viridis_c(
    name = "Shannon's Index\nChange", limits = c(-0.5, 0.5)
    ) +
  # plot labels
  labs(x = "Longitude", y = "Latitude", title = troph) +
  # Robinson projection
  coord_sf(crs = "+proj=robin", xlim = c(-13696602.639126,-3236173.981084 )) +
  # dark theme
  theme_minimal(base_size = 14) +
  theme(
    plot.background   = element_rect(fill = "#525e68ff", color = NA),
    panel.background  = element_rect(fill = "#525e68ff", color = NA),
    panel.grid        = element_line(color = "grey50"),
    axis.text         = element_blank(),
    axis.ticks        = element_blank(),
    axis.title        = element_text(color = "white"),
    plot.title        = element_text(color = "white", hjust = 0.5),
    legend.text       = element_text(color = "white"),
    legend.title      = element_text(color = "white"))
carnivoreNA

df <- Shannon_indexes[["13Sep_NorthAmerica_ssp126"]][["Herbivore"]]
# make the plot
herbivoreNA <- ggplot() +
  geom_tile(data = df, aes(x = x, y = y, fill = Shannon_change)) +
  # country borders
  geom_sf(data = northAmerica, fill = NA, color = "white", linewidth = 0.3) +
  # color scale
  scale_fill_viridis_c(
    name = "Shannon's Index\nChange", limits = c(-0.5, 0.5)) +
  # plot labels
  labs(x = "Longitude", y = "Latitude", title = "Herbivore") +
  # Robinson projection
  coord_sf(crs = "+proj=robin", xlim = c(-13696602.639126,-3236173.981084 )) +
  # dark theme
  theme_minimal(base_size = 14) +
  theme(
    plot.background   = element_rect(fill = "#525e68ff", color = NA),
    panel.background  = element_rect(fill = "#525e68ff", color = NA),
    panel.grid        = element_line(color = "grey50"),
    axis.text         = element_blank(),
    axis.ticks        = element_blank(),
    axis.title        = element_text(color = "white"),
    plot.title        = element_text(color = "white", hjust = 0.5),
    legend.text       = element_text(color = "white"),
    legend.title      = element_text(color = "white"))
herbivoreNA

carnivoreNA + herbivoreNA +
  plot_layout(guides = "collect") &
  theme(legend.position = "right") 

df <- Shannon_indexes[["13Sep_Africa_ssp126"]][["Carnivore"]]

# get country polygons
africa <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf")
# make the plot
carnivoreA <- ggplot() +
  geom_tile(data = df, aes(x = x, y = y, fill = Shannon_change)) +
  # country borders
  geom_sf(data = africa, fill = NA, color = "white", linewidth = 0.3) +
  # color scale
  scale_fill_viridis_c(
    name = "Shannon's Index\nChange", limits = c(-0.2, 0.2)) +
  # plot labels
  labs(x = "Longitude", y = "Latitude", title = troph) +
  # Robinson projection
  coord_sf(crs = "+proj=robin") +
  # dark theme
  theme_minimal(base_size = 14) +
  theme(
    plot.background   = element_rect(fill = "#525e68ff", color = NA),
    panel.background  = element_rect(fill = "#525e68ff", color = NA),
    panel.grid        = element_line(color = "grey50"),
    axis.text         = element_blank(),
    axis.ticks        = element_blank(),
    axis.title        = element_text(color = "white"),
    plot.title        = element_text(color = "white", hjust = 0.5),
    legend.text       = element_text(color = "white"),
    legend.title      = element_text(color = "white"))
carnivoreA

df <- Shannon_indexes[["13Sep_Africa_ssp126"]][["Herbivore"]]
# make the plot
herbivoreA <- ggplot() +
  geom_tile(data = df, aes(x = x, y = y, fill = Shannon_change)) +
  # country borders
  geom_sf(data = africa, fill = NA, color = "white", linewidth = 0.3) +
  # color scale
  scale_fill_viridis_c(
    name = "Shannon's Index\nChange", limits = c(-0.2, 0.2)) +
  # plot labels
  labs(x = "Longitude", y = "Latitude", title = "Herbivore") +
  # Robinson projection
  coord_sf(crs = "+proj=robin") +
  # dark theme
  theme_minimal(base_size = 14) +
  theme(
    plot.background   = element_rect(fill = "#525e68ff", color = NA),
    panel.background  = element_rect(fill = "#525e68ff", color = NA),
    panel.grid        = element_line(color = "grey50"),
    axis.text         = element_blank(),
    axis.ticks        = element_blank(),
    axis.title        = element_text(color = "white"),
    plot.title        = element_text(color = "white", hjust = 0.5),
    legend.text       = element_text(color = "white"),
    legend.title      = element_text(color = "white"))
herbivoreA

carnivoreA + herbivoreA +
  plot_layout(guides = "collect") &
  theme(legend.position = "right") 
