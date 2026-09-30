## Name: shannonWiener_overTime.R ##
## Author: Inês Silva ##
## Date: 23 Setembro 2026 ##
## Description: Combine Shannon Wiener and Bray Curtis dissimilarity in a single
## plot with two axis.

source("./src/libraries.R")
source("./src/customFunctions2.R")

##########
# STEP 1 # Import indices timeseries
##########

# Shannon-Wiener Change (each timestep - 2015)
ShannonWiener_TimeSeries <- read.csv("E:/metaRange_May26/outputs/Shannon_Wiener/ShannonWiener_TimeSeries.csv")   

# order region more logically
region_levels <- c("North America", "Europe+Asia",
                   "South America", "Africa", "Asia")
ShannonWiener_TimeSeries$region <- factor(ShannonWiener_TimeSeries$region,
                                          levels = region_levels)

# Bray Curtis Dissimilarity (each timestep vs 2015)
BrayCurtis_TimeSeries <- read_excel("E:/metaRange_May26/outputs/BrayCurtis/BrayCurtis_TimeSeries.xlsx")


BrayCurtis_TimeSeries <- BrayCurtis_TimeSeries %>%
  # split runame into multiple columns to match the shannon df
  separate(run_name, into = c("region", "future_scenario", "date"), sep = "_") %>% 
  dplyr::select(-c(date, run)) %>% 
  # add biome based on region names
  mutate(
    biome = case_when(
      region %in% c("Europe+Asia", "North America") ~ "Boreal Forests/Taiga",
      region %in% c("Asia", "Africa", "South America") ~ 
        "Tropical & Subtropical Moist Broadleaf Forests",
      TRUE ~ NA_character_))

# order regions more logically
BrayCurtis_TimeSeries$region <- factor(BrayCurtis_TimeSeries$region, levels = region_levels)

##########
# STEP 2 # Find a factor that multiplies one of the axis to match the other
##########

shannon_max <- max(ShannonWiener_TimeSeries$Shannon_Wiener_Index)
bray_max <- max(BrayCurtis_TimeSeries$bray)

bray_to_shannon <- shannon_max / bray_max
## 3.6

##########
# STEP 3 # Plot everything together
##########

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

trophic_cols <- c("Herbivore" = "#6A8F52", "Carnivore" = "#F2A65A", "Omnivore"  = "#5B8FA8")


Shannon_Bray_perRegion <- ggplot() +
  # plot Shannon Wiener change
  stat_summary(data = ShannonWiener_TimeSeries,
    aes(x = timestep, y = Shannon_Wiener_Index, colour = trophic_level),
    fun = mean, geom = "line", linewidth = 0.6) +
  # plot Bray Curtis dissimilarity
  stat_summary(data = BrayCurtis_TimeSeries,
    aes(x = timestep, y = 0.5 + bray * (shannon_max - 0.5) / bray_max, colour = trophic),
    fun = mean, geom = "line", linewidth = 0.6, linetype = "dashed") +
  # facet by scenario and region
  ggh4x::facet_nested(biome + region ~ future_scenario,
                      scales = "free_x", switch = "y", #axes = "all",
                      #remove_labels = "x",
                      labeller = labeller(
                        future_scenario = as_labeller(scenario_names),
                        region = as_labeller(region_names),
                        biome = as_labeller(biome_names)),
                      # add spanner for biomes next to regions
                      strip = ggh4x::strip_nested(background_x = element_rect(fill = "grey90", colour = NA),
                                                  by_layer_x = TRUE),
                      nest_line = element_line(linewidth = 0.5, colour = "grey30")) +
  scale_y_continuous(# primary axis
                     name = "Shannon-Wiener index", limits = c(0.4, 2.4),
                     breaks = seq(0.5, 2.0, 0.5),
                     # secondary axis
                     sec.axis = sec_axis(~ (. - 0.5) * bray_max / (shannon_max - 0.5),
                                         name = "Bray-Curtis dissimilarity",
                                         breaks = seq(0, 0.6, 0.1))) +
  scale_x_continuous(expand = c(0, 0), breaks = c(26, 40, 60, 110),
                     labels = c("2015", "2030", "2050", "2100")) +
  # scenario boundaries
  geom_vline(xintercept = c(40, 60, 110), linetype = "dashed", colour = "gray75", linewidth = 0.2) +
  # color scale
  scale_color_manual("Trophic levels", values = trophic_cols) +
  #scale_fill_manual(values = trophic_cols) +
  labs(x = "Time") +
  theme_minimal(base_size = 10) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour = "gray90", linetype = "dashed",
                                          linewidth = 0.2),
        #axis.line = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.line.y.right = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        strip.background.x = element_rect(fill = "grey90", colour = NA),
        strip.background.y = element_blank(),
        strip.text = element_text(face = "bold", size = rel(1)),
        strip.placement = "outside",
        legend.position = "bottom",
        #legend.justification = "right",
        plot.margin = unit(c(0, 0.5, 0, 0.5), "cm"),
        panel.spacing.x = unit(1, "lines"))


ggsave(filename = "E:/metaRange_May26/outputs/Figure2_ShannonWiener_BrayCurtis_TwoAxis.png", # path
       Shannon_Bray_perRegion, # plot
       bg = 'white', 
       width = 180, height = 240, units = "mm", dpi = 1200
       #width = 340, height = 150, units = "mm", dpi = 1200,
       #compression = "lzw"
) # image parameters
