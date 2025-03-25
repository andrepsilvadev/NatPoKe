## Name: BiomeMap.R ##
## Authors:  André P. Silva ##
## Description: Creates global overview map with highlighted biomes ##
## Date: March 21th 2025 ##

# Settings & libraries -------------------------------------------
source("./src/libraries.R") # libraries

## Extract tropical moist forest and boreal forest shp
ecoregions_2017 <- st_read("~/data/data/Ecoregions2017/Ecoregions2017/Ecoregions2017.shp")

# work on flat earth
sf_use_s2(FALSE) 

# subset only Tropical Moist and Boreal Forests
forests_2017 <- ecoregions_2017 %>%
  subset(
    BIOME_NAME %in% c(
      "Tropical & Subtropical Moist Broadleaf Forests",
      "Boreal Forests/Taiga"
    )
  ) %>%
  group_by(BIOME_NAME) %>%
  summarize(geometry = st_union(geometry))

# world map
world <- ne_countries(scale = "medium", returnclass = "sf") #world map

# world map with forests biomes
forests_2017_map <- ggplot() +
  geom_sf(data = world, size = .2, col="grey20") +
  geom_sf(data = forests_2017, aes(fill = as.factor(BIOME_NAME)), show.legend = TRUE) +
  labs(fill = "Biomes")  +
  theme(legend.position = "bottom")
forests_2017_map

# Continents accroding to ne_countries
countries <- ne_countries(scale = "medium", returnclass = "sf")
# Plot the continents
ggplot(data = countries) +
  geom_sf(aes(fill = continent), color = "black", size = 0.2) +  # Map continent to fill
  scale_fill_brewer(palette = "Set3", name = "Continent") +  # Use a color palette
  labs(
    title = "Continents in the ne_countries Dataset",
    subtitle = "Visualized with Different Colors",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )