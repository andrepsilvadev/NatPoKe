# ANOTHER TRY #
##### MIS #####

# packages
library(readr)
library(ggplot2)
library(dplyr)

# import data
example_01_res_df <- read_csv(
  "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/example_01_res_df.csv"
)
#View(example_01_res_df)


# # Define coordinates and create initial data frame
# x_coords <- seq(-180, 180, by = 2)
# y_coords_tropical <- seq(-23.5, 23.5, by = 1)
# y_coords_boreal <- seq(50, 70, by = 1)
#
# tropical_pixels <- expand.grid(x = x_coords, y = y_coords_tropical) %>%
#   mutate(Biome = "Tropical & Subtropical Moist Broadleaf Forests")
#
# boreal_pixels <- expand.grid(x = x_coords, y = y_coords_boreal) %>%
#   mutate(Biome = "Boreal Forests/Taiga")
#
# spatial_grid <- bind_rows(tropical_pixels, boreal_pixels)
#
# species_list <- unique(example_01_res_df$species)  # Replace `species_name` with your column name
# timesteps <- unique(example_01_res_df$time)         # Replace `timestep` with your column name
#
#
# # Combine species, timesteps, and spatial grid
# expanded_dummy_data <- expand.grid(species = species_list,
#                                    timestep = timesteps,
#                                    x = spatial_grid$x,
#                                    y = spatial_grid$y)



# ABUNDANCE ACROSS TIME

ggplot(example_01_res_df, aes(x = time, y = n_abundance, color = species)) +
  geom_line() +
  scale_fill_viridis_c() +
  labs(
    title = "Species Abundance through time",
    x = "Time Step",
    y = "Abundance",
    color = "Species"
  ) +
  theme_minimal()

# how does abundance change across time (alternative to line plots? only useful if we are not focused on values)
# otherwise "normal line plots" are more informative
# this is more visual
# migt be usefull to have an imediate visualsation for multiple species? or groups
ggplot(example_01_res_df, aes(x = time, y = species, fill = n_abundance)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Species Abundance Heatmap",
       x = "Time Step",
       y = "Species",
       fill = "Abundance") +
  theme_minimal()

# ID ABRUPT CHANGES

# we can see if there is an abrupt chnage in the abundance of species within each
# scenarios and determine at what point in time that occurs with the segmented
# function (see my thesis C. aper OR ~ TL to show this)



# SUITABLE VS OCCUPIED

# can we have the proportion of suitable vs occupied cells per biome in each policy?
# like in jana's paper but instead of the correlation plot just a stacked barplot?
# assuming that occupied + suitable = 100 %




# ABUNDANCE VS SUITBALE (Jana's paper)

# but this time per biome and policy



# DIVERSITY INDEXES THROUGH TIME



# DIVERISTY INDEXES IN MAPS
