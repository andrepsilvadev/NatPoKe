######################################################
# MAMMALS WITH COMPLETE TRAITS AND MODELLINGRES PLOT #
######################################################
# Inês Silva
# 27 March 2025

# packages
library(tidyverse)
library(xlsx)

# import csv
sps_traits <- read_csv("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/traits/all_mammals_available_for_metaRange_model.csv")

# delete weird column and keep only one row per species
fileToSave <- sps_traits %>% 
  dplyr::select(!"...1") %>% 
  distinct() %>% 
  as.data.frame()

write.csv(fileToSave, "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/traits/all_mammals_available_for_metaRange_model.csv", row.names = FALSE)
write.xlsx(fileToSave, "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/traits/all_mammals_available_for_metaRange_model.xlsx", row.names = FALSE)

sps_modelling_res <- species_traits %>%
  dplyr::filter(Biome %in% c("Tropical & Subtropical Moist Broadleaf Forests", "Boreal Forests/Taiga")) %>% 
  dplyr::select(Species, ModellingRes) %>% 
  distinct()

free <- ggplot(sps_modelling_res, aes(x = "", y = ModellingRes)) + 
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +  # Box plot
  geom_jitter(aes(color = Species), size = 2, alpha = 0.7, position = position_jitter(seed = 1)) + # Points for each species
  geom_text(aes(label = Species), hjust = -0.1, size = 3, check_overlap = TRUE, position = position_jitter(seed = 1)) + # Labels next to points
  labs(y = "Modelling Resolution", x = "", title = "Species Modelling Resolution") + 
  theme_minimal() +
  theme(legend.position = "none") 

limited <- ggplot(sps_modelling_res, aes(x = "", y = ModellingRes)) + 
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.5) +  # Box plot
  geom_jitter(aes(color = Species), size = 2, alpha = 0.7, position = position_jitter(seed = 1)) + # Points for each species
  geom_text(aes(label = Species), hjust = -0.1, size = 3, check_overlap = TRUE, position = position_jitter(seed = 1)) + # Labels next to points
  labs(y = "Modelling Resolution", x = "") + 
  ylim(0,10) +
  theme_minimal() +
  theme(legend.position = "none")  

# Compute Q1, Q3, and IQR
Q1 <- quantile(sps_modelling_res$ModellingRes, 0.25, na.rm = TRUE)
Q3 <- quantile(sps_modelling_res$ModellingRes, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define lower and upper fences for outliers
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR
# Count outliers
outlier_count <- sum(sps_modelling_res$ModellingRes < lower_fence | 
                       sps_modelling_res$ModellingRes > upper_fence, na.rm = TRUE)

# Total number of species
total_count <- nrow(sps_modelling_res)
# Calculate percentage
outlier_percentage <- (outlier_count / total_count) * 100


library(patchwork)

sps_modellingRes <- free + limited + plot_annotation(caption = paste0("Percentage of species outside the boxplot: ", round(outlier_percentage, 2), "%\n"))

ggsave(plot = sps_modellingRes,
       file = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/all_mammals_resolution.tiff",
       bg = 'white', width = 400, height = 200, units = "mm", dpi = 1200, compression = "lzw")
