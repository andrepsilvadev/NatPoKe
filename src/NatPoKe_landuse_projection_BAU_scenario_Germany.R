added libraries in libraries.R (duble check if there are some missing)
scenarios.R separate file to read the land-use scenarios (seems that some land-use scenarios are missing - dont forget to add)
avoid have long scripts  (more than 400 lines) - split in several scripts if needed


# Settings & libraries -------------------------------------------
source("./src/libraries.R") # libraries
source("./src/customFunctions.R") # customized functions
source("./src/scenarios.R") # customized functions

#plot(LULC_ESA_2017_germany)
#plot(BAU_PNAS_2030_germany)

# Simpliy and define ESA LULC types (39) to the 7 (SEALS) LULC types
# Source of ESA LULC simplification scheme in Table S.2.4.1 of Supporting Information Appendix in Johnson et al. 2023 (https://www.pnas.org/doi/10.1073/pnas.2220401120#supplementary-materials)

value_to_land_use <- list(
  "190" = 1,  # Urban
  "10" = 2, "11" = 2, "12" = 2, "20" = 2, "30" = 2,  # Cropland
  "130" = 3,  # Pasture/Grassland
  "40" = 4, "50" = 4, "60" = 4, "61" = 4, "62" = 4, "70" = 4, "71" = 4, "72" = 4, "80" = 4, "81" = 4, "82" = 4, "90" = 4, "100" = 4,  # Forest
  "110" = 5, "120" = 5, "121" = 5, "122" = 5, "140" = 5,  # Non-forest vegetation
  "210" = 6,  # Water
  "150" = 7, "151" = 7, "152" = 7, "153" = 7, "160" = 7, "170" = 7, "180" = 7, "200" = 7, "201" = 7, "202" = 7, "210" =7, "220" = 7  # Barren or Other
)

# Apply the mapping to the LULC_ESA_2017_germany raster
mapped_LULC_ESA_2017_germany <- terra::app(
  x = LULC_ESA_2017_germany,
  fun = map_values_to_land_use)

# create binary maps for each land-use type
LULC_Types <- 1:7
landUse_LULC_ESA_2017_germany <- lapply(LULC_Types, function(cat){
  terra::app(mapped_LULC_ESA_2017_germany, fun = function(x){
    return(ifelse(x == cat,1,0))
  })
})

landUse_BAU_PNAS_germany <- lapply(LULC_Types,function(cat){
  app(BAU_PNAS_2030_germany, fun = function(x){
    return(ifelse(x==cat,1,0))
  })
})

# Assign the names of land-use types
LULC_Types_names <- c(
  "Urban",
  "Cropland",
  "Pasture/Grassland",
  "Forest",
  "Non-forest vegetation",
  "Water",
  "Barren or other"
)

# combine the layers into stacks
stack_LULC_ESA_2017_germany <- rast(landUse_LULC_ESA_2017_germany)
names(stack_LULC_ESA_2017_germany) <- LULC_Types_names
stack_BAU_PNAS_2030_germany <- rast(landUse_BAU_PNAS_germany)
names(stack_BAU_PNAS_2030_germany) <- LULC_Types_names
plot(stack_LULC_ESA_2017_germany) # dont forget to double-check if the land-use maps match the patterns in mapped_LULC_ESA_2017_germany
plot(stack_BAU_PNAS_2030_germany) # dont forget to double-check if the land-use maps match the patterns in BAU_PNAS_2030_germany

# next two lines can be deleted
unique(values(landUse_LULC_ESA_2017_germany[[1]])) # notice that here the values are 0 and 1 not percentages
unique(values(landUse_BAU_PNAS_germany[[1]])) # notice that here the values are 0 and 1 not percentages

# Calculate the percentage of each land-use type

here You can maybe follow two approaches

1. agregate the values of the raster and calculate the percentage of each land-use type at lower resolution (e.g. 10km)
2. calculate the overall percentage of each land-use type in the raster land_use_counts / total_cells
 (this is different the input for the landUseProjection funtion, but maybe you can use this information to run the plots directly )

give it a thought and let me know, best



# Verify percentages with calculations of percentages without NAs
landUse_percentages_LULC_ESA_2017_germany <- sapply(landUse_LULC_ESA_2017_germany, function(layer) {
  sum(values(layer), na.rm = TRUE) / sum(!is.na(values(layer))) * 100
})
landUse_percentages_BAU_PNAS_2030_germany <- sapply(landUse_BAU_PNAS_germany, function(layer) {
  sum(values(layer), na.rm = TRUE) / sum(!is.na(values(layer))) * 100
})

# rename categories to land-use types
names(landUse_percentages_LULC_ESA_2017_germany)<-LULC_Types_names
names(landUse_percentages_BAU_PNAS_2030_germany)<-LULC_Types_names

# check results and total percentage
print(landUse_percentages_LULC_ESA_2017_germany)
print(landUse_percentages_BAU_PNAS_2030_germany)
total_percentage_LULC_ESA_2017_germany<-sum(landUse_percentages_LULC_ESA_2017_germany)
total_percentage_BAU_PNAS_2030_germany<-sum(landUse_percentages_BAU_PNAS_2030_germany)


# I have not checked from here below


# combine the stacks into a list
combined_stack_germany<-list(stack_LULC_ESA_2017_germany,stack_BAU_PNAS_2030_germany)

#4. Create land use projections and data frame


# land use projections
t <- c(2017,2030)
var.names <- LULC_Types_names 

var <- var.names
var.label <- c("Urban",
               "Cropland",
               "Pasture/Grassland",
               "Forest",
               "Non-forest vegetation",
               "Water",
               "Barren or other")

# projection but based on stack that does not include the correct percentages without NAs
landUseProjection <- function(stack, t, var, var.label) {
  # based on the ouput stacks creates stacked chart 
  # functional is not general for other types of variables
  list <- list()
  value <- c()
  variable <- c()
  landuse <- c()
  for (i in 1:length(t)){
    for (j in 1:length(var.names)){
      r <- stack[[i]][[j]]
      value[j] <- mean(values(r), na.rm = TRUE)
      variable[j] = var[j]
      landuse[j] = var.label[j]
    }
    data <- data.frame(time = t[i],
                       value = value,
                       variable = variable,
                       landuse = landuse)
    list[[i]] <- data
  }
  
  ## without water layer
  landusedata <- bind_rows(list) %>%
    dplyr::filter(variable != "Water" )
  
  return(landusedata)
  
}

# use the landUseProjection function to create the projection
BAU_projection_data_germany<-landUseProjection(combined_stack_germany,t,var,var.label)

View(BAU_projection_data_germany)
# print the projection data to verify the result
print(BAU_projection_data_germany)

# Extend the baseline data for the years 2017 to 2029 and scenarios from 2030 to 2050
BAU_projection_data_germany_extended<-data.frame()
for(year in 2017:2030){
  temp_df<-BAU_projection_data_germany[BAU_projection_data_germany$time== 2017,]
  temp_df$time<-year
  BAU_projection_data_germany_extended<-rbind(BAU_projection_data_germany_extended,temp_df)
}
for(year in 2030:2050){
  temp_df<-BAU_projection_data_germany[BAU_projection_data_germany$time== 2030,]
  temp_df$time<-year
  BAU_projection_data_germany_extended<-rbind(BAU_projection_data_germany_extended,temp_df)
}

print(BAU_projection_data_germany_extended)


#5&6. Plot the land use change from baseline 2017 to scenario 2030
# stacked area chart
BAU_scenario_plot_germany <- ggplot(BAU_projection_data_germany_extended, aes(x=time, y=value, fill=landuse)) + 
  geom_area() +
  scale_fill_viridis_d("Land Use") +
  #scale_fill_manual(values = cbPalette) +
  theme_classic() +
  labs(x = "Year",
       y = "Average fraction of grid cell",
       title = "Land use change in Germany based on BAU scenario")

print(BAU_scenario_plot_germany)

ggsave(path = "./output",
       filename = "BAU_scenario_plot_germany.png",
       plot = BAU_scenario_plot_germany,
       dpi = 600,
       width = 25,
       height = 10,
       units = "cm")

# not really sure what this part is doing so decided to remove it but let me know if it is important 

# # Define the mapping from LULC types to the 7 LULC types (SEALS)
# #value_to_land_use_scenario<-list(
#   "1" = 1,  # Urban
#   "2" = 2,  # Cropland
#   "3" = 3,  # Pasture/Grassland
#   "4" = 4,  # Forest
#   "5" = 5,  # Non-forest vegetation
#   "6" = 6,  # Water
#   "7" = 7,  # Barren or Other
#   "255" = NA # No Data value
# )

# Function to map scenario LULC values to the 7 LULC types
# map_values_to_land_use_scenario <- function(x) {
#   sapply(x, function(val) {
#     if (val %in% names(value_to_land_use_scenario)) {
#       return(value_to_land_use_scenario[[as.character(val)]])
#     } else {
#       return(NA)  # Handle values that do not map to any land-use type
#     }
#   })
# }

# Apply the mapping to the BAU_PNAS_germany raster
#mapped_BAU_PNAS_germany <- app(BAU_PNAS_2030_germany, fun = map_values_to_land_use_scenario)
