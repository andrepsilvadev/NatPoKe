
## Name: percentageByAggregation.R ##
## Authors: Andre P. Silva & Jorinde-M. Rieger ##
## Description: builds and applyies function to calculate land-use percenateges by cell aggregation ##

calculateRasterClassPercentages <- function(OriginalRaster, extent) {
# Function to calculate the percentage of each land-use class
# agregates 10x and calculates the percentage of each land-use class in the raster

#raster <- LULC_ESA_2017_germany
#extent <- germany_sp

# Crop and mask the raster to Germany's boundary
raster <- mask(crop(OriginalRaster,germany_sp),germany_sp)

# Define the unique land-use classes and remove NAs
land_use_classes <- unique(values(raster))
land_use_classes <- na.omit(land_use_classes)

# Function to create binary raster for each land-use class
create_binary_raster <- function(raster, land_use_class) {
  binary_raster <- app(raster, fun = function(x) {
    ifelse(x == land_use_class, 1, 0)
  })
  return(binary_raster)
}

# Create a list to store binary rasters
binary_rasters <- list()

# Loop through each land-use class and create binary rasters
for (class in land_use_classes) {
  binary_rasters[[as.character(class)]] <- create_binary_raster(raster, class)
}

plot(rast(binary_rasters))

# Aggregate each binary raster by a factor of 10 using the custom function, crop, and mask by the extent of the original layer
aggregated_rasters <- list()
for (class in names(binary_rasters)) {
  aggregated_raster <- aggregate(binary_rasters[[class]], fact = 10, fun = function(x) sum(x > 0, na.rm = TRUE))
  masked_raster <- mask(crop(aggregated_raster, extent), extent)
  aggregated_rasters[[class]] <- masked_raster
}

plot(rast(aggregated_rasters))

# Calculate the mean value for each land-use class in the aggregated rasters
mean_values <- sapply(aggregated_rasters, function(raster) {
  mean(values(raster), na.rm = TRUE)
})

# Create a dataframe with the results
mean_values_df <- data.frame(
  Land_Use_Class = names(mean_values),
  Mean_Value = mean_values
)

return(mean_values_df)

}

# Simplify and define ESA LULC types (39) to the 7 (SEALS) LULC types
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
mapped_LULC_ESA_2017 <- terra::app(
  x = LULC_ESA_2017,
  fun = map_values_to_land_use) # this is taking a bit of time to run, give it a few minutes, otherwise 
  for now we go with the mapped_germany as you had in your previous script

# apply function to calculate the percentage of each land-use class
LULC_ESA_2017_germany_percentages <- calculateRasterClassPercentages(
  OriginalRaster = mapped_LULC_ESA_2017_germany,
  extent = germany_sp) 

BAU_PNAS_2030_germany_percentages <- calculateRasterClassPercentages(
  OriginalRaster = BAU_PNAS,
  extent = germany_sp)

PESGC_2030_germany_percentages <- calculateRasterClassPercentages(
  OriginalRaster = PESGC,
  extent = germany_sp)


# Join the two dataframes by land use class
joined_percentages <- merge(
  BAU_PNAS_2030_germany_percentages,
  PESGC_2030_germany_percentages,
  by = "Land_Use_Class",
  suffixes = c("_BAU_PNAS_2030", "_PESGC_2030")
)

# Add a new column that is the difference between PESGC_2030 and BAU_PNAS_2030
joined_percentages$Difference <- joined_percentages$Mean_Value_PESGC_2030 - joined_percentages$Mean_Value_BAU_PNAS_2030

# Print the joined dataframe
print(joined_percentages)

# Optionally, save the joined dataframe to a CSV file
write.csv(joined_percentages, "joined_land_use_percentages.csv", row.names = FALSE)

sum(BAU_PNAS_2030_germany_percentages$Mean_Value) # 98.43502 not sure why it is not 100
sum(PESGC_2030_germany_percentages$Mean_Value) # 98.43502


something to think about,can it be that even if the percentages do not change much
the spatial arrangement of the classes changes? to investigate this we should do 
maps of change per each class, but lets finish the tables and compare them first before 
exploring spatial patterns