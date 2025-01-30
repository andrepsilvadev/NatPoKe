########################################
# SAVING metaRange OUTPUT RASTER FILES #
########################################
# MIS
# 28 Jan 25

# GOAL: Have a script to import all model output in raster format to a dataframe
# with coordinates

# IT ONLY WORKS FOR ONE VARIABLE MEANING WE HAVE TO CHNAGE THE PATTERN IN list.file MANNUALY TO
# HAVE THE RASTERS FOR OTHER VARIABLES

# packages
library(here)
library(dplyr)
library(stringr) # for strsplit
library(tools) # for file_path_sans_ext
library(readr)
library(ggplot2)

##########
# STEP 1 #  Have a list of species that entered the model
##########

# create empty list
results <- list()

species_traits <- read.csv(here("example/mammals_try2/clean_data_2species/target_metarange_mammals20250110.csv"))

species_names <- species_traits$species

##########
# Step 2 #
##########

# loop through species and process ONE raster at a time
for (sp in species_names) {
  
  # find raster files for the current species
  flist <- list.files(here("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/results30Jan2025"), 
                      pattern = paste0(sp, "_abundance.tif"), full.names = TRUE)
  
  # check if any files were found; if not, skip to the next
  if (length(flist) == 0) {
    message(paste("No rasters found for:", sp, "- Someone should check if this is a MISTAKE!"))
    next
  }
  
  # loop through the raster files for the current species
  for (raster in flist) {
    
    # read raster 
    r <- terra::rast(raster)
    
    # retrieve file name and split it
    filename <- basename(raster)
    filename_parts <- str_split(file_path_sans_ext(filename), "_")[[1]]
    
    # convert raster to a data frame with coordinates and values
    raster_data <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE, row.names = FALSE) 
    
    # add information for easier identification of each raster (sp, timestpe, scTRUE# add information for easier identification of each raster (sp, timestpe, scenario, etc..)
    raster_data$scenario <- filename_parts[1]  # BAU
    raster_data$biome <- filename_parts[2]     # Tropical
    raster_data$region <- filename_parts[3]    # Asia
    raster_data$timestep <- filename_parts[4] 
    raster_data$species <- sp                  # use current species name
    names(raster_data)[names(raster_data) == "lyr1"] <- filename_parts[6]
    invisible(gc())
    
    # store result in a list, then append by species
    if (!is.null(results[[sp]])) {
      results[[sp]] <- rbind(results[[sp]], raster_data)
    } else {
      results[[sp]] <- raster_data
      invisible(gc())
    }
  }
}

# combine all species results into one data frame
final_results <- do.call(rbind, results)
invisible(gc())
#View(final_results)

# #WRITE RESULTS TO .tsv
# write_tsv(final_results, "example/mammals_try2/results_28Jan/final_results28Jan.tsv")
# 
# library(data.table)
# 
# View(raster_data)
# 
# final_results <- fread("example/mammals_try2/results_28Jan/final_results28Jan.tsv")

final_results$taxa <- "Mammal"

##############################
# SIMPLE ABUNDANCE OVER TIME # just to check
##############################


final_results %>% 
  group_by(scenario, biome, timestep, taxa, species) %>% 
  summarise(mean_abundance = mean(abundance, na.rm = TRUE)) %>% 
  ggplot(aes(x = timestep, y = mean_abundance, group = species, color = species)) +
  geom_line()

# lynx30 <- rast("~/NatPoKe/example/mammals_try2/results_28Jan/BAU_Tropical_Asia_030_Lynxlynx_abundance.tif")
# plot(lynx30)
# 
# alces30 <- rast("~/NatPoKe/example/mammals_try2/results_28Jan/BAU_Tropical_Asia_030_Alcesalces_abundance.tif")
# plot(alces30)

###############################
# SIMPLE ABUNDANCE CHANCE MAP # just to check
###############################
final_results

# abundance change proportion
final_results2 <- final_results %>%
  group_by(scenario, biome, taxa, species) %>% 
  mutate(abund_change <- (abundance - abundance[timestep == "005"])/abundance[timestep == "005"])%>%
  rename(abund_change = 10)  %>% 
  ungroup()
invisible(gc())
hist(final_results2$abund_change)
library(RColorBrewer) # palletes for the maps on step 4
# Define the RdBu palette with a midpoint at 0
palette <- brewer.pal(11, "RdBu")

ggplot() +
  # plot data for the index in question (here Shannon wiener = sum just because these are dummydata)
  geom_raster(data = final_results2, aes(x = x, y = y, fill = abund_change))+
  facet_wrap(species ~ ., ncol = 2) +
  scale_fill_gradientn(colors = palette, na.value = "transparent",
                       limits = c(-5, 5))
