########################
# SENSITIVITY ANALYSIS #
########################
# MIS
# 31 Jan 2025

# Goal: Create a script to import sensitivity runs data and build a sensitivity
# analysis plot 

# Each sensitivity run should be stored in a separate folder
# BE CAREFUL WHEN IMPORTING EACH SENS RUN RASTER DATA, BECAUSE FILES ARE NAMED DIFFERENTLY
# SO LOOPS ARE DIFFERENT!

# Combining rasters and writing a .tsv file for 4 species and 2 variables takes
# 5 minutes and uses 3.61 GiB of R memory

start.time <- Sys.time() # start the clock
# packages
library(data.table) # efficient and fast df transdformations (instead of dplyr option for e.g.)
library(ggplot2)
library(here)
library(dplyr)
library(tidyr)
library(readr)

##########
# STEP 1 #  Import each sensitivity run (including the control also) separetly
##########

# Define raster types
raster_types <- c("abundance", "reproductionRate", "mortality", "carrying_capacity", "dispersal_distance")

# Read species data
species_traits <- read.csv(here("example/mammals_try2/clean_data_2species/target_metarange_mammals20250110.csv"))
species_names <- species_traits$species


# control run ------------------------------------------------------------------

# Create empty list to store results for each raster type
control_list <- list()

for (sp in species_names) {
  
  # create an empty list to store values for a sps
  species_data <- list()
  
  for (raster_type in raster_types) {
    
    # find the raster files (for a sps and raster type)
    flist <- list.files(here("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/results30Jan2025"), 
                        pattern = paste0(sp, "_", raster_type, ".tif"), full.names = TRUE)
    # free unused R memory
    invisible(gc())
    
    # skip if no files found print WARNING
    if (length(flist) == 0) {
      message(paste("No rasters found for", sp, raster_type, "- Someone should check if this is a mistake!"))
      next
    }
    
    # process each raster
    for (raster in flist) {
      
      # read
      r <- terra::rast(raster)
      
      # retrieve filename and split it
      filename <- basename(raster)
      filename_parts <- strsplit(tools::file_path_sans_ext(filename), "_")[[1]]
      invisible(gc())
      
      # convert raster to data frame (with coordinates and values)
      raster_data <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE, row.names = FALSE)
      
      # add more info as new columns
      raster_data$scenario <- filename_parts[1] # BAU
      raster_data$biome <- filename_parts[2] # TropicalForests
      raster_data$region <- filename_parts[3] # Asia    
      raster_data$timestep <- filename_parts[4] # 001  
      raster_data$species <- sp                  
      
      # rename raster value column to the corresponding variable = raster type
      names(raster_data)[names(raster_data) == "lyr1"] <- raster_type
      
      # store list for that species
      if (!is.null(species_data[[raster_type]])) {
        species_data[[raster_type]] <- rbind(species_data[[raster_type]], raster_data)
      } else {
        species_data[[raster_type]] <- raster_data
      }
    }
  }
  # free unused R memory
  invisible(gc())
  
  # merge all rasters for that species 
  merged_species_data <- Reduce(function(x, y) merge(x, y, by = intersect(names(x), names(y)), all = TRUE), species_data)
  
  # store final merged data for that species in a list (esch specis a new element in this list)
  control_list[[sp]] <- merged_species_data
}

# remove r obj to save space
rm(r)
invisible(gc()) 

# remove raster_data obj to save space
rm(raster_data)
invisible(gc()) 

# remove species_data obj to save space
rm(species_data)
invisible(gc())

# remove merged_species_data obj to save space
rm(merged_species_data)
invisible(gc())

# combine all results for all species into one big data frame
final_results_control <- do.call(rbind, control_list)
invisible(gc())

# remove control_list obj to save space
rm(control_list)
invisible(gc())

# write cnotrol run results to a tsv
write_tsv(final_results_control, "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/results30Jan2025/final_results_control.tsv")
invisible(gc())

# 095 sensitivity run ----------------------------------------------------------

# Create empty list to store results for each raster type
SR095_list <- list()

for (sp in species_names) {
  
  # create an empty list to store values for a sps
  species_data <- list()
  
  for (raster_type in raster_types) {
    
    # find the raster files (for a sps and raster type)
    flist <- list.files(here("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/SR095reproductionRate"), 
                        pattern = paste0(sp, "_", raster_type, ".tif"), full.names = TRUE)
    # free unused R memory
    invisible(gc())
    
    # skip if no files found print WARNING
    if (length(flist) == 0) {
      message(paste("No rasters found for", sp, raster_type, "- Someone should check if this is a mistake!"))
      next
    }
    
    # process each raster
    for (raster in flist) {
      
      # read
      r <- terra::rast(raster)
      
      # retrieve filename and split it
      filename <- basename(raster)
      filename_parts <- strsplit(tools::file_path_sans_ext(filename), "_")[[1]]
      invisible(gc())
      
      # convert raster to data frame (with coordinates and values)
      raster_data <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE, row.names = FALSE)
      
      # add more info as new columns
      raster_data$scenario <- filename_parts[2] # BAU
      raster_data$biome <- filename_parts[3] # TropicalForests
      raster_data$region <- filename_parts[4] # Asia    
      raster_data$timestep <- filename_parts[5] # 001  
      raster_data$species <- sp
      
      # rename raster value column to the corresponding variable = raster type
      names(raster_data)[names(raster_data) == "lyr1"] <- raster_type
      
      # store list for that species
      if (!is.null(species_data[[raster_type]])) {
        species_data[[raster_type]] <- rbind(species_data[[raster_type]], raster_data)
      } else {
        species_data[[raster_type]] <- raster_data
      }
    }
  }
  # free unused R memory
  invisible(gc())
  
  # merge all rasters for that species 
  merged_species_data <- Reduce(function(x, y) merge(x, y, by = intersect(names(x), names(y)), all = TRUE), species_data)
  
  # store final merged data for that species in a list (esch specis a new element in this list)
  SR095_list[[sp]] <- merged_species_data
}

# remove r obj to save space
rm(r)
invisible(gc()) 

# remove raster_data obj to save space
rm(raster_data)
invisible(gc()) 

# remove species_data obj to save space
rm(species_data)
invisible(gc())

# remove species_data obj to save space
rm(merged_species_data)
invisible(gc())

# combine all results for all species into one big data frame
final_results_SR095 <- do.call(rbind, SR095_list)

# remove SR095_list obj to save space
rm(SR095_list)
invisible(gc())

# add suffix to all variable columns 
final_results_SR095 <- final_results_SR095 %>% 
  rename_with(~paste0(.x, "_reproductionRate095"), .cols = 8:ncol(final_results_SR095)) 

# write cnotrol run results to a tsv
write_tsv(final_results_SR095, "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/SR095reproductionRate/final_results_SensRun095.tsv")
invisible(gc())

# 105 sensitivity run ----------------------------------------------------------

# Create empty list to store results for each raster type
SR105_list <- list()

for (sp in species_names) {
  
  # create an empty list to store values for a sps
  species_data <- list()
  
  for (raster_type in raster_types) {
    
    # find the raster files (for a sps and raster type)
    flist <- list.files(here("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/SR105reproductionRate"), 
                        pattern = paste0(sp, "_", raster_type, ".tif"), full.names = TRUE)
    
    # skip if no files found print WARNING
    if (length(flist) == 0) {
      message(paste("No rasters found for", sp, raster_type, "- Someone should check if this is a mistake!"))
      next
    }
    
    # process each raster
    for (raster in flist) {
      
      # read
      r <- terra::rast(raster)
      
      # retrieve filename and split it
      filename <- basename(raster)
      filename_parts <- strsplit(tools::file_path_sans_ext(filename), "_")[[1]]
      
      # convert raster to data frame (with coordinates and values)
      raster_data <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE, row.names = FALSE)
      
      # add more info as new columns
      raster_data$scenario <- filename_parts[2] # BAU
      raster_data$biome <- filename_parts[3] # TropicalForests
      raster_data$region <- filename_parts[4] # Asia    
      raster_data$timestep <- filename_parts[5] # 001  
      raster_data$species <- sp                  
      
      # rename raster value column to the corresponding variable = raster type
      names(raster_data)[names(raster_data) == "lyr1"] <- raster_type
      
      # store list for that species
      if (!is.null(species_data[[raster_type]])) {
        species_data[[raster_type]] <- rbind(species_data[[raster_type]], raster_data)
      } else {
        species_data[[raster_type]] <- raster_data
      }
    }
  }
  
  # merge all rasters for that species 
  merged_species_data <- Reduce(function(x, y) merge(x, y, by = intersect(names(x), names(y)), all = TRUE), species_data)
  
  # store final merged data for that species in a list (esch specis a new element in this list)
  SR105_list[[sp]] <- merged_species_data
}

# remove r obj to save space
rm(r)
invisible(gc()) 

# remove raster_data obj to save space
rm(raster_data)
invisible(gc()) 

# remove species_data obj to save space
rm(species_data)
invisible(gc())

# remove species_data obj to save space
rm(merged_species_data)
invisible(gc())

# combine all results for all species into one big data frame
final_results_SR105 <- do.call(rbind, SR105_list)

# remove SR095_list obj to save space
rm(SR105_list)
invisible(gc())

# add suffix to all variable columns 
final_results_SR105 <- final_results_SR105 %>% 
  rename_with(~paste0(.x, "_reproductionRate105"), .cols = 8:ncol(final_results_SR105))

# write cnotrol run results to a tsv
write_tsv(final_results_SR105, "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/SR105reproductionRate/final_results_SensRun105.tsv")
invisible(gc())

# Just saving space in R -------------------------------------------------------

# MAYBE IT WILL BE BEST TO REMOVE THE OBJECTS AND RE-IMPORT WRITTEN .TSV FILES
# IMPORTING IS LESS HEAVY ON R

rm(final_results_control)
rm(final_results_SR095)
rm(final_results_SR105)

final_results_control <- fread("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/results30Jan2025/final_results_control.tsv")
final_results_SR095 <- fread("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/SR095reproductionRate/final_results_SensRun095.tsv")
final_results_SR105 <- fread("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/SR105reproductionRate/final_results_SensRun105.tsv")


##########
# STEP 2 #  Combine all three model runs into one big one
##########

# combining dataframes
sens_run_baseline <- final_results_control %>% 
  left_join(final_results_SR095, by = c("timestep", "x", "y", "species", "scenario", "biome", "region")) %>% 
  left_join(final_results_SR105, by = c("timestep", "x", "y", "species", "scenario", "biome", "region"))
invisible(gc())

# be careful with integer64 columns (chnage to numeric)
sens_run_baseline[] <- lapply(sens_run_baseline, function(col) {
  if (bit64::is.integer64(col)) {
    as.numeric(col)
  } else {
    col
  }
}) 

##########
# STEP 3 #  Calculate proportions between runs
##########


#calculate the proportion between each sensitivity run and the control runs for each variable for each parameter change option
sens_run_baseline <- sens_run_baseline %>% 
  mutate(#abundance
    abund_propreproductionRate095 = abundance_reproductionRate095/abundance,
    abund_propreproductionRate105 = abundance_reproductionRate105/abundance, 
    # abund_propbevmort105 = abundance_bevmort105/abundance,
    # abund_propbevmort095 = abundance_bevmort095/abundance,
    # abund_propmax105 = abundance_max105/abundance, 
    # abund_propmax095 = abundance_max095/abundance, 
    # abund_propmass105 = abundance_mass105/abundance,
    # abund_propmass095 = abundance_mass095/abundance,
    # abund_propcarry105 = abundance_carry105/abundance, 
    # abund_propcarry095 = abundance_carry095/abundance,
    # abund_propmean105 = abundance_mean105/abundance,
    # abund_propmean095 = abundance_mean095/abundance, 
    #reproduction
    reprod_propreproductionRate095 = reproductionRate_reproductionRate095/reproductionRate,
    reprod_propreproductionRate105 = reproductionRate_reproductionRate105/reproductionRate, 
    # reprod_propbevmort105 = reproduction_bevmort105/reproduction,
    # reprod_propbevmort095 = reproduction_bevmort095/reproduction,
    # reprod_propmax105 = reproduction_max105/reproduction, 
    # reprod_propmax095 = reproduction_max095/reproduction, 
    # reprod_propmass105 = reproduction_mass105/reproduction,
    # reprod_propmass095 = reproduction_mass095/reproduction,
    # reprod_propcarry105 = reproduction_carry105/reproduction, 
    # reprod_propcarry095 = reproduction_carry095/reproduction,
    # reprod_propmean105 = reproduction_mean105/reproduction,
    # reprod_propmean095 = reproduction_mean095/reproduction,
    #habitat
    # hab_propgrowrate105 = habitat_growrate105/habitat,
    # hab_propgrowrate105 = habitat_growrate095/habitat, 
    # hab_propbevmort105 = habitat_bevmort105/habitat,
    # hab_propbevmort095 = habitat_bevmort095/habitat,
    # hab_propmax105 = habitat_max105/habitat, 
    # hab_propmax095 = habitat_max095/habitat, 
    # hab_propmass105 = habitat_mass105/habitat,
    # hab_propmass095 = habitat_mass095/habitat,
    # hab_propcarry105 = habitat_carry105/habitat, 
    # hab_propcarry095 = habitat_carry095/habitat,
    # hab_propmean105 = habitat_mean105/habitat,
    # hab_propmean095 = habitat_mean095/habitat,
    # #carrying capacity
    # carry_propgrowrate105 = carry_growrate105/carry,
    # carry_propgrowrate105 = carry_growrate095/carry, 
    # carry_propbevmort105 = carry_bevmort105/carry,
    # carry_propbevmort095 = carry_bevmort095/carry,
    # carry_propmax105 = carry_max105/carry, 
    # carry_propmax095 = carry_max095/carry, 
    # carry_propmass105 = carry_mass105/carry,
    # carry_propmass095 = carry_mass095/carry,
    # carry_propcarry105 = carry_carry105/carry, 
    # carry_propcarry095 = carry_carry095/carry,
    # carry_propmean105 = carry_mean105/carry,
    # carry_propmean095 = carry_mean095/carry,
    # #bevmort
    # bevmort_propgrowrate105 = bevmort_growrate105/bevmort,
    # bevmort_propgrowrate105 = bevmort_growrate095/bevmort, 
    # bevmort_propbevmort105 = bevmort_bevmort105/bevmort,
    # bevmort_propbevmort095 = bevmort_bevmort095/bevmort,
    # bevmort_propmax105 = bevmort_max105/bevmort, 
    # bevmort_propmax095 = bevmort_max095/bevmort, 
    # bevmort_propmass105 = bevmort_mass105/bevmort,
    # bevmort_propmass095 = bevmort_mass095/bevmort,
    # bevmort_propcarry105 = bevmort_carry105/bevmort, 
    # bevmort_propcarry095 = bevmort_carry095/bevmort,
    # bevmort_propmean105 = bevmort_mean105/bevmort,
    # bevmort_propmean095 = bevmort_mean095/bevmort,
  )
invisible(gc())


# write a tsv file with all the data
write_tsv(sens_run_baseline, "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/sensitivity_runs_baseline_scenario.tsv")
invisible(gc())
# AFTER WRITTING THS TSV EITHER CLEAN THE GLOBAL ENVIRONMENT OR SHUT DOWN R
# ENTIERLY AND IMPORT THE WRITTEN TSV WITH FREAD, THE PREVIOUS ANALYSIS USE TOO
# MUCH MEMORY AND FROM HERE ON IT CAN NOT DO ANYTHING WITHOUT CRASHING!
# (FATAL FLAW OR ERROR MESSAGE SHOWS)

# re-import complete dataframe for the sensitivity analysis in the control scenario
sens_run_baseline <- fread("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/sensitivity_runs_baseline_scenario.tsv", integer64 = "numeric")
invisible(gc())  # remove unecessary memory


# subset the necessary columns (the timestep, x, y, species and proportion for each variable)
sens_run_baseline_subset <- sens_run_baseline[, c(1:7, 14:17)]

# transform all necessary column into rows (pivot longer operation)
simulations_long <- sens_run_baseline_subset %>% 
  as.data.table() %>% 
  data.table::melt(id.vars = c(1:7), variable.name = "Var_sim", value.name = "Value")
invisible(gc())

# split the Var_sim column into two to have the Simulation and Variable names in the correct format for the boxplots
simulations_long[, c("Variable", "Simulation") := data.table::tstrsplit(Var_sim, "_")]
invisible(gc())


variables.labs <- c("abund" = "Abundance",
                    "bevmort" = "Mortality",
                    "carry" = "Carrying capacity",
                    "hab" = "Habitat suitability",
                    "reprod" = "Reproduction")



#plot the results into a boxplot 
#this running and saving this plot takes 40 min 
sens_analysis <- ggplot(simulations_long, aes(x = as.factor(Simulation), y = Value)) +
  geom_boxplot(outlier.shape = NA) +
  ylim(0,1.6) +
  facet_wrap(.~Variable, labeller = as_labeller(variables.labs), as.table = F, ncol = 2) +
  stat_summary(fun = mean, geom = "point", aes(group = interaction(species, Variable, Simulation), color = species), shape = 16, size = 1.5, position = position_jitter(width = 0.5, height = 0)) +
  scale_color_viridis_d(option = "D") +
  labs(x = "Simulation", y = "value", color = "species") +
  geom_hline(yintercept = 1.2, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.0),
        panel.background = element_blank (), 
        panel.grid.major = element_line(color = "gray90", size = 0.25),
        panel.grid.minor = element_line(color ="gray90", size = 0.25),
        strip.text = element_text(face = "bold", size = rel(0.8)),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.key = element_rect(fill = "white", colour = NA))
#sens_analysis

#ggsave(plot = sens_analysis,
      # file = "C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/Sensitivity_analysis_31Jan2025.tiff",
       #bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")


end.time <- Sys.time() # end the clock
time.taken <- round(end.time - start.time) # calculate time taken to run the complete script
time.taken