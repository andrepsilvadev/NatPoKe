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

##########
# STEP 1 #  Have a list of species that entered the model
##########

# create empty list
results <- list()

species_names <- c("Alcesalces", "Ursusarctos", "Lynxlynx")

##########
# Step 2 #
##########

# loop through species and process ONE raster at a time
for (sp in species_names) {
  
  # find raster files for the current species
  flist <- list.files(here("example/mammals_try2/results_28Jan"), 
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
    
    # add information for easier identification of each raster (sp, timestpe, scenario, etc..)
    raster_data$scenario <- filename_parts[1]  # BAU
    raster_data$biome <- filename_parts[2]     # Tropical
    raster_data$region <- filename_parts[3]    # Asia
    raster_data$timestep <- filename_parts[4] 
    raster_data$species <- sp                  # use current species name
    names(raster_data)[names(raster_data) == "lyr1"] <- filename_parts[6]
    
    # store result in a list, then append by species
    if (!is.null(results[[sp]])) {
      results[[sp]] <- rbind(results[[sp]], raster_data)
    } else {
      results[[sp]] <- raster_data
    }
  }
}

# combine all species results into one data frame
final_results <- do.call(rbind, results)
#View(final_results)

#WRITE RESULTS TO .tsv
write_tsv(final_results, "example/mammals_try2/results_28Jan/final_results28Jan.tsv")

library(data.table)


final_results <- fread("example/mammals_try2/results_28Jan/final_results28Jan.tsv")

final_results$taxa <- "Mammal"

##############################
# SIMPLE ABUNDANCE OVER TIME #
##############################
final_results %>% 
  group_by(scenario, biome, timestep, taxa, species) %>% 
  summarise(mean_abundance = mean(abundance, na.rm = TRUE)) %>% 
  ggplot(aes(x = timestep, y = mean_abundance, color = species)) +
  geom_line()


################################
# CALCULATE RESILIENCE METRICS #
################################

t_burnin <- 2
t_policy <- 5

# calculate post policy mean value for the recovery time metric 
# to be possible in one go with the other metrics)
post_disturbance_values <- final_results %>%
  filter(timestep >= t_burnin) %>% # remove burn-in period
  mutate(period = ifelse(timestep >= t_burnin &
                           timestep <= t_policy, "Pre", "Post")) %>%  # code pre and post policy periods
  group_by(biome, species, scenario, period, taxa) %>%
  filter(period == "Post") %>% # filter for the post policy period only
  summarise(mean_post = mean(abundance, na.rm = TRUE))
invisible(gc())

# calculate all stability metrics per biome, policy & species
stability_sps <- final_results %>%
  filter(timestep >= t_burnin) %>% # remove burn-in period
  mutate(period = ifelse(timestep >= t_burnin & timestep <= t_policy, "Pre", "Post")) %>%  # code pre and post policy
  left_join(post_disturbance_values,by = c("biome", "species", "scenario", "period", "taxa")) %>%
  group_by(biome, species, scenario, period, taxa) %>%
  summarise(mean = mean(abundance, na.rm = TRUE),
            # find mean nº of individuals
            min = min(abundance, na.rm = TRUE),
            # find min. nº of individuals
            max = max(abundance, na.rm = TRUE),
            # find max. nº of individuals
            impact_year = timestep[which.min(abundance)],
            # find the year the pop. reaches a min. value in the post policy period
            recovery_year = ifelse(any(timestep > t_policy & abundance >= mean_post),
                                   min(timestep[timestep > t_policy & abundance >= mean_post], na.rm = TRUE), # find the year where n_abundance is equal or smaller than the post policy mean 
                                   NA), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = c(mean, min, max, impact_year, recovery_year)) %>%
  dplyr::select(!c(impact_year_Pre, recovery_year_Pre)) %>% # remove year of min. nº of individuals in the pre policy period and the year in which the nº ind is equal to the mean values of the post policy period
  mutate(impact = ifelse( mean_Post > mean_Pre,
                          (max_Post - mean_Pre) / mean_Pre,
                          (min_Post - mean_Pre) / mean_Pre),
         time_impact = impact_year_Post - t_policy,
         # WORTH CALCULATING RECOVERY IF IMPACT IS POSITIVE? See metrics explanation canva
         recovery = ifelse(impact <= 0,
                           (mean_Post - mean_Pre) / mean_Pre,
                           NA),
         time_recovery = recovery_year_Post - t_policy)
invisible(gc())


# average stability metrics ACROSS TAXA
stability_avg <- stability_sps %>%
  group_by(biome, scenario, taxa) %>%
  dplyr::summarize(
    impact_avg = mean(impact, na.rm = TRUE),
    impact_sd = sd(impact, na.rm = TRUE),
    recovery_avg = mean(recovery, na.rm = TRUE),
    recovery_sd = sd(recovery, na.rm = TRUE),
    timeimpact_avg = mean(time_impact, na.rm = TRUE),
    timeimpact_sd = sd(time_impact, na.rm = TRUE),
    timerecovery_avg = mean(time_recovery, na.rm = TRUE),
    timerecovery_sd = sd(time_recovery, na.rm = TRUE)
  )
invisible(gc())

#stability_avg
library(grr)
library(ggplot2)
# stability metrics in long format for plots
stability_avg_long <- stability_avg %>%
  pivot_longer(
    cols = matches("_avg$|_sd$"),
    names_to = c("metric", ".value"),
    names_sep = "_")

############
# FIGURE 1 # Impact and Recovery per taxa for both biomes
############

# new facet label names
metric.labs <- c("Impact (units)", "Recovery (units)", "Time to Impact (years)" , "Time to recovery (years)")
names(metric.labs) <- c("impact",
                        "recovery",
                        "timeimpact",
                        "timerecovery")

# Custom color palette
custom_colors <- c("Bird" = "#38b2fe", "Mammal" = "#ffab27", "Insect" = "#99cc00")

# Updated plot
figure1 <- stability_avg_long %>%
  dplyr::filter(metric %in% c("impact", "recovery")) %>%
  ggplot(aes(x = scenario, y = avg, fill = taxa)) +
  geom_bar(stat = "identity", position = position_dodge(0.6), width = 0.6) +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd), width = 0.2, colour = "black", alpha = 0.9, size = 0.4, position = position_dodge(0.6)) +
  facet_grid(metric ~ biome, scales = "free", labeller = labeller(metric = metric.labs), switch = "y") +
  geom_hline(yintercept = 0) +
  # use custom colors for taxa
  scale_fill_manual("Taxa", values = custom_colors, ) +
  ylab("") +
  xlab("\nEconomic policy scenario") +
  theme_minimal() +
  theme(
    # remove gridlines 
    panel.grid = element_blank(),
    # add subtle horizontal lines 
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    # modify facet labels
    strip.text = element_text(face = "bold", size = rel(1)),
    strip.placement = "outside",
    # adjust legend
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    # modify y & x-axis text
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_text(face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
    # remove panel borders
    panel.border = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(2, "lines"))
figure1
invisible(gc())
