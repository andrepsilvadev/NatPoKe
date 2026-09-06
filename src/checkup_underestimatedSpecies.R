## checkup of underpredicted species ##
## Inês Silva ##
## 05Sept2026 ##
## Goal? Check what is happening to species showing underpredictions in the
## validation plots

##########
# STEP 1 # List species showing underestimation 
##########

validation_dir <- "E:/metaRange_May26/modelValidation"
# create a new folder to save plots and checkups
underest_sps_dir <- file.path(validation_dir, "underest_sps")
if (!dir.exists(underest_sps_dir)) {
  dir.create(underest_sps_dir, recursive = TRUE)
}

validation_sps <- read.csv("E:/metaRange_May26/modelValidation/Validation_labelledSpecies.csv")

underest_sps <- validation_sps %>% 
  dplyr::filter(validation %in% "Below 95% interval") %>% 
  distinct(species, region)

##########
# STEP 2 # Get pop trends for those sps
##########

TNIND_yr <- read.csv("E:/metaRange_May26/completeMetaRangeRun_20260517.csv")

## differences within EACH replicate
TNIND_diff <- TNIND_yr %>%
  arrange(species, biome, rep_num, timestep) %>%
  group_by(future_scenario, biome, region, species, rep_num) %>%
  mutate(diff_TNIND = TNIND - lag(TNIND)) %>%
  ungroup() %>% 
  dplyr::select(future_scenario, biome, region, species, timestep, rep_num, TNIND, diff_TNIND) %>% 
  arrange(future_scenario, biome, region, species)

## mean TNIND and diff ACROSS replicates
TNIND_mean <- TNIND_diff %>%
  group_by(future_scenario, biome, region, species, timestep) %>%
  summarise(mean_TNIND = round(mean(TNIND, na.rm = TRUE), 0)) %>% 
  mutate(mean_diff_TNIND = mean_TNIND - lag(mean_TNIND, n = 1),
         rep_num = "Mean") %>%
  ungroup() %>% 
  dplyr::select(future_scenario, biome, region, species, timestep, mean_TNIND, mean_diff_TNIND, rep_num)

TNIND_underest <- TNIND_mean %>%
  semi_join(underest_sps, by = c("species", "region"))

## plot new pop trends for undestimated sps INDIVIDUALLY
TNIND_underest_plots <- list()

for (i in seq_len(nrow(underest_sps))) {
  
  sp <- underest_sps$species[i]
  reg <- underest_sps$region[i]
  
  df <- TNIND_underest %>%
    # chose one ssp to avoid plotting both scenarios on top of each other
    dplyr::filter(species == sp, region == reg, future_scenario == "ssp126")
  
  p <- ggplot(df, aes(x = timestep, y = mean_TNIND)) +
    geom_line() +
    # burn-in line
    geom_vline(xintercept = 25, linetype = "dotted", color = "red") +
    labs(title = paste0(sp, " – ", reg), subtitle = "Species population trends",
         x = "Timestep", y = "Mean population size") +
    theme_minimal() +
    theme(plot.title = element_text(face = "italic"))
  
  # save plot in list
  TNIND_underest_plots[[paste(sp, reg, sep = "_")]] <- p
  
  # plot file name
  fname <- file.path(underest_sps_dir, paste0(gsub(" ", "", sp), "_", gsub(" ", "", reg), "_popTrend.png"))
  
  # save plot
  ggsave(filename = fname,
         plot = p, bg = "white", width = 350, height = 210,
         units = "mm", dpi = 300)
  
}

TNIND_underest_plots$`Aepyceros melampus_Africa`


## plot new pop trends for undestimated sps AGAINST OTHER SPS IN THE REGION
TNIND_ssp126_plots <- list()

combo_list <- TNIND_diff %>%
  filter(future_scenario == "ssp126") %>%
  distinct(biome, region, future_scenario)

for (i in seq_len(nrow(combo_list))) {
  
  b <- combo_list$biome[i]
  r <- combo_list$region[i]
  s <- combo_list$future_scenario[i]
  
  # filter data for this combination
  df <- TNIND_mean %>%
    filter(biome == b, region == r, future_scenario == s) %>%
    mutate(underest = paste(species, region) %in%
             paste(underest_sps$species, underest_sps$region))
  # skip if nothing there
  if (nrow(df) == 0) next
  # nice title
  biome_title <- gsub("([A-Z])", " \\1", b) |> trimws()
  
  # plot
  p <- ggplot(df, aes(x = timestep, y = mean_TNIND, group = species,
                      # color lines based on undervalidation or not
                      colour = underest)) +
    geom_line() +
    # burn-in line
    geom_vline(xintercept = 25, linetype = "dotted", color = "grey40") +
    facet_wrap(~species, scales = "free_y") +
    scale_colour_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                        labels = c("FALSE" = "Other species", "TRUE" = "Underestimated species"),
                        name = NULL) +
    labs(title = paste0(biome_title, " – ", r),
         subtitle = "Species population trends",
         x = "Timestep", y = "Mean population size") +
    theme_minimal() +
    theme(strip.text = element_text(face = "italic"),
          legend.position = "bottom")
  
  # list name
  plot_name <- paste(gsub("[/& ]", "", b), gsub("[/& ]", "", r), s, sep = "_")
  
  # add plot to list
  TNIND_ssp126_plots[[plot_name]] <- p
  
  # safe biome name for files
  b_file <- gsub("[/& ]", "", b)
  
  # # filename
  fname <- file.path(underest_sps_dir, paste0(b_file, "_", gsub(" ", "", r), "_",
                                              gsub(" ", "", s),
                                              "_speciesPopulationTrends_underestimated.png"))
  
  # save plot
  ggsave(filename = fname, plot = p, bg = "white", width = 350, height = 210,
         units = "mm", dpi = 300)
  
  rm(df, p)
  gc()
}

##########
# STEP 3 # Get trait data for those sps
##########

traits <- read_excel("E:/metaRange_May26/completeTraitDataframe_allSps.xlsx")

# add indicator for underestimated species
traits_table <- traits %>%
  mutate(underest = Species %in% underest_sps$species)

# create table with underestimated sps highlighted
underest_sps_traits <- traits_table %>%
  gt() %>%
  tab_style(style = list(cell_fill(color = "#FFF2CC"),
                         cell_text(weight = "bold")),
            locations = cells_body(rows = underest == TRUE)) %>%
  cols_hide(columns = underest)

# save as a formatted word document
gt::gtsave(underest_sps_traits, file.path(underest_sps_dir,
                                      paste0("modelValidationTable_underestSps", Sys.Date(), ".docx")))
