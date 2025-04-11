################################
# UPDATED SENSITIVITY ANALYSIS #
################################
# Inês Silva
# 07 Apr 2025

## !!!!!!!! CAREFULL !!!!!!!! need to have acolumn in the data for replicate
## then summarize across replicates


# select target year 101
# then summarise across replicate (WE STILL DONT HAVE)
# get baseline data for that year per cell
# get sensitivity data for that year per cell
# combine sensitivity runs and baseline data (make sure baseline TNIND values are a single column)
# calculate proportions
# make plot

target_sps <- c("Alcesalces", "Lynxlynx", "Cervuselaphus", "Rangifertarandus", "Susscrofa", "Damadama", "Canislupus")

##########
# Step 1 # Select data from folders and convert raster to dfs
##########

for

# find raster for timestep to validate
abund101 <- rast(list.files("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/26Mar2025_Europe/Outputs",
                            pattern = paste0("101_", target_sps, "_abundance\\.tif"), full.names = TRUE))
  abundance101_df <- as.data.frame(abund101, xy = TRUE)

##########
# Step 2 # Combine all datasets
##########

# baseline data ----------------------------------------------------------------

TNIND_europe <- fread("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/26Mar2025_Europe/Outputs/TNIND_yr_26Mar2025_Europe.csv") %>% 
  mutate(scenario = "BAU",
         taxa = "Mammals",
         simulation = "baseline")
colnames(TNIND_europe) <- c("TNIND_baseline", "biome", "region", "species", "timestep", "scenario", "taxa", "simulation_baseline")
TNIND_europe$region <- case_when(
  TNIND_europe$region == "Sweden" ~ "Europe",
  TRUE ~ TNIND_europe$region
)
TNIND_europe$biome <- gsub(" ", "", TNIND_europe$biome)

# sensitivity data -------------------------------------------------------------

TNIND_europe0.95 <- fread("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/10April_Europe_abund0.95/Outputs/TNIND_yr_10April_Europe_abund0.95.csv") %>% 
  mutate(scenario = "BAU",
         taxa = "Mammals",
         simulation = "abund095") 
head(TNIND_europe0.95)

TNIND_europe1.05 <- fread("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/NatPoKe/trial_runs/10April_Europe_abund1.05/Outputs/TNIND_yr_10April_Europe_abund1.05.csv") %>% 
  mutate(scenario = "BAU",
         taxa = "Mammals", 
         simulation = "abund105") 



# then join with baseline

TNIND_yr_sensitivity <- TNIND_europe0.95 %>% 
  left_join(TNIND_europe1.05, by = c("species", "scenario", "biome", "region", "timestep", "taxa", "simulation","TNIND")) %>% 
  left_join(TNIND_europe, by = c("species", "scenario", "biome", "region", "timestep", "taxa")) %>% 
  mutate(prop_abund = TNIND/TNIND_baseline)



#write.csv(TNIND_yr_sensitivity, file = "C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs/sensitivityData.csv")

sensitivity_plotData <- TNIND_yr_sensitivity %>% 
  dplyr::select(!c("TNIND", "TNIND_baseline", "simulation_baseline", "timestep")) %>% 
  pivot_longer(cols = !c("biome", "region", "species", "scenario", "taxa", "simulation")) 


ggplot(sensitivity_plotData, aes(x=simulation, y=value)) + 
  geom_boxplot() +
  geom_hline(yintercept=1.20, linetype="dashed", color = "red") +
  geom_hline(yintercept=0.80, linetype="dashed", color = "red") +
  stat_summary(fun = mean, geom = "point", aes(group = interaction(species, simulation), color = species), shape = 16, size = 1.5, position = position_jitter(width = 0.5, height = 0)) +
  #geom_jitter(shape=16, position=position_jitter(0.2), aes(colour = species)) +
  scale_colour_viridis(discrete = TRUE) +
  facet_wrap(~biome, ncol=3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


 
