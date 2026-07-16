## Name: correlationsBrayCurtis ##
## Authors: Inês Silva ##
## Date: 02 JUly 2026 ##
## Description: assess correlation between the Bray Curtis results and other variables ##

source("src/libraries.R")

##########
# STEP 1 # Import rasters to asses
##########

# bray curtis dissimilarity rasters
bray_r <- list.files(path = "E:/metaRange_May26/BrayCurtis",
                        pattern = "\\.tif",
                        full.names = TRUE)


# shannon wiener index change rasters
shannon_r <- list.files(path = "E:/metaRange_May26/Shannon_output",
                        pattern = "\\.tif",
                        full.names = TRUE)

##########
# STEP 2 # Organise pairs to correlate
##########

# bray metadata
bray_df <- data.frame(file = bray_r) %>%
  mutate(name = basename(file)) %>%
  mutate(
    region   = str_extract(name, "(?<=bray_).*(?=_ssp)"),
    scenario = str_extract(name, "ssp126|ssp585"),
    trophic  = str_extract(name, "Carnivore|Herbivore|Omnivore"))

# shannon metadata
shannon_df <- data.frame(file = shannon_r) %>%
  mutate(name = basename(file)) %>%
  mutate(
    region   = str_extract(name, "^[^_]+.*(?=_ssp)"),
    scenario = str_extract(name, "ssp126|ssp585"),
    trophic  = str_extract(name, "Carnivore|Herbivore|Omnivore"))

pairs <- left_join(bray_df, shannon_df,
                   by = c("region", "scenario", "trophic"),
                   suffix = c("_bray", "_shannon"))

##########
# STEP 3 # Do overall landscape correlation per pair shannon-bray
##########

cor_results <- list()

for(i in 1:nrow(pairs)) {
  
  # read rasters
  r1 <- rast(pairs$file_bray[i])
  r2 <- rast(pairs$file_shannon[i])
  r2 <- terra::resample(r2, r1)
  
  # extract values
  v1 <- values(r1)
  v2 <- values(r2)
  
  # correlation
  r <- cor(
    v1[,1],
    v2[,1],
    use = "complete.obs"
  )
  
  cor_results[[i]] <- data.frame(
    region = pairs$region[i],
    scenario = pairs$scenario[i],
    trophic = pairs$trophic[i],
    correlation = r
  )
}

cor_table <- bind_rows(cor_results)

#print(cor_table)
write.csv(cor_table, "E:/metaRange_May26/Shannon_BrayCurtis_correlationPerRegion.csv",
          row.names = FALSE)
# see dispersion of overall correlarion values (is this usefull)

df <- data.frame(
  bray = values(r1)[,1],
  shannon = values(r2)[,1])

ggplot(df, aes(bray, shannon)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")
