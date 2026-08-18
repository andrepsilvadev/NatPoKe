## checkup of variable importance ##
## Inês Silva ##
## 05Feb2026 ##

# load data from October

# target mammal species
species_table <- read.csv("E:/NatPoKe_SDMs/SDMsFigures/VariableImportanceSummaryTable2026-08-18.csv",
                          stringsAsFactors = FALSE)

# average modelling resolution values per biome&region
avg_varImportance_perBiome <- species_table %>% 
  dplyr::filter(metrics == "Mean") %>% 
  group_by(biome)%>%
  summarise_at(c(#"bio1", "bio10", 
    "bio11", "bio12"
    #, "bio16", "bio17"
    ), mean, na.rm = TRUE)
# # A tibble: 2 × 7
# biome                                            bio1  bio10 bio11  bio12  bio16  bio17
# <chr>                                           <dbl>  <dbl> <dbl>  <dbl>  <dbl>  <dbl>
# 1 Boreal Forests/Taiga                           0.0608 0.0512 0.213 0.0528 0.0460 0.0794
# 2 Tropical & Subtropical Moist Broadleaf Forests 0.0211 0.0584 0.450 0.107  0.0755 0.0735