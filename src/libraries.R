## Name: libraries ##
## Authors: Andre P. Silva  & Inês Silva ##
## Description: Includes all libraries needed to run the repository ##

library(easypackages)
easypackages::packages(
  
  # file paths & directories
  "here",
  "fs",
  "tools",
  
  # file storage & reading
  "googledrive",
  "data.table",
  "readr",
  "readxl",
  "writexl",
  
  # spatial data processing  
  "terra",
  "raster",
  "sp",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "rworldmap",
  
  #Species Distribution Modelling
  "biomod2", 
  "gam",
  "mda", 
  "earth", 
  "maxnet",
  "xgboost",
  "MAXENT", 
  "randomForest",
  "rgbif",
  "ggpubr",
  
  # modelling
  "metaRange",
  
  # data manipulation & visulisation
    "dplyr",
    "tidyverse",
    "ggplot2",
    "ggh4x",
    "stringr",
    "grr",
    "tibble",
    "tidyterra",
    "tidyr",
    "rphylopic",
    "viridis",
    "circlize",
    "grid",
    "gridExtra",
    "patchwork",
    "RColorBrewer",
     "crayon",
  "HomeRange",
  "naniar",
  
  prompt = FALSE)
