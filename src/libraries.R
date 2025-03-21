## Name: libraries ##
## Authors: Andre P. Silva ##
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
  
  # spatial data processing  
  "terra",
  "raster",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  
  # modelling
  "metaRange",
  
  # data manipulation & visulisation
    "dplyr",
    "tidyverse",
    "ggplot2",
    "stringr",
    "tibble",
    "tidyr",
    "rphylopic",
    "viridis",
    "circlize",
    "grid",
    "gridExtra",
    "patchwork",
  
  prompt = FALSE)


