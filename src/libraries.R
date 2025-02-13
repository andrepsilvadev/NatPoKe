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
    "ggplot2",
    "stringr",
    "tibble",
    "tidyr",
  
  prompt = FALSE)
