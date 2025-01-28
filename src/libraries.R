## Name: libraries ##
## Authors: Andre P. Silva ##
## Description: Includes all libraries needed to run the repository ##

library(easypackages)
easypackages::packages(
    "dplyr",
    "terra",
    "rnaturalearth",
    "rnaturalearthdata",
  prompt = FALSE)
