## Name: zipInputsForZenodo.R ##
## Author: Inês Silva ##
## Date: 30 Ago 2026
## Description: Zip Input folders all at once for storage in Zenodo ##


base_dir <- "E:/metaRange_May26"

folders <- file.path(base_dir,
  c("Africa_ssp126_20260517/Inputs", "Africa_ssp585_20260517/Inputs",
    "Asia_ssp126_20260517/Inputs", "Asia_ssp585_20260517/Inputs",
    "Europe+Asia_ssp126_20260517/Inputs", "Europe+Asia_ssp585_20260517/Inputs",
    "North America_ssp126_20260517/Inputs", "North America_ssp585_20260517/Inputs",
    "South America_ssp126_20260517/Inputs", "South America_ssp585_20260517/Inputs"))

zip(zipfile = file.path(base_dir, "My_Project_Files.zip"),
    files = folders,
    flags = "-r")
gc()
