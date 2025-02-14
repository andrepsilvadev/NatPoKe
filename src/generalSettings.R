####################
# GENERAL SETTINGS #
####################
# Inês Silva
# 14 Feb 2025

# create a working directory for each simulation run ---------------------------
runpath <- file.path("C:/Users/maria/OneDrive - Universidade de Lisboa/ANDRE/NatPoKe/trial_runs", runname)
dir.create(runpath, showWarnings = TRUE)
dir.create(file.path(runpath, "Inputs"), showWarnings = TRUE)
dir.create(file.path(runpath, "Outputs"), showWarnings = TRUE)
#dir.create(file.path(runpath, "Output_Maps"), showWarnings = TRUE)
dirinput <- file.path(runpath, "Inputs")
dirout <- file.path(runpath, "Ouputs")


# spatial projections ----------------------------------------------------------
# UTM North-Sweden minimizes local distortion
# Tried this before. Generates the following:
# "unequal horizontal and vertical resolutions. Such data cannot be stored in arc-ascii format"
#utm34 <- "+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs"

# Lambert Azimuthal Equal Area - preserves the relative sizes of areas
# throughout the projection. This property makes it unsuitable for
# large regions
#crs.laea <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# wgs84
#wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"
#wgs84_crs <- "EPSG:4326"

# country crs
#sweden_crs <- "EPSG:3006"