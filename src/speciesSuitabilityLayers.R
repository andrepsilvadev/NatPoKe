## Name: Creating SDM-based environmental layer ##
## Author: Inês Silva ##
## Description: stack .tif outputs from SDMs, for multiple species and multiple environmental scenarios ##
## Date: September 10th 2025 ##


source("./src/libraries.R")
source("./src/customFunctions2.R")

##########
# STEP 1 # Define parameters 
##########

# target species
target_species <- c(
  ##############
  # BOREAL SPS #
  ##############
  
  # Europe
  #"Alces alces", "Bison bonasus", "Cervus elaphus", "Sus scrofa", "Vulpes vulpes",
  #"Panthera tigris", "Lynx lynx", "Ursus arctos", "Canis lupus", "Rangifer tarandus"#,
  
  # North America
  #"Alces alces", "Canis latrans", "Lynx rufus", "Martes americana", "Taxidea taxus",
  #"Ursus americanus", "Vulpes vulpes", "Puma concolor", "Bison bison", "Ursus arctos",
  #"Canis lupus", "Rangifer tarandus"#,
  
  # South America
  #"Leontopithecus caissara", 
  #"Leopardus pardalis", "Nasua nasua", "Panthera onca",
  #"Puma concolor"#,
  
  # Africa
  #"Aepyceros melampus", "Colobus angolensis",# "Daubentonia madagascariensis",
  #"Diceros bicornis", "Erythrocebus patas", "Gorilla beringei", "Gorilla gorilla",
  #"Orycteropus afer", "Pan paniscus", "Pan troglodytes", "Papio anubis", "Papio ursinus", 
  #"Crocuta crocuta", "Mandrillus sphinx", "Panthera pardus", "Syncerus caffer",
  #"Acinonyx jubatus", "Panthera leo", "Connochaetes taurinus", "Loxodonta africana"#,
  
  # Asia
  "Cervus nippon", "Cuon alpinus", "Felis chaus", "Macaca fuscata", "Pongo abelii",
  "Pongo pygmaeus", "Sus scrofa", "Vulpes vulpes", "Panthera pardus", "Acinonyx jubatus",
  "Lynx lynx", "Panthera leo", "Panthera tigris", 
  "Ursus arctos",
  "Canis lupus"
)


target_species <- gsub(" ", ".", target_species)
#target_biome <- "Boreal Forests/Taiga"
# biome
if (target_biome == "Tropical & Subtropical Moist Broadleaf Forests") {
  biome <- "tropical"
} else if (target_biome == "Boreal Forests/Taiga") {
  biome <- "boreal"
}

biomes <- c("tropical", "boreal")  

# path for each biomes' SDM outputs
biome_paths <- list(                      
  tropical = "./output/NatPoke_October25_tropical",
  boreal   = "./output/NatPoke_October25_boreal"
)

# years to repeat each period
rep_scheme <- c("current" = 15, "2030" = 20, "2050" = 50, "2100" = 50)
future_scenarios <- c("ssp126", "ssp585")


##########
# STEP 2 # Stack projections per scenario and species. Save that output
##########

# STEP 2 RUNS ONCE AND FOLLOWING STEPS RUN AS MANY TIME AS REGIONS MODELLED WITH METARANGE

# for (sp in target_species) {
#   for (biom in biomes) {
# 
#     ## NAVIGATION WARNING ##
#     ### Warnings will appear in the end it's totally fine.
#     ### we just intiated with an empty raster in line 48
# 
#     path <- biome_paths[[biom]]
# 
#     # Load current raster
#     current_file <- file.path(path, paste0("proj_Current_EM_", sp, "_continuous.tif"))
#     if (!file.exists(current_file)) next
#     r_current <- rast(current_file)
# 
#     for (sc in future_scenarios) {
# 
#       # Load future rasters for this scenario and repeat according to the scheme
#       future_files <- list.files(
#         path,
#         pattern = paste0("proj_", sc, "_.*_", biom, "_", sp, "_continuous.tif$"),
#         full.names = TRUE
#       )
#       if (length(future_files) == 0) next
# 
#       # start empty stack
#       r_stack <- rast()
#       year_counter <- 2015
#       layer_names <- character()
# 
#       # repeat current raster n times
#       for (i in 1:rep_scheme["current"]) {
#         r_stack <- c(r_stack, r_current)
#         layer_names <- c(layer_names, as.character(year_counter))
#         year_counter <- year_counter + 1
#       }
# 
#       # repeat each future raster n times
#       for (f in future_files) {
#         r <- rast(f)
#         yr <- strsplit(basename(f), "_")[[1]][3]  # 2030, 2050, 2100
#         if (!yr %in% names(rep_scheme)) next
# 
#         for (i in 1:rep_scheme[[yr]]) {
#           r_stack <- c(r_stack, r)
#           layer_names <- c(layer_names, as.character(year_counter))
#           year_counter <- year_counter + 1
#         }
#       }
# 
#       # rename layers for years
#       names(r_stack) <- layer_names
# 
#             # define output file path
#       out_dir <- "./data/SDMlandscapes_October25/biome"
#       out_file <- file.path(out_dir, paste0(sp, "_", biom, "_", sc, ".tif"))
# 
#       # ensure the folder exists
#       if (!dir.exists(out_dir)) {
#         dir.create(out_dir, recursive = TRUE)}
#       # save a stack per species & scenario
#       writeRaster(r_stack, out_file, overwrite = TRUE)
#       message("Saved: ", out_file, " with ", nlyr(r_stack), " layers")
#       #rm(r, r_stack, r_current, current_file)
#     }
#   }
# }
#plot(r_stack$`2015`)
#plot(r_stack$`2030`)
#plot(r_stack$`2050`)
#plot(r_stack$`2120`)


##########
# STEP 3 # Rescale, crop & mask for each region. Save output again
##########

message("Start Rescaling, Cropping and Masking Output Rasters")

# list of all our target sps, biome & scenario stacked rasters
raster_files <- list.files(
  path = "./data/SDMlandscapes_October25/biome",
  pattern = paste0("^(", paste(gsub(" ", ".", target_species), collapse = "|"), ")_", biome, "_", scenario, ".*\\.tif$"),
  full.names = TRUE)

#r_file <- "data/SDMlandscapes/biome/Panthera.onca_tropical_ssp126.tif"
#target_biome <- "Tropical & Subtropical Moist Broadleaf Forests"
#target_region <- "South America"
#target_species<- "Panthera onca"

for (r_file in raster_files) {
  sp_raster <- rast(r_file)
  
  # RESCALE 0-1000 → 0-1
  sp_raster <- sp_raster / 1000
  invisible(gc())
  
  # load biome and continent geometries
  biome_sf <- load_biome(biome_name = target_biome)
  continent_sf <- load_select_continents(continent_names = target_region)
  
  # Crop biome to continents
  sf_use_s2(FALSE)
  target_geom <- crop_biome_to_continent(biome_sf, continent_sf)
  invisible(gc())
  
  # CROP and MASK raster
  sp_raster <- terra::crop(sp_raster, target_geom)
  sp_raster <- terra::mask(sp_raster, target_geom)
  invisible(gc())
  #plot(sp_raster)
  
  # Save processed raster
  output_path <- file.path(dirinput, paste0(tools::file_path_sans_ext(basename(r_file)), "_cropped.tif"))
  writeRaster(sp_raster, output_path, overwrite = TRUE)
  
  message("Processed and saved: ", basename(output_path))
  
  rm(sp_raster)
  gc()
}

##########
# STEP 4 # Reprojected Rasters & save output
##########

# define target CRS to reproject landscapes
targetRegionCRS <- ifelse(target_region == "Europe", "ESRI:54030",
                          ifelse(target_region == "North America", "ESRI:54030",
                                 ifelse(target_region == "Africa", "ESRI:54030",
                                        ifelse(target_region == "South America", "ESRI:54030",
                                               ifelse(target_region == "Asia", "ESRI:54030",
                                                      NA)))))


# list all cropped rasters
landscapes <- list.files(path = dirinput,
                         pattern = ".*_cropped\\.tif$",
                         full.names = TRUE)
gc()
message("Start Reprojecting and Converting to km...")

#landscape <- landscapes[[1]]
for (landscape in landscapes) {
  message("Processing: ", basename(landscape))
  
  ## WARNINGS may appear in the end!! It might be ok, but still check
  ## GDAL couldn’t compute the outer bounds reliably when using the Robinson projection system
  
  # load raster stack
  r <- terra::rast(landscape)
  #plot(r)
  
  # Reproject to target CRS
  r_utm <- terra::project(r, targetRegionCRS)
  
  # crop and mask - to ensure no dead pixels in the corners
  
  # load biome and continent geometries
  biome_sf <- load_biome(biome_name = target_biome)
  continent_sf <- load_select_continents(continent_names = target_region)
  # crop biome to continents
  sf_use_s2(FALSE)
  target_geom <- crop_biome_to_continent(biome_sf, continent_sf)
  # project target region to ronbinson
  target_geom_robinson <- st_transform(target_geom, crs = "ESRI:54030")
  invisible(gc())
  # CROP and MASK raster
  r_utm_masked <- terra::crop(r_utm, target_geom_robinson)
  r_utm_masked <- terra::mask(r_utm_masked, target_geom_robinson)
  invisible(gc(rm(biome_sf, continent_sf, target_geom, target_geom_robinson)))
  
  # convert to rasterStack (note: here we changed packages because terra was removing the rasters' values when changing the crs)
  r_raster <- raster::stack(r_utm_masked)
  
  # get original CRS
  orig_crs <- crs(r_utm_masked)
  
  # rescale extent (divide by 1000 to convert meters to kilometers)
  extent(r_raster) <- extent(r_raster) / 1000
  
  # extract species name from filename
  species_name <- gsub("_.*", "", file_path_sans_ext(basename(landscape)))
  
  # select target species
  species_traits <- read.csv(file.path(dirinput,"metaRangeSpeciesDataframe.csv"))
  
  # get an aggregation factor from species traits
  species_fact <- ceiling(species_traits$ModellingRes[species_traits$Species == species_name] / sqrt(species_traits$CellResolution[species_traits$Species == species_name]))
  
  # aggregate raster using terra
  r_agg <- aggregate(r_utm, fact = species_fact, fun = mean, na.rm = TRUE)
  
  # build an output filename
  output_filename <- gsub("\\.tif$", "_reprojectedKm.tif", landscape)
  #file.path(dirinput, paste0(gsub("\\.", "", species_name), "_", biome, "_", scenario, "_cropped_reprojectedKm.tif"))
  
  # save aggregated raster
  writeRaster(r_agg, output_filename, overwrite = TRUE)
  
  # clean memory & save space
  rm(r, r_utm, r_agg)
  gc()
}

message("✅ Environmental layers prepared successfully! ")