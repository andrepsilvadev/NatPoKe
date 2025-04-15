## Name: Custom functions ##
## Authors: Andre P. Silva & Jorinde-M. Rieger ##
## Description: Loads all developed customised functions ##

# Function to map ESA LULC values to the 7 LULC types
map_values_to_land_use <- function(x) {
  sapply(x, function(val) {
    if (val %in% names(value_to_land_use)) {
      return(value_to_land_use[[as.character(val)]])
    } else {
      return(NA)  # Handle values that do not map to any land-use type
    }
  })
}
# Function to calculate the percentages for each land-use type
calculate_land_use_percentages <- function(raster_stack, land_use_types, land_use_names,time) {
  # Create binary maps for each land-use type
  land_use_layers <- lapply(land_use_types, function(cat) {
    terra::app(raster_stack, fun = function(x) {
      return(ifelse(x == cat, 1, 0))
    })
  })
  
  # Calculate the percentages for each land-use type without NAs
  land_use_percentages <- sapply(land_use_layers, function(layer) {
    sum(values(layer), na.rm = TRUE) / sum(!is.na(values(layer))) * 100
  })
  
  # Create a data frame with the results
  percentage_df <- data.frame(
    time = time,
    landuse = land_use_names,
    variable = land_use_names,
    value = land_use_percentages
  )
  
  return(percentage_df)
}

#####################################
# Beverton & Holt demographic model #
#####################################

beverton_holt <- function(abundance, reproduction_rate, carrying_capacity, survival_rate) {
  # Safeguarding the input
  # you may remove this part if you are sure that the input is correct
  survival_rate <- ifelse(survival_rate > 1, 1, survival_rate)
  survival_rate <- ifelse(survival_rate < 0, 0, survival_rate)
  reproduction_rate <- ifelse(reproduction_rate < 0, 0, reproduction_rate)
  
  
  abundance <- abundance * survival_rate
  abundance_t1 <- (reproduction_rate * abundance) /
    (1 + ((reproduction_rate - 1) / carrying_capacity) * abundance)
  abundance_t1[abundance_t1 < 0] <- 0
  return(abundance_t1)
}

####################################
# Model Validation for one species #
####################################

validateModel_1sps <- function(
    targetspecies, independentDensity, estimatedDensity, spData) {
  # compares mean density estimated by model per cell with
  # predicted density from independent model extract predicted abundance 
  # and join with observed abundance
  # based on validateModel1 from MechSpatCons but uses estimated
  # number of individuals from rangeshifter output dataframe
  # instead from raster
  
  ## species density estimates by an independent source (akin to observed density)
  independentDensity <- independentDensity %>% 
    dplyr::filter(Species %in% target_sps) %>%
    dplyr::select(Species, lw95, lw75, PredMd, up75, up95) %>%
    mutate(
      lw95 = as.numeric(lw95),
      lw75 = as.numeric(lw75),
      PredMd = as.numeric(PredMd), # Predicted population density (individuals/km2)
      up75 = as.numeric(up75),
      up95 = as.numeric(up95)) %>%
    rename(
      species = Species,
      meanDensity = PredMd
    )
  
  ## species density estimated by metaRange
  predicted <- estimatedDensity %>%
    mutate(species = target_sps) %>% 
    dplyr::group_by(species, x, y) %>%
    dplyr::summarise(
      meanNInd = mean(lyr1),
      .groups = 'drop') %>%
    as.data.frame()
  
  spData2 <- spData %>%
    dplyr::select(Species, ModellingRes) %>%
    dplyr::filter(Species == target_sps) %>% 
    rename(species = Species) %>%
    as.data.frame()
  
  estimatedDensityJoin <- dplyr::inner_join(predicted, spData2, by = "species") %>%
    mutate(estimatedDensity = meanNInd/ModellingRes)
  
  ## compare observed with predicted density
  list <- list(independentDensity, estimatedDensityJoin)
  names(list) <- c("independentDensity", "estimatedDensity")
  return(list)
}

#########################################
# Model validation for multiple species #
#########################################

validateModel1.2 <- function(targetspecies, independentDensity, dirouts, spData, validationYear) {
  # Compares mean density estimated by model per cell with
  # predicted density from independent model extract predicted abundance
  # and join with observed abundance
  # 1. Loads rasters - 2. Sample ~300 random cells per species - 3. Calculate predicted densities - 4. Returns a comparison-ready list
  
  ## Species density estimates by an INDEPENDENT SOURCE (akin to observed density)
  independentDensity <- independentDensity %>%
    dplyr::filter(Species %in% targetspecies) %>%
    dplyr::select(Species, lw95, lw75, PredMd, up75, up95) %>%
    mutate(
      lw95 = as.numeric(lw95),
      lw75 = as.numeric(lw75),
      PredMd = as.numeric(PredMd), # Predicted population density (individuals/km2)
      up75 = as.numeric(up75),
      up95 = as.numeric(up95)
    ) %>%
    rename(
      species = Species,
      meanDensity = PredMd
    )
  
  ## Species density estimated by METARANGE from multiple directories
  abundance_files <- list()
  for (target_sps in targetspecies) {
    all_files <- character()
    for (dirout in dirouts) { #Iterate through each directory
      files <- list.files(
        path = dirout,
        pattern = paste0(validationYear, "_", target_sps, "_abundance\\.tif$"),
        full.names = TRUE
      )
      all_files <- c(all_files, files)
    }
    abundance_files[[target_sps]] <- all_files
    if(length(all_files) > 0){
      message(paste("Used rasters for", target_sps, ":", paste(basename(all_files), collapse = ", ")))
    } else {
      warning(paste("No rasters found for", target_sps, "in the given directories."))
    }
  }
  
  abundance_rasters <- lapply(abundance_files, function(files) {
    lapply(files, terra::rast)
  })
  
  abundance_stack_list <- lapply(abundance_rasters, function(raster_list){
    if(length(raster_list) > 0){
      terra::rast(unlist(raster_list))
    } else {
      NULL
    }
  })
  
  # convert raster stack to df
  species_df <- lapply(names(abundance_stack_list), function(sps_name){
    stack <- abundance_stack_list[[sps_name]]
    if(!is.null(stack)){
      lapply(1:terra::nlyr(stack), function(i){
        as.data.frame(stack[[i]], xy = TRUE) %>%
          mutate(species = sps_name) %>%
          rename(abundance = 3)
      }) %>% bind_rows()
    } else {
      NULL
    }
  }) %>% bind_rows()
  
  # sample 300 abundance values for each species
  species_df_sampled <- species_df %>%
    group_by(species) %>%
    group_modify(~ {
      df <- .x
      if (nrow(df) >= 300) {
        df[sample(nrow(df), 300), ]
      } else {
        df  # keep all if there are fewer values than 300
      }
    }) %>%
    ungroup()
  
  
  # format raster's dataframe for validation
  predicted <- species_df_sampled %>%
    dplyr::filter(species %in% targetspecies) %>%
    dplyr::group_by(species, x, y) %>%
    dplyr::summarise(
      meanNInd = mean(abundance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    as.data.frame()
  
  spData2 <- spData %>%
    dplyr::select(Species, ModellingRes) %>%
    rename(species = Species) %>%
    #mutate(ModellingRes = ifelse(ModellingRes == unique(ModellingRes)[1], unique(ModellingRes)[1], unique(ModellingRes)[1])) %>% #modified to take the first unique value of ModellingRes
    as.data.frame()
  
  estimatedDensityJoin <- dplyr::inner_join(predicted, spData2, by = "species") %>%
    mutate(estimatedDensity = meanNInd / ModellingRes)
  
  ## compare observed with predicted density
  result_list <- list(independentDensity, estimatedDensityJoin)
  names(result_list) <- c("independentDensity", "estimatedDensity")
  return(result_list)
}
