# Data Folder

This folder contains all the **input datasets** for the proposed modeling workflow.

> [!IMPORTANT]
> Contents meantioned next can be downloaded from mentioned sources or from our [Zenodo repository]()

## Contents

#### `run_table.csv` 
A table specifying which regions and scenarios to run. It helps automate the pipeline.

#### `sensrun_table.csv`
A table specifying which sensitivity runs to perform, this means changing model parameters for each species by +5% or -5%. It help automate the process for sensitivity analysis.

#### `species_by_region.csv`
A table specifying which species to run in each region according to their distribution. This was decided by intersecting IUCN distribution polygons with Dinerstein's ecoregion and continents shapefiles. 

#### `GBIF_occurrence_mammals_2026-02-04.csv`
A .csv file with occurrence records for all target species, containing only decimal latitude, longitude and species name, with records for domestic species already removed.

#### 📁 `externaldata`

This folder should contain datasets from external sources such as:

1. IUCN spatial data for terrestrial mammals (download from [https://www.iucnredlist.org/resources/spatial-data-download](https://www.iucnredlist.org/resources/spatial-data-download))
2. continents shapefiles (download from [https://figshare.com/articles/dataset/Continent_Polygons/12555170](https://figshare.com/articles/dataset/Continent_Polygons/12555170)) 
3. ecoregions shapefile (download from [https://academic.oup.com/bioscience/article/67/6/534/3102935?login=false](https://academic.oup.com/bioscience/article/67/6/534/3102935?login=false))
4. Santini et al. 2022 estimates (.csv)

#### 📁 `sdm`

This folder contains all SDM-related outputs, including raw SDM outputs, stacked and interpolated suitability landscapes that serve as inputs for **MetaRange**, and figures used to visually assess SDM outputs (e.g., presence maps, variable importance, and evaluation metrics plots).

1. **`boreal_SDMS`**
   Contains the biomod2 output folder for each species, as well as:

   * EvalScores_*SPECIESNAME*_boreal.csv
   * EvalScoresEM_*SPECIESNAME*_boreal.csv
   * PresencePoints_*SPECIESNAME*_boreal.csv
   * Raster outputs (.tif) for the current period and 2030, 2050, and 2100 under SSP1-2.6 and SSP5-8.5.

2. **`tropical_SDMS`**
   Contains the biomod2 output folder for each species, as well as:

   * EvalScores_*SPECIESNAME*_tropical.csv
   * EvalScoresEM_*SPECIESNAME*_tropical.csv
   * PresencePoints_*SPECIESNAME*_tropical.csv
   * Raster outputs (.tif) for the current period and 2030, 2050, and 2100 under SSP1-2.6 and SSP5-8.5.

3. **`processedSDMs`**
   Contains stacked SDM outputs for all years and future scenarios. These outputs were interpolated (see [speciesSuitabilityLayers.R](https://github.com/andrepsilvadev/NatPoKe/blob/b8a9e877521f840f03b6e308cbfc04d9c9aaa185/src/speciesSuitabilityLayers.R)) to produce continuous suitability landscapes, which serve as inputs for **MetaRange** environments.

4. **`SDMsFigures`**
   Contains figures and tables used to assess and summarise SDM outputs:

   1. **`ContinuousLandscapes`**
      PNG files split into boreal and tropical biomes, visualising SDM suitability for different years, species, and future scenarios.

   2. **`presencePlots`**
      PNG files split into boreal and tropical biomes, visualising presence points available from GBIF and those used to produce the SDMs.

   3. **EvaluationMetrics.png**
      Plot showing evaluation metrics for each species, with one plot per biome.

   4. **VariableImportance_perBiome.csv/.docx**
      Tables showing variable importance values for each predictor used in the SDMs, summarised by biome.

   5. **VariableImportance_perSpecies.csv/.docx**
      Tables showing variable importance values for each predictor used in the SDMs, summarised by species.

   6. **AverageSuitabilityPerBiome.csv** and **AverageSuitabilityPerTrophicGroup.csv**
      Tables showing average suitability and the change between 2015 and 2100, summarised by biome and trophic group.


#### 📁 `traitData`

1. MammalSpecies_selection.xlsx - excel file with one sheet per region/continent with all species and raw trait data available (produced in [selectSpecies.R](https://github.com/andrepsilvadev/NatPoKe/blob/b8a9e877521f840f03b6e308cbfc04d9c9aaa185/src/selectSpecies.R))
2. CompleteMammalSpsDataframe_2025-12-20.csv - dataframe with species keeping only necessary traits for metaRange and those species with more than 30 occurrence points since 2015 (to be used in [metaRangeSpeciesDataframe_mammals.R](https://github.com/andrepsilvadev/NatPoKe/blob/b8a9e877521f840f03b6e308cbfc04d9c9aaa185/src/metaRangeSpeciesDataframe_mammals.R))
