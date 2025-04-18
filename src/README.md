# Src folder - NatPoKe

[![Master Script](https://img.shields.io/badge/src-source-blue)](https://github.com/andrepsilvadev/NatPoKe/blob/8cb276119a3c2ad5cd751932f9ee613dd05304fc/src/run.R) [![Input Data](https://img.shields.io/badge/input-data-green)](#0) [![Models: Mammals](https://img.shields.io/badge/_Models-🦣_Mammals-yellow?style=flat&labelColor=grey)](https://github.com/andrepsilvadev/NatPoKe/blob/8cb276119a3c2ad5cd751932f9ee613dd05304fc/src/mammalModel.R) [![Figures & Maps](https://img.shields.io/badge/visualizations-figures-orange)](#0) [![Validation](https://img.shields.io/badge/validation-modelValidation-red)](https://github.com/andrepsilvadev/NatPoKe/blob/8cb276119a3c2ad5cd751932f9ee613dd05304fc/src/modelValidation.R)

------------------------------------------------------------------------

## 🚀 Structure

| Purpose | Key Files |
|---------------------------------|---------------------------------------|
| Run complete pipeline | `run.R` |
| Environment setup | `generalSettings.R`, `libraries.R`, `customFunctions.R` |
| Input data preparation | `mammalMetaRangeSpeciesDataframe.R`, `inputFiles.R` |
| Species modelling | `mammalModelSpecificRes.R` `mammalModel.R`  |
| Visualizations | `mammalSpeciesSpecificPlots.R`, `speciesResilienceMetrics.R`, `communityMetrics.R`, `updatedSpatiallyExplicitMaps.R` |
| Validation & Sensitivity Analysis | `modelValidation.R`, `updatedSensitivityAnalysis.R` |

------------------------------------------------------------------------

## 📖 Quick guide

`run.R` - this is a master script that runs the entire pipeline (from loading packages up to building plots and maps); here you can specify which biome, region and species to model

`generalSettings.R` - create file directories to save runs inputs and outputs

`libraries.R` - install & load all necessary packages

`customFunctions.R` - load created functions necessary throughout the pipeline

`mammalMetaRangeSpeciesDataframe.R` - format traget species traits, from a .csv file in the [data folder](https://github.com/andrepsilvadev/NatPoKe/blob/6bca303d6b0c17e94915c44f5e6323e978c0763e/data/mammalTraits_2025-03-17.csv), into an input dataframe ready to use within the metaRange model.

`inputFiles.R` - download and crop global suitability raster files for multiple species in a designated biome and region, from the SRIT Database google drive

`mammalModel.R` - runs the metaRange model for mammals with all species at the same resolution ⚠️*Currently not in use.*
`mammalModelSpecificRes.R` - runs the metaRange model for mammals with species specific resolution (which is based on species Mean Home Range)

`mammalSpeciesSpecificPlots.R` - creates figures for each mammal species suitability over time, abundance in the last time step, model validation, average abundance over time, proportion of abundance change, average dispersal change and produces a model overview figure, combining all maps/plots per species. See here an [example]().

`speciesResilienceMetrics.R` - calculates stability metrics, such as impact, recovery, time to impact and time to recovery, and builds figures per taxa and scenario

`communityMetrics.R` - calculates community metrics, such as species richness, Shannon diversity and Functional diversity indexes, and build a figure for each over time

`updatedSpatiallyExplicitMaps.R` - takes metrics calulated in the communityMetricsFigures.R and builds maps showing the change in these indexes per taxa in a spatially-explicit manner

`modelValidation.R` - performs model validation by comparing mean species densities estimated from two sources (metaRange model & santini et al. 2022)

`updatedSensitivityAnalysis.R` - performs a sensitivity analysis from additional runs where model parameter are changed by x% to see if response variables (e.g. abundance) are affected

[![Models: Mammals](https://img.shields.io/badge/_Models-🦣_Mammals-yellow?style=flat&labelColor=grey)](#mammals-model) [![🕊 Birds](https://img.shields.io/badge/🐦_Birds-blue?style=flat)](#birds-model) [![🌳 Trees](https://img.shields.io/badge/🌳_Trees-green?style=flat)](#trees-model) \# extra button for the future
