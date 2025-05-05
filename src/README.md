# NatPoKe src rationale

>[!WARNING]
>This is still under construction.

## master script
```
run.R <- this is teh master script that runs the entire pipeline (from loading packages up to building plots and maps
```

## settings & libraries
```
generalSettings.R <- create file directories to save runs inputs and outputs
libraries.R <- install & load all necessary packages to run this pipeline
customFunctions.R <- load custom functions created
```

## input files

```
metaRangeSpeciesDataframe.R <- transforms a species traits dataframe into an input dataframe for metaRange
inputFiles.R <- download global suitability raster files for multiple species from SRIT Database google drive
TaxaOccurence.R <- download taxa occurrence of multiple species from GBIF Database
inputSpeciesData.R <- cleans species occurrence data, one species per grid cell, filters by year
inputClimate.R <- calculates environmental input data (bioclimates) in various time periods, builds training and prediction landscapes

```

## models

```
OLDmammalModel.R <- this is the current running metaRange model script for mammals (20250214) it will be replaced with a more accurate version SOON
SMD.R <- Main function for species distribution modelling for multiple true species occurrences
```

## saving outputs as dataframe

```
savingSimulationOutputs.R <- converts the metaRange model rasters output to a dataframe per cell and saves it into a .csv file (SHOULD BE CHANGED IN THE NEAR FUTURE)
```

## figure and maps

```
speciesResilienceMetrics.R <- calculates stability metrics, such as impact, recovery, time to impact and time to recovery, and builds figures per taxa and scenario
communityMetricsFigures.R <- calculates community metrics, such as species richness, Shannon diversity and Functional diversity indexes, and builds figures over time
spatiallyExplicitMaps.R <- takes metrics calulated in the communityMetricsFigures.R and builds maps showing the change in these indexes per taxa
ClimateChange.R <- calculates spatially explicit bioclimatic changes in biomes
LandUseChange.R <- calculates land-use changes over time and spatially explicit changes in percent
SDMRun.R <- creates outputs of species distribution modeling, Species presence points, Evaluation plots for ensemble model evaluation, current and future suitability landscapes for multiple species
```

## model "evaluation"
```
modelValidation.R <- does the model validation by comparing mean species densities estimated from two sources (metaRange model & santini et al. 2022)
sensitivityAnalysis.R <- converts the metaRange model rasters output for each sensitivity run (which varies a parameter by x%) to dataframes and builds a sensitivity analysis boxplot for all variables
```
