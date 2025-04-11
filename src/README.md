# NatPoKe — Nature Policy Effects on Keystone Species  
<br>
> Pipeline to explore how nature policy impacts keystone species in Boreal and Tropical forests using process-based modelling approaches and analysis of resilience metrics and biodiversity indicators.

---

## 🚀 Project Structure  

| Purpose | Key Files |
|---------|-----------|
| Main source code | `run.R` (master script) |
| Environment setup | `generalSettings.R`, `libraries.R`, `customFunctions.R` |
| Data preparation | `metaRangeSpeciesDataframe.R`, `inputFiles.R` |
| Species modelling | `OLDmammalModel.R` |
| Results storage | `savingSimulationOutputs.R` |
| Visualizations | `speciesResilienceMetrics.R`, `communityMetricsFigures.R`, `spatiallyExplicitMaps.R` |
| Validation & Sensitivity | `modelValidation.R`, `sensitivityAnalysis.R` |

---

## 🧭 Quick Navigation  

[![Source Code](https://img.shields.io/badge/src-source-blue)](./src)  
[![Input Data](https://img.shields.io/badge/input-data-green)](./src/input)  
[![Models](https://img.shields.io/badge/models-model-yellow)](./src/models)  
[![Figures & Maps](https://img.shields.io/badge/visualizations-figures-orange)](./src/figures_maps)  
[![Evaluation](https://img.shields.io/badge/evaluation-validation-red)](./src/model_evaluation)

---

## 📖 How to Run  

```r
# Run the master script from R
source("src/run.R")


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
```

## models

```
OLDmammalModel.R <- this is the current running metaRange model script for mammals (20250214) it will be replaced with a more accurate version SOON
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
```

## model "evaluation"
```
modelValidation.R <- does the model validation by comparing mean species densities estimated from two sources (metaRange model & santini et al. 2022)
sensitivityAnalysis.R <- converts the metaRange model rasters output for each sensitivity run (which varies a parameter by x%) to dataframes and builds a sensitivity analysis boxplot for all variables
```
