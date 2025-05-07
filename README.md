# NatPoKe — Nature Policy Effects on Keystone Species

[![DOI](https://img.shields.io/badge/DOI-coming_soon-blue?logo=doi&logoColor=white)](https://doi.org/10.0000/placeholder) [![Project Page](https://img.shields.io/badge/Project_Website-MISTRAFinBio-green?logo=leaflet&logoColor=white)](https://finbio.org/)

A research pipeline to explore how nature policy interventions affect keystone species in Boreal and Tropical forests.<br>
The project leverages process-based modeling (via [metaRange](https://metarange.github.io/metaRange/#)) and resilience metrics to evaluate ecological responses under various policy scenarios.


> 🚧 **Under active development** 🚧 
<br>



## Repository Overview

This repository contains all scripts and supporting materials used in the modeling and analysis pipeline.<br>

[![Input Data](https://img.shields.io/badge/input-data-green)](#0) [![Models: Mammals](https://img.shields.io/badge/_Models-🦣_Mammals-yellow?style=flat&labelColor=grey)](https://github.com/andrepsilvadev/NatPoKe/blob/8cb276119a3c2ad5cd751932f9ee613dd05304fc/src/mammalModel.R) [![Figures & Maps](https://img.shields.io/badge/visualizations-figures-orange)](#0) [![Validation](https://img.shields.io/badge/validation-modelValidation-red)](https://github.com/andrepsilvadev/NatPoKe/blob/8cb276119a3c2ad5cd751932f9ee613dd05304fc/src/modelValidation.R)

| Folder      | Description                                                                                                                                                                                                                                                                                      |
|:----------- |:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| **data**    | Raw data, including trait data for XX mammal species, sourced from multiple sources.                                                                                                                                                                                                             |
| **input**   | Intermediate files and data transformations used during pre-processing. ⚠️ *Currently not in use.*                                                                                                                                                                                               |
| **output**  | Generated results: figures, tables, and model outputs. *Currently with dummy figure only.*                                                                                                                                                                                                       |
| **src**     | All R scripts used in data analysis, modeling, and visualization.<br>➡️ For full details, see the [`src/README.md`](https://github.com/andrepsilvadev/NatPoKe/tree/4295ecc339433ef3d49ab62a288edb9beb6d765f/src#readme).<br><ul><li>model input data preparation</li><li>species models</li><li>visualizations</li><li>Validation & Sensitivity Analysis</li></ul> |
| **reports** | R Markdown files producing reports from model runs (single and multispecies), and main and supplementary project figures.                                                                                                                                                                        |

