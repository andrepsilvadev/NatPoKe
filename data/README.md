# 📁 Data Folder

This folder contains the **input datasets** for the proposed trait-based and spatial modeling workflow. It includes starting trait databases for mammal and bird species, as well as global suitability index landscapes.

## Contents

### 🗺️ `global_suitability_landscapes/`

Contains raster files representing **global suitability index maps** for various species.\
\> *Note: These are temporary placeholders and will eventually be replaced by species distribution models (SDMs).*

### 🐾 `mammalTraits_2025-03-17.csv`

The most recent version of the **raw mammal trait database**.\
Includes unprocessed or minimally cleaned data aggregated from multiple sources.

### 🐦 `birdTraits_2025-07-19*.csv`

If present, these files contain raw trait data for bird species compiled from sources like: - AVONET - Tobias et al. 2022 - Santini et al. 2023 (Abundance, carrying capacity) - Jetz et al. 2008 (Clutch size) - Amniote database (Body mass) - Bird et al. 2020 (Survival, reproduction, longevity)

## Notes

-   All data in this folder are either raw or semi-processed.
-   Refer to the corresponding R scripts for details on how these files are generated and formatted for the metaRange model.
