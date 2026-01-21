# Remote Sensing for Animal Movement Ecology 🛰️🦓

**Course Materials for the Wildlife Tracking & Movement Ecology Workshop**
*Peking University | July 2024*

![R](https://img.shields.io/badge/Language-R-blue)
![License](https://img.shields.io/badge/License-MIT-green)
![Context](https://img.shields.io/badge/Focus-Movement%20Ecology-orange)

## 🔗 Live Tutorial
**[Click here to view the full Workshop Tutorial & Handout](https://str1ng-th3ory.github.io/PKU-Movement-Ecology-Workshophop/)**

## 📖 Overview
This repository contains the instructional materials and code designed for the **"Selecting, Obtaining, and Manipulating Remote Sensing Data"** module.

Using GPS tracking data from 12 adult white-bearded wildebeest (*Connochaetes taurinus*) in Kenya (2010-2012), this workshop demonstrates how to annotate animal movement trajectories with environmental covariates using R (`sf`, `terra`, `raster`).

## 👨‍🏫 Instructor Role
**Qianru Liao**, *University of Maryland, College Park*
* **Designed Curriculum:** Created a comprehensive workflow covering data acquisition (NASA AppEEARS), processing, and analysis.
* **Developed Materials:** Wrote the RMarkdown tutorials integrating ecological theory with technical execution.
* **Led Instruction:** Taught participants to handle Vegetation Indices (NDVI), Elevation models (DEM), and Land Cover classifications.

## 🛠️ Key Topics Covered
1.  **Data Acquisition:** Step-by-step guide to using [NASA AppEEARS](https://appeears.earthdatacloud.nasa.gov/) to define area samples and download environmental data.
2.  **Land Cover Analysis:** calculating distances to anthropogenic features (Urban areas).
3.  **Terrain Analysis:** Deriving Slope, Aspect, TPI, TRI, and Roughness from SRTM DEM.
4.  **Time-Series Integration:** Matching dynamic NDVI data (MOD13Q1) to animal GPS timestamps.

## 📚 Data & Citations
The wildebeest data used in this workshop is subsetted from Jared Stabach's field research. If you use this data or workflow, please refer to the original datasets:

* **Stabach JA, et al. (2022).** Increasing anthropogenic disturbance restricts wildebeest movement across East African grazing systems. *Frontiers in Ecology and Evolution*. [10.3389/fevo.2022.846171](https://doi.org/10.3389/fevo.2022.846171)
* **Stabach JA, et al. (2020).** Data from: Comparison of movement strategies of three populations of white-bearded wildebeest. *Movebank Data Repository*. [doi:10.5441/001/1.h0t27719](https://www.datarepository.movebank.org/handle/10255/move.1095)

## 📂 Repository Structure
* `Workshop_Tutorial.Rmd`: The source code for the lecture notes.
* `RemoteSensing.R`: Standalone R script for the analysis pipeline.
* `data/`: Contains shapefiles and raster inputs (Landcover, Elevation, NDVI).

---
*Created by Qianru Liao. Materials developed for the PKU Wildlife Tracking Workshop.*
