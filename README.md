# Phenotypic-Data-Pipeline
An automated R pipeline for the pre-analysis, validation, and visualization of multi-location phenotypic data. Includes automated data cleaning, outlier detection, and publication-ready GxE visualizatio

# Phenotypic Data Pre-Analysis Pipeline

This repository contains a suite of R scripts designed to clean, validate, and explore multi-location agricultural data before formal statistical modeling.

## 🛠️ Pipeline Components

### 1. Data Validation & Pre-Analysis (`Pre_Analysis.R`)
A comprehensive script that prepares raw Excel data for analysis.
* **Integrity Checks:** Automatically flags data issues such as negative values, constant columns, and missing data percentages.
* **Statistical Summaries:** Generates group counts and descriptive statistics for every location.

### 2. Multi-Location Comparative Analysis (`across_locations.R`)
* **Combined Datasets:** Merges and cleans data across different environmental sites (e.g., Pullman, Almota).
* **Introgression Analysis:** Evaluates the status of mutant (MT) vs. wild-type (WT) lines across specific genetic backgrounds.

### 3. Advanced Publication Visualization (`Plots_kb-hb.R`)
* **Enhanced Graphics:** Uses `ggtext` for HTML/Markdown styling in titles and `ggnewscale` for complex, multi-layered legends.
* **GxE Insights:** Visualizes the "Parent Status" (Recurrent vs. Donor) across locations to identify phenotypic patterns.

## 📦 Requirements
* `readxl`, `dplyr`, `tidyr`
* `ggplot2`, `ggtext`, `ggnewscale`
* `viridis`, `car`, `e1071`
