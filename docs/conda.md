# Using conda to Install and Run Shiny-DNAmBenchmarking <!-- omit in toc -->

This guide explains how to install and run the **[Shiny web application](https://sunny.cmb.ugent.be/3fy5CTR4gXjcKMHj0zz1bxsGEEkHVsOnC8BjXYT5miRB0QGwid/)** using a Conda environment.

### Table of Contents <!-- omit in toc -->
- [Quick Start](#quick-start)
  - [1. Clone the repository](#1-clone-the-repository)
  - [2. Create and activate the Conda environment](#2-create-and-activate-the-conda-environment)
  - [3. Install required R packages (one-time setup)](#3-install-required-r-packages-one-time-setup)
  - [4. Input File](#4-input-file)
  - [5. Running the App](#5-running-the-app)
- [Notes](#notes)

## Quick Start

### 1. Clone the repository

```bash
git clone git@github.com:VIBTOBIlab/Shiny-DNAmBenchmarking.git
cd Shiny-DNAmBenchmarking/
```

### 2. Create and activate the Conda environment
Use the provided environment YAML file:
```bash
# If you are in the repo directory
conda env create -f ./resources/rshiny-4.4.3.yaml
conda activate rshiny-4.4.3
```

### 3. Install required R packages (one-time setup)
The app requires two R packages that are not reliably available via CRAN or require specific versions for compatibility with Conda environments:

####  a. `shinycssloaders`  <!-- omit in toc -->

This app uses the `withSpinner()` function with the `delay` parameter from the `shinycssloaders` package,
which is only available in development versions (e.g., `v1.1.0.9005`). Install it from GitHub:

```bash
# Activate your Conda environment
conda activate rshiny-4.4.3
R 
# Inside R
> remotes::install_github("daattali/shinycssloaders")
```

If prompted to update packages:
```sql 
These packages have more recent versions available.
It is recommended to update all of them.
Which would you like to update?
1: All                               
2: CRAN packages only                
3: None                              
...
Enter one or more numbers, or an empty line to skip updates:
```
Choose option `3: None` to avoid altering other package versions that may be required for compatibility.

Verify installation
```bash
# Activate your Conda environment
conda activate rshiny-4.4.3
R
# Inside R
> packageVersion("shinycssloaders") # Expected: ‘1.1.0.9005’ or later
```

####  b. `r-philentropy` <!-- omit in toc -->

```bash
# Activate your Conda environment
conda activate rshiny-4.4.3
R 
# Inside R
> remotes::install_github('drostlab/philentropy') 
```
>⚠️ These installation steps only need to be performed once — unless you reinstall or reset your R environment.

### 4. Input File
The app requires an input CSV file with the following structure. The default file `results/benchmarking_dataset.csv` is included in the repository, but you can provide your own.

Make sure your file uses **comma delimiters** and contains the following columns:


| Column Name         | Description                                                              |
|---------------------|---------------------------------------------------------------------------|
| `sample`             | Sample identifier   |
| `predicted_tf`      | Predicted tumoral factor (TF) for the sample                        |
| `deconv_tool`       | Deconvolution tool used                     |
| `unknown`           | Unknown fraction                                          |
|`dmr_tool`          | DMR method used                            |
| `seq_depth`         | Sequencing depth                                   |
| `tumor_type`        | Tumor type|
| `collapse_approach` | Method of collapsing                                                  |
| `mixture_type`      | Type of mixture                                     |
| `seq_method`        | Sequencing method                                |
| `expected_tf`       | Expected tumoral factor (TF) for the sample                        |

📎 Tip: You can inspect the structure of the default [results/benchmarking_dataset.csv](results/benchmarking_dataset.csv) for reference.


### 5. Running the App
You can run the app via command line or RStudio:

#### Option 1: Command Line <!-- omit in toc -->

To launch the app:
```bash
conda activate rshiny-4.4.3
R --no-save -e "shiny::runApp(appDir = '/path/to/app/folder/', host = '0.0.0.0', port = 3838)"
```
**Parameters to configure:**
* *appDir*: Use the absolute path of folder containing app.R .
* *host*: Use '0.0.0.0' to allow external access, or a specific IP (e.g., '10.32.8.17').
* *port*: Choose an available port (e.g., 8888, 2222, etc.).
  
Once the app is running, access the app from your browser at: 
```php 
http://localhost:3838
```

#### Option 2: RStudio (for development) <!-- omit in toc -->

Ideal for development and interactive debugging.
1. Open the repo in RStudio.
2. Set R to use Conda libraries:
```r
.libPaths("/path/to/conda/envs/rshiny-4.4.3/lib/R/library")
```
3. Run the app
```r
setwd("/absolute/path/to/folder/")
shiny::runApp()
```

## Notes
* **No need to install R separately**: the Conda environment handles this.
* **Input file must be present** at results/benchmarking_dataset.csv or a custom path hardcoded into your global.R.
* **Default port:** `3838` is used by Shiny. You can map it to a different host port if needed.
