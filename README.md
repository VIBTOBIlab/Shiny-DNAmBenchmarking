# Shiny Application DNAmBenchmarking

This repository provides the codebase for a **[Shiny web application](https://sunny.cmb.ugent.be/3fy5CTR4gXjcKMHj0zz1bxsGEEkHVsOnC8BjXYT5miRB0QGwid/)** designed to interactively explore the data and results in the following scientific publication:

> **"[Full Paper Title]"**  
> [Authors]  
> *[Journal]*, 2025
> 
<br>

## Table of Contents

1. [Prerequisites](#prerequisites)  
2. [Installation Instructions](#installation-instructions)  
3. [Run the Application](#run-the-application)  
   - [Option 1: Command Line](#option-1-command-line)  
   - [Option 2: RStudio](#option-2-rstudio)  
4. [Support and Contact](#support-and-contact)

<br>

## Prerequisites
Ensure you have either [**Miniconda**](https://docs.conda.io/en/latest/) or [**Anaconda**](https://www.anaconda.com/) installed before proceeding.

<br>

## Installation Instructions

### 1. Clone the repository

```bash
git clone git@github.com:VIBTOBIlab/Shiny-DNAmBenchmarking.git
cd Shiny-DNAmBenchmarking/
```

### 2. Set up the Conda environment
Run the provided [conda.sh](./resources/conda.sh) script to configure the environment:
```bash
# If you are in the repo directory
sbatch ./resources/conda.sh
```
Alternatively, create the environment manually using the [rshiny-4.3.1.yaml](./resources/rshiny-4.3.1.yaml) file:
```bash
# If you are in the repo directory
conda env create -f ./resources/rshiny-4.3.1.yaml
conda activate rshiny-4.3.1
```

### 3. (One-time) Install GitHub version of `shinycssloaders`

This app uses the `withSpinner()` function from the `shinycssloaders` package with the `delay` parameter, which is **only available in development versions** (e.g., `v1.1.0.9005`) and may be missing from CRAN releases.

To ensure full functionality—especially in environments using R 4.3.1 or byte-compiled builds—you must install the latest GitHub version:

#### Installation steps
```bash
# Activate your Conda environment
conda activate rshiny-4.3.1
R 
# Inside R
> remotes::install_github("daattali/shinycssloaders")
```
This step only needs to be done once — unless you reinstall or reset your R environment.

If you are prompted with the following message:
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

#### Confirm installation
```bash
# Activate your Conda environment
conda activate rshiny-4.3.1
R
# Inside R
> packageVersion("shinycssloaders")
# Expected: ‘1.1.0.9005’ or later
```

<br>

## Run the Application
You can launch the app either from the command line or via RStudio:

### Option 1: Command Line
To launch the app in a background `screen` session:
#### 1. Start a new screen session 
```bash
screen -S shiny
```
#### 2. Launch the app 
```bash
conda activate rshiny-4.3.1
cd <path-to-Shiny-DNAmBenchmarking-folder>              # e.g. cd /kyukon/scratch/gent/vo/002/gvo00224/TOBI/Projects/Shiny-DNAmBenchmarking/
R --no-save -e "shiny::runApp(appDir = '.', host = '127.0.0.1', port = 8888)"
```
* To detach from the screen: Press Ctrl+A then D.
* To resume later: screen -r shiny

**Parameters to configure:**
* *appDir*: Set to '.' (the current directory), assuming you cd into the app folder beforehand. Alternatively, you can use the absolute path of folder containing app.R .
* *host*: Use '0.0.0.0' to allow external access, or a specific IP (e.g., '10.32.8.17').
* *port*: Choose an available port (e.g., 8888, 2222, etc.).
  
#### 3. Access the app in your browser
Once the app is running, access the app from your browser at: 
```php 
python -m webbrowser http://127.0.0.1:8888
```

<br>

### Option 2: RStudio
Ideal for development and interactive debugging.

#### 1. Open the Repository in RStudio
* Open the `Shiny-DNAmBenchmarking` folder as a project or set it as your working directory in RStudio.
* Make sure the file app.R is visible in the editor.

#### 2. Configure R to Use Conda Libraries via .Rprofile
To ensure R loads the correct packages, create or edit a `.Rprofile` file in the app directory:
```r
.libPaths(<path-to-conda-Rlibraries>)       # e.g. .libPaths("/kyukon/scratch/gent/vo/002/gvo00224/TOBI/Projects/Shiny-DNAmBenchmarking/.conda/envs/rshiny-4.3.1/lib/R/library")
```
Replace with your actual Conda R library path (check with .libPaths() inside an R session).

#### 3. Run the App
* Click the "Run App" button in RStudio (top-right corner of the script editor)
* Run the following in the R console:
```r
setwd(<path-to-Shiny-DNAmBenchmarking-folder> )     # e.g. setwd("/kyukon/scratch/gent/vo/002/gvo00224/TOBI/Projects/Shiny-DNAmBenchmarking/")
source(".Rprofile")
shiny::runApp()
```
This will launch the app in your default web browser.

> **Why `source(".Rprofile")`?**  
> RStudio does not automatically use the Conda environment.  
> Sourcing `.Rprofile` ensures that R loads the correct packages from the Conda environment.
> 
<br>

## Support and Contact
This Shiny application was developed and maintained by [Sofie Van de Velde](https://github.com/sofvdvel).

For questions, issues, or feature requests, please open an issue on the GitHub repository or contact the maintainer directly.s
