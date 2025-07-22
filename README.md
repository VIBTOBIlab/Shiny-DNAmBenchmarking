# Shiny Application DNAmBenchmarking

This repository provides the codebase  for a **[Shiny web application](https://sunny.cmb.ugent.be/3fy5CTR4gXjcKMHj0zz1bxsGEEkHVsOnC8BjXYT5miRB0QGwid/)** designed to interactively explore the data and results in the following scientific publication:

> **"[Full Paper Title]"**  
> [Authors]  
> *[Journal]*, 2025


<br>

## Prerequisites
Ensure you have either [**Miniconda**](https://docs.conda.io/en/latest/) or **Anaconda** installed before proceeding.

<br>

## Installation Instructions

### 1. Clone the repository

```bash
git clone git@github.com:VIBTOBIlab/Shiny-DNAmBenchmarking.git
cd Shiny-DNAmBenchmarking
```

### 2. Set up the Conda environment
Run the provided [conda.sh](./resources/conda.sh) script to configure the environment:
```bash
# If you are in the repo directory
sbatch ./resources/conda.sh
```

Alternatively (for manual setup of [rshiny-4.3.1.yaml](./resources/rshiny-4.3.1.yaml)):
```bash
# If you are in the repo directory
conda env create -f ./resources/rshiny-4.3.1.yaml
conda activate rshiny-4.3.1
```

### 3. (One-time step) Install required version of `shinycssloaders` from GitHub
This app uses the `withSpinner()` function from the `shinycssloaders` package with the `delay` parameter — which controls how long to wait before showing a spinner.  
**However, the CRAN version of the package may not include this argument**, depending on your R version or environment.

To fix this, you must manually install the GitHub version of the package from [daattali/shinycssloaders](https://github.com/daattali/shinycssloaders), which includes full support for the `delay` parameter.

> ℹ️ **Why this is necessary:**  
> Even though `shinycssloaders` version 1.1.0 is available on CRAN, some environments (especially R 4.3.1 or byte-compiled deployments) may not reflect the latest GitHub changes.  
> The `delay` argument is only available in development builds like `v1.1.0.9005`.

#### Installation steps:
```bash
# Activate your Conda environment
conda activate rshiny-4.3.1
# Start R
R 
> remotes::install_github("daattali/shinycssloaders")
```
This step only needs to be done once — unless you reinstall or reset your R environment.


#### Check installation went right:
```bash
# Activate your Conda environment
conda activate rshiny-4.3.1
# Start R
R 
> packageVersion("shinycssloaders")
```


<br>

## Option 1: Run via Command Line
To launch the application interactively in a detached terminal session using screen:
### 1. Start a new screen session: 
```bash
screen -S shiny
```
### 2. Launch the app inside the screen session:
```bash
conda activate rshiny-4.3.1
cd <path-to-Shiny-DNAmBenchmarking-folder>              # e.g. /kyukon/scratch/gent/vo/002/gvo00224/TOBI/Projects/Shiny-DNAmBenchmarking/
R --no-save -e "shiny::runApp(appDir = '.', host = '127.0.0.1', port = 8888)"
```
* To detach from the screen: Press Ctrl+A then D.
* To resume later: screen -r shiny

**Parameters to configure:**
* *appDir*: Set to '.' (the current directory), assuming you cd into the app folder beforehand. Alternatively, you can use the absolute path of folder containing app.R .
* *host*: Use '0.0.0.0' to allow external access, or a specific IP (e.g., '10.32.8.17').
* *port*: Choose an available port (e.g., 8888, 2222, etc.).
  
### 3. Access the app in your browser
Once the app is running, access the app from your browser at: 
```php 
python -m webbrowser http://127.0.0.1:8888
```

<br>

## Option 2: Run in RStudio
If you're working with RStudio, you can launch the app interactively without the terminal:
### 1. Open RStudio
Make sure RStudio is launched from a terminal or command line session where the rshiny-4.3.1 Conda environment is activated:
``` bash
activate rshiny-4.3.1
rstudio &
```
Alternatively, configure RStudio to use the R binary from your Conda environment (rshiny-4.3.1).

### 2. Open the Repository in RStudio
Open the **Shiny-DNAmBenchmarking** folder in RStudio as a project or working directory.

### 3. Run the App
Open **app.R** and click the "Run App" button in the upper-right corner of the RStudio script editor.
You can also run this command in the R console:

```r
shiny::runApp()
```
This will launch the app in your default web browser.


<br>


## Support and Contact
This Shiny application was developed and maintained by [Sofie Van de Velde](https://github.com/sofvdvel).

For questions, issues, or feature requests, please open an issue on the GitHub repository or contact the maintainer directly.


