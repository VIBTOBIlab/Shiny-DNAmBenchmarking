# Shiny Application DNAmBenchmarking

This repository provides the codebase  for a **[Shiny web application](https://sunny.cmb.ugent.be/3fy5CTR4gXjcKMHj0zz1bxsGEEkHVsOnC8BjXYT5miRB0QGwid/)** designed to interactively explore the data and results in the following scientific publication:

> **"[Full Paper Title]"**  
> [Authors]  
> *[Journal]*, 2025



## Prerequisites
Ensure you have either [**Miniconda**](https://docs.conda.io/en/latest/) or **Anaconda** installed before proceeding.


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

## Running the Shiny Application
To launch the application interactively in a detached terminal session using screen:
```bash
screen -S shiny
```
Then inside the screen session:
```bash
conda activate rshiny-4.3.1
R --no-save -e "shiny::runApp(
appDir = '/mnt/c/Users/Sofie/OneDrive - UGent/Documents/Projects/DecoNFlow_Benchmarking', 
host = '127.0.0.1', 
port = 8888)"
```

**Parameters to configure:**
* *appDir*: Absolute path to the folder containing the Shiny app (app.R, server.R, ui.R, etc.).
* *host*: Use '0.0.0.0' to allow external access, or a specific IP (e.g., '10.32.8.17').
* *port*: Choose an available port (e.g., 8888, 2222, etc.).

Once the app is running, access the app from your browser at: 
```php 
http://<host>:<port>
```


## Support and Contact
This Shiny application was developed and maintained by [Sofie Van de Velde](https://github.com/sofvdvel).

For questions, issues, or feature requests, please open an issue on the GitHub repository or contact the maintainer directly.


