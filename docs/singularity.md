# Run Shiny-DNAmBenchmarking using Singularity
This guide shows how to launch the **Shiny DNAmBenchmarking** app with [Singularity/Apptainer](https://sylabs.io/singularity/), commonly used on HPC systems. It uses the Docker image `sofvdvel/rshiny-dnambenchmarking_amd:v1` and converts it to a local `.sif`.

## Prerequisites
- Singularity (or Apptainer) installed and available on your system
- Your benchmarking input CSV (or use the repo’s default at `results/benchmarking_dataset.csv`)


## Usage
### 1. Pull the image
Create a local SIF file from the Docker image:
```bash
singularity pull docker://sofvdvel/rshiny-dnambenchmarking_amd:v1
```
This produces: `rshiny-dnambenchmarking_amd_v1.sif`

### 2. Run the container
#### ✅ Option A: Use the repo's default dataset
Bind-mount the results/benchmarking_dataset.csv into the expected path:
```bash
singularity run \
  --bind "$(pwd)/results/benchmarking_dataset.csv":/home/app/results/benchmarking_dataset.csv \
  rshiny-dnambenchmarking_amd_v1.sif
```
#### ✅ Option B: Use your own dataset
Replace `my_custom_dataset.csv` with the path to your file:
```bash
singularity run \
  --bind "$(pwd)/my_custom_dataset.csv":/home/app/results/benchmarking_dataset.csv \
  rshiny-dnambenchmarking_amd_v1.sif
```

### 3. Open the app
After the container starts, open:
```arduino
http://localhost:3838
```
To use a different host port (e.g., 8888), start Shiny explicitly on that port:
```bash
singularity exec \
  --bind "$(pwd)/results/benchmarking_dataset.csv":/home/app/results/benchmarking_dataset.csv \
  rshiny-dnambenchmarking_amd_v1.sif \
  R -q -e "shiny::runApp('/home/app', host='0.0.0.0', port=8888)"
# → open http://localhost:8888
```