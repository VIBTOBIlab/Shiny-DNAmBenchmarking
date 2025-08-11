# Using Singularity to Run Shiny-DNAmBenchmarking

This guide explains how to run the **[Shiny web application](https://sunny.cmb.ugent.be/3fy5CTR4gXjcKMHj0zz1bxsGEEkHVsOnC8BjXYT5miRB0QGwid/)** application using [Singularity](https://sylabs.io/singularity/), which is commonly used in HPC environments as an alternative to Docker.


## Quick Start

### 1. Input File
The app requires an input CSV file with the following structure. The default file `results/benchmarking_dataset.csv` is included in the repository, but you can provide your own.

Make sure your file uses **comma delimiters** and contains the following columns:


| Column Name         | Description                                                              |
|---------------------|---------------------------------------------------------------------------|
| `sample`             | Sample identifier   |
| `predicted_tf`      | Predicted transcription factor (TF) for the sample                        |
| `deconv_tool`       | Deconvolution tool used                     |
| `unknown`           | Unknown fraction                                          |
|`dmr_tool`          | DMR method used                            |
| `seq_depth`         | Sequencing depth                                   |
| `tumor_type`        | Tumor type|
| `collapse_approach` | Method of collapsing                                                 |
| `mixture_type`      | Type of mixture                                     |
| `seq_method`        | Sequencing method                                |
| `expected_tf`       | Expected transcription factor (TF) for the sample                        |

📎 Tip: You can inspect the structure of the default [results/benchmarking_dataset.csv](results/benchmarking_dataset.csv) for reference.


### 2. Pull the Docker Image as a Singularity Image
You can pull the Docker image and convert it into a local `.sif` file using:
```bash
singularity pull docker://sofvdvel/rshiny-dnambenchmarking_amd:v1
```
This creates the image file: `rshiny-dnambenchmarking_amd_v1.sif`


### 3. Run the App
Assuming you are in the project root and your dataset is at results/benchmarking_dataset.csv: 
```bash
singularity run \
  --bind "$(pwd)/results":/home/app/results \
  rshiny-dnambenchmarking_amd_v1.sif
```

### 4. Accessing the Application
After running the container, you can access the Shiny application in your web browser at:
```arduino
http://localhost:3838
```
If you're running on a remote server, replace localhost with the server's IP address.


## Notes
* **No build required**: the image includes all dependencies and app code.
* **Dataset is not included:** You must mount your dataset at runtime using `-bind`.
* **Default port:** `3838` is used by Shiny. You can map it to a different host port if needed:
```bash
singularity exec \
  --env DATASET_PATH=/mnt/my_data.csv \
  --bind "$(pwd)/results":/home/app/results \
  rshiny-dnambenchmarking_amd_v1.sif \
  Rscript -e "shiny::runApp('/home/app', host = '0.0.0.0', port = 8888)"
```
