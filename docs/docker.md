# Using Docker to Run Shiny-DNAmBenchmarking

This guide explains how to run the **[Shiny web application](https://sunny.cmb.ugent.be/3fy5CTR4gXjcKMHj0zz1bxsGEEkHVsOnC8BjXYT5miRB0QGwid/)** application using the pre-built Docker image:  `sofvdvel/rshiny-dnambenchmarking_amd:v1`


## Quick Start

### 1. Input File
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
| `collapse_approach` | Method of collapsing                                                 |
| `mixture_type`      | Type of mixture                                     |
| `seq_method`        | Sequencing method                                |
| `expected_tf`       | Expected tumoral factor (TF) for the sample                        |

📎 Tip: You can inspect the structure of the default [results/benchmarking_dataset.csv](results/benchmarking_dataset.csv) for reference.


### 2. Pull the Docker image (optional)

This step ensures you're using the latest version of the image:

```bash
docker pull sofvdvel/rshiny-dnambenchmarking_amd:v1
```

### 3. Run the container
#### ✅ Option 1: Use default dataset from the cloned GitHub repo
If you’ve cloned this repository and want to use the default results/benchmarking_dataset.csv file:
```bash
docker run -p 3838:3838 \
  -v "$(pwd)/results/benchmarking_dataset.csv":/home/app/results/benchmarking_dataset.csv \
  sofvdvel/rshiny-dnambenchmarking_amd:v1
```

#### ✅ Option 2: Use a custom dataset
If you have a custom dataset you want to use, replace `my_custom.csv` with the path to your dataset:
```bash
docker run -p 3838:3838 \
  -v "$(pwd)/my_custom.csv":/home/app/results/benchmarking_dataset.csv \
  sofvdvel/rshiny-dnambenchmarking_amd:v1
```
This mounts your own file directly into the expected location.

### Accessing the Application
After running the container, you can access the Shiny application in your web browser at:
```arduino
http://localhost:3838
```
If you're running on a remote server, replace localhost with the server's IP address.


## Notes
* **No build required**: the image includes all dependencies and app code.
* **Dataset is not included:** You must mount your dataset at runtime using `-v`.
* **Default port:** `3838` is used by Shiny. You can map it to a different host port if needed:
```bash
docker run -p 8888:3838 \
  -v "$(pwd)/my_custom.csv":/home/app/results/benchmarking_dataset.csv \
  sofvdvel/rshiny-dnambenchmarking_amd:v1
```
