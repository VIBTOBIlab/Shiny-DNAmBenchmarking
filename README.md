# Shiny DNAmBenchmarking
[![run with docker](https://img.shields.io/badge/run%20with-docker-0db7ed?labelColor=000000&logo=docker)](https://www.docker.com/)
[![run with Singularity](https://img.shields.io/badge/run%20with-singularity-1d355c.svg?labelColor=000000)](https://sylabs.io/docs/)

## Overview
This [Shiny application](https://sunny.cmb.ugent.be/3fy5CTR4gXjcKMHj0zz1bxsGEEkHVsOnC8BjXYT5miRB0QGwid/) allows users to visualize and analyze the results of the benchmarking study, providing an intuitive interface for exploring the data.


## Background
**DecoNFlow** is a bioinformatics pipeline for computational deconvolution of DNA methylation data, supporting both reference-based and reference-free tools. Learn more: [VIBTOBIlab/DecoNFlow](https://github.com/VIBTOBIlab/DecoNFlow).


## Quick Start
### 1. Clone
```bash
git clone git@github.com:VIBTOBIlab/Shiny-DNAmBenchmarking.git
cd Shiny-DNAmBenchmarking/
```

### 2. Input format
By default het app loads [results/benchmarking_dataset.csv](results/benchmarking_dataset.csv). 
If you provide your own CSV, include these columns:

| Column Name         | Description |
|---------------------|----------------------------------|
| `sample`             | Sample identifier   |
| `predicted_tf`      | Predicted tumoral factor (TF) for the sample  |
| `deconv_tool`       | Deconvolution tool used |
| `tumor_type`        | Tumor type|
|`dmr_tool`          | DMR method used |
| `seq_depth`         | Sequencing depth |
| `mixture_type`      | Type of mixture |
| `seq_method`        | Sequencing method |
| `expected_tf`       | Expected tumoral factor (TF) for the sample  |

**Example (first rows):**
```csv 
sample,predicted_tf,deconv_tool,tumor_type,dmr_tool,seq_depth,seq_method,mixture_type,expected_tf
healthy_merged_BRCA_0.0001_310000000_rep1,0.025818,UXM,BRCA,wgbstools,310M,wgbs,in_silico,1.00E-04
healthy_merged_BRCA_0.0001_310000000_rep10,0.0057756,UXM,BRCA,wgbstools,310M,wgbs,in_silico,1.00E-04
healthy_merged_BRCA_0.0001_310000000_rep2,0.0138815,UXM,BRCA,wgbstools,310M,wgbs,in_silico,1.00E-04
```
Place your file at `results/benchmarking_dataset.csv` or bind-mount it as shown below.

### 3. Run the app
For more information, please refer to the [docs/](./docs).

#### ▶️ Docker
```bash
docker run -p 3838:3838 \
  -v "$(pwd)/results/benchmarking_dataset.csv":/home/app/results/benchmarking_dataset.csv \
  sofvdvel/rshiny-dnambenchmarking:v1
```

#### ▶️ Singularity
```bash
singularity pull docker://sofvdvel/rshiny-dnambenchmarking:v1
singularity run \
  --bind "$(pwd)/results/benchmarking_dataset.csv":/home/app/results/benchmarking_dataset.csv \
  rshiny-dnambenchmarking_v1.sif
```

### 4. Open the app
After the container starts, open: [http://localhost:3838](http://localhost:3838). 

## Credits
The scripts and containers have been written and built by [Sofie Van de Velde](https://github.com/sofvdvel), who is also the maintainer.

## Citation
> **A benchmark of DNA methylation deconvolution methods for tumoral fraction estimation using DecoNFlow**
>
> Edoardo Giuili, Sofie Van de Velde, Sam Kint, Maísa R Ferro dos Santos, Lotte Cornelli, Sofie Roelandt, Kathleen Schoofs, Renske Imschoot, Ruben Van Paemel, Leander Meuris, Celine Everaert, Katleen De Preter.
>
> bioRxiv 2025 Nov 27. doi: [10.1101/2025.11.27.688590](https://www.biorxiv.org/content/10.1101/2025.11.27.688590v1)


## Support
If you encounter any issues or have questions, please:
* [Open an issue](https://github.com/VIBTOBIlab/Shiny-DNAmBenchmarking/issues)
* Or contact the maintainer directly

