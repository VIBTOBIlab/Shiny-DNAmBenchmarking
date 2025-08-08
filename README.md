# Shiny Application DNAmBenchmarking
[![run with docker](https://img.shields.io/badge/run%20with-docker-0db7ed?labelColor=000000&logo=docker)](https://www.docker.com/)
[![Run with Conda](http://img.shields.io/badge/run%20with-conda-3EB049?labelColor=000000&logo=anaconda)](https://docs.conda.io/en/latest/)
[![run with Singularity](https://img.shields.io/badge/run%20with-singularity-1d355c.svg?labelColor=000000)](https://sylabs.io/docs/)

## Introduction 
This **[Shiny application](https://sunny.cmb.ugent.be/3fy5CTR4gXjcKMHj0zz1bxsGEEkHVsOnC8BjXYT5miRB0QGwid/)** allows users to visualize and analyze the results of the benchmarking study, providing an intuitive interface for exploring the data.

> **Full Name of Paper**
> 
> Edoardo Giuili, Maísa R Ferro dos Santos, Sofie Van de Velde, ... Sam Kint, Celine Everaert, Katleen De Preter. 
> 
> _Nat Biotechnol._ 2020 Feb 13. doi: [10.1038/s41587-020-0439-x](https://dx.doi.org/10.1038/s41587-020-0439-x).


## Usage
### Input File
The app requires an input CSV file with the following structure. The default file `results/benchmarking_dataset.csv` is included in the repository, but you can provide your own.

Make sure your file uses **comma delimiters** and contains the following columns:


| Column Name         | Description                                                              |
|---------------------|---------------------------------------------------------------------------|
| `sample`             | Sample identifier   |
| `predicted_tf`      | Predicted transcription factor (TF) for the sample                        |
| `deconv_tool`       | Deconvolution tool used                     |
| `unknown`           | Unknown fraction                                         |
|`dmr_tool`          | DMR method used                            |
| `seq_depth`         | Sequencing depth                                   |
| `tumor_type`        | Tumor type|
| `collapse_approach` | Method of collapsing                                                 |
| `mixture_type`      | Type of mixture                                     |
| `seq_method`        | Sequencing method                                |
| `expected_tf`       | Expected transcription factor (TF) for the sample                        |

📎 Tip: You can inspect the structure of the default [results/benchmarking_dataset.csv](resources/benchmarking_dataset.csv) for reference.

### Run the App
For more details on how to install and run Shiny application via Conda and Docker, please refer to the [documentation](./docs).

#### ▶️ Docker
```bash
docker run -p 3838:3838 \
  -v $(pwd)/results/benchmarking_dataset.csv:results/benchmarking_dataset.csv \
  sofvdvel/rshiny-dnambenchmarking_amd:v1
```
#### ▶️ Conda
```bash
conda activate rshiny-4.4.3
R --no-save -e "shiny::runApp(appDir = getwd(), host = '0.0.0.0', port = 3838)"
```

## Credits
This Shiny-DNAmBenchmarking was developed and maintained by [Sofie Van de Velde](https://github.com/sofvdvel).

## Support
If you encounter any issues or have questions, please:
* [Open an issue on GitHub](https://github.com/VIBTOBIlab/Shiny-DNAmBenchmarking/issues)
* Or contact the maintainer directly. 

