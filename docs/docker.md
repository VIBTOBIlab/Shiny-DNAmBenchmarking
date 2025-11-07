# Run Shiny-DNAmBenchmarking using Docker 
This guide shows how to launch the **Shiny DNAmBenchmarking** app using the prebuilt image: `sofvdvel/rshiny-dnambenchmarking_amd:v1`.

## Prerequisites
- Docker installed and running (macOS, Windows, or Linux)
- Your benchmarking input CSV (or use the repo’s default at `results/benchmarking_dataset.csv`)


## Usage
### 1. (Optional) Pull the image
Ensure you’re on the latest tagged image:
```bash
docker pull sofvdvel/rshiny-dnambenchmarking_amd:v1
```

### 2. Run the container
#### ✅ Option A: Use the repo's default dataset
Mount the CSV from this repository into the expected path inside the container:
```bash
docker run -p 3838:3838 \
  -v "$(pwd)/results/benchmarking_dataset.csv":/home/app/results/benchmarking_dataset.csv \
  sofvdvel/rshiny-dnambenchmarking_amd:v1
```

#### ✅ Option B: Use your own dataset
Replace `my_custom_dataset.csv`with the path to your file:
```bash
docker run -p 3838:3838 \
  -v "$(pwd)/my_custom_dataset.csv":/home/app/results/benchmarking_dataset.csv \
  sofvdvel/rshiny-dnambenchmarking_amd:v1
```

### 3. Open the app
After the container starts, open:
```arduino
http://localhost:3838
```
To use a different host port (e.g., 8888), start Shiny explicitly on that port:
```bash
docker run -p 8888:3838 \
  -v "$(pwd)/results/benchmarking_dataset.csv":/home/app/results/benchmarking_dataset.csv \
  sofvdvel/rshiny-dnambenchmarking_amd:v1
# -> open http://localhost:8888
```
