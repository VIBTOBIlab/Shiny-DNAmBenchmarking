#!/bin/bash
#SBATCH --job-name=create_rshiny_env
#SBATCH --output=logs/conda_create_%j.out
#SBATCH --error=logs/conda_create_%j.err
#SBATCH --mem=32G
#SBATCH --time=02:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=4


set -e  # Exit on error
set -u  # Treat unset variables as errors


# Check if environment already exists
if conda env list | grep -q "rshiny-4.3.1"; then
  echo "Environment 'rshiny-4.3.1' already exists. Skipping creation."
  exit 0
fi


# Optional: Load Anaconda module (UGent HPC-specific)
module purge
module load Anaconda3/2024.06-1

# Activate conda base environment
source "$(conda info --base)/etc/profile.d/conda.sh"

echo "==============================="
echo "Starting Conda environment creation"
echo "Start time: $(date)"
echo "Working directory: $(pwd)"
echo "Environment file: rshiny-4.3.1.yaml"
echo "==============================="

mkdir -p logs

# Optional: Print memory info for logging/debugging
echo "System memory snapshot:"
free -h || vmstat -s

# Create the conda environment
conda env create -f rshiny-4.3.1.yaml

echo "==============================="
echo "Environment successfully created!"
echo "End time: $(date)"
echo "==============================="