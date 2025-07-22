#!/bin/bash
#SBATCH --job-name=shiny_app
#SBATCH --output=shiny_app_%j.out
#SBATCH --error=shiny_app_%j.err
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --time=01:00:00
#SBATCH --mem=8G
#SBATCH --workdir=/kyukon/scratch/gent/vo/002/gvo00224/TOBI/Projects/Shiny-DNAmBenchmarking

# Load required modules if needed
module purge
module load Anaconda3/2024.06-1


# Activate conda environment
source ~/.bashrc
conda activate rshiny-4.3.1

# Run the Shiny app
R --no-save -e "shiny::runApp(appDir = '.', host = '0.0.0.0', port = 8888)"
