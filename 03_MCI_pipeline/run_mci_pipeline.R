# Master MCI Pipeline Runner
# This script runs the complete MCI pipeline for all VETSA Waves

# Set working directory
working_dir <- "~/netshare/M/Projects/PracEffects_GEE"
setwd(working_dir)

# Source individual pipeline runners
cat("==========================================\n")
cat("Running VETSA1 MCI Pipeline...\n")
cat("==========================================\n")
source("code/pe_gee_manuscript/03_MCI_pipeline/run_vetsa1_mci_pipeline.R")  # Source from MCI_pipeline subfolder

cat("\n\n==========================================\n")
cat("Running VETSA2 MCI Pipeline...\n")
cat("==========================================\n")
source("code/pe_gee_manuscript/03_MCI_pipeline/run_vetsa2_mci_pipeline.R")  # Source from MCI_pipeline subfolder

cat("\n\n==========================================\n")
cat("Running VETSA3 MCI Pipeline...\n")
cat("==========================================\n")
source("code/pe_gee_manuscript/03_MCI_pipeline/run_vetsa3_mci_pipeline.R")  # Source from MCI_pipeline subfolder

cat("\n\n==========================================\n")
cat("Running VETSA4 MCI Pipeline...\n")
cat("==========================================\n")
source("code/pe_gee_manuscript/03_MCI_pipeline/run_vetsa4_mci_pipeline.R")  # Source from MCI_pipeline subfolder

cat("\n\n==========================================\n")
cat("All MCI Pipeline Processes Completed!\n")
cat("==========================================\n")
