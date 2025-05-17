# VETSA1 MCI Pipeline Runner
# This script runs the complete MCI pipeline for VETSA Wave 1

#################################################################
# User Configuration Section - Modify as needed
#################################################################

# Working directory - location of the MCI pipeline code
working_dir <- "~/netshare/M/Projects/PracEffects_GEE"

# Directory containing the MCI pipeline scripts
mci_scripts_dir <- file.path(working_dir, "code/pe_gee_manuscript/03_MCI_pipeline")

# Input file - Practice effects and AFQT-adjusted cognitive data
input_cog_data <- "data/intermediate_data/MCI_01_V1V2V3V4_cog_data_pe-adjusted_afqt-adjusted.csv"
# input_cog_data <- "data/intermediate_data/MCI_01_V1V2V3V4_cog_data_raw_afqt-adjusted.csv"

# Admin file with demographic data
admin_file <- "~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/Admin/vetsa_admin_file_20250205.sas7bdat"

# Master data file for clinical exclusion criteria
merge_file <- "~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/VETSA 1 Aging/vetsa1merged_22dec2016_nomiss.sas7bdat"

# Brain cancer IDs file 
brain_cancer_ids_file <- "data/raw_data/brain_cancer_ids.txt"

# Final output file with date stamp
date_stamp <- format(Sys.Date(), "%Y-%m-%d")
output_file <- paste0("data/output_data/vetsa1_mci_adjusted_", date_stamp, ".csv")
# output_file <- paste0("data/output_data/vetsa1_mci_raw_", date_stamp, ".csv")

# Intermediate files (these will be overwritten if they exist)
pre_imputation_file <- "data/intermediate_data/MCI_02a01_vetsa1_MCI_PreImputation.csv"
post_imputation_file <- "data/intermediate_data/MCI_03a04_vetsa1_MCI_AllData.csv"

#################################################################
# Pipeline Execution - No need to modify below this point
#################################################################

# Load required libraries
library(dplyr)
library(readr)
library(haven)
library(mice)

# Set the working directory
setwd(working_dir)

# Add absolute paths for scripts and norms
# These will be used if normal path resolution fails
norm_dir <- "/home/jelman/netshare/M/Projects/PracEffects_GEE/code/pe_gee_manuscript/03_MCI_pipeline/norms"
script_dir <- "/home/jelman/netshare/M/Projects/PracEffects_GEE/code/pe_gee_manuscript/03_MCI_pipeline"

# Create folder for logs if it doesn't exist
dir.create(file.path(working_dir, "logs"), showWarnings = FALSE, recursive = TRUE)
log_file <- file.path(working_dir, "logs", paste0("vetsa1_pipeline_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
cat("Log file:", log_file, "\n")

# Helper function to locate and source a script with robust path handling
source_script <- function(script_name) {
  # Try different paths to find the script
  possible_paths <- c(
    file.path(script_dir, script_name),
    file.path(mci_scripts_dir, script_name),
    file.path("code/pe_gee_manuscript/03_MCI_pipeline", script_name),
    file.path("03_MCI_pipeline", script_name),
    script_name
  )
  
  cat("Looking for script:", script_name, "\n")
  for (path in possible_paths) {
    cat("  Trying path:", path, "\n")
    if (file.exists(path)) {
      cat("  Found! Sourcing from:", path, "\n")
      
      # Set environment variables for norms to use
      if(!exists("norm_dir_env", envir = .GlobalEnv)) {
        assign("norm_dir_env", norm_dir, envir = .GlobalEnv) 
        cat("  Set norm_dir_env to:", norm_dir, "\n")
      }
      
      source(path)
      return(TRUE)
    }
  }
  
  # If we still cannot find the script, try one last-ditch approach
  # Temporarily change directory to the script directory
  original_dir <- getwd()
  cat("  Last resort: Changing to script directory:", script_dir, "\n")
  tryCatch({
    setwd(script_dir)
    if (file.exists(script_name)) {
      cat("  Found script in script directory, sourcing...\n")
      source(script_name)
      return(TRUE)
    } else {
      stop("Could not find script: ", script_name, " in any expected location")
    }
  }, finally = {
    # Always return to the original directory
    setwd(original_dir)
    cat("  Returned to original directory\n")
  })
}

# Execute Step 1: Data Preparation
cat("VETSA1 Pipeline: Starting data preparation...\n")
source_script("MCI_02a_vetsa1_data_prep.R")
cat("VETSA1 Pipeline: Data preparation complete.\n")

# Execute Step 2: Create MCI diagnosis
cat("VETSA1 Pipeline: Creating MCI diagnosis...\n")
source_script("MCI_03a_vetsa1_create_mci_dx.R")
cat("VETSA1 Pipeline: MCI diagnosis creation complete.\n")

# Execute Step 3: Create final output file
cat("VETSA1 Pipeline: Creating final output file...\n")
source_script("MCI_04a_vetsa1_create_output_file.R")
cat("VETSA1 Pipeline: Final output file created at:", output_file, "\n")

cat("VETSA1 MCI pipeline completed successfully.\n")
