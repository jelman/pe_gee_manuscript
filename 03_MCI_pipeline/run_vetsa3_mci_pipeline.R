# VETSA3 MCI Pipeline Runner
# This script runs the complete MCI pipeline for VETSA Wave 3

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
merge_file <- "~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/VETSA 3 interim datasets/vetsa3_master_db_20200618.sas7bdat"

# Brain cancer IDs file 
brain_cancer_ids_file <- "data/raw_data/brain_cancer_ids.txt"

# Final output file with date stamp
date_stamp <- format(Sys.Date(), "%Y-%m-%d")
output_file <- paste0("data/output_data/vetsa3_mci_adjusted_", date_stamp, ".csv")
# output_file <- paste0("data/output_data/vetsa3_mci_raw_", date_stamp, ".csv")

# MRI additional exclusion file (for waves 3 and 4)
mri_excludes_file <- "~/netshare/M/VETSA DATA FILES_852014/MRI/MRI_Admin/MRI_addtl_excludes.csv"

# Intermediate files (these will be overwritten if they exist)
pre_imputation_file <- "data/intermediate_data/MCI_02c01_vetsa3_MCI_PreImputation.csv"
post_imputation_file <- "data/intermediate_data/MCI_03c04_vetsa3_MCI_AllData.csv"

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

# Execute Step 1: Data Preparation
cat("VETSA3 Pipeline: Starting data preparation...\n")
source("code/pe_gee_manuscript/03_MCI_pipeline/MCI_02c_vetsa3_data_prep.R")
cat("VETSA3 Pipeline: Data preparation complete.\n")

# Execute Step 2: Create MCI diagnosis
cat("VETSA3 Pipeline: Creating MCI diagnosis...\n")
source("code/pe_gee_manuscript/03_MCI_pipeline/MCI_03c_vetsa3_create_mci_dx.R")
cat("VETSA3 Pipeline: MCI diagnosis creation complete.\n")

# Execute Step 3: Create final output file
cat("VETSA3 Pipeline: Creating final output file...\n")
source("code/pe_gee_manuscript/03_MCI_pipeline/MCI_04c_vetsa3_create_output_file.R")
cat("VETSA3 Pipeline: Final output file created at:", output_file, "\n")

cat("VETSA3 MCI pipeline completed successfully.\n")
