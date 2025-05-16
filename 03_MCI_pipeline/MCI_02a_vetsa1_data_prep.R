#############################################################################
# MCI_02a_vetsa1_Data_prep.R
#
# Overview: This script processes VETSA wave 1 cognitive data for MCI diagnosis.
# It loads the practice effect and AFQT-adjusted cognitive scores, merges with 
# demographic data, converts raw scores to scaled/standardized scores using 
# normative data, and computes missing data metrics across cognitive domains.
# Note: Wave 1 includes Spatial Span but not Boston Naming Test.
#
# Input: AFQT-adjusted cognitive data, admin data with age and education
# Output: Prepared dataset for MCI classification with normalized scores
#############################################################################

# MCI_02a_vetsa1_Data_prep.R
# Processing vetsa1 cognitive variables for MCI status generation

library(dplyr)
library(readr)
library(haven)

# Set working directory
setwd("~/netshare/M/Projects/PracEffects_GEE")

# Source the R versions of the norming scripts
source("code/pe_gee_manuscript/03_MCI_pipeline/norms/MCI_apply_all_norms.R")

# Set file paths
cog_data_file <- "data/intermediate_data/MCI_01_V1V2V3V4_cog_data_pe-adjusted_afqt-adjusted.csv"
output_file <- "data/intermediate_data/MCI_02a01_vetsa1_MCI_PreImputation.csv"
admin <- "~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/Admin/vetsa_admin_file_20250205.sas7bdat"

# Read in the practice effects data
V1V2V3V4_CogData_PE <- read_csv(cog_data_file)

# Read in admin file to get age at wave 1
admin_data <- read_sas(admin, NULL)

# Convert all variable names to uppercase
names(V1V2V3V4_CogData_PE) <- toupper(names(V1V2V3V4_CogData_PE))
names(admin_data) <- toupper(names(admin_data))

# Select only relevant columns from admin data
admin_data <- admin_data %>%
  select(VETSAID, AGE_V1, TEDALL) %>%
  mutate(AGE_V1 = as.numeric(AGE_V1),
         TEDALL = as.numeric(TEDALL))

# Merge with admin data to get age at wave 1
V1V2V3V4_CogData_PE <- V1V2V3V4_CogData_PE %>%
  left_join(admin_data, by = "VETSAID")

# Filter for VETSA1 participants based on non-missing age
V1V2V3V4_CogData_PE <- V1V2V3V4_CogData_PE %>%
  filter(!is.na(AGE_V1)) 

# Convert all variable names to uppercase (again, just to be safe)
names(V1V2V3V4_CogData_PE) <- toupper(names(V1V2V3V4_CogData_PE))

# Convert trails measures from logarithm scale to raw scale
V1V2V3V4_CogData_PE <- V1V2V3V4_CogData_PE %>%
  mutate(
    TRL1T_V1P = exp(TRL1TLOG_V1P),
    TRL2T_V1P = exp(TRL2TLOG_V1P),
    TRL3T_V1P = exp(TRL3TLOG_V1P),
    TRL4T_V1P = exp(TRL4TLOG_V1P),
    TRL5T_V1P = exp(TRL5TLOG_V1P)
  )

# # Calculate age and education predicted stroop scores
# V1V2V3V4_CogData_PE <- V1V2V3V4_CogData_PE %>%
#   mutate(
#     # Predicted word score
#     STRWPREDEXT_V1 = 80.305 + (1.971 * TEDALL) - (0.105 * AGE_V1),
#     STRWPRED_V1 = round(STRWPREDEXT_V1, 1),
    
#     # Predicted color score
#     STRCPREDEXT_V1 = 68.810 + (1.026 * TEDALL) - (0.1434 * AGE_V1),
#     STRCPRED_V1 = round(STRCPREDEXT_V1, 1),
    
#     # Predicted color-word score
#     STRCWPREDEXT_V1 = 32.3655 + (1.351 * TEDALL) - (0.231 * AGE_V1),
#     STRCWPRED_V1 = round(STRCWPREDEXT_V1, 1)
#   )

# Select only VETSA1 variables
wave_specific_df <- V1V2V3V4_CogData_PE %>%
  select(
    VETSAID, AGE_V1, TEDALL,
    # Cognitive test scores with _V1 or _V1P suffix
    contains("_V1"), contains("_V1P")
  )

# Create wave-agnostic data for norming
wave_specific_df <- wave_specific_df %>%
  # Add AGE variable for the norming functions
  mutate(AGE = AGE_V1)

# Create a copy with wave suffix removed from variable names
prenorm_df <- wave_specific_df

# Get names of all columns with "_V1" or "_V1P" suffix
v1_cols <- names(prenorm_df)[grepl("_V1P?$", names(prenorm_df))]

# Create new variable names without the wave suffix
new_names <- sub("_V1P?$", "", v1_cols)

# Create a named vector for renaming
names_map <- setNames(v1_cols, new_names)

# Add wave-agnostic variables to the dataframe
for (wave_agnostic_name in new_names) {
  wave_specific_name <- names_map[wave_agnostic_name]
  prenorm_df[[wave_agnostic_name]] <- prenorm_df[[wave_specific_name]]
}

# Apply all normative transformations (will process wave-agnostic variable names)
# Pass wave=1 to indicate this is VETSA wave 1 (has Spatial Span but no Boston Naming)
normed_df <- apply_all_norms(prenorm_df, wave=1)

# Create a new data frame for the results
result_df <- normed_df

# Copy the wave-agnostic scaled scores back to wave-specific variables
# Note: Wave 1 has Spatial Span but not Boston Naming Test
norm_scores <- c("MTXT", "CVATOTSS", "CVLDFSS", "LM1ASS", "LM1BSS", "LM2ASS", "LM2BSS", 
                "DSPSS", "LNSC", "TRL1TSC", "TRL2TSC", "TRL3TSC", "TRL4TSC", "TRL5TSC", 
                "SSPSS", "STRWT", "STRCT", "STRIT", "HFTOTCOR", "MR1COR",
                "LFCORSC", "CFCORSC", "CSCORSC", 
                "CSSACCSC", "VRITOTSS", "VRDTOTSS", "VRCTOTSS")

# Copy norm scores to wave-specific variables
for (score in norm_scores) {
  wave_specific_score <- paste0(score, "_V1")
  if (score %in% names(normed_df)) {
    result_df[[wave_specific_score]] <- normed_df[[score]]
  }
}

# Add missing data counters
# Note: Wave 1 has no Boston Naming Test so it's excluded from verbal domain
result_df <- result_df %>%
  mutate(
    # Memory domain missing data count
    MEMORYMISSING = rowSums(is.na(result_df[, c("CVATOTSS_V1", "CVLDFSS_V1", 
                                               "LM1ASS_V1", "LM1BSS_V1", 
                                               "LM2ASS_V1", "LM2BSS_V1", 
                                               "VRITOTSS_V1", "VRDTOTSS_V1")])),
    
    # Executive function domain missing data count
    EXECMISSING = rowSums(is.na(result_df[, c("TRL4TSC_V1", "CSSACCSC_V1", 
                                            "STRIT_V1", "MTXT_V1")])),
    
    # Attention/Working Memory domain missing data count (includes Spatial Span)
    ATTNMISSING = rowSums(is.na(result_df[, c("TRL1TSC_V1", "DSPSS_V1", 
                                             "LNSC_V1", "SSPSS_V1")])),
    
    # Verbal domain missing data count (no Boston Naming Test in wave 1)
    VERBALMISSING = rowSums(is.na(result_df[, c("LFCORSC_V1", "CFCORSC_V1")])),
    
    # Visuospatial domain missing data count
    VISUALMISSING = rowSums(is.na(result_df[, c("VRCTOTSS_V1", "HFTOTCOR_V1", 
                                               "MR1COR_V1")])),
    
    # Processing Speed domain missing data count
    PROCMISSING = rowSums(is.na(result_df[, c("TRL2TSC_V1", "TRL3TSC_V1", 
                                            "STRWT_V1", "STRCT_V1")])),
    
    # Total missing data count (25 tests total - no Boston Naming Test)
    TOTALMISSING = MEMORYMISSING + EXECMISSING + ATTNMISSING + 
                   VERBALMISSING + VISUALMISSING + PROCMISSING
  )

# If number of missing tests is 12 or more, exclude from analysis 
# This has been standard practice across all waves.
result_df <- result_df %>%
  filter(TOTALMISSING < 12)

# Select only wave-specific variables for output
mci_prep_vars <- result_df %>%
  select(
    VETSAID, AGE_V1,
    # Missing data counts
    TOTALMISSING, MEMORYMISSING, EXECMISSING, ATTNMISSING, 
    VERBALMISSING, VISUALMISSING, PROCMISSING,
    # Flag and raw data accuracy variables
    contains("Z"),
    # Wave-specific cognitive test variables
    contains("_V1")
  )

# Write out the MCI pre-imputation dataset
write_csv(mci_prep_vars, output_file)

cat("VETSA 1 data preparation complete. Files written to:", output_file, "\n")
