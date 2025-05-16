#############################################################################
# MCI_02b_vetsa2_Data_prep.R
#
# Overview: This script processes VETSA wave 2 cognitive data for MCI diagnosis.
# It loads the practice effect and AFQT-adjusted cognitive scores, merges with 
# demographic data, converts raw scores to scaled/standardized scores using 
# normative data, and computes missing data metrics across cognitive domains.
# Note: Wave 2 includes Spatial Span but not Boston Naming Test.
#
# Input: AFQT-adjusted cognitive data, admin data with age and education
# Output: Prepared dataset for MCI classification with normalized scores
#############################################################################

library(dplyr)
library(readr)
library(haven)

# Set working directory
setwd("~/netshare/M/Projects/PracEffects_GEE")

# Source the R versions of the norming scripts
source("code/pe_gee_manuscript/03_MCI_pipeline/norms/MCI_apply_all_norms.R")

# Set file paths
cog_data_file <- "data/intermediate_data/MCI_01_V1V2V3V4_cog_data_pe-adjusted_afqt-adjusted.csv"
output_file <- "data/intermediate_data/MCI_02b01_vetsa2_MCI_PreImputation.csv"
admin <- "~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/Admin/vetsa_admin_file_20250205.sas7bdat"

# Read in the practice effects data
V1V2V3V4_CogData_PE <- read_csv(cog_data_file)

# Read in admin file to get age at wave 2
admin_data <- read_sas(admin, NULL)

# Convert all variable names to uppercase
names(V1V2V3V4_CogData_PE) <- toupper(names(V1V2V3V4_CogData_PE))
names(admin_data) <- toupper(names(admin_data))

# Select only relevant columns from admin data
admin_data <- admin_data %>%
  select(VETSAID, AGE_V2, TEDALL) %>%
  mutate(AGE_V2 = as.numeric(AGE_V2),
         TEDALL = as.numeric(TEDALL))

# Merge with admin data to get age at wave 2
V1V2V3V4_CogData_PE <- V1V2V3V4_CogData_PE %>%
  left_join(admin_data, by = "VETSAID")

# Filter for VETSA2 participants based on non-missing age
V1V2V3V4_CogData_PE <- V1V2V3V4_CogData_PE %>%
  filter(!is.na(AGE_V2)) 

# Convert all variable names to uppercase (again, just to be safe)
names(V1V2V3V4_CogData_PE) <- toupper(names(V1V2V3V4_CogData_PE))

# Convert trails measures from logarithm scale to raw scale
V1V2V3V4_CogData_PE <- V1V2V3V4_CogData_PE %>%
  mutate(
    TRL1T_V2P = exp(TRL1TLOG_V2P),
    TRL2T_V2P = exp(TRL2TLOG_V2P),
    TRL3T_V2P = exp(TRL3TLOG_V2P),
    TRL4T_V2P = exp(TRL4TLOG_V2P),
    TRL5T_V2P = exp(TRL5TLOG_V2P)
  )

# # Calculate age and education predicted stroop scores
# V1V2V3V4_CogData_PE <- V1V2V3V4_CogData_PE %>%
#   mutate(
#     # Predicted word score
#     STRWPREDEXT_V2 = 80.305 + (1.971 * TEDALL) - (0.105 * AGE_V2),
#     STRWPRED_V2 = round(STRWPREDEXT_V2, 1),
    
#     # Predicted color score
#     STRCPREDEXT_V2 = 68.810 + (1.026 * TEDALL) - (0.1434 * AGE_V2),
#     STRCPRED_V2 = round(STRCPREDEXT_V2, 1),
    
#     # Predicted color-word score
#     STRCWPREDEXT_V2 = 32.3655 + (1.351 * TEDALL) - (0.231 * AGE_V2),
#     STRCWPRED_V2 = round(STRCWPREDEXT_V2, 1)
#   )

# Select only VETSA2 variables
wave_specific_df <- V1V2V3V4_CogData_PE %>%
  select(
    VETSAID, AGE_V2, TEDALL,
    # Cognitive test scores with _V2 or _V2P suffix
    contains("_V2"), contains("_V2P")
  )

# Create wave-agnostic data for norming
wave_specific_df <- wave_specific_df %>%
  # Add AGE variable for the norming functions
  mutate(AGE = AGE_V2)

# Create a copy with wave suffix removed from variable names
prenorm_df <- wave_specific_df

# Get names of all columns with "_V2" or "_V2P" suffix
v2_cols <- names(prenorm_df)[grepl("_V2P?$", names(prenorm_df))]

# Create new variable names without the wave suffix
new_names <- sub("_V2P?$", "", v2_cols)

# Create a named vector for renaming
names_map <- setNames(v2_cols, new_names)

# Add wave-agnostic variables to the dataframe
for (wave_agnostic_name in new_names) {
  wave_specific_name <- names_map[wave_agnostic_name]
  prenorm_df[[wave_agnostic_name]] <- prenorm_df[[wave_specific_name]]
}

# Apply all normative transformations (will process wave-agnostic variable names)
# Pass wave=2 to indicate this is VETSA wave 2 (has Spatial Span but no Boston Naming)
normed_df <- apply_all_norms(prenorm_df, wave=2)

# Create a new data frame for the results
result_df <- normed_df

# Copy the wave-agnostic scaled scores back to wave-specific variables
# Note: Wave 2 has Spatial Span but not Boston Naming Test
norm_scores <- c("MTXT", "CVATOTSS", "CVLDFSS", "LM1ASS", "LM1BSS", "LM2ASS", "LM2BSS", 
                "DSPSS", "LNSC", "TRL1TSC", "TRL2TSC", "TRL3TSC", "TRL4TSC", "TRL5TSC", 
                "SSPSS", "STRWT", "STRCT", "STRIT", "HFTOTCOR", "MR1COR",
                "LFCORSC", "CFCORSC", "CSCORSC", 
                "CSSACCSC", "VRITOTSS", "VRDTOTSS", "VRCTOTSS")

# Copy norm scores to wave-specific variables
for (score in norm_scores) {
  wave_specific_score <- paste0(score, "_V2")
  if (score %in% names(result_df)) {
    result_df[[wave_specific_score]] <- result_df[[score]]
  }
}

# Add missing data counters
# Note: Wave 2 has no Boston Naming Test so it's excluded from verbal domain
result_df <- result_df %>%
  mutate(
    # Memory domain missing data count
    MEMORYMISSING = rowSums(is.na(result_df[, c("CVATOTSS_V2", "CVLDFSS_V2", 
                                               "LM1ASS_V2", "LM1BSS_V2", 
                                               "LM2ASS_V2", "LM2BSS_V2", 
                                               "VRITOTSS_V2", "VRDTOTSS_V2")])),
    
    # Executive function domain missing data count
    EXECMISSING = rowSums(is.na(result_df[, c("TRL4TSC_V2", "CSSACCSC_V2", 
                                            "STRIT_V2", "MTXT_V2")])),
    
    # Attention/Working Memory domain missing data count (includes Spatial Span)
    ATTNMISSING = rowSums(is.na(result_df[, c("TRL1TSC_V2", "DSPSS_V2", 
                                             "LNSC_V2", "SSPSS_V2")])),
    
    # Verbal domain missing data count (no Boston Naming Test in wave 2)
    VERBALMISSING = rowSums(is.na(result_df[, c("LFCORSC_V2", "CFCORSC_V2")])),
    
    # Visuospatial domain missing data count
    VISUALMISSING = rowSums(is.na(result_df[, c("VRCTOTSS_V2", "HFTOTCOR_V2", 
                                               "MR1COR_V2")])),
    
    # Processing Speed domain missing data count
    PROCMISSING = rowSums(is.na(result_df[, c("TRL2TSC_V2", "TRL3TSC_V2", 
                                            "STRWT_V2", "STRCT_V2")])),
    
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
    VETSAID, AGE_V2,
    # Missing data counts
    TOTALMISSING, MEMORYMISSING, EXECMISSING, ATTNMISSING, 
    VERBALMISSING, VISUALMISSING, PROCMISSING,
    # Flag and raw data accuracy variables
    contains("Z"),
    # Wave-specific cognitive test variables
    contains("_V2")
  )

# Write out the MCI pre-imputation dataset
write_csv(mci_prep_vars, output_file)

cat("VETSA 2 data preparation complete. Files written to:", output_file, "\n")
