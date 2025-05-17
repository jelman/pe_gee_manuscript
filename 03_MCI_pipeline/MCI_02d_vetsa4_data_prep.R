#############################################################################
# MCI_02d_vetsa4_Data_prep.R
#
# Overview: This script processes VETSA wave 4 cognitive data for MCI diagnosis. 
# It loads the practice effect and AFQT-adjusted cognitive scores, merges with 
# demographic data, converts raw scores to scaled/standardized scores using 
# normative data, and computes missing data metrics across cognitive domains.
# Note: Wave 4 includes Boston Naming Test but not Spatial Span.
#
# Input: AFQT-adjusted cognitive data, admin data with age and education
# Output: Prepared dataset for MCI classification with normalized scores
#############################################################################

library(dplyr)
library(readr)
library(haven)

# Note: Working directory should be set by the runner script

# Source the R versions of the norming scripts
# Try multiple potential paths to find the norms file
norms_paths <- c(
  "code/pe_gee_manuscript/03_MCI_pipeline/norms/MCI_apply_all_norms.R",
  "./code/pe_gee_manuscript/03_MCI_pipeline/norms/MCI_apply_all_norms.R",
  "03_MCI_pipeline/norms/MCI_apply_all_norms.R",
  "./03_MCI_pipeline/norms/MCI_apply_all_norms.R"
)

norms_file_found <- FALSE
for (path in norms_paths) {
  if (file.exists(path)) {
    source(path)
    cat("Sourced norms from:", path, "\n")
    norms_file_found <- TRUE
    break
  }
}

if (!norms_file_found) {
  stop("Could not locate MCI_apply_all_norms.R file in any expected location")
}

# Use file paths from runner script if they exist, otherwise use defaults
if(!exists("input_cog_data")) {
  input_cog_data <- "data/intermediate_data/MCI_01_V1V2V3V4_cog_data_pe-adjusted_afqt-adjusted.csv"
}
if(!exists("pre_imputation_file")) {
  pre_imputation_file <- "data/intermediate_data/MCI_02d01_vetsa4_MCI_PreImputation.csv"
}
if(!exists("admin_file")) {
  admin_file <- "~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/Admin/vetsa_admin_file_20250205.sas7bdat"
}

# Read in the practice effects data
V1V2V3V4_CogData_PE <- read_csv(input_cog_data)

# Read in admin file to get age at wave 4
admin_data <- read_sas(admin_file, NULL)
names(admin_data) <- toupper(names(admin_data))

# Remove V1NE participants if VGRP_procvar exists
if("VGRP_PROCVAR" %in% toupper(names(admin_data))) {
  admin_data <- admin_data %>% 
    filter(!grepl("V1NE", VGRP_PROCVAR))
} else if("vgrp_procvar" %in% names(admin_data)) {
  admin_data <- admin_data %>% 
    filter(!grepl("V1NE", vgrp_procvar))
}

# Convert all variable names to uppercase
names(V1V2V3V4_CogData_PE) <- toupper(names(V1V2V3V4_CogData_PE))
names(admin_data) <- toupper(names(admin_data))

# Select only relevant columns from admin data
admin_data <- admin_data %>%
  select(VETSAID, AGE_V4, TEDALL) %>%
  mutate(AGE_V4 = as.numeric(AGE_V4),
         TEDALL = as.numeric(TEDALL))

# Merge with admin data to get age at wave 4
V1V2V3V4_CogData_PE <- V1V2V3V4_CogData_PE %>%
  left_join(admin_data, by = "VETSAID")

# Filter for VETSA4 participants based on non-missing age
V1V2V3V4_CogData_PE <- V1V2V3V4_CogData_PE %>%
  filter(!is.na(AGE_V4)) 

# Convert all variable names to uppercase (again, just to be safe)
names(V1V2V3V4_CogData_PE) <- toupper(names(V1V2V3V4_CogData_PE))

# Convert trails measures from logarithm scale to raw scale
V1V2V3V4_CogData_PE <- V1V2V3V4_CogData_PE %>%
  mutate(
    TRL1T_V4P = exp(TRL1TLOG_V4P),
    TRL2T_V4P = exp(TRL2TLOG_V4P),
    TRL3T_V4P = exp(TRL3TLOG_V4P),
    TRL4T_V4P = exp(TRL4TLOG_V4P),
    TRL5T_V4P = exp(TRL5TLOG_V4P)
  )

# # Calculate age and education predicted stroop scores
# V1V2V3V4_CogData_PE <- V1V2V3V4_CogData_PE %>%
#   mutate(
#     # Predicted word score
#     STRWPREDEXT_V4 = 80.305 + (1.971 * TEDALL) - (0.105 * AGE_V4),
#     STRWPRED_V4 = round(STRWPREDEXT_V4, 1),
    
#     # Predicted color score
#     STRCPREDEXT_V4 = 68.810 + (1.026 * TEDALL) - (0.1434 * AGE_V4),
#     STRCPRED_V4 = round(STRCPREDEXT_V4, 1),
    
#     # Predicted color-word score
#     STRCWPREDEXT_V4 = 32.3655 + (1.351 * TEDALL) - (0.231 * AGE_V4),
#     STRCWPRED_V4 = round(STRCWPREDEXT_V4, 1)
#   )

# Select only VETSA4 variables
wave_specific_df <- V1V2V3V4_CogData_PE %>%
  select(
    VETSAID, AGE_V4, TEDALL,
    # Cognitive test scores with _V4 or _V4P suffix
    contains("_V4"), contains("_V4P")
  )

# Create wave-agnostic data for norming
wave_specific_df <- wave_specific_df %>%
  # Add AGE variable for the norming functions
  mutate(AGE = AGE_V4)

# Create a copy with wave suffix removed from variable names
prenorm_df <- wave_specific_df

# Get names of all columns with "_V4" or "_V4P" suffix
v4_cols <- names(prenorm_df)[grepl("_V4P?$", names(prenorm_df))]

# Create new variable names without the wave suffix
new_names <- sub("_V4P?$", "", v4_cols)

# Create a named vector for renaming
names_map <- setNames(v4_cols, new_names)

# Add wave-agnostic variables to the dataframe
for (wave_agnostic_name in new_names) {
  wave_specific_name <- names_map[wave_agnostic_name]
  prenorm_df[[wave_agnostic_name]] <- prenorm_df[[wave_specific_name]]
}

# Apply all normative transformations (will process wave-agnostic variable names)
# Pass wave=4 to indicate this is VETSA wave 4 (has Boston Naming but no Spatial Span)
normed_df <- apply_all_norms(prenorm_df, wave=4)

# Copy the wave-agnostic scaled scores back to wave-specific variables
# Get names of all computed norm scores that need to be copied back with _V4 suffix
norm_scores <- c("MTXT", "CVATOTSS", "CVLDFSS", "LM1ASS", "LM1BSS", "LM2ASS", "LM2BSS", 
                "DSPSS", "LNSC", "TRL1TSC", "TRL2TSC", "TRL3TSC", "TRL4TSC", "TRL5TSC", 
                "STRWT", "STRCT", "STRIT", "HFTOTCOR", "MR1COR",
                "LFCORSC", "CFCORSC", "CSCORSC", 
                "CSSACCSC", "VRITOTSS", "VRDTOTSS", "VRCTOTSS", "BNTSS")

# Create a new data frame for the results
result_df <- normed_df

# Copy norm scores to wave-specific variables
for (score in norm_scores) {
  wave_specific_score <- paste0(score, "_V4")
  if (score %in% names(normed_df)) {
    result_df[[wave_specific_score]] <- normed_df[[score]]
  }
}

# Add missing data counters
result_df <- result_df %>%
  mutate(
    # Memory domain missing data count (2 CVLT tests, 4 LM tests, 2 VR tests = 8 total)
    MEMORYMISSING = rowSums(is.na(result_df[, c("CVATOTSS_V4", "CVLDFSS_V4", 
                                               "LM1ASS_V4", "LM1BSS_V4", 
                                               "LM2ASS_V4", "LM2BSS_V4", 
                                               "VRITOTSS_V4", "VRDTOTSS_V4")])),
    
    # Executive function domain missing data count (4 tests)
    EXECMISSING = rowSums(is.na(result_df[, c("TRL4TSC_V4", "CSSACCSC_V4", 
                                              "STRIT_V4", "MTXT_V4")])),
    
    # Attention/Working Memory domain missing data count (4 tests)
    ATTNMISSING = rowSums(is.na(result_df[, c("TRL1TSC_V4", "DSPSS_V4", 
                                             "LNSC_V4")])),
    
    # Verbal domain missing data count (3 tests)
    VERBALMISSING = rowSums(is.na(result_df[, c("LFCORSC_V4", "CFCORSC_V4", 
                                              "BNTSS_V4")])),
    
    # Visuospatial domain missing data count (3 tests)
    VISUALMISSING = rowSums(is.na(result_df[, c("VRCTOTSS_V4", "HFTOTCOR_V4", 
                                              "MR1COR_V4")])),
    
    # Processing Speed domain missing data count (4 tests)
    PROCMISSING = rowSums(is.na(result_df[, c("TRL2TSC_V4", "TRL3TSC_V4", 
                                            "STRWT_V4", "STRCT_V4")])),
    
    # Total missing data count (26 tests total)
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
    VETSAID, AGE_V4,
    # Missing data counts
    TOTALMISSING, MEMORYMISSING, EXECMISSING, ATTNMISSING, 
    VERBALMISSING, VISUALMISSING, PROCMISSING,
    # Wave-specific cognitive test variables
    contains("_V4")
  )

# Write out the MCI pre-imputation dataset
write_csv(mci_prep_vars, pre_imputation_file)

cat("VETSA 4 data preparation complete. Files written to:", pre_imputation_file, "\n")
