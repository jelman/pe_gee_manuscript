#############################################################################
# Script: MCI_01_residualize_afqt.R
# 
# Description: This script adjusts cognitive scores for premorbid cognitive 
# ability using Age 20 AFQT scores. Each cognitive variable is regressed on 
# AFQT scores, and the residuals (with intercept added back) are saved.
# This creates cognitive scores that are independent of early adult 
# cognitive ability, potentially highlighting cognitive changes over time.
#
# Input: V1V2V3V4_cog_data_pe-adjusted_[date].csv - Combined cognitive data
#        AFQT_Age20_2020_05_19_revised.csv - Age 20 AFQT scores
#
# Output: MCI_01_V1V2V3V4_cog_data_pe-adjusted_afqt-adjusted.csv - 
#         Cognitive data with AFQT-residualized scores
#############################################################################

rm(list = ls())
library(dplyr)
library(haven)

# Define the input and output file paths
setwd("~/netshare/M/Projects/PracEffects_GEE")

input_file <- "data/raw_data/V1V2V3V4_cog_data_pe-adjusted_2025-05-14.csv"
output_file <- "data/intermediate_data/MCI_01_V1V2V3V4_cog_data_pe-adjusted_afqt-adjusted.csv"
nas201tran_file <- "~/netshare/M/NAS VETSA MASTER DATAFILES/Other cognitive measures/AFQT--age 20 cannot be distributed outside VETSA/AFQT_Age20_2020_05_19_revised.csv"
admin <- read_sas("~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/Admin/vetsa_admin_file_20250205.sas7bdat", NULL)

# Load input data
df <- read.csv(input_file)

# Load age 20 afqt data
afqt_data <- read.csv(nas201tran_file)

# Set names to upper case
names(df) <- toupper(names(df))
names(afqt_data) <- toupper(names(afqt_data))

# Merge age 20 afqt data with the main dataset
df <- df %>%
  left_join(afqt_data, by = "VETSAID") 

# Check for duplicates in the merged data
duplicates <- df[duplicated(df$VETSAID), ]
if (nrow(duplicates) > 0) {
  cat("Duplicates found in the merged data:\n")
  print(duplicates)
} else {
  cat("No duplicates found in the merged data.\n")
}

# Get list of V1NE participants
v1ne_subjs <- admin %>% 
  filter(grepl("V1NE", VGRP_procvar)) %>%
  pull(vetsaid)

# Remove V1NE participants from the dataset
df <- df %>% 
  filter(!VETSAID %in% v1ne_subjs)

admin <- admin %>% 
  select(VETSAID=vetsaid, TEDALL, AGE_V1, AGE_V2, AGE_V3, AGE_V4) %>%
  mutate(AGE_V1 = as.numeric(AGE_V1),
         AGE_V2 = as.numeric(AGE_V2),
         AGE_V3 = as.numeric(AGE_V3),
         AGE_V4 = as.numeric(AGE_V4),
         TEDALL = as.numeric(TEDALL))
df <- df %>% left_join(admin, by="VETSAID")

# Calculate age and education predicted stroop scores
df <- df %>%
  mutate(
    # Predicted word score
    STRWPREDEXT_V1p = 80.305 + (1.971 * TEDALL) - (0.105 * AGE_V1),
    STRWPRED_V1p = round(STRWPREDEXT_V1p, 1),
    STRWDEV_V1p = STRWRAW_V1p - STRWPRED_V1p,
    STRWPREDEXT_V2p = 80.305 + (1.971 * TEDALL) - (0.105 * AGE_V2),
    STRWPRED_V2p = round(STRWPREDEXT_V2p, 1),
    STRWDEV_V2p = STRWRAW_V2p - STRWPRED_V2p,
    STRWPREDEXT_V3p = 80.305 + (1.971 * TEDALL) - (0.105 * AGE_V3),
    STRWPRED_V3p = round(STRWPREDEXT_V3p, 1),
    STRWDEV_V3p = STRWRAW_V3p - STRWPRED_V3p,
    STRWPREDEXT_V4p = 80.305 + (1.971 * TEDALL) - (0.105 * AGE_V4),
    STRWPRED_V4p = round(STRWPREDEXT_V4p, 1),
    STRWDEV_V4p = STRWRAW_V4p - STRWPRED_V4p,
    
    # Predicted color score
    STRCPREDEXT_V1p = 68.810 + (1.026 * TEDALL) - (0.1434 * AGE_V1),
    STRCPRED_V1p = round(STRCPREDEXT_V1p, 1),
    STRCDEV_V1p = STRCRAW_V1p - STRCPRED_V1p,
    STRCPREDEXT_V2p = 68.810 + (1.026 * TEDALL) - (0.1434 * AGE_V2),
    STRCPRED_V2p = round(STRCPREDEXT_V2p, 1),
    STRCDEV_V2p = STRCRAW_V2p - STRCPRED_V2p,
    STRCPREDEXT_V3p = 68.810 + (1.026 * TEDALL) - (0.1434 * AGE_V3),
    STRCPRED_V3p = round(STRCPREDEXT_V3p, 1),
    STRCDEV_V3p = STRCRAW_V3p - STRCPRED_V3p,
    STRCPREDEXT_V4p = 68.810 + (1.026 * TEDALL) - (0.1434 * AGE_V4),
    STRCPRED_V4p = round(STRCPREDEXT_V4p, 1),
    STRCDEV_V4p = STRCRAW_V4p - STRCPRED_V4p,
    
    # Predicted color-word score
    STRCWPREDEXT_V1p = 32.3655 + (1.351 * TEDALL) - (0.231 * AGE_V1),
    STRCWPRED_V1p = round(STRCWPREDEXT_V1p, 1),
    STRCWDEV_V1p = STRCWRAW_V1p - STRCWPRED_V1p,
    STRCWPREDEXT_V2p = 32.3655 + (1.351 * TEDALL) - (0.231 * AGE_V2),
    STRCWPRED_V2p = round(STRCWPREDEXT_V2p, 1),
    STRCWDEV_V2p = STRCWRAW_V2p - STRCWPRED_V2p,
    STRCWPREDEXT_V3p = 32.3655 + (1.351 * TEDALL) - (0.231 * AGE_V3),
    STRCWPRED_V3p = round(STRCWPREDEXT_V3p, 1),
    STRCWDEV_V3p = STRCWRAW_V3p - STRCWPRED_V3p,
    STRCWPREDEXT_V4p = 32.3655 + (1.351 * TEDALL) - (0.231 * AGE_V4),
    STRCWPRED_V4p = round(STRCWPREDEXT_V4p, 1),
    STRCWDEV_V4p = STRCWRAW_V4p - STRCWPRED_V4p
  ) %>%
  select(-TEDALL, -AGE_V1, -AGE_V2, -AGE_V3, -AGE_V4) # Remove unnecessary columns


# The dataframe df now contains all cognitive scores and the age 20 AFQT scores (NAS201TRAN). 
# For every score in df, regress on NAS201TRAN and save the residuals. 
# These should not be mean centered, so the intercept will need to be added back in.

# Define the cognitive variables to be residualized
cog_vars <- df %>% select(-VETSAID, -NAS201TRAN)

# Initialize a list to store the residuals
residuals_list <- list()
# Loop through each cognitive variable and calculate residuals
for (var in names(cog_vars)) {  
  # Create formula using variable substitution
  formula <- as.formula(paste(var, "~ NAS201TRAN"))
  
  # Fit linear model with the formula
  lm_model <- lm(formula, data = df, na.action = na.exclude)
  
  # Get the residuals
  residuals <- resid(lm_model)
  
  # Add the intercept back to the residuals
  residuals <- residuals + lm_model$coefficients[1]
  
  # Store the residuals in the list
  residuals_list[[var]] <- residuals
}

# Combine the residuals into a data frame
residuals_df <- as.data.frame(residuals_list)

# Add the VETSAID column back to the residuals data frame
residuals_df$VETSAID <- df$VETSAID

# Reorder the columns to have VETSAID first
residuals_df <- residuals_df %>% select(VETSAID, everything())

# Save the residuals data frame to a CSV file
write.csv(residuals_df, output_file, row.names = FALSE)
