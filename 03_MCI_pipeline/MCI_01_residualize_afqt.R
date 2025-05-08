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

# Define the input and output file paths
setwd("~/netshare/M/VETSA DATA FILES_852014/MCI Data/Revised_pipeline")

input_file <- "data/raw_data/V1V2V3V4_cog_data_pe-adjusted_2025-04-25.csv"
output_file <- "data/intermediate_data/MCI_01_V1V2V3V4_cog_data_pe-adjusted_afqt-adjusted.csv"
nas201tran_file <- "~/netshare/M/NAS VETSA MASTER DATAFILES/Other cognitive measures/AFQT--age 20 cannot be distributed outside VETSA/AFQT_Age20_2020_05_19_revised.csv"

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
