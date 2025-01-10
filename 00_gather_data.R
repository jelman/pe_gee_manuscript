library(dplyr)


# Read in the data
uncorr_df <- read.csv("M:/VETSA DATA FILES_852014/PracticeEffects/data/temp_data/V1V2V3V4_cog_data_2025-01-07.csv")
str(uncorr_df)
corr_df <- read.csv("M:/VETSA DATA FILES_852014/PracticeEffects/data/output_data/V1V2V3V4_cog_data_pe_adjusted_2025-01-07.csv")

# Get names of corrected variables
corr_vars <- corr_df %>% select(ends_with("p")) %>% names()

# Add "p" suffix to uncorrected variables. This will allow the dataset to be 
# used as input to the cognitive factor score and MCI pipelines.
uncorr_df <- uncorr_df %>% rename_at(vars(-VETSAID), ~paste0(., "p"))

# Select variables of interest in uncorrected data
uncorr_df <- uncorr_df %>% select(VETSAID, all_of(corr_vars))

# Confirm that variables and order are the same in both dataframes
all.equal(names(uncorr_df), names(corr_df))

# Write out the data
write.csv(uncorr_df, "V:/PROJ/PracEffects_GEE/data/V1V2V3V4_cog_data_raw_2025-01-08.csv", row.names = FALSE)
write.csv(corr_df, "V:/PROJ/PracEffects_GEE/data/V1V2V3V4_cog_data_pe_adjusted_2025-01-08.csv", row.names = FALSE)
