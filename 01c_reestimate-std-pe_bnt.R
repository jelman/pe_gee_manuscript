library(dplyr)
library(tidyr)
library(geepack)
library(haven)
library(broom)


#--------------------------------#
# Set directories and load data  #
#--------------------------------#

# Set working directory
# setwd("~/netshare/M/Projects/PracEffects_GEE")
setwd("~/netshare/M/Projects/PracEffects_GEE")

# Load admin data and get age 
admin <- read_sas("~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/Admin/vetsa_admin_file_20250205.sas7bdat", NULL)

# # Remove V1NE participants
# admin <- admin %>% 
#   filter(!grepl("V1NE", VGRP_procvar))

# Select only the columns we need
admin <- admin %>% select(VETSAID=vetsaid, starts_with("AGE"))

# Age 20 AFQT file
nas201 <- read.csv("~/netshare/M/NAS VETSA MASTER DATAFILES/Other cognitive measures/AFQT--age 20 cannot be distributed outside VETSA/AFQT_Age20_2020_05_19_revised.csv")
nas201 <- nas201 %>% rename_all(toupper)

# Load in raw (unadjusted) outcome data. These are variables we want to adjust for PEs
outcome_data <- read.csv("data/raw_data/V1V2V3V4_cog_data_raw_2025-05-17.csv", stringsAsFactors = FALSE)

# Merge age 20 afqt into outcome data
outcome_data <- outcome_data %>% 
  left_join(nas201, by=c("VETSAID"))

# Remove "p" suffix from raw data.
colnames(outcome_data) <- gsub("p$", "", colnames(outcome_data))

#----------------------------------------------------------------#
# Define outcomes that we want to estimate practice effects for  #
#----------------------------------------------------------------#

# Variables that are in all waves 
outcome_varList <- c("BNTTOTCOR")


#---------------------------------------------------------#
# Clean up and format outcome data:                       #
#     Rename variables, recode factors, set levels, etc   #
#---------------------------------------------------------#

# Pivot outcome file from wide to long format. 
outcome_data_long <- outcome_data %>% 
  pivot_longer(cols = -c(VETSAID, NAS201TRAN),  
               names_to = c(".value", "WAVE"),
               names_pattern = "(\\w+)_V(\\d+)",
               values_drop_na = TRUE) %>%
  mutate(WAVE = as.numeric(WAVE)) %>%
  filter(WAVE!=1 & WAVE!=2) # Remove wave 1 and 2 data

# Sort by VETSAID and WAVE
outcome_data_long <- outcome_data_long %>% 
  arrange(VETSAID, WAVE)

# Create several variables:
#   ASSESSMENT: how many assessments has an individual completed?
assessment_outcome_long <- outcome_data_long %>%
  group_by(VETSAID) %>%
  mutate(ASSESSMENT = row_number()) %>%
  ungroup()

# Create variables that will be used in the GEE model.
#   WAVE4: If WAVE is 4, then 1, else 0
assessment_outcome_long <- assessment_outcome_long %>%
  mutate(WAVE4 = ifelse(WAVE == 4, 1, 0))


# Pivot age file to long format
admin_long <- admin %>% 
  pivot_longer(cols = -c(VETSAID),  
               names_to = c(".value", "WAVE"),
               names_pattern = "(\\w+)_V(\\d+)",
               values_drop_na = TRUE) %>%
  mutate(WAVE = as.integer(WAVE),
         AGE = as.numeric(AGE)) %>%
  filter(WAVE!=1 & WAVE!=2) # Remove wave 1 and 2 data

# Merge age with outcome data
assessment_outcome_long <- assessment_outcome_long %>%
  inner_join(admin_long, by=c("VETSAID", "WAVE"))


#--------------------------#
# Begin running GEE loop   #
#--------------------------#

# Initialize an empty list to store results
results_list <- list()

#### Start loop here ####

for (outcome in outcome_varList) {
  print(paste("Running GEE model for", outcome))

  # Define columns that we want to obtain estimates for in GEE model
  geeCols = c("AGE", "NAS201TRAN","WAVE4")
  
  # Select columns used in GEE model and filter for complete cases
  gee_data <- assessment_outcome_long %>% 
    select(VETSAID, WAVE, all_of(geeCols), all_of(outcome)) %>%
    filter(complete.cases(.)) %>% 
    arrange(VETSAID, WAVE)
  
  # Calculate Wave 3 means and SDs (since Wave 3 is the first wave with this measure)
  wave3_stats <- gee_data %>%
    filter(WAVE == 3) %>%
    summarise(
      nas_mean = mean(NAS201TRAN, na.rm = TRUE),
      nas_sd = sd(NAS201TRAN, na.rm = TRUE),
      outcome_mean = mean(!!sym(outcome), na.rm = TRUE),
      outcome_sd = sd(!!sym(outcome), na.rm = TRUE)
    )
  
  # Calculate full sample statistics for age
  age_stats <- gee_data %>%
    summarise(
      age_mean = mean(AGE, na.rm = TRUE),
      age_sd = sd(AGE, na.rm = TRUE)
    )
  
  # Apply different standardization approaches
  gee_data <- gee_data %>%
    mutate(
      AGE = (AGE - age_stats$age_mean) / age_stats$age_sd,  # full sample standardization
      NAS201TRAN = (NAS201TRAN - wave3_stats$nas_mean) / wave3_stats$nas_sd,  # Wave 3 standardization
      !!outcome := (!!sym(outcome) - wave3_stats$outcome_mean) / wave3_stats$outcome_sd  # Wave 3 standardization
    )
  
  # Create formula for GEE model.  
  # Note: We can use caret::findLinearCombos(model_matrix) to verify no linear combinations
  fmla <- as.formula(paste(outcome, "~ AGE + NAS201TRAN + WAVE4"))
  
  # Run GEE model
  gee_mod <- geepack::geeglm(fmla, id = as.factor(gee_data$VETSAID), data = gee_data, 
                             family = gaussian, corstr = "exchangeable")
  
  # Use tidy to extract 
  gee_summary <- broom::tidy(gee_mod, conf.int = TRUE)
  # Convert to term to wide format. There should be a column for every combination of existing columns with values from term.
  # Add a column for the outcome variable
  gee_summary <- gee_summary %>% mutate(outcome = outcome)

  # Append the results
  results_list[[outcome]] <- gee_summary
}

# Combine results into a single data frame
all_results <- bind_rows(results_list)

# Reshape to wide format
wide_results <- all_results %>%
  pivot_wider(
    id_cols = outcome,
    names_from = term,
    values_from = c(estimate, std.error, statistic, p.value, conf.low, conf.high),
    names_sep = "_"
  )

# Remove parentheses from column names
colnames(wide_results) <- gsub("//(|//)", "", colnames(wide_results))

# Save results to file
date <- format(Sys.Date(), "%Y-%m-%d")
write.csv(wide_results, paste0("results/gee_standardized_results_bnt_", date, ".csv"), row.names = FALSE)