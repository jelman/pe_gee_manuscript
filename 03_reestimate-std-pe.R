library(dplyr)
library(tidyr)
library(geepack)
library(haven)


#--------------------------------#
# Set directories and load data  #
#--------------------------------#

# Set working directory
setwd("~/netshare/M/VETSA DATA FILES_852014/PracticeEffects")
# setwd("M:/VETSA DATA FILES_852014/PracticeEffects")

# Load admin data and get age 
admin <- read_sas("~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/Admin/vetsa_admin_file_20241014.sas7bdat", NULL)
admin <- admin %>% select(VETSAID=vetsaid, starts_with("AGE"))

# Load in outcome data. These are variables we want to adjust for PEs
outcome_data <- read.csv("data/temp_data/V1V2V3V4_cog_data_2025-01-07.csv")

#----------------------------------------------------------------#
# Define outcomes that we want to estimate practice effects for  #
#----------------------------------------------------------------#

# Variables that are in all waves ### TODO: Change afqt to half-rounded variable names
outcome_varList <- c("MR1COR","TRL1TLOG","TRL2TLOG","TRL3TLOG","TRL4TLOG","TRL5TLOG",
                     "CSSACC","MTXRAW","CVA1RAW","CVATOT","CVSDFR","CVLDFR",
                     "AFQTPCTTRAN_R","AFQTVOCPCTTRAN_R","AFQTARPCTTRAN_R","AFQTTLPCTTRAN_R","AFQTBXPCTTRAN_R",
                     "DSFRAW","DSBRAW","DSFMAX","DSTOT","LNTOT",
                     "LM1A","LM1B","LM2A","LM2B","LMITOT","LMDTOT",
                     "VRITOT","VRDTOT","VRCTOT","HFTOTCOR","STRWRAW","STRCRAW","STRCWRAW","STRIT",
                     "LFFCOR","LFACOR","LFSCOR","LFCOR","CFANCOR","CFBNCOR","CFCOR","CSCOR",
                     "RSATOT","SRTGMEANLOG","SRTGSTDLOG","CHRTGMEANLOG","CHRTGSTDLOG")

# Variable not collected at wave 4. Adjust separately
outcome_varList_noV4 <- c("SSPFRAW","SSPBRAW","SSPTOTP")

#---------------------------------------------------------#
# Clean up and format outcome data:                       #
#     Rename variables, recode factors, set levels, etc   #
#---------------------------------------------------------#

# Pivot outcome file from wide to long format. 
outcome_data_long <- outcome_data %>% 
  pivot_longer(cols = -c(VETSAID, CASE, NAS201TRAN),  
               names_to = c(".value", "WAVE"),
               names_pattern = "(\\w+)_V(\\d+)",
               values_drop_na = TRUE) %>%
  mutate(WAVE = as.numeric(WAVE))

# Sort by VETSAID and WAVE
outcome_data_long <- outcome_data_long %>% 
  arrange(VETSAID, WAVE)

# Create several variables:
#   ASSESSMENT: how many assessments has an individual completed?
#   GAP: how many waves have elapsed since last assessment?
#   SKIP: indicate how many 
#   OUTCOME_LAG: Version of outcome variable for missingness module, will have NAs filled

assessment_outcome_long <- outcome_data_long %>%
  group_by(VETSAID) %>%
  mutate(ASSESSMENT = row_number(),
         GAP = WAVE - lag(WAVE, default = first(WAVE)),
         SKIP = ifelse(GAP>1, GAP-1, 0)) %>%
  ungroup()

# Create variables that will be used in the GEE model.
#   SKIP1: If SKIP is 1, then 1, else 0
#   SKIP2: If SKIP is 2, then 1, else 0
#   WAVE2: If WAVE is 2, then 1, else 0
#   WAVE3: If WAVE is 3, then 1, else 0
#   WAVE4: If WAVE is 4, then 1, else 0
#   WAVE2_ASSESSMENT2: If WAVE is 2 and ASSESSMENT is 2, then 1, else 0
#   WAVE3_ASSESSMENT2: If WAVE is 3 and ASSESSMENT is 2, then 1, else 0
#   WAVE3_ASSESSMENT3: If WAVE is 3 and ASSESSMENT is 3, then 1, else 0
#   WAVE4_ASSESSMENT3: If WAVE is 4 and ASSESSMENT is 3, then 1, else 0
#   WAVE4_ASSESSMENT4: If WAVE is 4 and ASSESSMENT is 4, then 1, else 0
assessment_outcome_long <- assessment_outcome_long %>%
  mutate(SKIP1 = ifelse(SKIP == 1, 1, 0),
         SKIP2 = ifelse(SKIP == 2, 1, 0),
         WAVE2 = ifelse(WAVE == 2, 1, 0),
         WAVE3 = ifelse(WAVE == 3, 1, 0),
         WAVE4 = ifelse(WAVE == 4, 1, 0),
         WAVE2_ASSESSMENT2 = ifelse(WAVE == 2 & ASSESSMENT == 2, 1, 0),
         WAVE3_ASSESSMENT2 = ifelse(WAVE == 3 & ASSESSMENT == 2, 1, 0),
         WAVE3_ASSESSMENT3 = ifelse(WAVE == 3 & ASSESSMENT == 3, 1, 0),
         WAVE4_ASSESSMENT3 = ifelse(WAVE == 4 & ASSESSMENT == 3, 1, 0),
         WAVE4_ASSESSMENT4 = ifelse(WAVE == 4 & ASSESSMENT == 4, 1, 0))

# Pivot age file to long format
admin_long <- admin %>% 
  pivot_longer(cols = -c(VETSAID),  
               names_to = c(".value", "WAVE"),
               names_pattern = "(\\w+)_V(\\d+)",
               values_drop_na = TRUE) %>%
  mutate(WAVE = as.integer(WAVE),
         AGE = as.numeric(AGE))

# Merge age with outcome data
assessment_outcome_long <- assessment_outcome_long %>%
  left_join(admin_long, by=c("VETSAID", "WAVE"))


#--------------------------#
# Begin running GEE loop   #
#--------------------------#

# Initialize an empty list to store results
results_list <- list()

#### Start loop here ####

for (outcome in outcome_varList) {
  print(paste("Running GEE model for", outcome))

  # Define columns that we want to obtain estimates for in GEE model
  geeCols = c("AGE", "NAS201TRAN", "WAVE2", "WAVE3", "WAVE4",
              "WAVE2_ASSESSMENT2", "WAVE3_ASSESSMENT2", "WAVE3_ASSESSMENT3",
              "WAVE4_ASSESSMENT3", "WAVE4_ASSESSMENT4", 
              "SKIP1", "SKIP2")
  
  # Select columns used in GEE model and filter for complete cases
  gee_data <- assessment_outcome_long %>% 
    select(VETSAID, WAVE, all_of(geeCols), all_of(outcome)) %>%
    filter(complete.cases(.)) %>% 
    arrange(VETSAID, WAVE)
  
  # Create formula for GEE model.  
  # Note: We can use caret::findLinearCombos(model_matrix) to verify no linear combinations
  fmla <- as.formula(paste(outcome, "~ AGE + NAS201TRAN + WAVE2 + WAVE3 + WAVE4 + WAVE2_ASSESSMENT2 + WAVE3_ASSESSMENT2 + WAVE3_ASSESSMENT3 + WAVE4_ASSESSMENT3 + WAVE4_ASSESSMENT4 + SKIP1 + SKIP2"))
  
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

# Remove parantheses from column names
colnames(wide_results) <- gsub("//(|//)", "", colnames(wide_results))

# Save results to file
date <- format(Sys.Date(), "%Y-%m-%d")
write.csv(wide_results, paste0("results/model_estimates/gee_results_", date, ".csv"), row.names = FALSE)
