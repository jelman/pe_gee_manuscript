############################################################
# Analyses and descriptions of practice effect estimates   # 
# and their impact on cognitive trajectories and MCI dx    #
############################################################

library(dplyr)
library(dotwhisker)
library(ggplot2)

#---------------------------------#
# Set directories and load data   #
#---------------------------------#

# Set working directory
setwd("V:/PROJ/PracEffects_GEE")

# Load model estimates of practice effects
pe_estimates <- read.csv("M:/VETSA DATA FILES_852014/PracticeEffects/results/model_estimates/gee_results_2025-01-07.csv")

# Load raw and adjusted cognitive test scores
tests_raw <- read.csv("data/V1V2V3V4_cog_data_raw_2025-01-08.csv")
tests_adj <- read.csv("data/V1V2V3V4_cog_data_pe_adjusted_2025-01-08.csv")

# Load raw and adjusted cognitive factor scores
factors_raw <- read.csv("data/V1V2V3V4_cog_factor_scores_raw_2024-01-08.csv")
factors_adj <- read.csv("data/V1V2V3V4_cog_factor_scores_pe_adjusted_2024-01-08.csv")

# Load raw and adjusted MCI diagnosis
# mci_raw <- read.csv()
# mci_adj <- read.csv()


#---------------------------------------------------------------#
# Descriptive statistics of estimated practice effects          #
#---------------------------------------------------------------#

# Pivot the data to long format. The columns we want to pivot begin with "estimate_",
# "conf.low_", "conf.high_" and "p.value_". The suffixes of these columns after first "_" 
# are converted to new rows with their values stored in a variable called "model". 

  
pe_estimates_long <- pe_estimates %>%
  pivot_longer(cols = -outcome, 
               names_to = c(".value", "model"), 
               names_pattern = "(.+?)_(.+)") %>%
  rename(term = outcome)
  
models_of_interest <- c("WAVE2_ASSESSMENT2",  
                       "WAVE3_ASSESSMENT3", 
                       "WAVE4_ASSESSMENT4")

pe_estimates_long %>%
  filter(model %in% models_of_interest) %>% dwplot()
  