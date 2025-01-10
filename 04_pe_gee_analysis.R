############################################################
# Analyses and descriptions of practice effect estimates   # 
# and their impact on cognitive trajectories and MCI dx    #
############################################################

library(tidyverse)
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

### Prep data ###

# Pivot to long format
pe_estimates_long <- pe_estimates %>%
  pivot_longer(cols = -outcome, 
               names_to = c(".value", "assessment"), 
               names_pattern = "(.+?)_(.+)") %>%
  rename(term = outcome)


### Define order of tests and test groups ###

# Select tests of interest
gca_tests <- c("AFQTPCTTRAN_R")
epmem_tests <- c("LMITOT","LMDTOT","CVATOT","CVSDFR","CVLDFR")
vismem_tests <- c("VRCTOT","VRITOT","VRDTOT")
ef_tests <- c("STRCWRAW","TRL4TLOG","CSSACC", "LNTOT", "RSATOT", "DSTOT")
fluency_tests <- c("LFFCOR","LFACOR","LFSCOR","CFANCOR","CFBNCOR","CSCOR")
speed_tests <- c("TRL2TLOG","TRL3TLOG","STRWRAW","STRCRAW","SRTGMEANLOG","CHRTGMEANLOG")
visspat_tests <- c("MR1COR","AFQTBXPCTTRAN_R","HFTOTCOR")

# Order of tests
order_of_tests <- c(gca_tests, epmem_tests, vismem_tests, ef_tests, fluency_tests, speed_tests, visspat_tests)

# Grouping of tests. Each bracket is specified by label, first test, last test
test_groups <- list(
  epmem = c("Episodic memory", epmem_tests[1], epmem_tests[length(epmem_tests)]),
  vismem = c("Visual memory", vismem_tests[1], vismem_tests[length(vismem_tests)]),
  ef = c("Executive function", ef_tests[1], ef_tests[length(ef_tests)]),
  fluency = c("Fluency", fluency_tests[1], fluency_tests[length(fluency_tests)]),
  speed = c("Processing speed", speed_tests[1], speed_tests[length(speed_tests)]),
  visspat = c("Visuospatial", visspat_tests[1], visspat_tests[length(visspat_tests)])
)

### Create "model" variable with descriptive versions of coefficient ###
pe_estimates_long$model <- pe_estimates_long$assessment %>%
  recode_factor(
    ".Intercept" = "Intercept",
    "AGE" = "Age",
    "NAS201TRAN" = "Age 20 AFQT",
    "WAVE2" = "AR 2 baseline",
    "WAVE3" = "AR 3 baseline",
    "SKIP1" = "Skipped 1 assessment",
    "SKIP2" = "Skipped 2 assessments",
    "WAVE2_ASSESSMENT2" = "PE: Follow-up 1",
    "WAVE3_ASSESSMENT3" = "PE: Follow-up 2",
    "WAVE4_ASSESSMENT4" = "PE: Follow-up 3",
    "WAVE3_ASSESSMENT2" = "PE: Follow-up 1, wave 4",
    "WAVE4" = "PE: Follow-up 1, wave 4",
    "WAVE4_ASSESSMENT3" = "PE: Follow-up 2, wave 4",
  )

models_of_interest <- c("WAVE2_ASSESSMENT2",  
                        "WAVE3_ASSESSMENT3", 
                        "WAVE4_ASSESSMENT4")

pe_plot_df <- pe_estimates_long %>%
  filter(assessment %in% models_of_interest,
         term %in% order_of_tests) %>%
  mutate(model = factor(model))

{
  dwplot(pe_plot_df,
         vars_order = order_of_tests) +
    facet_wrap(vars(model)) +
    theme_bw() +
    xlab("Coefficient Estimate") + ylab("") +
    geom_vline(xintercept = 0,
               colour = "grey60",
               linetype = 2)
} |>
add_brackets(test_groups)
