############################################################
# Analyses and descriptions of practice effect estimates   # 
# and their impact on cognitive trajectories and MCI dx    #
############################################################

library(tidyverse)
library(dotwhisker)
library(ggplot2)
library(ggthemes)
library(ggsci)
library(extrafont)
# loadfonts(device = 'win')
library(knitr)
library(kableExtra)
library(ggpubr)
library(lmerTest)
library(parameters)
library(tableone)
library(flextable)
library(haven)

#---------------------------------#
# Set directories and load data   #
#---------------------------------#

# Set working directory
setwd("~/netshare/M/Projects/PracEffects_GEE")

# Load admin file
admin <- read_sas("~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/Admin/vetsa_admin_file_20250205.sas7bdat", NULL)

# Load model estimates of practice effects
pe_estimates <- read.csv("results/gee_standardized_results_complete_2025-05-17.csv")

# Load raw and adjusted cognitive test scores
tests_raw <- read.csv("data/raw_data/V1V2V3V4_cog_data_raw_2025-05-17.csv")
tests_adj <- read.csv("data/raw_data/V1V2V3V4_cog_data_pe-adjusted_2025-05-17.csv")

# Load raw and adjusted cognitive factor scores
factors_raw <- read.csv("data/output_data/V1V2V3V4_cog_factor_scores_raw_2025-05-17.csv")
factors_adj <- read.csv("data/output_data/V1V2V3V4_cog_factor_scores_pe-adjusted_2025-05-17.csv")

# Load raw and adjusted MCI diagnosis
mci_v1_raw <- read.csv("data/output_data/vetsa1_mci_raw_2025-05-17.csv")
mci_v2_raw <- read.csv("data/output_data/vetsa2_mci_raw_2025-05-17.csv")
mci_v3_raw <- read.csv("data/output_data/vetsa3_mci_raw_2025-05-17.csv")
mci_v4_raw <- read.csv("data/output_data/vetsa4_mci_raw_2025-05-17.csv")

mci_v1_adj <- read.csv("data/output_data/vetsa1_mci_adjusted_2025-05-17.csv")
mci_v2_adj <- read.csv("data/output_data/vetsa2_mci_adjusted_2025-05-17.csv")
mci_v3_adj <- read.csv("data/output_data/vetsa3_mci_adjusted_2025-05-17.csv")
mci_v4_adj <- read.csv("data/output_data/vetsa4_mci_adjusted_2025-05-17.csv")

# #-----------------------------------------------#
# #     Remove V1NE subjects from all datasets    #
# #-----------------------------------------------#
# 
# # Get list of V1NE participants
# v1ne_subjs <- admin %>% 
#   filter(grepl("V1NE", VGRP_procvar)) %>%
#   pull(vetsaid)
# 
# # Create function to remove V1NE subjects from a dataset
# remove_v1ne <- function(data) {
#   data %>% 
#     filter(!VETSAID %in% v1ne_subjs)
# }
# 
# # Remove V1NE subjects from all datasets
# tests_raw <- remove_v1ne(tests_raw)
# tests_adj <- remove_v1ne(tests_adj)
# factors_raw <- remove_v1ne(factors_raw)
# factors_adj <- remove_v1ne(factors_adj)
# mci_v1_raw <- remove_v1ne(mci_v1_raw)
# mci_v2_raw <- remove_v1ne(mci_v2_raw)
# mci_v3_raw <- remove_v1ne(mci_v3_raw)
# mci_v4_raw <- remove_v1ne(mci_v4_raw)
# mci_v1_adj <- remove_v1ne(mci_v1_adj)
# mci_v2_adj <- remove_v1ne(mci_v2_adj)
# mci_v3_adj <- remove_v1ne(mci_v3_adj)
# mci_v4_adj <- remove_v1ne(mci_v4_adj)


#-------------------------------------------#
#     Prep practice effect estimate data    #
#-------------------------------------------#

### Define order of tests and test groups ###

# Select tests of interest
gca_tests <- c("AFQTPCTTRAN_R")
epmem_tests <- c("LMITOT","LMDTOT","CVATOT","CVSDFR","CVLDFR")
ef_tests <- c("TRL4TLOG","CSSACC","STRIT")
fluency_tests <- c("LFCOR","CFCOR")
speed_tests <- c("TRL2TLOG","TRL3TLOG","STRWRAW","STRCRAW","SRTGMEANLOG","CHRTGMEANLOG")
wm_tests <- c("DSTOT","SSPTOTP","LNTOT","RSATOT")
visspat_tests <- c("MR1COR","AFQTBXPCTTRAN_R","HFTOTCOR")
vismem_tests <- c("VRCTOT","VRITOT","VRDTOT")
abstract_tests <- c("MTXAGE")

# Order of tests
order_of_tests <- c(gca_tests, epmem_tests, ef_tests, fluency_tests, speed_tests, 
                    wm_tests, visspat_tests, vismem_tests, abstract_tests)

# # Grouping of tests. Each bracket is specified by label, first test, last test
# test_groups <- list(
#   epmem = c("Episodic memory", epmem_tests[1], epmem_tests[length(epmem_tests)]),
#   ef = c("Executive function", ef_tests[1], ef_tests[length(ef_tests)]),
#   fluency = c("Fluency", fluency_tests[1], fluency_tests[length(fluency_tests)]),
#   speed = c("Processing speed", speed_tests[1], speed_tests[length(speed_tests)]),
#   wm = c("Working memory", wm_tests[1], wm_tests[length(wm_tests)]),
#   visspat = c("Visuospatial", visspat_tests[1], visspat_tests[length(visspat_tests)]),
#   vismem = c("Visual memory", vismem_tests[1], vismem_tests[length(vismem_tests)])
# )

### Prep data ###

# Pivot to long format
pe_estimates_long <- pe_estimates %>%
  pivot_longer(cols = -outcome, 
               names_to = c(".value", "Assessment"), 
               names_pattern = "(.+?)_(.+)") %>%
  rename(term = outcome) %>%
  filter(term %in% order_of_tests)

# Add domain to pe_estimates_long. If term is in one of the lists above, assign the domain
pe_estimates_long$Domain <- pe_estimates_long$term %>%
  recode_factor(
    !!!setNames(rep("GCA", length(gca_tests)), gca_tests),
    !!!setNames(rep("Episodic memory", length(epmem_tests)), epmem_tests),
    !!!setNames(rep("Executive function", length(ef_tests)), ef_tests),
    !!!setNames(rep("Fluency", length(fluency_tests)), fluency_tests),
    !!!setNames(rep("Processing speed", length(speed_tests)), speed_tests),
    !!!setNames(rep("Working memory", length(wm_tests)), wm_tests),
    !!!setNames(rep("Visuospatial", length(visspat_tests)), visspat_tests),
    !!!setNames(rep("Visual memory", length(vismem_tests)), vismem_tests),
    !!!setNames(rep("Abstract reasoning", length(abstract_tests)), abstract_tests)
  )

# Set order of domain to be GCA, Episodic memory, Visual memory, Executive function, Fluency, Processing speed, Visuospatial
pe_estimates_long$Domain <- factor(pe_estimates_long$Domain, 
                                   levels = c("GCA", "Episodic memory", 
                                              "Executive function", 
                                              "Fluency", "Processing speed", 
                                              "Working memory","Visuospatial",
                                              "Visual memory", "Abstract reasoning"))

### Create "model" variable with descriptive versions of coefficient ###
pe_estimates_long$Model <- pe_estimates_long$Assessment %>%
  recode_factor(
    ".Intercept" = "Intercept",
    "AGE" = "Age",
    "NAS201TRAN" = "Age 20 AFQT",
    "WAVE2" = "AR 2 baseline",
    "WAVE3" = "AR 3 baseline",
    "SKIP1" = "Skipped 1 assessment",
    "SKIP2" = "Skipped 2 assessments",
    "WAVE2_ASSESSMENT2" = "Follow-up 1",
    "WAVE3_ASSESSMENT3" = "Follow-up 2",
    "WAVE4_ASSESSMENT4" = "Follow-up 3",
    "WAVE3_ASSESSMENT2" = "Follow-up 1, wave 4",
    "WAVE4" = "Follow-up 1, wave 4",
    "WAVE4_ASSESSMENT3" = "Follow-up 2, wave 4",
  )

models_of_interest <- c("WAVE2_ASSESSMENT2",  
                        "WAVE3_ASSESSMENT3", 
                        "WAVE4_ASSESSMENT4")

pe_plot_df <- pe_estimates_long %>%
  filter(Assessment %in% models_of_interest,
         term %in% order_of_tests) %>%
  mutate(Model = factor(Model),
         Assessment = factor(Assessment)) %>%
  arrange(desc(Domain), desc(term)) %>%  # Arrange data by domain and term in descending order
  mutate(term = factor(term, levels = unique(term)))  # Set order of terms

#---------------------------------------------------------------#
# Descriptive statistics of estimated practice effects          #
#---------------------------------------------------------------#

# Summary table of practice effect estimates
pe_summary <- pe_plot_df %>%
  group_by(Domain, Model) %>%
  summarize(
    n = n(),
    Mean = round(mean(estimate), 2),
    Median = round(median(estimate), 2),
    SD = round(sd(estimate), 2),
    Min = round(min(estimate), 2),
    Max = round(max(estimate), 2),
    N_sig = sum(p.value < 0.05),
    .groups = "drop"
  ) %>%
  arrange(Domain, Model) %>%
  flextable() %>% 
  theme_apa() %>%
  colformat_double(digits=2) %>% 
  merge_v("Domain") %>% 
  valign(j="Domain", valign="top") %>%
  autofit()

# Save summary table
pe_summary_outname = paste0("results/pe_summary_", Sys.Date(), ".docx")
save_as_docx(pe_summary, path = pe_summary_outname)

#-----------------------------------#
# Plot practice effects estimates   #
#-----------------------------------#

# Create annotation dataframe with max_y variable for positioning
min_y <- min(pe_plot_df$conf.low, na.rm=TRUE)
max_y <- max(pe_plot_df$conf.high, na.rm=TRUE)
text_df <- pe_plot_df %>%
  mutate(text_label = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high))

# Custom d3 color palette. Black as added as first color for GCA in forest plot
d3_colors <- pal_d3()(8)  # Get 8 additional colors (black is first color)
custom_d3 <- c("black", d3_colors)  # Add black as first color

# Create annotated forest plot that includes estimates and CIs
forest_plot <- ggplot(pe_plot_df, aes(x = term, y = estimate, 
                                      ymin = conf.low, ymax = conf.high, 
                                      color = Domain)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(position = position_dodge(width = 0.5), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  facet_wrap(~ Model) +
  ylab("Practice Effect Estimates (SD units)") + xlab("") +
  geom_text(data = text_df, 
            aes(x = term, y = max_y * 1.5, # Adjust position of text (change scale_y_continuous as well)  
                label = text_label),
            hjust = 1,
            color = "black",
            size = 3) +
  scale_y_continuous(limits = c(min_y, max_y * 1.5)) +
  scale_color_manual(values = custom_d3) +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        axis.title = element_text(size = 12))

# Save out forest plot
forestplot_name = paste0("results/forest_plot_", Sys.Date(), ".svg")
ggsave(forestplot_name, forest_plot, width = 16, height = 8, 
       device = "svg", dpi = 300)

# Version of plot without estimates and CIs
forest_plot_no_text <- ggplot(pe_plot_df, aes(x = term, y = estimate, 
                                              ymin = conf.low, ymax = conf.high, 
                                              color = Domain)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(position = position_dodge(width = 0.5), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  facet_wrap(~ Model) +
  ylab("Practice Effect Estimates (SD units)") + xlab("") +
  scale_color_manual(values = custom_d3) +
  theme_bw() +
  theme(text = element_text(family = "Arial"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        axis.title = element_text(, size = 12))


#-------------------------------------------#
#     Prep cognitive factor score data      #
#-------------------------------------------#

# Pivot to long format
factors_raw_long <- factors_raw %>%
  pivot_longer(cols = -VETSAID, 
               names_to = c(".value", "WAVE"), 
               names_pattern = "(.+?)_V(.$)",
               values_drop_na = TRUE) %>%
  mutate(Adjustment = "Unadjusted")

factors_adj_long <- factors_adj %>%
  pivot_longer(cols = -VETSAID, 
               names_to = c(".value", "WAVE"), 
               names_pattern = "(.+?)_V(.$)",
               values_drop_na = TRUE) %>%
  mutate(Adjustment = "PE_corrected")

# Bind raw and adjusted data
factors_long <- factors_raw_long %>% 
  bind_rows(factors_adj_long) %>%
  mutate(WAVE = as.integer(WAVE))

# Pivot longer so that each cognitive factor is in a single column
factors_long_domain <- factors_long %>%
  pivot_longer(cols = -c(VETSAID, WAVE, Adjustment), 
               names_to = "Domain", 
               values_to = "Score")

# Filter and rename factors
factors_long_domain <- factors_long_domain %>%
  filter(Domain %in% c("memory", "commonEF", "fluency", "speed", "vis_mem", "vis_spat")) %>%
  mutate(Domain = recode(Domain, 
                         memory = "Episodic memory",
                         commonEF = "Executive function",
                         fluency = "Fluency",
                         speed = "Processing speed",
                         vis_mem = "Visual memory",
                         vis_spat = "Visuospatial"))

#--------------------------------------------------------------------#
# Descriptive and statistics tests of cognitive factor scores with   #
# and without PE adjustment                                          #
#--------------------------------------------------------------------#

# Get difference scores for each domain
factors_diff <- factors_long_domain %>%
  pivot_wider(names_from = Adjustment, values_from = Score) %>%
  mutate(Diff = Unadjusted - PE_corrected)

# Get summary statistics of difference scores by wave and domain
factors_diff_summary <- factors_diff %>%
  filter(WAVE!=1) %>%
  group_by(Domain, WAVE) %>%
  summarize(
    Mean = mean(Diff, na.rm=T),
    SE = sd(Diff, na.rm=T) / sqrt(n()),
    Min = min(Diff, na.rm=T),
    Max = max(Diff, na.rm=T),
    .groups = "drop") %>% 
  flextable() %>% 
  theme_apa() %>%
  colformat_double(digits=3) %>% 
  merge_v("Domain") %>% 
  valign(j=1, valign="top") %>%
  autofit()

# Save summary table
factors_diff_outname = paste0("results/factors_diff_summary_", Sys.Date(), ".docx")
save_as_docx(factors_diff_summary, path = factors_diff_outname)

# Get summary statistics for only subjects with all WAVES. There should be 4 (1, 2, 3, 4)
factors_diff_summary_4tp <- factors_diff %>%
  group_by(VETSAID) %>%
  add_count() %>%
  filter(n == 24, WAVE!=1) %>% # 4 waves * 6 domains
  group_by(Domain, WAVE) %>%
  summarize(
    Mean = mean(Diff, na.rm=T),
    SE = sd(Diff, na.rm=T) / sqrt(n()),
    Min = min(Diff, na.rm=T),
    Max = max(Diff, na.rm=T),
    .groups = "drop") %>%
  flextable() %>% 
  theme_apa() %>%
  colformat_double(digits=3) %>% 
  merge_v("Domain") %>% 
  valign(j="Domain", valign="top") %>%
  autofit()
  
# Save summary table
factors_diff_4tp_outname = paste0("results/factors_diff_summary_4tp_", Sys.Date(), ".docx")
save_as_docx(factors_diff_summary_4tp, path = factors_diff_4tp_outname)

### Test for differences in adjustment by domain and wave ###
factors_long$WAVE = as.factor(factors_long$WAVE)
factors_long$Adjustment = factor(factors_long$Adjustment, levels = c("Unadjusted", "PE_corrected"))

# Test for differences in adjustment by domain and wave
# Memory
memory_summ = lmer(memory ~ WAVE * Adjustment + (1|VETSAID), data = factors_long) %>% 
  model_parameters() %>% mutate(Domain = "Episodic memory")
# Executive Function
ef_summ = lmer(commonEF ~ WAVE * Adjustment + (1|VETSAID), data = factors_long) %>% 
  model_parameters()  %>% mutate(Domain = "Executive function")
# Fluency
fluency_summ = lmer(fluency ~ WAVE * Adjustment + (1|VETSAID), data = factors_long) %>% 
  model_parameters() %>% mutate(Domain = "Fluency")
# Processing Speed
speed_summ = lmer(speed ~ WAVE * Adjustment + (1|VETSAID), data = factors_long) %>% 
  model_parameters() %>% mutate(Domain = "Processing speed")
# Visual Memory
vis_mem_summ = lmer(vis_mem ~ WAVE * Adjustment + (1|VETSAID), data = factors_long) %>% 
  model_parameters() %>% mutate(Domain = "Visual memory")
# Visuospatial
vis_spat_summ = lmer(vis_spat ~ WAVE * Adjustment + (1|VETSAID), data = factors_long) %>% 
  model_parameters() %>% mutate(Domain = "Visuospatial")

# Combine results
factors_diff_test <- bind_rows(memory_summ, ef_summ, fluency_summ, speed_summ, vis_mem_summ, vis_spat_summ) 

factors_diff_test_table <- factors_diff_test %>% 
  flextable() %>% 
  theme_apa() %>%
  colformat_double(digits=3) %>% 
  merge_v("Domain") %>% 
  valign(j="Domain", valign="top") %>%
  autofit()

# Save summary table
factors_diff_test_outname = paste0("results/factors_diff_test_summary_", Sys.Date(), ".docx")
save_as_docx(factors_diff_test_table, path = factors_diff_test_outname)

#-----------------------------------------------------------------#
#   Plot factor scores trajectories with and without adjustment   #
#-----------------------------------------------------------------#

# Create dataframe with mean and within-subject SE of cognitive factor scores by Adjustment and Domain
factor_score_summary <- factors_long_domain %>%
  # First center scores within subject for each Domain and Adjustment combination
  group_by(VETSAID, Domain, Adjustment) %>%
  mutate(
    subject_mean = mean(Score, na.rm = TRUE),
    Score_centered = Score - subject_mean
  ) %>%
  ungroup() %>%
  # Then calculate group-level statistics 
  group_by(WAVE, Domain, Adjustment) %>%
  summarize(
    Mean = mean(Score, na.rm = TRUE),
    within_var = var(Score_centered, na.rm = TRUE),
    n = sum(!is.na(Score)),
    SE = sqrt(within_var/n),
    .groups = "drop"
  )


# Order domains
factor_score_summary$Domain <- factor(factor_score_summary$Domain, 
                                      levels = c("Episodic memory", 
                                                 "Executive function", 
                                                 "Fluency", 
                                                 "Processing speed", 
                                                 "Visual memory", 
                                                 "Visuospatial"))

# Create line plot of cognitive score trajectories across wave. Facet by domain.
factor_score_summary_plot <- ggplot(factor_score_summary, aes(x = WAVE, y = Mean, color=Domain)) +
  geom_point(aes(shape = Adjustment)) +
  geom_line(aes(linetype = Adjustment)) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  facet_wrap(~ Domain, scales = "free_y") +
  ylab("Score (SD units)") + xlab("Wave") +
  scale_color_d3(guide = "none") + # Use standard d3 palette because no GCA
  theme_bw(14) +
  theme(text = element_text(family = "Arial"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 18),
        axis.title = element_text(size = 18))

# Save plot
factor_score_outname = paste0("results/factor_score_plot_", Sys.Date(), ".svg")
ggsave(factor_score_outname, factor_score_summary_plot, width = 12, height = 8, 
       device = "svg", dpi = 300)


# Create dataframe with mean and within-subject SE of cognitive factor scores by Adjustment and Domain
# Only include subjects with all 4 assessments
factor_score_summary_4tp <- factors_long_domain %>%
  group_by(VETSAID) %>%
  add_count() %>%
  filter(n==48) %>% # 4 waves * 6 domains * 2 adjustments
  # First center scores within subject for each Domain and Adjustment combination
  group_by(VETSAID, Domain, Adjustment) %>%
  mutate(
    subject_mean = mean(Score, na.rm = TRUE),
    Score_centered = Score - subject_mean
  ) %>%
  ungroup() %>%
  # Then calculate group-level statistics
  group_by(WAVE, Domain, Adjustment) %>%
  summarize(
    Mean = mean(Score, na.rm = TRUE),
    within_var = var(Score_centered, na.rm = TRUE),
    n = sum(!is.na(Score)),
    SE = sqrt(within_var/n),
    .groups = "drop"
  )


# Order domains
factor_score_summary_4tp$Domain <- factor(factor_score_summary_4tp$Domain, 
                                      levels = c("Episodic memory", 
                                                 "Executive function", 
                                                 "Fluency", 
                                                 "Processing speed", 
                                                 "Visual memory", 
                                                 "Visuospatial"))

# Create line plot of cognitive score trajectories for people with all 4 waves
factor_score_summary_plot_4tp <- ggplot(factor_score_summary_4tp, aes(x = WAVE, y = Mean, color=Domain)) +
  geom_point(aes(shape = Adjustment)) +
  geom_line(aes(linetype = Adjustment)) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  facet_wrap(~ Domain, scales = "free_y") +
  ylab("Score (SD units)") + xlab("Wave") +
  scale_color_d3(guide = "none") + # Use standard d3 palette because no GCA
  theme_bw(14) +
  theme(text = element_text(family = "Arial"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 18),
        axis.title = element_text(size = 18))

# Save plot
factor_score_plot_4tp_outname = paste0("results/factor_score_plot_4tp_", Sys.Date(), ".svg")
ggsave(factor_score_plot_4tp_outname, factor_score_summary_plot_4tp, width = 12, height = 8, 
       device = "svg", dpi = 300)


#--------------------------#
#     Prep MCI dx  data    #
#--------------------------#

# Join all waves of diagnosis data based on raw scores
mci_raw <- mci_v1_raw %>%
  select(VETSAID, rMCI_cons_V1) %>%
  full_join(mci_v2_raw %>% select(VETSAID, rMCI_cons_V2), by = "VETSAID") %>%
  full_join(mci_v3_raw %>% select(VETSAID, rMCI_cons_V3), by = "VETSAID") %>%
  full_join(mci_v4_raw %>% select(VETSAID, rMCI_cons_V4), by = "VETSAID") %>%
  # filter(!is.na(rMCI_cons_V1) & !is.na(rMCI_cons_V2) & !is.na(rMCI_cons_V3) & !is.na(rMCI_cons_V4)) %>%
  mutate(Adjustment = "Uncorrected")

# Join all waves of diagnosis data based on adjusted scores
mci_adj <- mci_v1_adj %>%
  select(VETSAID, rMCI_cons_V1) %>%
  full_join(mci_v2_adj %>% select(VETSAID, rMCI_cons_V2), by = "VETSAID") %>%
  full_join(mci_v3_adj %>% select(VETSAID, rMCI_cons_V3), by = "VETSAID") %>%
  full_join(mci_v4_adj %>% select(VETSAID, rMCI_cons_V4), by = "VETSAID") %>%
  # filter(!is.na(rMCI_cons_V1) & !is.na(rMCI_cons_V2) & !is.na(rMCI_cons_V3) & !is.na(rMCI_cons_V4)) %>%
  mutate(Adjustment = "PE-corrected")

# Create anyMCI variables for raw data
mci_raw <- mci_raw %>%
  mutate(anyMCI_V1 = ifelse(rMCI_cons_V1 > 0, 1, 0),
         anyMCI_V2 = ifelse(rMCI_cons_V2 > 0, 1, 0),
         anyMCI_V3 = ifelse(rMCI_cons_V3 > 0, 1, 0),
         anyMCI_V4 = ifelse(rMCI_cons_V4 > 0, 1, 0))

# Create anyMCI variables for adjusted data
mci_adj <- mci_adj %>%
  mutate(anyMCI_V1 = ifelse(rMCI_cons_V1 > 0, 1, 0),
         anyMCI_V2 = ifelse(rMCI_cons_V2 > 0, 1, 0),
         anyMCI_V3 = ifelse(rMCI_cons_V3 > 0, 1, 0),
         anyMCI_V4 = ifelse(rMCI_cons_V4 > 0, 1, 0))

# Bind raw and adjusted data
mci_long <- mci_raw %>% 
  bind_rows(mci_adj)

# Convert rMCI_cons and anyMCI to factor.
# Levels for anyMCI are: 0 = CU, 1 = MCI
# Levels for rMCI_cons are: 0 = CU, 1 = nonamn sMCI, 2 = amn sMCI, 3 = nonamn mMCI, 4 = amn mMCI
mci_long <- mci_long %>%
  mutate_at(vars(starts_with("rMCI_cons")), 
            ~ factor(., levels = c(0, 1, 2, 3, 4), 
                     labels = c("CU", "nonamn sMCI", "amn sMCI", "nonamn mMCI", "amn mMCI"))) %>%
  mutate_at(vars(starts_with("anyMCI")), 
            ~ factor(., levels = c(0, 1), labels = c("CU", "MCI"))) %>%
  mutate(Adjustment = factor(Adjustment, levels = c("Uncorrected", "PE-corrected"))) 



#---------------------------------#
#     Summary tables of MCI dx    #
#---------------------------------#

# Get rates of MCI categories at each wave by adjustment status
rmci_vars <- mci_long %>% select(contains("rMCI")) %>% names()
rmci_tab <- print(CreateTableOne(vars = rmci_vars, 
                                 strata = "Adjustment", 
                                 data = mci_long, 
                                 test = FALSE), 
      quote = FALSE, noSpaces = TRUE, printToggle = TRUE) 
# Save out summary table
rmci_tab_outfile = paste0("results/mci_rates_rMCI_", Sys.Date(), ".csv")
write.csv(rmci_tab, rmci_tab_outfile)

# Get rates of any MCI at each wave by adjustment status
anymci_vars <- mci_long %>% select(contains("anyMCI")) %>% names()
anymci_tab <- print(CreateTableOne(vars = anymci_vars, 
                                 strata = "Adjustment", 
                                 data = mci_long, 
                                 test = TRUE), 
                  quote = FALSE, noSpaces = TRUE, printToggle = TRUE) 
anymci_tab_outfile = paste0("results/mci_rates_anyMCI_", Sys.Date(), ".csv")
write.csv(anymci_tab, anymci_tab_outfile)

# Run chisq test for each wave
for (i in 1:4) {
  wave_var <- paste0("anyMCI_V", i)
  chisq_test <- chisq.test(table(mci_long$Adjustment, mci_long[[wave_var]]))
  print(paste("Chi-squared test for wave", i))
  print(chisq_test)
}

#------------------------#
#     Plots of MCI dx    #
#------------------------#

### Any MCI ###

# Step 1: Calculate MCI percentages by wave and adjustment method
anymci_percentages <- mci_long %>%
  group_by(Adjustment) %>%
  summarize(
    # For factor variables, we count the proportion where the factor equals "MCI"
    MCI_V1 = mean(anyMCI_V1 == "MCI", na.rm = TRUE) * 100,
    MCI_V2 = mean(anyMCI_V2 == "MCI", na.rm = TRUE) * 100,
    MCI_V3 = mean(anyMCI_V3 == "MCI", na.rm = TRUE) * 100,
    MCI_V4 = mean(anyMCI_V4 == "MCI", na.rm = TRUE) * 100,
  )

# Step 2: Convert to long format for plotting
anymci_percentages <- anymci_percentages %>%
  pivot_longer(
    cols = starts_with("MCI_"),
    names_to = "Wave",
    values_to = "Percentage"
  ) %>%
  mutate(Wave = factor(Wave, 
                       levels = c("MCI_V1", "MCI_V2", "MCI_V3", "MCI_V4"),
                       labels = c("1", "2", "3", "4")))

# Step 3: Create bar plot
anymci_plot <- ggplot(anymci_percentages, aes(x = Wave, y = Percentage, fill = Adjustment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 5) +
  labs(
    x = "Wave",
    y = "MCI diagnosis (%)",
    fill = NULL
  ) +
  scale_fill_manual(values = c("gray74","gray33")) + 
  theme_pubr() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18, face = "bold")
  )

# Save plot
anymci_plot_outname = paste0("results/mci_rates_plot_anyMCI_", Sys.Date(), ".svg")
ggsave(anymci_plot_outname, anymci_plot, width = 12, height = 8, 
       device = "svg", dpi = 300)

