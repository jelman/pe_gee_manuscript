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

#---------------------------------#
# Set directories and load data   #
#---------------------------------#

# Set working directory
setwd("~/netshare/M/Projects/PracEffects_GEE")

# Load model estimates of practice effects
pe_estimates <- read.csv("results/gee_standardized_results_2025-04-17.csv")

# Load raw and adjusted cognitive test scores
tests_raw <- read.csv("data/raw_data/V1V2V3V4_cog_data_raw_2025-04-17.csv")
tests_adj <- read.csv("data/raw_data/V1V2V3V4_cog_data_pe-adjusted_2025-04-17.csv")

# Load raw and adjusted cognitive factor scores
factors_raw <- read.csv("data/created_data/V1V2V3V4_cog_factor_scores_raw_2025-04-17.csv")
factors_adj <- read.csv("data/created_data/V1V2V3V4_cog_factor_scores_pe-adjusted_2025-04-17.csv")

# Load raw and adjusted MCI diagnosis
mci_v1_raw <- read.csv("data/created_data/Unadjusted/MCI_05a_vetsa1_final_2025_01_23.csv")
mci_v2_raw <- read.csv("data/created_data/Unadjusted/MCI_05b_vetsa2_final_2025_01_15.csv")
mci_v3_raw <- read.csv("data/created_data/Unadjusted/MCI_05c_vetsa3_final_2025_01_15.csv")
mci_v4_raw <- read.csv("data/created_data/Unadjusted/MCI_05d_vetsa4_final_2025_01_16.csv")

mci_v1_adj <- read.csv("data/created_data/Adjusted/MCI_05a_vetsa1_final_2025_01_23.csv")
mci_v2_adj <- read.csv("data/created_data/Adjusted/MCI_05b_vetsa2_final_2025_01_23.csv")
mci_v3_adj <- read.csv("data/created_data/Adjusted/MCI_05c_vetsa3_final_2025_01_23.csv")
mci_v4_adj <- read.csv("data/created_data/Adjusted/MCI_05d_vetsa4_final_2025_01_23.csv")

#-------------------------------------------#
#     Prep practice effect estimate data    #
#-------------------------------------------#

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
  ef = c("Executive function", ef_tests[1], ef_tests[length(ef_tests)]),
  fluency = c("Fluency", fluency_tests[1], fluency_tests[length(fluency_tests)]),
  speed = c("Processing speed", speed_tests[1], speed_tests[length(speed_tests)]),
  vismem = c("Visual memory", vismem_tests[1], vismem_tests[length(vismem_tests)]),
  visspat = c("Visuospatial", visspat_tests[1], visspat_tests[length(visspat_tests)])
)

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
    !!!setNames(rep("Visual memory", length(vismem_tests)), vismem_tests),
    !!!setNames(rep("Visuospatial", length(visspat_tests)), visspat_tests)
  )

# Set order of domain to be GCA, Episodic memory, Visual memory, Executive function, Fluency, Processing speed, Visuospatial
pe_estimates_long$Domain <- factor(pe_estimates_long$Domain, 
                                   levels = c("GCA", "Episodic memory", 
                                              "Executive function", 
                                              "Fluency", "Processing speed", 
                                              "Visual memory", "Visuospatial"))

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
  kbl() %>%
  kable_classic(full_width=FALSE, html_font="Times New Roman") %>%
  collapse_rows(columns = 1, valign = "top")

#-----------------------------------#
# Plot practice effects estimates   #
#-----------------------------------#

# Create annotation dataframe with max_y variable for positioning
max_y <- max(pe_plot_df$conf.high)
text_df <- pe_plot_df %>%
  mutate(text_label = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high))

# Custom d3 color palette. Black as added as first color for GCA in forest plot
d3_colors <- pal_d3()(6)  # Get 6 d3 colors for 6 domains
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
  scale_y_continuous(limits = c(min(pe_plot_df$conf.low), max_y * 1.5)) +
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
forestplot_name = paste0("results/forest_plot_", Sys.Date(), "_")
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
  kbl(digits=3) %>%
  kable_classic(full_width=FALSE, html_font="Times New Roman") %>%
  collapse_rows(columns = 1, valign = "top")

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
  kbl(digits=3) %>%
  kable_classic(full_width=FALSE, html_font="Times New Roman") %>%
  collapse_rows(columns = 1, valign = "top")
  
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

factors_diff_test %>% 
  kbl(digits=3) %>%
  kable_classic(full_width=FALSE, html_font="Times New Roman") %>%
  collapse_rows(columns = 1, valign = "top")

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
factor_score_outname = paste0("results/factor_score_plot_", Sys.Date(), "_")
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
ggsave("results/factor_score_plot_color_4tp.svg", factor_score_summary_plot_4tp, width = 12, height = 8, 
       device = "svg", dpi = 300)


#--------------------------#
#     Prep MCI dx  data    #
#--------------------------#

# Join all waves of diagnosis data based on raw scores
mci_raw <- mci_v1_raw %>%
  select(vetsaid, MCI_cons_V1) %>%
  full_join(mci_v2_raw %>% select(vetsaid, MCI_cons_V2), by = "vetsaid") %>%
  full_join(mci_v3_raw %>% select(vetsaid, MCI_cons_V3), by = "vetsaid") %>%
  full_join(mci_v4_raw %>% select(vetsaid, MCI_cons_V4), by = "vetsaid") %>%
  # filter(!is.na(MCI_cons_V1) & !is.na(MCI_cons_V2) & !is.na(MCI_cons_V3) & !is.na(MCI_cons_V4)) %>%
  mutate(Adjustment = "Unadjusted")

# Join all waves of diagnosis data based on adjusted scores
mci_adj <- mci_v1_adj %>%
  select(vetsaid, MCI_cons_V1) %>%
  full_join(mci_v2_adj %>% select(vetsaid, MCI_cons_V2), by = "vetsaid") %>%
  full_join(mci_v3_adj %>% select(vetsaid, MCI_cons_V3), by = "vetsaid") %>%
  full_join(mci_v4_adj %>% select(vetsaid, MCI_cons_V4), by = "vetsaid") %>%
  # filter(!is.na(MCI_cons_V1) & !is.na(MCI_cons_V2) & !is.na(MCI_cons_V3) & !is.na(MCI_cons_V4)) %>%
  mutate(Adjustment = "PE_corrected")

# Create anyMCI variables for raw data
mci_raw <- mci_raw %>%
  mutate(anyMCI_V1 = ifelse(MCI_cons_V1 > 0, 1, 0),
         anyMCI_V2 = ifelse(MCI_cons_V2 > 0, 1, 0),
         anyMCI_V3 = ifelse(MCI_cons_V3 > 0, 1, 0),
         anyMCI_V4 = ifelse(MCI_cons_V4 > 0, 1, 0))

# Create anyMCI variables for adjusted data
mci_adj <- mci_adj %>%
  mutate(anyMCI_V1 = ifelse(MCI_cons_V1 > 0, 1, 0),
         anyMCI_V2 = ifelse(MCI_cons_V2 > 0, 1, 0),
         anyMCI_V3 = ifelse(MCI_cons_V3 > 0, 1, 0),
         anyMCI_V4 = ifelse(MCI_cons_V4 > 0, 1, 0))

# Bind raw and adjusted data
mci_long <- mci_raw %>% 
  bind_rows(mci_adj)

# Convert MCI_cons to factor 
mci_long <- mci_long %>%
  mutate_at(vars(starts_with("MCI_cons")), as.factor)

# Get rates of MCI at each wave by adjustment status
mci_vars <- mci_long %>% select(contains("MCI")) %>% names()
CreateTableOne(vars = mci_vars, 
               strata = "Adjustment", data = mci_long, test = FALSE) 
