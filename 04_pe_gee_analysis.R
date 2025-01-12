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
loadfonts(device = 'win')
library(knitr)
library(kableExtra)


#---------------------------------#
# Set directories and load data   #
#---------------------------------#

# Set working directory
setwd("M:/Projects/PracEffects_GEE")

# Load model estimates of practice effects
pe_estimates <- read.csv("data/gee_standardized_results_2025-01-11.csv")

# Load raw and adjusted cognitive test scores
tests_raw <- read.csv("data/V1V2V3V4_cog_data_raw_2025-01-08.csv")
tests_adj <- read.csv("data/V1V2V3V4_cog_data_pe_adjusted_2025-01-08.csv")

# Load raw and adjusted cognitive factor scores
factors_raw <- read.csv("data/V1V2V3V4_cog_factor_scores_raw_2024-01-08.csv")
factors_adj <- read.csv("data/V1V2V3V4_cog_factor_scores_pe_adjusted_2024-01-08.csv")

# Load raw and adjusted MCI diagnosis
# mci_raw <- read.csv()
# mci_adj <- read.csv()


#-------------------#
#     Prep data     #
#-------------------#

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

### Prep data ###

# Pivot to long format
pe_estimates_long <- pe_estimates %>%
  pivot_longer(cols = -outcome, 
               names_to = c(".value", "assessment"), 
               names_pattern = "(.+?)_(.+)") %>%
  rename(term = outcome) %>%
  filter(term %in% order_of_tests)

# Add domain to pe_estimates_long. If term is in one of the lists above, assign the domain
pe_estimates_long$domain <- pe_estimates_long$term %>%
  recode_factor(
    !!!setNames(rep("GCA", length(gca_tests)), gca_tests),
    !!!setNames(rep("Episodic memory", length(epmem_tests)), epmem_tests),
    !!!setNames(rep("Visual memory", length(vismem_tests)), vismem_tests),
    !!!setNames(rep("Executive function", length(ef_tests)), ef_tests),
    !!!setNames(rep("Fluency", length(fluency_tests)), fluency_tests),
    !!!setNames(rep("Processing speed", length(speed_tests)), speed_tests),
    !!!setNames(rep("Visuospatial", length(visspat_tests)), visspat_tests)
  )

# Set order of domain to be GCA, Episodic memory, Visual memory, Executive function, Fluency, Processing speed, Visuospatial
pe_estimates_long$domain <- factor(pe_estimates_long$domain, 
                                   levels = c("GCA", "Episodic memory", 
                                              "Visual memory", "Executive function", 
                                              "Fluency", "Processing speed", 
                                              "Visuospatial"))

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
  filter(assessment %in% models_of_interest,
         term %in% order_of_tests) %>%
  mutate(model = factor(model),
         assessment = factor(assessment)) %>%
  arrange(desc(domain), desc(term)) %>%  # Arrange data by domain and term in descending order
  mutate(term = factor(term, levels = unique(term)))  # Set order of terms

#---------------------------------------------------------------#
# Descriptive statistics of estimated practice effects          #
#---------------------------------------------------------------#

# Summary table of practice effect estimates
pe_summary <- pe_plot_df %>%
  group_by(domain, model) %>%
  summarize(
    n = n(),
    mean_est = round(mean(estimate), 2),
    median_est = round(median(estimate), 2),
    sd_est = round(sd(estimate), 2),
    min_est = round(min(estimate), 2),
    max_est = round(max(estimate), 2),
    n_sig = sum(p.value < 0.05),
    .groups = "drop"
  ) %>%
  arrange(domain, model) %>%
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

# Custom d3 color palette
d3_colors <- pal_d3()(6)  # Get 4 d3 colors
custom_d3 <- c("black", d3_colors)  # Replace first color with black

# Create annotated forest plot that includes estimates and CIs
forest_plot <- ggplot(pe_plot_df, aes(x = term, y = estimate, 
                                      ymin = conf.low, ymax = conf.high, 
                                      color = domain)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(position = position_dodge(width = 0.5), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  facet_wrap(~ model) +
  ylab("Standardized Practice Effect Estimates") + xlab("") +
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
        axis.title = element_text(, size = 12))

# Save out plot
ggsave("results/forest_plot.png", forest_plot, width = 16, height = 8, dpi = 300)

# Version of plot without estimates and CIs
forest_plot_no_text <- ggplot(pe_plot_df, aes(x = term, y = estimate, 
                                              ymin = conf.low, ymax = conf.high, 
                                              color = domain)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(position = position_dodge(width = 0.5), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  facet_wrap(~ model) +
  ylab("Standardized Practice Effect Estimates") + xlab("") +
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