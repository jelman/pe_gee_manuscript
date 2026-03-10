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
library(geepack)

#---------------------------------#
# Set directories and load data   #
#---------------------------------#

# Set working directory
setwd("~/netshare/M/Projects/PracEffects_GEE")

# Load admin file
admin <- read_sas("~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/Admin/vetsa_admin_file_20250205.sas7bdat", NULL)

# Load model estimates of practice effects
pe_estimates <- read.csv("results/gee_standardized_results_complete_2026-03-10_covariates.csv")

# Load cognitive test names
cog_test_names <- read.csv("docs/test_abbreviations.csv")
# Add superscripts to specific measure values
cog_test_names <- cog_test_names %>%
  mutate(measure = case_when(
    term == "BNTTOTCOR" ~ paste0(measure, "*"),
    term == "SSPTOTP" ~ paste0(measure, "†"),
    TRUE ~ measure
  ))



#-------------------------------------------#
#     Prep practice effect estimate data    #
#-------------------------------------------#

### Define order of scores and scores groups ###

# Select scores used for cognitive factor scores. Additional tests used for MCI
# dx are included at the end.
episodic_cog_tests <- c("CVATOT","CVSDFR","CVLDFR","LMITOT","LMDTOT","VRITOT","VRDTOT")
ef_cog_tests <- c("TRL4TLOG","CSSACC","STRIT","DSTOT","LNTOT","RSATOT")
fluency_cog_tests <- c("LFCOR","CFCOR","CSCOR")
speed_cog_tests <- c("TRL2TLOG","TRL3TLOG","STRWRAW","STRCRAW","SRTGMEANLOG","CHRTGMEANLOG")
visspat_cog_tests <- c("MR1COR","HFTOTCOR","AFQTBXPCTTRAN_R")
other_cog_tests <- c("TRL1TLOG","SSPTOTP","MTXAGE","VRCTOT","BNTTOTCOR")

# Gather all domain scores
cog_tests <- c(episodic_cog_tests, ef_cog_tests, fluency_cog_tests, 
               speed_cog_tests, visspat_cog_tests, other_cog_tests)


### Prep data ###

# Pivot to long format
pe_estimates_long <- pe_estimates %>%
  pivot_longer(cols = -outcome, 
               names_to = c(".value", "Assessment"), 
               names_pattern = "(.+?)_(.+)") %>%
  rename(term = outcome) %>%
  filter(term %in% cog_tests)

# Copy estimates for BNTTOTCOR from assessment WAVE4 to WAVE2_ASSESSMENT2.
# The BNT was administered starting in wave 3. Thus, the estimate for WAVE4 
# corresponds to the practice effect at the second assessment, which occurred 
# at wave 4. A note in the figure captin will indicate this
pe_estimates_long <- pe_estimates_long %>%
  mutate(estimate = ifelse(term == "BNTTOTCOR" & Assessment == "WAVE2_ASSESSMENT2",
                           pe_estimates_long$estimate[pe_estimates_long$term == "BNTTOTCOR" & pe_estimates_long$Assessment == "WAVE4"],
                           estimate),
         conf.low = ifelse(term == "BNTTOTCOR" & Assessment == "WAVE2_ASSESSMENT2",
                           pe_estimates_long$conf.low[pe_estimates_long$term == "BNTTOTCOR" & pe_estimates_long$Assessment == "WAVE4"],
                           conf.low),
         conf.high = ifelse(term == "BNTTOTCOR" & Assessment == "WAVE2_ASSESSMENT2",
                            pe_estimates_long$conf.high[pe_estimates_long$term == "BNTTOTCOR" & pe_estimates_long$Assessment == "WAVE4"],
                            conf.high),
         p.value = ifelse(term == "BNTTOTCOR" & Assessment == "WAVE2_ASSESSMENT2",
                          pe_estimates_long$p.value[pe_estimates_long$term == "BNTTOTCOR" & pe_estimates_long$Assessment == "WAVE4"],
                          p.value)
         )

# Add domain to pe_estimates_long. If term is in one of the lists above, assign the domain
pe_estimates_long$Domain <- pe_estimates_long$term %>%
  recode_factor(
    !!!setNames(rep("Episodic memory", length(episodic_cog_tests)), episodic_cog_tests),
    !!!setNames(rep("Executive function", length(ef_cog_tests)), ef_cog_tests),
    !!!setNames(rep("Fluency", length(fluency_cog_tests)), fluency_cog_tests),
    !!!setNames(rep("Processing speed", length(speed_cog_tests)), speed_cog_tests),
    !!!setNames(rep("Visuospatial", length(visspat_cog_tests)), visspat_cog_tests),
    !!!setNames(rep("Other", length(other_cog_tests)), other_cog_tests)
  )

# Set order of domain to be Episodic memory, Executive function, Fluency, Processing speed, Visuospatial, Other
pe_estimates_long$Domain <- factor(pe_estimates_long$Domain, 
                                   levels = c("Episodic memory", 
                                              "Executive function", 
                                              "Fluency", 
                                              "Processing speed", 
                                              "Visuospatial",
                                              "Other"))

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
    "WAVE3_ASSESSMENT2" = "Follow-up 1 (wave 4)",
    "WAVE4" = "Follow-up 1 (wave 4)",
    "WAVE4_ASSESSMENT3" = "Follow-up 2 (wave 4)",
  )

models_of_interest <- c("WAVE2_ASSESSMENT2",  
                        "WAVE3_ASSESSMENT3", 
                        "WAVE4_ASSESSMENT4")

# Merge in test names
pe_estimates_long <- pe_estimates_long %>%
  left_join(cog_test_names, by = "term") 

pe_plot_df <- pe_estimates_long %>%
  filter(Assessment %in% models_of_interest,
         term %in% cog_tests) %>%
  mutate(Model = factor(Model),
         Assessment = factor(Assessment)) %>%
  arrange(desc(Domain), desc(measure)) %>%  # Arrange data by domain and term in descending order
  mutate(measure = factor(measure, levels = unique(measure)))  # Set order of terms



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
  colformat_double(digits=2) %>% 
  merge_v("Domain") %>% 
  valign(j="Domain", valign="top") %>%
  autofit()

# Save summary table
pe_summary_outname = paste0("results/pe_summary_", Sys.Date(), "_covariates.docx")
save_as_docx(pe_summary, path = pe_summary_outname)

#-----------------------------------#
# Plot practice effects estimates   #
#-----------------------------------#

# Create annotation dataframe with max_y variable for positioning
min_y <- min(pe_plot_df$conf.low, na.rm=TRUE)
max_y <- max(pe_plot_df$conf.high, na.rm=TRUE)
text_df <- pe_plot_df %>%
  mutate(text_label = str_glue(" 
                              {format(round(estimate, 2), nsmall = 2)}, ({format(round(conf.low, 2), nsmall = 2)}, {format(round(conf.high, 2), nsmall = 2)})
                              "))

# Custom d3 color palette. Black as added as first color for GCA in forest plot
d3_colors <- pal_d3()(5)  # Get 5  colors for cognitive domains
custom_d3 <- c(d3_colors, "#7F7F7FFF")  # Add gray to end for Other tests
# Should produce: "#1F77B4FF" "#FF7F0EFF" "#2CA02CFF" "#D62728FF" "#9467BDFF" "#7F7F7FFF"

# Create annotated forest plot that includes estimates and CIs
forest_plot <- ggplot(pe_plot_df, aes(x = measure, y = estimate, 
                                      ymin = conf.low, ymax = conf.high, 
                                      color = Domain)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(linewidth = 1, position = position_dodge(width = 0.5), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  facet_wrap(~ Model) +
  ylab("Practice Effect Estimates (SD units)") + xlab("") +
  geom_text(data = text_df, 
            aes(x = measure, y = max_y * 1.5, # Adjust position of text (change scale_y_continuous as well)  
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
        legend.text = element_text(face = "bold", size = 14),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Save out forest plot
forestplot_name = paste0("results/forest_plot_", Sys.Date(), "_covariates.tiff")
ggsave(forestplot_name, forest_plot, width = 16, height = 8, 
       device = "tiff", dpi = 300)

