# R script to plot cognitive factor scores (raw and PE-adjusted) by age across 
# 4 waves of VETSA data.

library(ggplot2)
library(tidyverse)
library(gamm4)


# Set working directory
setwd("~/netshare/M/Projects/PracEffects_GEE")


# Load raw and adjusted cognitive factor scores
factors_raw <- read.csv("data/output_data/V1V2V3V4_cog_factor_scores_raw_2025-10-24.csv")
factors_adj <- read.csv("data/output_data/V1V2V3V4_cog_factor_scores_pe-adjusted_2025-10-24.csv")

# Load admin file
admin <- read_sas("~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/Admin/vetsa_admin_file_20250205.sas7bdat", NULL)

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
  mutate(Adjustment = "PE-adjusted")

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
  filter(Domain %in% c("memory", "commonEF", "fluency", "speed", "vis_spat")) %>%
  mutate(Domain = recode(Domain, 
                         memory = "Episodic memory",
                         commonEF = "Executive function",
                         fluency = "Fluency",
                         speed = "Processing speed",
                         vis_spat = "Visuospatial"))


# Create long format data of AGE from admin file. Use _V# suffix to define values of WAVE
age_long <- admin %>%
  select(vetsaid, AGE_V1, AGE_V2, AGE_V3, AGE_V4) %>%
  pivot_longer(cols = starts_with("AGE_V"), 
               names_to = "WAVE", 
               names_prefix = "AGE_V", 
               values_to = "AGE",
               values_drop_na = TRUE) %>%
  mutate(WAVE = as.integer(WAVE))

# Add age to factors_long_domain for age-based plots
factors_long_domain = factors_long_domain %>% 
  left_join(age_long, by=c("VETSAID"="vetsaid","WAVE"))

# Plot factor scores by age, with different lines for adjustment. Use smoothed lines. 
factor_score_age_plot <- ggplot(factors_long_domain, aes(x = AGE, y = Score, color=Adjustment)) +
  geom_smooth(method = "loess", linewidth = 1.2, se = TRUE) +
  # geom_smooth(method = "lm", formula = y ~ poly(x, 3), linewidth = 1.2, se = TRUE) +
  facet_wrap(~ Domain, scales = "free_y") +
  ylab("Score (SD units)") + xlab("Age (years)") +
  scale_color_d3() + 
  theme_bw(18) +
  ggtitle("Factors scores by age: all participants") +
  theme(text = element_text(family = "Arial"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 18),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 18))

# Plot factors scores by age, with different lines for adjustment. Use smoothed lines. Only
# include participants with 4 waves of data
factor_score_age_plot_4tp <- factors_long_domain %>%
  group_by(VETSAID) %>%
  add_count() %>%
  filter(n==40) %>% # 4 waves * 5 domains * 2 adjustments
  ggplot(aes(x = AGE, y = Score, color=Adjustment)) +
  geom_smooth(method = "loess", linewidth = 1.2, se = TRUE) +
  # geom_smooth(method = "lm", formula = y ~ poly(x, 3), linewidth = 1.2, se = TRUE) +
  facet_wrap(~ Domain, scales = "free_y") +
  ylab("Score (SD units)") + xlab("Age (years)") +
  scale_color_d3() + 
  theme_bw(18) +
  ggtitle("Factors scores by age: participants with 4 waves") +
  theme(text = element_text(family = "Arial"),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(face = "bold", size = 18),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 18))

# Fit and plot a GAMM of factor scores by age for each Domain, with different lines for adjustment. 
gamm_results <- list()
for (domain in unique(factors_long_domain$Domain)) {
  domain_data <- factors_long_domain %>%
    filter(Domain == domain) %>%
    mutate(Adjustment = as.factor(Adjustment))
  gamm_model <- gamm4(Score ~ s(AGE, by = Adjustment) + Adjustment, 
                      random = ~(1|VETSAID), 
                      data = domain_data)
  gamm_results[[domain]] <- gamm_model
}

# Plot results of gamm models
gamm_plots <- list()
for (domain in names(gamm_results)) {
  domain_data <- factors_long_domain %>%
    filter(Domain == domain) %>%
    mutate(Adjustment = as.factor(Adjustment))
  pred_data <- expand.grid(
    AGE = seq(min(domain_data$AGE, na.rm=TRUE), max(domain_data$AGE, na.rm=TRUE), length.out = 100),
    Adjustment = levels(domain_data$Adjustment)
  )
  pred_data$Score <- predict(gamm_results[[domain]]$gam, newdata = pred_data)
  p <- ggplot() +
    geom_line(data = pred_data, aes(x = AGE, y = Score, color = Adjustment), size = 1.2) +
    ylab("Score (SD units)") + xlab("Age (years)") +
    scale_color_d3() +
    theme_bw(18) +
    theme(text = element_text(family = "Arial"),
          panel.grid.major.y = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(face = "bold", size = 18),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 18),
          axis.title = element_text(face = "bold", size = 18))
  gamm_plots[[domain]] <- p
}

# Plot only unadjusted factor scores by age. Use smoothed lines. Add lines for 
# each individual Only plot a subset of the individuals though, because there are too many.
set.seed(123)  # For reproducibility
subset_ids <- sample(unique(factors_long_domain$VETSAID), 1000)  # Sample 100 unique VETSAIDs
factor_score_age_plot_unadj <- factors_long_domain %>%
  filter(Adjustment == "PE-adjusted",
         VETSAID %in% subset_ids) %>%
  ggplot(aes(x = AGE, y = Score, group = VETSAID)) +
  geom_line(alpha = 0.2, color = "black") +
  geom_smooth(aes(group=1), method = "loess", color = "blue", size = 1.5, se = TRUE) +
  facet_wrap(~ Domain, scales = "free_y") +
  ylim(c(-2.5,2.5)) +
  ylab("Score (SD units)") + xlab("Age (years)") +
  theme_bw(18) +
  ggtitle("PE-adjusted factor scores by age: subset of participants") +
  theme(text = element_text(family = "Arial"),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 18))


# Plot only unadjusted factor scores by age for subjects with 4 waves. Use smoothed lines. Add lines for 
# each individual Only plot a subset of the individuals though, because there are too many.
factor_score_age_plot_unadj <- factors_long_domain %>%
  group_by(VETSAID) %>%
  add_count() %>%
  filter(n==40) %>% # 4 waves * 5 domains * 2 adjustments
  filter(Adjustment == "Unadjusted") %>%
  ggplot(aes(x = AGE, y = Score, group = VETSAID)) +
  geom_line(alpha = 0.2, color = "black") +
  geom_smooth(aes(group=1), method = "loess", color = "blue", size = 1.5, se = TRUE) +
  facet_wrap(~ Domain, scales = "free_y") +
  ylim(c(-2.5,2.5)) +
  ylab("Score (SD units)") + xlab("Age (years)") +
  theme_bw(18) +
  ggtitle("Unadjusted factor scores by age: subset of participants") +
  theme(text = element_text(family = "Arial"),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 18),
        axis.title = element_text(face = "bold", size = 18))