library(dplyr)
library(tidyr)
library(tableone)
library(ComplexUpset)
library(ggplot2)
library(ggridges)
library(haven)
library(forcats)
library(labelled)
library(ggpubr)
library(effectsize)

# Set working directory
setwd("~/netshare/M/Projects/PracEffects_GEE")

#---------------#
#   Load data   #
#---------------#

# Load the admin data
admin = haven::read_sas("~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/Admin/vetsa_admin_file_20250205.sas7bdat")
names(admin) <- toupper(names(admin))

# Load age 20 afqt data
afqt_20 = read.csv("~/netshare/M/NAS VETSA MASTER DATAFILES/Other cognitive measures/AFQT--age 20 cannot be distributed outside VETSA/AFQT_Age20_2020_05_19_revised.csv")
names(afqt_20) <- toupper(names(afqt_20))

# Load Charlson index
charlson <- read.csv("~/netshare/M/NAS VETSA MASTER DATAFILES/Other data/Morbidity Data/Charlson Morbidity Measure/charlson_v1v2v3v4.csv")
names(charlson) <- toupper(names(charlson))

# Pivot Charlson to long format. The suffix should indicate wave.
charlson_long = charlson %>% 
  select(VETSAID, starts_with("CHARLSON")) %>%
  pivot_longer(cols=-VETSAID, 
               names_to=c(".value","WAVE"), 
               names_sep="_",
               values_drop_na=TRUE)  %>%
  mutate(WAVE = as.integer(gsub("V","",WAVE)))


# Load individual test data
tests_adj <- read.csv("data/raw_data/V1V2V3V4_cog_data_pe-adjusted_2025-05-17.csv")

# Load cognitive factor scores
factors_adj <- read.csv("data/output_data/V1V2V3V4_cog_factor_scores_pe-adjusted_2025-10-24.csv")

# Load and merge MCI data
mci_v1_adj <- read.csv("data/output_data/vetsa1_mci_adjusted_2025-05-17.csv")
mci_v2_adj <- read.csv("data/output_data/vetsa2_mci_adjusted_2025-05-17.csv")
mci_v3_adj <- read.csv("data/output_data/vetsa3_mci_adjusted_2025-05-17.csv")
mci_v4_adj <- read.csv("data/output_data/vetsa4_mci_adjusted_2025-05-17.csv")
mci_adj <- mci_v1_adj %>%
  select(VETSAID, rMCI_cons_V1) %>%
  full_join(mci_v2_adj %>% select(VETSAID, rMCI_cons_V2), by = "VETSAID") %>%
  full_join(mci_v3_adj %>% select(VETSAID, rMCI_cons_V3), by = "VETSAID") %>%
  full_join(mci_v4_adj %>% select(VETSAID, rMCI_cons_V4), by = "VETSAID")

#--------------------------------#
#   Create and label variables   #
#--------------------------------#

# Remove attributes
admin = admin %>%
  remove_labels() %>%
  remove_attributes(attributes = "format.sas") 

# # Remove V1ne subjects
# admin = admin %>%
#   filter(!grepl("V1NE", VGRP_PROCVAR))

# Label race and ethnicity, label regions
admin = admin %>% 
  mutate(
    Race = case_when(
      RACEALL==1 ~ "American Indian or Alaskan Native",
      RACEALL==2 ~ "Asian",
      RACEALL==3 ~ "Native Hawaiian or Pacific Islander",
      RACEALL==4 ~ "Black or African-American",
      RACEALL==5 ~ "White",
      RACEALL==6 ~ "More than one race",
      RACEALL==7 ~ "Decline to answer",),
    Ethnicity = case_when(
      ETHNALL==1 ~ "Hispanic",
      ETHNALL==2 ~ "Non-Hispanic",
      ETHNALL==3 ~ "Decline to answer")
    ) %>%
  mutate_at(vars(RACEALL, ETHNALL), ~ as.factor(.)) 

  
# Create a variable for each wave called AR. indicating attrition replacements.
admin <- admin %>%
  mutate(
    AR_V1 = ifelse(grepl("V1AR", VGRP_PROCVAR), 1, 0),
    AR_V2 = ifelse(grepl("V2AR", VGRP_PROCVAR), 1, 0),
    AR_V3 = ifelse(grepl("V3AR", VGRP_PROCVAR), 1, 0),
    AR_V4 = ifelse(grepl("V4AR", VGRP_PROCVAR), 1, 0)
  )

# Merge in age 20 afqt data and get convert to percentile score
admin <- admin %>% 
  left_join(afqt_20, by = "VETSAID") %>%
  mutate(NAS201 = pnorm(NAS201TRAN))


#-------------------------------------------------------------------------#
#   Create upset plot of assessment participation and ridge plot of ages  #
#-------------------------------------------------------------------------#

upset_plot <- upset(
  data=admin,
  intersect = c("VETSA_1","VETSA_2","VETSA_3","VETSA_4"),
  name = "Longitudinal assessment pattern",
  base_annotations = list(
    'Intersection size'=(
      intersection_size()
      + ylab('N per pattern')
    )
  ),
  set_sizes = 
    upset_set_size() + 
    geom_text(aes(label=..count..), hjust=1.1, stat="count") +
    expand_limits(y=1600) +
    ylab("N per wave"),
  themes=upset_default_themes(text=element_text(face='bold', size=18)),
  queries = list(
    upset_query(set='VETSA_1', fill='#E69F00'),
    upset_query(set='VETSA_2', fill='#56B4E9'),
    upset_query(set='VETSA_3', fill='#009E73'),
    upset_query(set='VETSA_4', fill='#D55E00')
  )
)


# Pivot AGE and AR variables to long format. The suffix should indicate wave. 
admin_long = admin %>% 
  select(VETSAID, CASE, starts_with("AGE_"), starts_with("AR_")) %>%
  pivot_longer(cols=-c("VETSAID", "CASE"), 
               names_to=c(".value","WAVE"), 
               values_to=c("AGE", "AR"), 
               names_sep="_",
               values_drop_na=TRUE)  %>%
  mutate(WAVE = as.integer(gsub("V","",WAVE)))

# Create a variable called GROUP which is the combination of the AR and WAVE variables.
admin_long <- admin_long %>%
  mutate(GROUP = ifelse(AR == 1, paste("Wave", WAVE, "AR"), paste("Wave", WAVE)))

# Create a ridgeline plot with ggridges for AGE. 
age_plot <- ggplot(admin_long, aes(x=AGE, y=fct_rev(GROUP), fill=factor(WAVE), alpha=factor(AR))) +
  geom_density_ridges(scale=1.75, rel_min_height = 0.05) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#D55E00")) +
  scale_alpha_manual(values=c("0"=1, "1"=0.5)) +
  theme_minimal() +
  labs(x="Age", y="") +
  theme(
    panel.background = element_rect(fill="white", color="white"),
    panel.border = element_blank(),
    legend.position="none",
    axis.title=element_text(face="bold", size=18),
    axis.text=element_text(face="bold", size=18)
  )

# Combine plots
age_upset_plot <- ggpubr::ggarrange(upset_plot, age_plot, 
                  nrow =1, 
                  labels="AUTO", 
                  font.label = list(size = 24)) +  bgcolor("white") +
  theme(plot.background = element_rect(fill="white", color="white"),
        panel.background = element_rect(fill="white", color="white"),
        panel.border = element_blank())

# Save combined plot
age_upset_plot_outname = paste0("results/age_upset_plots_", Sys.Date(), ".svg")
ggsave(age_upset_plot_outname, age_upset_plot, width = 16, height = 6, 
       device = "svg", dpi = 300)

#--------------------------------#
#   Create sample descriptives   #
#--------------------------------#

# Get dataframe of demographic variables to include in table
demo_df <- admin %>%
  select(starts_with("AGE"), TEDALL, Race, Ethnicity, NAS201, starts_with("TESTDIFF")) 

# Create a table of demographic variables
CreateTableOne(data = demo_df,
               includeNA = TRUE) %>%
  print(showAllLevels = TRUE, quote=TRUE, noSpaces = TRUE)
psych::describe(demo_df)

# For every column in tests_adj, find how many rows have non-missing data
nonmissing_tests <- tests_adj %>% 
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "test", values_to = "n") 

  
#----------------------------------------------------#
#   Evaluate balance of returnees and replacements   #
#----------------------------------------------------#

# Create dataframe with covariates that we want to evaluate for balance
balance_df <- admin %>%
  select(VETSAID, EDUCATION=TEDALL, Race, Ethnicity, NAS201) %>%
  right_join(admin_long, by="VETSAID") %>%
  left_join(charlson_long, by=c("VETSAID", "WAVE")) 


# Wave 1
balance_df_v2 = balance_df %>% filter(WAVE==2 & !is.na(AGE))

# Create table showing SMD at wave 2
balance_v2 = CreateTableOne(data = balance_df_v2,
                            vars = c("AGE","EDUCATION", "NAS201", "Ethnicity", "Race", "CHARLSON"),
                            strata = "AR")
print(balance_v2, smd = TRUE, quote = TRUE, noSpaces = TRUE)

# Create table showing SMD at wave 3
balance_df_v3 = balance_df %>% filter(WAVE==3 & !is.na(AGE))
balance_v3 = CreateTableOne(data = balance_df_v3,
                            vars = c("AGE","EDUCATION", "NAS201", "Ethnicity", "Race", "CHARLSON"),
                            strata = "AR")
print(balance_v3, smd = TRUE, quote = TRUE, noSpaces = TRUE)

# Create table showing SMD at wave 4
# Set wave 2 AR participants to also be ARs at wave 4
ar_v4 = balance_df %>%
  filter(WAVE==3 & AR==1)
returness_v4 = balance_df %>%
  filter(WAVE==4 & !is.na(AGE) & !VETSAID %in% ar_v4$VETSAID)
balance_df_v4 = ar_v4 %>% bind_rows(returness_v4) 


balance_v4 = CreateTableOne(data = balance_df_v4,
                            vars = c("AGE","EDUCATION", "NAS201", "Ethnicity", "Race", "CHARLSON"),
                            strata = "AR")
print(balance_v4, smd = TRUE, quote = TRUE, noSpaces = TRUE)


