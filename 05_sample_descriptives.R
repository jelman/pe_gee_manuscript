library(dplyr)
library(tableone)
library(ComplexUpset)
library(ggplot2)
library(ggridges)
library(haven)
library(forcats)
library(UpSetR)
library(labelled)

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

# Load individual test data
tests_adj <- read.csv("data/raw_data/V1V2V3V4_cog_data_pe-adjusted_2025-04-25.csv")

# Load cognitive factor scores
factors_adj <- read.csv("data/output_data/V1V2V3V4_cog_factor_scores_pe-adjusted_2025-05-09.csv")

# Load and merge MCI data
mci_v1_adj <- read.csv("data/output_data/vetsa1_mci_adjusted_2025-05-09.csv")
mci_v2_adj <- read.csv("data/output_data/vetsa2_mci_adjusted_2025-05-09.csv")
mci_v3_adj <- read.csv("data/output_data/vetsa3_mci_adjusted_2025-05-09.csv")
mci_v4_adj <- read.csv("data/output_data/vetsa4_mci_adjusted_2025-05-09.csv")
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

# Label race and ethnicity, label regions
admin = admin %>% 
  mutate(
    Race = case_when(
      RACEALL==1 ~ "American Indian or Alaskan Native",
      RACEALL==4 ~ "Black or African-American",
      RACEALL==5 ~ "White",
      RACEALL==6 ~ "More than one race"),
    Ethnicity = case_when(
      ETHNALL==1 ~ "Hispanic",
      ETHNALL==2 ~ "Non-Hispanic")
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


#---------------------------------------------------#
#   Create upset plot of assessment participation   #
#---------------------------------------------------#

upset_plot <- admin %>%
  select(`Wave 1`=VETSA_1, `Wave 2`=VETSA_2, `Wave 3`=VETSA_3, `Wave 4`=VETSA_4) %>%
  as.matrix() %>%
  as.data.frame() %>%
  upset(
    sets = rev(c("Wave 1", "Wave 2", "Wave 3", "Wave 4")),
    keep.order = TRUE,
    order.by = "freq",
    set_size.show = TRUE,
    mainbar.y.label = "Number of Participants", 
    sets.x.label = "Participants per Timepoint",
    text.scale = 1.3,
    point.size = 3.5,
    line.size = 1,
    set_size.scale_max = 1500)



#--------------------------------#
#   Create sample descriptives   #
#--------------------------------#

# Get dataframe of demographic variables to include in table
demo_df <- admin %>%
  select(starts_with("AGE"), TEDALL, Race, Ethnicity, NAS201) 

# Create a table of demographic variables
CreateTableOne(data = demo_df,
               includeNA = TRUE) %>%
  print(showAllLevels = TRUE, noSpaces = TRUE)
psych::describe(demo_df)

# For every column in tests_adj, find how many rows have non-missing data
nonmissing_tests <- tests_adj %>% 
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "test", values_to = "n") 

#-----------------------------------------------#
#   Create plots of age distribution by wave    #
#-----------------------------------------------#

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
  geom_density_ridges(scale=1.5, rel_min_height = 0.01) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#D55E00")) +
  scale_alpha_manual(values=c("0"=1, "1"=0.5)) +
  theme_minimal() +
  labs(x="Age", y="") +
  theme(
    legend.position="none",
    axis.title=element_text(size=14),
    axis.text=element_text(size=14)
  )
  

