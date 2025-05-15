#--------------------------------------------------------------#
# Overview: VETSA3 MCI Results Output Generator                #
#                                                              #
# This script creates the final VETSA3 MCI output dataset by:  #
# 1. Selecting MCI classification variables from processed data#
# 2. Merging with common medical condition exclusion criteria  #
# 3. Creating a global exclusion indicator for medical issues  #
# 4. Producing a clean, date-stamped final output file         #
#                                                              #
# Key outputs:                                                 #
# - MCI classification types (typical, comprehensive, etc.)    #
# - Individual medical exclusion flags                         #
# - Global exclusion indicator (COMMON_EXCLUDE_V3)             #
#                                                              #
# Note: This generates the Wave 3 output with date stamp       #
# in the filename for proper versioning.                       #
#--------------------------------------------------------------#


library(dplyr)
library(haven)

setwd("~/netshare/M/Projects/PracEffects_GEE")

#-------------------#
#     Load data     #
#-------------------#

# Load the MCI dataset
mci <- read.csv("data/intermediate_data/MCI_03c04_vetsa3_MCI_AllData.csv")

# Load merge file to get common excludes
merge_file <- read_sas("~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/VETSA 3 interim datasets/vetsa3_master_db_20200618.sas7bdat")

# Load list of IDs with brain cancer dx. 
brain_cancer_ids <- readLines("data/raw_data/brain_cancer_ids.txt")

# Load MRI additional excludes
mri_excludes <- read.csv("~/netshare/M/VETSA DATA FILES_852014/MRI/MRI_Admin/MRI_addtl_excludes.csv")

#--------------------------------------------------------#
#   Select excludes and create global exclude indicator  #
#--------------------------------------------------------#

# Set all names in merge file to uppercase
names(merge_file) <- toupper(names(merge_file))

# Select the common excludes
common_excludes <- merge_file %>%
  select(VETSAID, SEIZYN_V3, MSYN_V3, STROYN_V3, HIVYN_V3, AIDSYN_V3, AAADYN_V3,
         SCHZYN_V3, DADDYN_V3)

# Add brain cancer dx
common_excludes$BRAIN_CANCER_V3 <- ifelse(common_excludes$VETSAID %in% brain_cancer_ids, 1, 0)

# Select MRI additional excludes
mri_excludes <- mri_excludes %>%
  select(VETSAID=vetsaid, MRI_ADD_EXCLUDE_V3=MRI_addtl_exclusion_v3)

# Merge the common excludes and MRI additional excludes
common_excludes <- common_excludes %>%
  left_join(mri_excludes, by = "VETSAID")

# Create COMMON_EXCLUDE_V3 variable indicating whether any of the common 
# exclude variables equal 1. If all are NA, then COMMON_EXCLUDE_V3 = NA.
common_excludes <- common_excludes %>%
  mutate(
    COMMON_EXCLUDE_V3 = case_when(
      rowSums(!is.na(select(., -VETSAID))) == 0 ~ NA_real_,
      rowSums(select(., -VETSAID) == 1, na.rm = TRUE) > 0 ~ 1,
      TRUE ~ 0
    )
  )

#-------------------------------------------#
#   Merge with MCI and save out final file  #
#-------------------------------------------#

# Select MCI classifications
final_mci <- mci %>%
  select(VETSAID, starts_with("rMCI"))

# Merge the common excludes and MCI classifications
final_mci <- final_mci %>%
  left_join(common_excludes, by = "VETSAID") 

# Save out final MCI dataset with date
output_file <- paste0("data/output_data/vetsa3_mci_adjusted_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
write.csv(final_mci, output_file, row.names = FALSE)