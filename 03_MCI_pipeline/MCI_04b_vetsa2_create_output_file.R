#--------------------------------------------------------------#
# Overview: VETSA2 MCI Results Output Generator                #
#                                                              #
# This script creates the final VETSA2 MCI output dataset by:  #
# 1. Selecting MCI classification variables from processed data#
# 2. Merging with common medical condition exclusion criteria  #
# 3. Creating a global exclusion indicator for medical issues  #
# 4. Producing a clean, date-stamped final output file         #
#                                                              #
# Key outputs:                                                 #
# - MCI classification types (typical, comprehensive, etc.)    #
# - Individual medical exclusion flags                         #
# - Global exclusion indicator (COMMON_EXCLUDE_V2)             #
#                                                              #
# Note: This generates the Wave 2 output with date stamp       #
# in the filename for proper versioning.                       #
#--------------------------------------------------------------#


library(dplyr)
library(haven)

setwd("~/netshare/M/Projects/PracEffects_GEE")

#-------------------#
#     Load data     #
#-------------------#

# Load the MCI dataset
mci <- read.csv("data/intermediate_data/MCI_03b04_vetsa2_MCI_AllData.csv")

# Load merge file to get common excludes
merge_file <- read_sas("~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/VETSA 2 Aging/vetsa2merged_23dec2016_nomiss.sas7bdat")

# Load list of IDs with brain cancer dx. 
brain_cancer_ids <- readLines("data/raw_data/brain_cancer_ids.txt")

#--------------------------------------------------------#
#   Select excludes and create global exclude indicator  #
#--------------------------------------------------------#

# Set all names in merge file to uppercase
names(merge_file) <- toupper(names(merge_file))

# Select the common excludes
common_excludes <- merge_file %>%
  select(VETSAID, SEIZYN_V2, MSYN_V2, STROYN_V2, HIVYN_V2, AIDSYN_V2, AAADYN_V2,
         SCHZYN_V2, DADDYN_V2)

# Add brain cancer dx
common_excludes$BRAIN_CANCER_V2 <- ifelse(common_excludes$VETSAID %in% brain_cancer_ids, 1, 0)

# Create COMMON_EXCLUDE_V2 variable indicating whether any of the common 
# exclude variables equal 1. If all are NA, then COMMON_EXCLUDE_V2 = NA.
common_excludes <- common_excludes %>%
  mutate(
    COMMON_EXCLUDE_V2 = case_when(
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
output_file <- paste0("data/output_data/vetsa2_mci_adjusted_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
write.csv(final_mci, output_file, row.names = FALSE)