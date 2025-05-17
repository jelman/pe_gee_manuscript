#--------------------------------------------------------------#
# Overview: VETSA1 MCI Results Output Generator                #
#                                                              #
# This script creates the final VETSA1 MCI output dataset by:  #
# 1. Selecting MCI classification variables from processed data#
# 2. Merging with common medical condition exclusion criteria  #
# 3. Creating a global exclusion indicator for medical issues  #
# 4. Producing a clean, date-stamped final output file         #
#                                                              #
# Key outputs:                                                 #
# - MCI classification types (typical, comprehensive, etc.)    #
# - Individual medical exclusion flags                         #
# - Global exclusion indicator (COMMON_EXCLUDE_V1)             #
#                                                              #
# Note: This generates the Wave 1 output with date stamp       #
# in the filename for proper versioning.                       #
#--------------------------------------------------------------#

library(dplyr)
library(haven)

# Note: Working directory should be set by the runner script

#-------------------#
#     Load data     #
#-------------------#

# Use file paths from runner script if they exist, otherwise use defaults
if(!exists("post_imputation_file")) {
  post_imputation_file <- "data/intermediate_data/MCI_03a04_vetsa1_MCI_AllData.csv"
}
if(!exists("merge_file")) {
  merge_file <- "~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/VETSA 1 Aging/vetsa1merged_22dec2016_nomiss.sas7bdat"
}
if(!exists("brain_cancer_ids_file")) {
  brain_cancer_ids_file <- "data/raw_data/brain_cancer_ids.txt"
}
if(!exists("output_file")) {
  date_stamp <- format(Sys.Date(), "%Y-%m-%d")
  output_file <- paste0("data/output_data/VETSA1_MCI_", date_stamp, ".csv")
}

# Load the MCI dataset
mci <- read.csv(post_imputation_file)

# Load merge file to get common excludes
merge_file <- read_sas(merge_file)

# Load list of IDs with brain cancer dx. 
brain_cancer_ids <- readLines(brain_cancer_ids_file)

#--------------------------------------------------------#
#   Select excludes and create global exclude indicator  #
#--------------------------------------------------------#

# Set all names in merge file to uppercase
names(merge_file) <- toupper(names(merge_file))

# Select the common excludes
common_excludes <- merge_file %>%
  select(VETSAID, SEIZYN, MSYN, STROYN, HIVYN, AIDSYN, AAADYN,
         SCHZYN, DADDYN)

# Add _V1 suffix to exclude variables
common_excludes <- common_excludes %>%
  rename_with(~ paste0(., "_V1"), -VETSAID)

# Add brain cancer dx
common_excludes$BRAIN_CANCER_V1 <- ifelse(common_excludes$VETSAID %in% brain_cancer_ids, 1, 0)

# Create COMMON_EXCLUDE_V1 variable indicating whether any of the common 
# exclude variables equal 1. If all are NA, then COMMON_EXCLUDE_V1 = NA.
common_excludes <- common_excludes %>%
  mutate(
    COMMON_EXCLUDE_V1 = case_when(
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

# Save out final MCI dataset 
# Note: output_file is set by runner script or defaults above
write.csv(final_mci, output_file, row.names = FALSE)

cat("VETSA1 MCI output file created:", output_file, "\n")
cat("Number of subjects in output:", nrow(final_mci), "\n")
