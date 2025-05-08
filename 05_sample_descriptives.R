library(dplyr)
library(tableone)
library(ComplexUpset)
library(ggplot2)
library(ggridges)
library(haven)

# Load the data
admin = haven::read_sas("~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/Admin/vetsa_admin_file_20250205.sas7bdat")

str(admin)
