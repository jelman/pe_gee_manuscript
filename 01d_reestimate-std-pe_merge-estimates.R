library(dplyr)

### Merge standardized estimates ###

# Load model estimates for all wave data
estimates_allwaves <- read.csv("~/netshare/M/Projects/PracEffects_GEE/results/gee_standardized_results_allwave_2025-05-17.csv")

# Load model estimates for spatial span data
estimates_span <- read.csv("~/netshare/M/Projects/PracEffects_GEE/results/gee_standardized_results_spatspan_2025-05-17.csv")

# Load model estimates for boston naming data
estimates_bnt <- read.csv("~/netshare/M/Projects/PracEffects_GEE/results/gee_standardized_results_bnt_2025-05-17.csv")


# Bind all dataframes together
estimates_complete <- estimates_allwaves %>% 
  bind_rows(estimates_span) %>%
  bind_rows(estimates_bnt) 

# Save out the combined dataframe
outfile <- "~/netshare/M/Projects/PracEffects_GEE/results/gee_standardized_results_complete_2025-05-17.csv"
write.csv(estimates_complete, outfile, row.names = FALSE)


### Merge unstandardized estimates ###

# Load model estimates for all wave data
estimates_allwaves_unstd <- read.csv("~/netshare/M/VETSA DATA FILES_852014/PracticeEffects/results/model_estimates/gee_results_allwave_2025-08-08.csv")

# Load model estimates for spatial span data
estimates_span_unstd <- read.csv("~/netshare/M/VETSA DATA FILES_852014/PracticeEffects/results/model_estimates/gee_results_spatspan_2025-08-08.csv")

# Load model estimates for boston naming data
estimates_bnt_unstd <- read.csv("~/netshare/M/VETSA DATA FILES_852014/PracticeEffects/results/model_estimates/gee_results_bnt_2025-08-08.csv")

# Bind all dataframes together
estimates_complete_unstd <- estimates_allwaves_unstd %>% 
  bind_rows(estimates_span_unstd) %>%
  bind_rows(estimates_bnt_unstd)

# Save out the combined dataframe
outfile_unstd <- "~/netshare/M/Projects/PracEffects_GEE/results/gee_unstandardized_results_complete_2025-11-18.csv"
write.csv(estimates_complete_unstd, outfile_unstd, row.names = FALSE)
