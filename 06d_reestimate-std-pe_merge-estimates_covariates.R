library(dplyr)

### Merge standardized estimates ###

# Load model estimates for all wave data
estimates_allwaves <- read.csv("~/netshare/M/Projects/PracEffects_GEE/results/gee_standardized_results_allwave_2026-03-10_covariates.csv")

# Load model estimates for spatial span data
estimates_span <- read.csv("~/netshare/M/Projects/PracEffects_GEE/results/gee_standardized_results_spatspan_2026-03-10_covariates.csv")

# Load model estimates for boston naming data
estimates_bnt <- read.csv("~/netshare/M/Projects/PracEffects_GEE/results/gee_standardized_results_bnt_2026-03-10_covariates.csv")


# Bind all dataframes together
estimates_complete <- estimates_allwaves %>% 
  bind_rows(estimates_span) %>%
  bind_rows(estimates_bnt) 

# Save out the combined dataframe
date <- format(Sys.Date(), "%Y-%m-%d")
outfile <- paste0("results/gee_standardized_results_complete_", date, "_covariates.csv")
write.csv(estimates_complete, outfile, row.names = FALSE)

