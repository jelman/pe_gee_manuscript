library(dplyr)

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
