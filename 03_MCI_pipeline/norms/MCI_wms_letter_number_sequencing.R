# WMS-III Letter Number Sequencing normative scoring functions

library(dplyr)

norm_letter_number_sequencing <- function(df) {
  # Create default LNSC column if it doesn't exist
  if (!"LNSC" %in% names(df)) {
    df$LNSC <- NA_real_
  }
  
  # Apply rounding to LN variables
  df <- df %>%
    mutate(
      LNTOT = round(LNTOT, 0)
    )
  
  df <- df %>%
    mutate(
      # Scale scores for Letter Number Sequencing
      LNSC = case_when(
        # First check for missing values
        is.na(LNTOT) ~ NA_real_,
        
        # Age 55-64
        AGE >= 55 & AGE < 65 & LNTOT == 21 ~ 19,
        AGE >= 55 & AGE < 65 & LNTOT == 20 ~ 19,
        AGE >= 55 & AGE < 65 & LNTOT == 19 ~ 19,
        AGE >= 55 & AGE < 65 & LNTOT == 18 ~ 19,
        AGE >= 55 & AGE < 65 & LNTOT == 17 ~ 18,
        AGE >= 55 & AGE < 65 & LNTOT == 16 ~ 17,
        AGE >= 55 & AGE < 65 & LNTOT == 15 ~ 16,
        AGE >= 55 & AGE < 65 & LNTOT == 14 ~ 15,
        AGE >= 55 & AGE < 65 & LNTOT == 13 ~ 14,
        AGE >= 55 & AGE < 65 & LNTOT == 12 ~ 13,
        AGE >= 55 & AGE < 65 & LNTOT == 11 ~ 12,
        AGE >= 55 & AGE < 65 & LNTOT == 10 ~ 11,
        AGE >= 55 & AGE < 65 & LNTOT == 9 ~ 10,
        AGE >= 55 & AGE < 65 & LNTOT == 8 ~ 8,
        AGE >= 55 & AGE < 65 & LNTOT == 7 ~ 7,
        AGE >= 55 & AGE < 65 & LNTOT == 6 ~ 6,
        AGE >= 55 & AGE < 65 & LNTOT == 5 ~ 5,
        AGE >= 55 & AGE < 65 & LNTOT == 4 ~ 4,
        AGE >= 55 & AGE < 65 & LNTOT == 3 ~ 3,
        AGE >= 55 & AGE < 65 & LNTOT == 2 ~ 2,
        AGE >= 55 & AGE < 65 & LNTOT == 1 ~ 1,
        AGE >= 55 & AGE < 65 & LNTOT == 0 ~ 1,
        
        # Age 65-69
        AGE >= 65 & AGE < 70 & LNTOT == 21 ~ 19,
        AGE >= 65 & AGE < 70 & LNTOT == 20 ~ 19,
        AGE >= 65 & AGE < 70 & LNTOT == 19 ~ 19,
        AGE >= 65 & AGE < 70 & LNTOT == 18 ~ 19,
        AGE >= 65 & AGE < 70 & LNTOT == 17 ~ 18,
        AGE >= 65 & AGE < 70 & LNTOT == 16 ~ 17,
        AGE >= 65 & AGE < 70 & LNTOT == 15 ~ 16,
        AGE >= 65 & AGE < 70 & LNTOT == 14 ~ 15,
        AGE >= 65 & AGE < 70 & LNTOT == 13 ~ 14,
        AGE >= 65 & AGE < 70 & LNTOT == 12 ~ 13,
        AGE >= 65 & AGE < 70 & LNTOT == 11 ~ 12,
        AGE >= 65 & AGE < 70 & LNTOT == 10 ~ 11,
        AGE >= 65 & AGE < 70 & LNTOT == 9 ~ 10,
        AGE >= 65 & AGE < 70 & LNTOT == 8 ~ 9,
        AGE >= 65 & AGE < 70 & LNTOT == 7 ~ 8,
        AGE >= 65 & AGE < 70 & LNTOT == 6 ~ 6,
        AGE >= 65 & AGE < 70 & LNTOT == 5 ~ 5,
        AGE >= 65 & AGE < 70 & LNTOT == 4 ~ 4,
        AGE >= 65 & AGE < 70 & LNTOT == 3 ~ 3,
        AGE >= 65 & AGE < 70 & LNTOT == 2 ~ 2,
        AGE >= 65 & AGE < 70 & LNTOT == 1 ~ 1,
        AGE >= 65 & AGE < 70 & LNTOT == 0 ~ 1,
        
        # Age 70-74
        AGE >= 70 & AGE < 75 & LNTOT == 21 ~ 19,
        AGE >= 70 & AGE < 75 & LNTOT == 20 ~ 19,
        AGE >= 70 & AGE < 75 & LNTOT == 19 ~ 19,
        AGE >= 70 & AGE < 75 & LNTOT == 18 ~ 19,
        AGE >= 70 & AGE < 75 & LNTOT == 17 ~ 19,
        AGE >= 70 & AGE < 75 & LNTOT == 16 ~ 18,
        AGE >= 70 & AGE < 75 & LNTOT == 15 ~ 17,
        AGE >= 70 & AGE < 75 & LNTOT == 14 ~ 16,
        AGE >= 70 & AGE < 75 & LNTOT == 13 ~ 15,
        AGE >= 70 & AGE < 75 & LNTOT == 12 ~ 14,
        AGE >= 70 & AGE < 75 & LNTOT == 11 ~ 13,
        AGE >= 70 & AGE < 75 & LNTOT == 10 ~ 12,
        AGE >= 70 & AGE < 75 & LNTOT == 9 ~ 11,
        AGE >= 70 & AGE < 75 & LNTOT == 8 ~ 10,
        AGE >= 70 & AGE < 75 & LNTOT == 7 ~ 9,
        AGE >= 70 & AGE < 75 & LNTOT == 6 ~ 7,
        AGE >= 70 & AGE < 75 & LNTOT == 5 ~ 6,
        AGE >= 70 & AGE < 75 & LNTOT == 4 ~ 5,
        AGE >= 70 & AGE < 75 & LNTOT == 3 ~ 4,
        AGE >= 70 & AGE < 75 & LNTOT == 2 ~ 3,
        AGE >= 70 & AGE < 75 & LNTOT == 1 ~ 2,
        AGE >= 70 & AGE < 75 & LNTOT == 0 ~ 1,
        
        # Age 75-79
        AGE >= 75 & AGE < 80 & LNTOT >= 16 & LNTOT <= 21 ~ 19,
        AGE >= 75 & AGE < 80 & LNTOT == 15 ~ 18,
        AGE >= 75 & AGE < 80 & LNTOT == 14 ~ 17,
        AGE >= 75 & AGE < 80 & LNTOT == 13 ~ 16,
        AGE >= 75 & AGE < 80 & LNTOT == 12 ~ 15,
        AGE >= 75 & AGE < 80 & LNTOT == 11 ~ 14,
        AGE >= 75 & AGE < 80 & LNTOT == 10 ~ 13,
        AGE >= 75 & AGE < 80 & LNTOT == 9 ~ 12,
        AGE >= 75 & AGE < 80 & LNTOT == 8 ~ 10,
        AGE >= 75 & AGE < 80 & LNTOT == 7 ~ 9,
        AGE >= 75 & AGE < 80 & LNTOT == 6 ~ 8,
        AGE >= 75 & AGE < 80 & LNTOT == 5 ~ 7,
        AGE >= 75 & AGE < 80 & LNTOT == 4 ~ 5,
        AGE >= 75 & AGE < 80 & LNTOT == 3 ~ 4,
        AGE >= 75 & AGE < 80 & LNTOT == 2 ~ 3,
        AGE >= 75 & AGE < 80 & LNTOT == 1 ~ 2,
        AGE >= 75 & AGE < 80 & LNTOT == 0 ~ 1,
        
        # Age 80-84
        AGE >= 80 & AGE < 85 & LNTOT >= 16 & LNTOT <= 21 ~ 19,
        AGE >= 80 & AGE < 85 & LNTOT == 15 ~ 18,
        AGE >= 80 & AGE < 85 & LNTOT == 14 ~ 17,
        AGE >= 80 & AGE < 85 & LNTOT == 13 ~ 16,
        AGE >= 80 & AGE < 85 & LNTOT == 12 ~ 15,
        AGE >= 80 & AGE < 85 & LNTOT == 11 ~ 14,
        AGE >= 80 & AGE < 85 & LNTOT == 10 ~ 13,
        AGE >= 80 & AGE < 85 & LNTOT == 9 ~ 12,
        AGE >= 80 & AGE < 85 & LNTOT == 8 ~ 11,
        AGE >= 80 & AGE < 85 & LNTOT == 7 ~ 10,
        AGE >= 80 & AGE < 85 & LNTOT == 6 ~ 9,
        AGE >= 80 & AGE < 85 & LNTOT == 5 ~ 8,
        AGE >= 80 & AGE < 85 & LNTOT == 4 ~ 7,
        AGE >= 80 & AGE < 85 & LNTOT == 3 ~ 6,
        AGE >= 80 & AGE < 85 & LNTOT == 2 ~ 5,
        AGE >= 80 & AGE < 85 & LNTOT == 1 ~ 3,
        AGE >= 80 & AGE < 85 & LNTOT == 0 ~ 1,
        
        TRUE ~ as.numeric(LNSC)  # Keep existing value if no match
      )
    )
  
  return(df)
}
