# WMS-III Digit Span normative scoring functions

library(dplyr)

norm_digit_span <- function(df) {
  # Create default DSPSS column if it doesn't exist
  if (!"DSPSS" %in% names(df)) {
    df$DSPSS <- NA_real_
  }
  
  # Apply rounding and compute digit span total
  df <- df %>%
    mutate(
      # Round digit span variables
      DSFRAW = round(DSFRAW, 0),
      DSBRAW = round(DSBRAW, 0),
      
      # Create sum of digit span forward and backward
      DSPTOT = case_when(
        is.na(DSFRAW) | is.na(DSBRAW) ~ NA_real_,
        TRUE ~ DSFRAW + DSBRAW
      )
    )
  
  df <- df %>%
    mutate(
      # Scale score conversion for Digit Span Total
      DSPSS = case_when(
        # First check for missing values
        is.na(DSPTOT) ~ NA_real_,
        
        # Age 45-54
        AGE >= 45 & AGE < 55 & DSPTOT >= 29 & DSPTOT <= 30 ~ 19,
        AGE >= 45 & AGE < 55 & DSPTOT == 28 ~ 18,
        AGE >= 45 & AGE < 55 & DSPTOT >= 26 & DSPTOT <= 27 ~ 17,
        AGE >= 45 & AGE < 55 & DSPTOT == 25 ~ 16,
        AGE >= 45 & AGE < 55 & DSPTOT == 24 ~ 15,
        AGE >= 45 & AGE < 55 & DSPTOT >= 22 & DSPTOT <= 23 ~ 14,
        AGE >= 45 & AGE < 55 & DSPTOT == 21 ~ 13,
        AGE >= 45 & AGE < 55 & DSPTOT == 20 ~ 12,
        AGE >= 45 & AGE < 55 & DSPTOT >= 18 & DSPTOT <= 19 ~ 11,
        AGE >= 45 & AGE < 55 & DSPTOT == 17 ~ 10,
        AGE >= 45 & AGE < 55 & DSPTOT >= 15 & DSPTOT <= 16 ~ 9,
        AGE >= 45 & AGE < 55 & DSPTOT == 14 ~ 8,
        AGE >= 45 & AGE < 55 & DSPTOT >= 12 & DSPTOT <= 13 ~ 7,
        AGE >= 45 & AGE < 55 & DSPTOT == 11 ~ 6,
        AGE >= 45 & AGE < 55 & DSPTOT >= 9 & DSPTOT <= 10 ~ 5,
        AGE >= 45 & AGE < 55 & DSPTOT >= 7 & DSPTOT <= 8 ~ 4,
        AGE >= 45 & AGE < 55 & DSPTOT >= 5 & DSPTOT <= 6 ~ 3,
        AGE >= 45 & AGE < 55 & DSPTOT >= 3 & DSPTOT <= 4 ~ 2,
        AGE >= 45 & AGE < 55 & DSPTOT >= 0 & DSPTOT <= 2 ~ 1,
        
        # Age 55-64
        AGE >= 55 & AGE < 65 & DSPTOT >= 28 & DSPTOT <= 30 ~ 19,
        AGE >= 55 & AGE < 65 & DSPTOT == 27 ~ 18,
        AGE >= 55 & AGE < 65 & DSPTOT == 26 ~ 17,
        AGE >= 55 & AGE < 65 & DSPTOT == 25 ~ 16,
        AGE >= 55 & AGE < 65 & DSPTOT >= 23 & DSPTOT <= 24 ~ 15,
        AGE >= 55 & AGE < 65 & DSPTOT == 22 ~ 14,
        AGE >= 55 & AGE < 65 & DSPTOT >= 20 & DSPTOT <= 21 ~ 13,
        AGE >= 55 & AGE < 65 & DSPTOT == 19 ~ 12,
        AGE >= 55 & AGE < 65 & DSPTOT >= 17 & DSPTOT <= 18 ~ 11,
        AGE >= 55 & AGE < 65 & DSPTOT == 16 ~ 10,
        AGE >= 55 & AGE < 65 & DSPTOT >= 14 & DSPTOT <= 15 ~ 9,
        AGE >= 55 & AGE < 65 & DSPTOT == 13 ~ 8,
        AGE >= 55 & AGE < 65 & DSPTOT >= 11 & DSPTOT <= 12 ~ 7,
        AGE >= 55 & AGE < 65 & DSPTOT >= 9 & DSPTOT <= 10 ~ 6,
        AGE >= 55 & AGE < 65 & DSPTOT == 8 ~ 5,
        AGE >= 55 & AGE < 65 & DSPTOT >= 6 & DSPTOT <= 7 ~ 4,
        AGE >= 55 & AGE < 65 & DSPTOT >= 4 & DSPTOT <= 5 ~ 3,
        AGE >= 55 & AGE < 65 & DSPTOT >= 2 & DSPTOT <= 3 ~ 2,
        AGE >= 55 & AGE < 65 & DSPTOT >= 0 & DSPTOT <= 1 ~ 1,
        
        # Age 65-69
        AGE >= 65 & AGE < 70 & DSPTOT >= 27 & DSPTOT <= 30 ~ 19,
        AGE >= 65 & AGE < 70 & DSPTOT == 26 ~ 18,
        AGE >= 65 & AGE < 70 & DSPTOT == 25 ~ 17,
        AGE >= 65 & AGE < 70 & DSPTOT == 24 ~ 16,
        AGE >= 65 & AGE < 70 & DSPTOT == 23 ~ 15,
        AGE >= 65 & AGE < 70 & DSPTOT >= 21 & DSPTOT <= 22 ~ 14,
        AGE >= 65 & AGE < 70 & DSPTOT == 20 ~ 13,
        AGE >= 65 & AGE < 70 & DSPTOT >= 18 & DSPTOT <= 19 ~ 12,
        AGE >= 65 & AGE < 70 & DSPTOT == 17 ~ 11,
        AGE >= 65 & AGE < 70 & DSPTOT == 16 ~ 10,
        AGE >= 65 & AGE < 70 & DSPTOT >= 14 & DSPTOT <= 15 ~ 9,
        AGE >= 65 & AGE < 70 & DSPTOT == 13 ~ 8,
        AGE >= 65 & AGE < 70 & DSPTOT >= 11 & DSPTOT <= 12 ~ 7,
        AGE >= 65 & AGE < 70 & DSPTOT >= 9 & DSPTOT <= 10 ~ 6,
        AGE >= 65 & AGE < 70 & DSPTOT == 8 ~ 5,
        AGE >= 65 & AGE < 70 & DSPTOT >= 6 & DSPTOT <= 7 ~ 4,
        AGE >= 65 & AGE < 70 & DSPTOT >= 4 & DSPTOT <= 5 ~ 3,
        AGE >= 65 & AGE < 70 & DSPTOT >= 2 & DSPTOT <= 3 ~ 2,
        AGE >= 65 & AGE < 70 & DSPTOT >= 0 & DSPTOT <= 1 ~ 1,
        
        # Age 70-74
        AGE >= 70 & AGE < 75 & DSPTOT >= 26 & DSPTOT <= 30 ~ 19,
        AGE >= 70 & AGE < 75 & DSPTOT == 25 ~ 18,
        AGE >= 70 & AGE < 75 & DSPTOT == 24 ~ 17,
        AGE >= 70 & AGE < 75 & DSPTOT == 23 ~ 16,
        AGE >= 70 & AGE < 75 & DSPTOT == 22 ~ 15,
        AGE >= 70 & AGE < 75 & DSPTOT >= 20 & DSPTOT <= 21 ~ 14,
        AGE >= 70 & AGE < 75 & DSPTOT == 19 ~ 13,
        AGE >= 70 & AGE < 75 & DSPTOT == 18 ~ 12,
        AGE >= 70 & AGE < 75 & DSPTOT >= 16 & DSPTOT <= 17 ~ 11,
        AGE >= 70 & AGE < 75 & DSPTOT == 15 ~ 10,
        AGE >= 70 & AGE < 75 & DSPTOT == 14 ~ 9,
        AGE >= 70 & AGE < 75 & DSPTOT >= 12 & DSPTOT <= 13 ~ 8,
        AGE >= 70 & AGE < 75 & DSPTOT == 11 ~ 8, # Note: This is a direct match from SAS which has duplicate 8 assignment
        AGE >= 70 & AGE < 75 & DSPTOT >= 9 & DSPTOT <= 10 ~ 6,
        AGE >= 70 & AGE < 75 & DSPTOT == 8 ~ 5,
        AGE >= 70 & AGE < 75 & DSPTOT >= 6 & DSPTOT <= 7 ~ 4,
        AGE >= 70 & AGE < 75 & DSPTOT >= 4 & DSPTOT <= 5 ~ 3,
        AGE >= 70 & AGE < 75 & DSPTOT >= 2 & DSPTOT <= 3 ~ 2,
        AGE >= 70 & AGE < 75 & DSPTOT >= 0 & DSPTOT <= 1 ~ 1,
        
        # Age 75-79
        AGE >= 75 & AGE < 80 & DSPTOT >= 25 & DSPTOT <= 30 ~ 19,
        AGE >= 75 & AGE < 80 & DSPTOT == 24 ~ 18,
        AGE >= 75 & AGE < 80 & DSPTOT == 23 ~ 17,
        AGE >= 75 & AGE < 80 & DSPTOT == 22 ~ 16,
        AGE >= 75 & AGE < 80 & DSPTOT == 21 ~ 15,
        AGE >= 75 & AGE < 80 & DSPTOT == 20 ~ 14,
        AGE >= 75 & AGE < 80 & DSPTOT == 19 ~ 13,
        AGE >= 75 & AGE < 80 & DSPTOT >= 17 & DSPTOT <= 18 ~ 12,
        AGE >= 75 & AGE < 80 & DSPTOT == 16 ~ 11,
        AGE >= 75 & AGE < 80 & DSPTOT == 15 ~ 10,
        AGE >= 75 & AGE < 80 & DSPTOT >= 12 & DSPTOT <= 14 ~ 8,
        AGE >= 75 & AGE < 80 & DSPTOT == 11 ~ 7,
        AGE >= 75 & AGE < 80 & DSPTOT >= 9 & DSPTOT <= 10 ~ 6,
        AGE >= 75 & AGE < 80 & DSPTOT == 8 ~ 5,
        AGE >= 75 & AGE < 80 & DSPTOT >= 6 & DSPTOT <= 7 ~ 4,
        AGE >= 75 & AGE < 80 & DSPTOT >= 4 & DSPTOT <= 5 ~ 3,
        AGE >= 75 & AGE < 80 & DSPTOT >= 2 & DSPTOT <= 3 ~ 2,
        AGE >= 75 & AGE < 80 & DSPTOT >= 0 & DSPTOT <= 1 ~ 1,
        
        # Age 80-84
        AGE >= 80 & AGE < 85 & DSPTOT >= 24 & DSPTOT <= 30 ~ 19,
        AGE >= 80 & AGE < 85 & DSPTOT == 23 ~ 18,
        AGE >= 80 & AGE < 85 & DSPTOT == 22 ~ 17,
        AGE >= 80 & AGE < 85 & DSPTOT == 21 ~ 16,
        AGE >= 80 & AGE < 85 & DSPTOT == 20 ~ 15,
        AGE >= 80 & AGE < 85 & DSPTOT == 19 ~ 14,
        AGE >= 80 & AGE < 85 & DSPTOT == 18 ~ 13,
        AGE >= 80 & AGE < 85 & DSPTOT == 17 ~ 12,
        AGE >= 80 & AGE < 85 & DSPTOT == 16 ~ 11,
        AGE >= 80 & AGE < 85 & DSPTOT >= 14 & DSPTOT <= 15 ~ 10,
        AGE >= 80 & AGE < 85 & DSPTOT == 13 ~ 9,
        AGE >= 80 & AGE < 85 & DSPTOT == 12 ~ 8,
        AGE >= 80 & AGE < 85 & DSPTOT >= 10 & DSPTOT <= 11 ~ 7,
        AGE >= 80 & AGE < 85 & DSPTOT == 9 ~ 6,
        AGE >= 80 & AGE < 85 & DSPTOT >= 7 & DSPTOT <= 8 ~ 5,
        AGE >= 80 & AGE < 85 & DSPTOT >= 5 & DSPTOT <= 6 ~ 4,
        AGE >= 80 & AGE < 85 & DSPTOT >= 3 & DSPTOT <= 4 ~ 3,
        AGE >= 80 & AGE < 85 & DSPTOT >= 1 & DSPTOT <= 2 ~ 2,
        AGE >= 80 & AGE < 85 & DSPTOT == 0 ~ 1,
        
        TRUE ~ as.numeric(DSPSS)  # Keep existing value if no match
      )
    )
  
  return(df)
}
