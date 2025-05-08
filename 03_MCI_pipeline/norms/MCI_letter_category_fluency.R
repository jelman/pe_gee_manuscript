# Letter and Category Fluency normative scoring functions

library(dplyr)

norm_fluency <- function(df) {
  # Create default variables if they don't exist
  for (col in c("LFCORSC", "CFCORSC", "CSCORSC", "CSSACCSC")) {
    if (!(col %in% names(df))) {
      df[[col]] <- NA_real_
    }
  }
  
  # Apply rounding to fluency variables
  df <- df %>%
    mutate(
      LFCOR = round(LFCOR, 0),
      CFCOR = round(CFCOR, 0),
      CSCOR = round(CSCOR, 0),
      CSSACC = round(CSSACC, 0)
    )
  
  df <- df %>%
    mutate(
      # Letter Fluency Scaled Scores
      LFCORSC = case_when(
        # Check for missing values first
        is.na(LFCOR) ~ NA_real_,
        
        # Age 50-59
        AGE >= 50 & AGE < 60 & LFCOR >= 0 & LFCOR <= 8 ~ 1,
        AGE >= 50 & AGE < 60 & LFCOR >= 9 & LFCOR <= 11 ~ 2,
        AGE >= 50 & AGE < 60 & LFCOR >= 12 & LFCOR <= 15 ~ 3,
        AGE >= 50 & AGE < 60 & LFCOR >= 16 & LFCOR <= 18 ~ 4,
        AGE >= 50 & AGE < 60 & LFCOR >= 19 & LFCOR <= 21 ~ 5,
        AGE >= 50 & AGE < 60 & LFCOR >= 22 & LFCOR <= 25 ~ 6,
        AGE >= 50 & AGE < 60 & LFCOR >= 26 & LFCOR <= 28 ~ 7,
        AGE >= 50 & AGE < 60 & LFCOR >= 29 & LFCOR <= 31 ~ 8,
        AGE >= 50 & AGE < 60 & LFCOR >= 32 & LFCOR <= 35 ~ 9,
        AGE >= 50 & AGE < 60 & LFCOR >= 36 & LFCOR <= 38 ~ 10,
        AGE >= 50 & AGE < 60 & LFCOR >= 39 & LFCOR <= 41 ~ 11,
        AGE >= 50 & AGE < 60 & LFCOR >= 42 & LFCOR <= 45 ~ 12,
        AGE >= 50 & AGE < 60 & LFCOR >= 46 & LFCOR <= 48 ~ 13,
        AGE >= 50 & AGE < 60 & LFCOR >= 49 & LFCOR <= 51 ~ 14,
        AGE >= 50 & AGE < 60 & LFCOR >= 52 & LFCOR <= 55 ~ 15,
        AGE >= 50 & AGE < 60 & LFCOR >= 56 & LFCOR <= 58 ~ 16,
        AGE >= 50 & AGE < 60 & LFCOR >= 59 & LFCOR <= 61 ~ 17,
        AGE >= 50 & AGE < 60 & LFCOR >= 62 & LFCOR <= 65 ~ 18,
        AGE >= 50 & AGE < 60 & LFCOR > 65 & LFCOR <= 90 ~ 19,
        
        # Age 60-69
        AGE >= 60 & AGE < 70 & LFCOR >= 0 & LFCOR <= 7 ~ 1,
        AGE >= 60 & AGE < 70 & LFCOR >= 8 & LFCOR <= 10 ~ 2,
        AGE >= 60 & AGE < 70 & LFCOR >= 11 & LFCOR <= 14 ~ 3,
        AGE >= 60 & AGE < 70 & LFCOR >= 15 & LFCOR <= 17 ~ 4,
        AGE >= 60 & AGE < 70 & LFCOR >= 18 & LFCOR <= 20 ~ 5,
        AGE >= 60 & AGE < 70 & LFCOR >= 21 & LFCOR <= 24 ~ 6,
        AGE >= 60 & AGE < 70 & LFCOR >= 25 & LFCOR <= 27 ~ 7,
        AGE >= 60 & AGE < 70 & LFCOR >= 28 & LFCOR <= 30 ~ 8,
        AGE >= 60 & AGE < 70 & LFCOR >= 31 & LFCOR <= 34 ~ 9,
        AGE >= 60 & AGE < 70 & LFCOR >= 35 & LFCOR <= 37 ~ 10,
        AGE >= 60 & AGE < 70 & LFCOR >= 38 & LFCOR <= 40 ~ 11,
        AGE >= 60 & AGE < 70 & LFCOR >= 41 & LFCOR <= 44 ~ 12,
        AGE >= 60 & AGE < 70 & LFCOR >= 45 & LFCOR <= 47 ~ 13,
        AGE >= 60 & AGE < 70 & LFCOR >= 48 & LFCOR <= 50 ~ 14,
        AGE >= 60 & AGE < 70 & LFCOR >= 51 & LFCOR <= 54 ~ 15,
        AGE >= 60 & AGE < 70 & LFCOR >= 55 & LFCOR <= 57 ~ 16,
        AGE >= 60 & AGE < 70 & LFCOR >= 58 & LFCOR <= 60 ~ 17,
        AGE >= 60 & AGE < 70 & LFCOR >= 61 & LFCOR <= 64 ~ 18,
        AGE >= 60 & AGE < 70 & LFCOR > 64 & LFCOR <= 90 ~ 19,
        
        # Age 70-79
        AGE >= 70 & AGE < 80 & LFCOR >= 0 & LFCOR <= 6 ~ 1,
        AGE >= 70 & AGE < 80 & LFCOR >= 7 & LFCOR <= 9 ~ 2,
        AGE >= 70 & AGE < 80 & LFCOR >= 10 & LFCOR <= 13 ~ 3,
        AGE >= 70 & AGE < 80 & LFCOR >= 14 & LFCOR <= 16 ~ 4,
        AGE >= 70 & AGE < 80 & LFCOR >= 17 & LFCOR <= 19 ~ 5,
        AGE >= 70 & AGE < 80 & LFCOR >= 20 & LFCOR <= 23 ~ 6,
        AGE >= 70 & AGE < 80 & LFCOR >= 24 & LFCOR <= 26 ~ 7,
        AGE >= 70 & AGE < 80 & LFCOR >= 27 & LFCOR <= 29 ~ 8,
        AGE >= 70 & AGE < 80 & LFCOR >= 30 & LFCOR <= 33 ~ 9,
        AGE >= 70 & AGE < 80 & LFCOR >= 34 & LFCOR <= 36 ~ 10,
        AGE >= 70 & AGE < 80 & LFCOR >= 37 & LFCOR <= 39 ~ 11,
        AGE >= 70 & AGE < 80 & LFCOR >= 40 & LFCOR <= 43 ~ 12,
        AGE >= 70 & AGE < 80 & LFCOR >= 44 & LFCOR <= 46 ~ 13,
        AGE >= 70 & AGE < 80 & LFCOR >= 47 & LFCOR <= 49 ~ 14,
        AGE >= 70 & AGE < 80 & LFCOR >= 50 & LFCOR <= 53 ~ 15,
        AGE >= 70 & AGE < 80 & LFCOR >= 54 & LFCOR <= 56 ~ 16,
        AGE >= 70 & AGE < 80 & LFCOR >= 57 & LFCOR <= 59 ~ 17,
        AGE >= 70 & AGE < 80 & LFCOR >= 60 & LFCOR <= 63 ~ 18,
        AGE >= 70 & AGE < 80 & LFCOR > 63 & LFCOR <= 90 ~ 19,
        
        TRUE ~ as.numeric(LFCORSC)  # Keep existing value if no match
      ),
      
      # Category Fluency Scaled Scores
      CFCORSC = case_when(
        # Check for missing values first
        is.na(CFCOR) ~ NA_real_,
        
        # Age 50-59
        AGE >= 50 & AGE < 60 & CFCOR >= 0 & CFCOR <= 16 ~ 1,
        AGE >= 50 & AGE < 60 & CFCOR >= 17 & CFCOR <= 19 ~ 2,
        AGE >= 50 & AGE < 60 & CFCOR >= 20 & CFCOR <= 21 ~ 3,
        AGE >= 50 & AGE < 60 & CFCOR >= 22 & CFCOR <= 24 ~ 4,
        AGE >= 50 & AGE < 60 & CFCOR >= 25 & CFCOR <= 26 ~ 5,
        AGE >= 50 & AGE < 60 & CFCOR >= 27 & CFCOR <= 29 ~ 6,
        AGE >= 50 & AGE < 60 & CFCOR >= 30 & CFCOR <= 31 ~ 7,
        AGE >= 50 & AGE < 60 & CFCOR >= 32 & CFCOR <= 34 ~ 8,
        AGE >= 50 & AGE < 60 & CFCOR >= 35 & CFCOR <= 36 ~ 9,
        AGE >= 50 & AGE < 60 & CFCOR >= 37 & CFCOR <= 39 ~ 10,
        AGE >= 50 & AGE < 60 & CFCOR >= 40 & CFCOR <= 41 ~ 11,
        AGE >= 50 & AGE < 60 & CFCOR >= 42 & CFCOR <= 44 ~ 12,
        AGE >= 50 & AGE < 60 & CFCOR >= 45 & CFCOR <= 46 ~ 13,
        AGE >= 50 & AGE < 60 & CFCOR >= 47 & CFCOR <= 49 ~ 14,
        AGE >= 50 & AGE < 60 & CFCOR >= 50 & CFCOR <= 51 ~ 15,
        AGE >= 50 & AGE < 60 & CFCOR >= 52 & CFCOR <= 54 ~ 16,
        AGE >= 50 & AGE < 60 & CFCOR >= 55 & CFCOR <= 56 ~ 17,
        AGE >= 50 & AGE < 60 & CFCOR >= 57 & CFCOR <= 59 ~ 18,
        AGE >= 50 & AGE < 60 & CFCOR > 59 & CFCOR <= 80 ~ 19,
        
        # Age 60-69
        AGE >= 60 & AGE < 70 & CFCOR >= 0 & CFCOR <= 13 ~ 1,
        AGE >= 60 & AGE < 70 & CFCOR >= 14 & CFCOR <= 16 ~ 2,
        AGE >= 60 & AGE < 70 & CFCOR >= 17 & CFCOR <= 18 ~ 3,
        AGE >= 60 & AGE < 70 & CFCOR >= 19 & CFCOR <= 21 ~ 4,
        AGE >= 60 & AGE < 70 & CFCOR >= 22 & CFCOR <= 23 ~ 5,
        AGE >= 60 & AGE < 70 & CFCOR >= 24 & CFCOR <= 26 ~ 6,
        AGE >= 60 & AGE < 70 & CFCOR >= 27 & CFCOR <= 28 ~ 7,
        AGE >= 60 & AGE < 70 & CFCOR >= 29 & CFCOR <= 31 ~ 8,
        AGE >= 60 & AGE < 70 & CFCOR >= 32 & CFCOR <= 33 ~ 9,
        AGE >= 60 & AGE < 70 & CFCOR >= 34 & CFCOR <= 36 ~ 10,
        AGE >= 60 & AGE < 70 & CFCOR >= 37 & CFCOR <= 38 ~ 11,
        AGE >= 60 & AGE < 70 & CFCOR >= 39 & CFCOR <= 41 ~ 12,
        AGE >= 60 & AGE < 70 & CFCOR >= 42 & CFCOR <= 43 ~ 13,
        AGE >= 60 & AGE < 70 & CFCOR >= 44 & CFCOR <= 46 ~ 14,
        AGE >= 60 & AGE < 70 & CFCOR >= 47 & CFCOR <= 48 ~ 15,
        AGE >= 60 & AGE < 70 & CFCOR >= 49 & CFCOR <= 51 ~ 16,
        AGE >= 60 & AGE < 70 & CFCOR >= 52 & CFCOR <= 53 ~ 17,
        AGE >= 60 & AGE < 70 & CFCOR >= 54 & CFCOR <= 56 ~ 18,
        AGE >= 60 & AGE < 70 & CFCOR > 56 & CFCOR <= 80 ~ 19,
        
        # Age 70-79
        AGE >= 70 & AGE < 80 & CFCOR >= 0 & CFCOR <= 11 ~ 1,
        AGE >= 70 & AGE < 80 & CFCOR >= 12 & CFCOR <= 14 ~ 2,
        AGE >= 70 & AGE < 80 & CFCOR >= 15 & CFCOR <= 16 ~ 3,
        AGE >= 70 & AGE < 80 & CFCOR >= 17 & CFCOR <= 19 ~ 4,
        AGE >= 70 & AGE < 80 & CFCOR >= 20 & CFCOR <= 21 ~ 5,
        AGE >= 70 & AGE < 80 & CFCOR >= 22 & CFCOR <= 24 ~ 6,
        AGE >= 70 & AGE < 80 & CFCOR >= 25 & CFCOR <= 26 ~ 7,
        AGE >= 70 & AGE < 80 & CFCOR >= 27 & CFCOR <= 29 ~ 8,
        AGE >= 70 & AGE < 80 & CFCOR >= 30 & CFCOR <= 31 ~ 9,
        AGE >= 70 & AGE < 80 & CFCOR >= 32 & CFCOR <= 34 ~ 10,
        AGE >= 70 & AGE < 80 & CFCOR >= 35 & CFCOR <= 36 ~ 11,
        AGE >= 70 & AGE < 80 & CFCOR >= 37 & CFCOR <= 39 ~ 12,
        AGE >= 70 & AGE < 80 & CFCOR >= 40 & CFCOR <= 41 ~ 13,
        AGE >= 70 & AGE < 80 & CFCOR >= 42 & CFCOR <= 44 ~ 14,
        AGE >= 70 & AGE < 80 & CFCOR >= 45 & CFCOR <= 46 ~ 15,
        AGE >= 70 & AGE < 80 & CFCOR >= 47 & CFCOR <= 49 ~ 16,
        AGE >= 70 & AGE < 80 & CFCOR >= 50 & CFCOR <= 51 ~ 17,
        AGE >= 70 & AGE < 80 & CFCOR >= 52 & CFCOR <= 54 ~ 18,
        AGE >= 70 & AGE < 80 & CFCOR > 54 & CFCOR <= 80 ~ 19,
        
        TRUE ~ as.numeric(CFCORSC)  # Keep existing value if no match
      ),
      
      # Category Switching Correct Scaled Scores
      CSCORSC = case_when(
        # Check for missing values first
        is.na(CSCOR) ~ NA_real_,
        
        # Age 50-59
        AGE >= 50 & AGE < 60 & CSCOR >= 0 & CSCOR <= 5 ~ 1,
        AGE >= 50 & AGE < 60 & CSCOR == 6 ~ 2,
        AGE >= 50 & AGE < 60 & CSCOR == 7 ~ 3,
        AGE >= 50 & AGE < 60 & CSCOR == 8 ~ 4,
        AGE >= 50 & AGE < 60 & CSCOR == 9 ~ 5,
        AGE >= 50 & AGE < 60 & CSCOR == 10 ~ 6,
        AGE >= 50 & AGE < 60 & CSCOR == 11 ~ 8,
        AGE >= 50 & AGE < 60 & CSCOR == 12 ~ 9,
        AGE >= 50 & AGE < 60 & CSCOR == 13 ~ 10,
        AGE >= 50 & AGE < 60 & CSCOR == 14 ~ 11,
        AGE >= 50 & AGE < 60 & CSCOR == 15 ~ 12,
        AGE >= 50 & AGE < 60 & CSCOR == 16 ~ 14,
        AGE >= 50 & AGE < 60 & CSCOR == 17 ~ 15,
        AGE >= 50 & AGE < 60 & CSCOR == 18 ~ 16,
        AGE >= 50 & AGE < 60 & CSCOR == 19 ~ 17,
        AGE >= 50 & AGE < 60 & CSCOR == 20 ~ 18,
        AGE >= 50 & AGE < 60 & CSCOR > 20 & CSCOR <= 30 ~ 19,
        
        # Age 60-69
        AGE >= 60 & AGE < 70 & CSCOR >= 0 & CSCOR <= 5 ~ 1,
        AGE >= 60 & AGE < 70 & CSCOR == 6 ~ 2,
        AGE >= 60 & AGE < 70 & CSCOR == 7 ~ 3,
        AGE >= 60 & AGE < 70 & CSCOR == 8 ~ 5,
        AGE >= 60 & AGE < 70 & CSCOR == 9 ~ 6,
        AGE >= 60 & AGE < 70 & CSCOR == 10 ~ 7,
        AGE >= 60 & AGE < 70 & CSCOR == 11 ~ 8,
        AGE >= 60 & AGE < 70 & CSCOR == 12 ~ 9,
        AGE >= 60 & AGE < 70 & CSCOR == 13 ~ 11,
        AGE >= 60 & AGE < 70 & CSCOR == 14 ~ 12,
        AGE >= 60 & AGE < 70 & CSCOR == 15 ~ 13,
        AGE >= 60 & AGE < 70 & CSCOR == 16 ~ 14,
        AGE >= 60 & AGE < 70 & CSCOR == 17 ~ 15,
        AGE >= 60 & AGE < 70 & CSCOR == 18 ~ 17,
        AGE >= 60 & AGE < 70 & CSCOR == 19 ~ 18,
        AGE >= 60 & AGE < 70 & CSCOR > 19 & CSCOR <= 30 ~ 19,
        
        # Age 70-79
        AGE >= 70 & AGE < 80 & CSCOR >= 0 & CSCOR <= 4 ~ 1,
        AGE >= 70 & AGE < 80 & CSCOR == 5 ~ 2,
        AGE >= 70 & AGE < 80 & CSCOR == 6 ~ 3,
        AGE >= 70 & AGE < 80 & CSCOR == 7 ~ 4,
        AGE >= 70 & AGE < 80 & CSCOR == 8 ~ 5,
        AGE >= 70 & AGE < 80 & CSCOR == 9 ~ 6,
        AGE >= 70 & AGE < 80 & CSCOR == 10 ~ 8,
        AGE >= 70 & AGE < 80 & CSCOR == 11 ~ 9,
        AGE >= 70 & AGE < 80 & CSCOR == 12 ~ 10,
        AGE >= 70 & AGE < 80 & CSCOR == 13 ~ 11,
        AGE >= 70 & AGE < 80 & CSCOR == 14 ~ 12,
        AGE >= 70 & AGE < 80 & CSCOR == 15 ~ 14,
        AGE >= 70 & AGE < 80 & CSCOR == 16 ~ 15,
        AGE >= 70 & AGE < 80 & CSCOR == 17 ~ 16,
        AGE >= 70 & AGE < 80 & CSCOR == 18 ~ 17,
        AGE >= 70 & AGE < 80 & CSCOR == 19 ~ 18,
        AGE >= 70 & AGE < 80 & CSCOR > 19 & CSCOR <= 30 ~ 19,
        
        TRUE ~ as.numeric(CSCORSC)  # Keep existing value if no match
      ),
      
      # Category Switching Accuracy Scaled Scores
      CSSACCSC = case_when(
        # Check for missing values first
        is.na(CSSACC) ~ NA_real_,
        
        # Age 50-59
        AGE >= 50 & AGE < 60 & CSSACC >= 0 & CSSACC <= 3 ~ 1,
        AGE >= 50 & AGE < 60 & CSSACC == 4 ~ 2,
        AGE >= 50 & AGE < 60 & CSSACC == 5 ~ 3,
        AGE >= 50 & AGE < 60 & CSSACC == 6 ~ 4,
        AGE >= 50 & AGE < 60 & CSSACC == 7 ~ 5,
        AGE >= 50 & AGE < 60 & CSSACC == 8 ~ 6,
        AGE >= 50 & AGE < 60 & CSSACC == 9 ~ 7,
        AGE >= 50 & AGE < 60 & CSSACC == 10 ~ 8,
        AGE >= 50 & AGE < 60 & CSSACC == 11 ~ 9,
        AGE >= 50 & AGE < 60 & CSSACC == 12 ~ 10,
        AGE >= 50 & AGE < 60 & CSSACC == 13 ~ 11,
        AGE >= 50 & AGE < 60 & CSSACC == 14 ~ 12,
        AGE >= 50 & AGE < 60 & CSSACC == 15 ~ 13,
        AGE >= 50 & AGE < 60 & CSSACC == 16 ~ 14,
        AGE >= 50 & AGE < 60 & CSSACC == 17 ~ 15,
        AGE >= 50 & AGE < 60 & CSSACC == 18 ~ 16,
        AGE >= 50 & AGE < 60 & CSSACC == 19 ~ 17,
        AGE >= 50 & AGE < 60 & CSSACC == 20 ~ 18,
        AGE >= 50 & AGE < 60 & CSSACC > 20 & CSSACC <= 30 ~ 19,
        
        # Age 60-69
        AGE >= 60 & AGE < 70 & CSSACC >= 0 & CSSACC <= 2 ~ 1,
        AGE >= 60 & AGE < 70 & CSSACC == 3 ~ 2,
        AGE >= 60 & AGE < 70 & CSSACC == 4 ~ 3,
        AGE >= 60 & AGE < 70 & CSSACC == 5 ~ 4,
        AGE >= 60 & AGE < 70 & CSSACC == 6 ~ 5,
        AGE >= 60 & AGE < 70 & CSSACC == 7 ~ 6,
        AGE >= 60 & AGE < 70 & CSSACC == 8 ~ 7,
        AGE >= 60 & AGE < 70 & CSSACC == 9 ~ 8,
        AGE >= 60 & AGE < 70 & CSSACC == 10 ~ 9,
        AGE >= 60 & AGE < 70 & CSSACC == 11 ~ 10,
        AGE >= 60 & AGE < 70 & CSSACC == 12 ~ 11,
        AGE >= 60 & AGE < 70 & CSSACC == 13 ~ 12,
        AGE >= 60 & AGE < 70 & CSSACC == 14 ~ 13,
        AGE >= 60 & AGE < 70 & CSSACC == 15 ~ 14,
        AGE >= 60 & AGE < 70 & CSSACC == 16 ~ 15,
        AGE >= 60 & AGE < 70 & CSSACC == 17 ~ 16,
        AGE >= 60 & AGE < 70 & CSSACC == 18 ~ 17,
        AGE >= 60 & AGE < 70 & CSSACC == 19 ~ 18,
        AGE >= 60 & AGE < 70 & CSSACC > 19 & CSSACC <= 30 ~ 19,
        
        # Age 70-79
        AGE >= 70 & AGE < 80 & CSSACC >= 0 & CSSACC <= 2 ~ 1,
        AGE >= 70 & AGE < 80 & CSSACC == 3 ~ 2,
        AGE >= 70 & AGE < 80 & CSSACC == 4 ~ 3,
        AGE >= 70 & AGE < 80 & CSSACC == 5 ~ 4,
        AGE >= 70 & AGE < 80 & CSSACC == 6 ~ 5,
        AGE >= 70 & AGE < 80 & CSSACC == 7 ~ 6,
        AGE >= 70 & AGE < 80 & CSSACC == 8 ~ 7,
        AGE >= 70 & AGE < 80 & CSSACC == 9 ~ 8,
        AGE >= 70 & AGE < 80 & CSSACC == 10 ~ 9,
        AGE >= 70 & AGE < 80 & CSSACC == 11 ~ 10,
        AGE >= 70 & AGE < 80 & CSSACC == 12 ~ 11,
        AGE >= 70 & AGE < 80 & CSSACC == 13 ~ 12,
        AGE >= 70 & AGE < 80 & CSSACC == 14 ~ 13,
        AGE >= 70 & AGE < 80 & CSSACC == 15 ~ 14,
        AGE >= 70 & AGE < 80 & CSSACC == 16 ~ 15,
        AGE >= 70 & AGE < 80 & CSSACC == 17 ~ 16,
        AGE >= 70 & AGE < 80 & CSSACC == 18 ~ 17,
        AGE >= 70 & AGE < 80 & CSSACC == 19 ~ 18,
        AGE >= 70 & AGE < 80 & CSSACC > 19 & CSSACC <= 30 ~ 19,
        
        TRUE ~ as.numeric(CSSACCSC)  # Keep existing value if no match
      )
    )
  
  return(df)
}