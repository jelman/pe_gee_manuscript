# D-KEFS Trail Making normative scoring functions

library(dplyr)

norm_trails <- function(df) {
  # Create default scaled score columns if they don't exist
  for (col in c("TRL1TSC", "TRL2TSC", "TRL3TSC", "TRL4TSC", "TRL5TSC")) {
    if (!(col %in% names(df))) {
      df[[col]] <- NA_real_
    }
  }
  
  
  # Condition 1
  df <- df %>% 
    mutate(
      TRL1TSC = case_when(
      # Check for missing values first
      is.na(TRL1T) ~ NA_real_,
      
      # Age 50-59
      AGE >= 50 & AGE < 60 & TRL1T >= 44.5 ~ 1,
      AGE >= 50 & AGE < 60 & TRL1T >= 41.5 & TRL1T < 44.5 ~ 2,
      AGE >= 50 & AGE < 60 & TRL1T >= 39.5 & TRL1T < 41.5 ~ 3,
      AGE >= 50 & AGE < 60 & TRL1T >= 36.5 & TRL1T < 39.5 ~ 4,
      AGE >= 50 & AGE < 60 & TRL1T >= 34.5 & TRL1T < 36.5 ~ 5,
      AGE >= 50 & AGE < 60 & TRL1T >= 31.5 & TRL1T < 34.5 ~ 6,
      AGE >= 50 & AGE < 60 & TRL1T >= 29.5 & TRL1T < 31.5 ~ 7,
      AGE >= 50 & AGE < 60 & TRL1T >= 26.5 & TRL1T < 29.5 ~ 8,
      AGE >= 50 & AGE < 60 & TRL1T >= 24.5 & TRL1T < 26.5 ~ 9,
      AGE >= 50 & AGE < 60 & TRL1T >= 21.5 & TRL1T < 24.5 ~ 10,
      AGE >= 50 & AGE < 60 & TRL1T >= 19.5 & TRL1T < 21.5 ~ 11,
      AGE >= 50 & AGE < 60 & TRL1T >= 17.5 & TRL1T < 19.5 ~ 12,
      AGE >= 50 & AGE < 60 & TRL1T >= 14.5 & TRL1T < 17.5 ~ 13,
      AGE >= 50 & AGE < 60 & TRL1T >= 12.5 & TRL1T < 14.5 ~ 14,
      AGE >= 50 & AGE < 60 & TRL1T >= 10.5 & TRL1T < 12.5 ~ 15,
      AGE >= 50 & AGE < 60 & TRL1T >= 7.5 & TRL1T < 10.5 ~ 16,
      AGE >= 50 & AGE < 60 & TRL1T >= 5.5 & TRL1T < 7.5 ~ 17,
      AGE >= 50 & AGE < 60 & TRL1T >= 3.5 & TRL1T < 5.5 ~ 18,
      AGE >= 50 & AGE < 60 & TRL1T > 0 & TRL1T < 3.5 ~ 19,
      
      # Age 60-69
      AGE >= 60 & AGE < 70 & TRL1T >= 50.5 ~ 1,
      AGE >= 60 & AGE < 70 & TRL1T >= 48.5 & TRL1T < 50.5 ~ 2,
      AGE >= 60 & AGE < 70 & TRL1T >= 45.5 & TRL1T < 48.5 ~ 3,
      AGE >= 60 & AGE < 70 & TRL1T >= 42.5 & TRL1T < 45.5 ~ 4,
      AGE >= 60 & AGE < 70 & TRL1T >= 40.5 & TRL1T < 42.5 ~ 5,
      AGE >= 60 & AGE < 70 & TRL1T >= 37.5 & TRL1T < 40.5 ~ 6,
      AGE >= 60 & AGE < 70 & TRL1T >= 34.5 & TRL1T < 37.5 ~ 7,
      AGE >= 60 & AGE < 70 & TRL1T >= 32.5 & TRL1T < 34.5 ~ 8,
      AGE >= 60 & AGE < 70 & TRL1T >= 29.5 & TRL1T < 32.5 ~ 9,
      AGE >= 60 & AGE < 70 & TRL1T >= 26.5 & TRL1T < 29.5 ~ 10,
      AGE >= 60 & AGE < 70 & TRL1T >= 24.5 & TRL1T < 26.5 ~ 11,
      AGE >= 60 & AGE < 70 & TRL1T >= 21.5 & TRL1T < 24.5 ~ 12,
      AGE >= 60 & AGE < 70 & TRL1T >= 18.5 & TRL1T < 21.5 ~ 13,
      AGE >= 60 & AGE < 70 & TRL1T >= 16.5 & TRL1T < 18.5 ~ 14,
      AGE >= 60 & AGE < 70 & TRL1T >= 13.5 & TRL1T < 16.5 ~ 15,
      AGE >= 60 & AGE < 70 & TRL1T >= 10.5 & TRL1T < 13.5 ~ 16,
      AGE >= 60 & AGE < 70 & TRL1T >= 8.5 & TRL1T < 10.5 ~ 17,
      AGE >= 60 & AGE < 70 & TRL1T >= 5.5 & TRL1T < 8.5 ~ 18,
      AGE >= 60 & AGE < 70 & TRL1T > 0 & TRL1T < 5.5 ~ 19,
      
      # Age 70-79
      AGE >= 70 & AGE < 80 & TRL1T >= 52.5 ~ 1,
      AGE >= 70 & AGE < 80 & TRL1T >= 49.5 & TRL1T < 52.5 ~ 2,
      AGE >= 70 & AGE < 80 & TRL1T >= 46.5 & TRL1T < 49.5 ~ 3,
      AGE >= 70 & AGE < 80 & TRL1T >= 44.5 & TRL1T < 46.5 ~ 4,
      AGE >= 70 & AGE < 80 & TRL1T >= 41.5 & TRL1T < 44.5 ~ 5,
      AGE >= 70 & AGE < 80 & TRL1T >= 38.5 & TRL1T < 41.5 ~ 6,
      AGE >= 70 & AGE < 80 & TRL1T >= 36.5 & TRL1T < 38.5 ~ 7,
      AGE >= 70 & AGE < 80 & TRL1T >= 33.5 & TRL1T < 36.5 ~ 8,
      AGE >= 70 & AGE < 80 & TRL1T >= 30.5 & TRL1T < 33.5 ~ 9,
      AGE >= 70 & AGE < 80 & TRL1T >= 28.5 & TRL1T < 30.5 ~ 10,
      AGE >= 70 & AGE < 80 & TRL1T >= 25.5 & TRL1T < 28.5 ~ 11,
      AGE >= 70 & AGE < 80 & TRL1T >= 22.5 & TRL1T < 25.5 ~ 12,
      AGE >= 70 & AGE < 80 & TRL1T >= 20.5 & TRL1T < 22.5 ~ 13,
      AGE >= 70 & AGE < 80 & TRL1T >= 17.5 & TRL1T < 20.5 ~ 14,
      AGE >= 70 & AGE < 80 & TRL1T >= 14.5 & TRL1T < 17.5 ~ 15,
      AGE >= 70 & AGE < 80 & TRL1T >= 12.5 & TRL1T < 14.5 ~ 16,
      AGE >= 70 & AGE < 80 & TRL1T >= 9.5 & TRL1T < 12.5 ~ 17,
      AGE >= 70 & AGE < 80 & TRL1T >= 6.5 & TRL1T < 9.5 ~ 18,
      AGE >= 70 & AGE < 80 & TRL1T > 0 & TRL1T < 6.5 ~ 19,
      
      TRUE ~ as.numeric(TRL1TSC)  # Keep existing value when no conditions match
      ),
      
      # Condition 2
      TRL2TSC = case_when(
      # Check for missing values first
      is.na(TRL2T) ~ NA_real_,
      
      # Age 50-59
      AGE >= 50 & AGE < 60 & TRL2T >= 79.5 ~ 1,
      AGE >= 50 & AGE < 60 & TRL2T >= 74.5 & TRL2T < 79.5 ~ 2,
      AGE >= 50 & AGE < 60 & TRL2T >= 69.5 & TRL2T < 74.5 ~ 3,
      AGE >= 50 & AGE < 60 & TRL2T >= 65.5 & TRL2T < 69.5 ~ 4,
      AGE >= 50 & AGE < 60 & TRL2T >= 60.5 & TRL2T < 65.5 ~ 5,
      AGE >= 50 & AGE < 60 & TRL2T >= 55.5 & TRL2T < 60.5 ~ 6,
      AGE >= 50 & AGE < 60 & TRL2T >= 51.5 & TRL2T < 55.5 ~ 7,
      AGE >= 50 & AGE < 60 & TRL2T >= 46.5 & TRL2T < 51.5 ~ 8,
      AGE >= 50 & AGE < 60 & TRL2T >= 41.5 & TRL2T < 46.5 ~ 9,
      AGE >= 50 & AGE < 60 & TRL2T >= 37.5 & TRL2T < 41.5 ~ 10,
      AGE >= 50 & AGE < 60 & TRL2T >= 32.5 & TRL2T < 37.5 ~ 11,
      AGE >= 50 & AGE < 60 & TRL2T >= 27.5 & TRL2T < 32.5 ~ 12,
      AGE >= 50 & AGE < 60 & TRL2T >= 23.5 & TRL2T < 27.5 ~ 13,
      AGE >= 50 & AGE < 60 & TRL2T >= 18.5 & TRL2T < 23.5 ~ 14,
      AGE >= 50 & AGE < 60 & TRL2T >= 13.5 & TRL2T < 18.5 ~ 15,
      AGE >= 50 & AGE < 60 & TRL2T >= 9.5 & TRL2T < 13.5 ~ 16,
      AGE >= 50 & AGE < 60 & TRL2T >= 4.5 & TRL2T < 9.5 ~ 17,
      AGE >= 50 & AGE < 60 & TRL2T >= 0.5 & TRL2T < 4.5 ~ 18,
      AGE >= 50 & AGE < 60 & TRL2T > 0 & TRL2T < 0.5 ~ 19,
      
      # Age 60-69
      AGE >= 60 & AGE < 70 & TRL2T >= 92.5 ~ 1,
      AGE >= 60 & AGE < 70 & TRL2T >= 87.5 & TRL2T < 92.5 ~ 2,
      AGE >= 60 & AGE < 70 & TRL2T >= 81.5 & TRL2T < 87.5 ~ 3,
      AGE >= 60 & AGE < 70 & TRL2T >= 76.5 & TRL2T < 81.5 ~ 4,
      AGE >= 60 & AGE < 70 & TRL2T >= 71.5 & TRL2T < 76.5 ~ 5,
      AGE >= 60 & AGE < 70 & TRL2T >= 65.5 & TRL2T < 71.5 ~ 6,
      AGE >= 60 & AGE < 70 & TRL2T >= 60.5 & TRL2T < 65.5 ~ 7,
      AGE >= 60 & AGE < 70 & TRL2T >= 55.5 & TRL2T < 60.5 ~ 8,
      AGE >= 60 & AGE < 70 & TRL2T >= 49.5 & TRL2T < 55.5 ~ 9,
      AGE >= 60 & AGE < 70 & TRL2T >= 44.5 & TRL2T < 49.5 ~ 10,
      AGE >= 60 & AGE < 70 & TRL2T >= 39.5 & TRL2T < 44.5 ~ 11,
      AGE >= 60 & AGE < 70 & TRL2T >= 33.5 & TRL2T < 39.5 ~ 12,
      AGE >= 60 & AGE < 70 & TRL2T >= 28.5 & TRL2T < 33.5 ~ 13,
      AGE >= 60 & AGE < 70 & TRL2T >= 23.5 & TRL2T < 28.5 ~ 14,
      AGE >= 60 & AGE < 70 & TRL2T >= 17.5 & TRL2T < 23.5 ~ 15,
      AGE >= 60 & AGE < 70 & TRL2T >= 12.5 & TRL2T < 17.5 ~ 16,
      AGE >= 60 & AGE < 70 & TRL2T >= 7.5 & TRL2T < 12.5 ~ 17,
      AGE >= 60 & AGE < 70 & TRL2T >= 1.5 & TRL2T < 7.5 ~ 18,
      AGE >= 60 & AGE < 70 & TRL2T > 0 & TRL2T < 1.5 ~ 19,
      
      # Age 70-79
      AGE >= 70 & AGE < 80 & TRL2T >= 107.5 ~ 1,
      AGE >= 70 & AGE < 80 & TRL2T >= 101.5 & TRL2T < 107.5 ~ 2,
      AGE >= 70 & AGE < 80 & TRL2T >= 95.5 & TRL2T < 101.5 ~ 3,
      AGE >= 70 & AGE < 80 & TRL2T >= 89.5 & TRL2T < 95.5 ~ 4,
      AGE >= 70 & AGE < 80 & TRL2T >= 83.5 & TRL2T < 89.5 ~ 5,
      AGE >= 70 & AGE < 80 & TRL2T >= 77.5 & TRL2T < 83.5 ~ 6,
      AGE >= 70 & AGE < 80 & TRL2T >= 71.5 & TRL2T < 77.5 ~ 7,
      AGE >= 70 & AGE < 80 & TRL2T >= 65.5 & TRL2T < 71.5 ~ 8,
      AGE >= 70 & AGE < 80 & TRL2T >= 59.5 & TRL2T < 65.5 ~ 9,
      AGE >= 70 & AGE < 80 & TRL2T >= 53.5 & TRL2T < 59.5 ~ 10,
      AGE >= 70 & AGE < 80 & TRL2T >= 47.5 & TRL2T < 53.5 ~ 11,
      AGE >= 70 & AGE < 80 & TRL2T >= 41.5 & TRL2T < 47.5 ~ 12,
      AGE >= 70 & AGE < 80 & TRL2T >= 35.5 & TRL2T < 41.5 ~ 13,
      AGE >= 70 & AGE < 80 & TRL2T >= 29.5 & TRL2T < 35.5 ~ 14,
      AGE >= 70 & AGE < 80 & TRL2T >= 23.5 & TRL2T < 29.5 ~ 15,
      AGE >= 70 & AGE < 80 & TRL2T >= 17.5 & TRL2T < 23.5 ~ 16,
      AGE >= 70 & AGE < 80 & TRL2T >= 11.5 & TRL2T < 17.5 ~ 17,
      AGE >= 70 & AGE < 80 & TRL2T >= 5.5 & TRL2T < 11.5 ~ 18,
      AGE >= 70 & AGE < 80 & TRL2T > 0 & TRL2T < 5.5 ~ 19,
      
      TRUE ~ as.numeric(TRL2TSC)  # Keep existing value when no conditions match
      ),
      
      # Condition 3
      TRL3TSC = case_when(
      # Check for missing values first
      is.na(TRL3T) ~ NA_real_,
      
      # Age 50-59
      AGE >= 50 & AGE < 60 & TRL3T >= 83.5 & TRL3T <= 150 ~ 1,
      AGE >= 50 & AGE < 60 & TRL3T >= 78.5 & TRL3T < 83.5 ~ 2,
      AGE >= 50 & AGE < 60 & TRL3T >= 73.5 & TRL3T < 78.5 ~ 3,
      AGE >= 50 & AGE < 60 & TRL3T >= 68.5 & TRL3T < 73.5 ~ 4,
      AGE >= 50 & AGE < 60 & TRL3T >= 63.5 & TRL3T < 68.5 ~ 5,
      AGE >= 50 & AGE < 60 & TRL3T >= 58.5 & TRL3T < 63.5 ~ 6,
      AGE >= 50 & AGE < 60 & TRL3T >= 53.5 & TRL3T < 58.5 ~ 7,
      AGE >= 50 & AGE < 60 & TRL3T >= 48.5 & TRL3T < 53.5 ~ 8,
      AGE >= 50 & AGE < 60 & TRL3T >= 43.5 & TRL3T < 48.5 ~ 9,
      AGE >= 50 & AGE < 60 & TRL3T >= 38.5 & TRL3T < 43.5 ~ 10,
      AGE >= 50 & AGE < 60 & TRL3T >= 33.5 & TRL3T < 38.5 ~ 11,
      AGE >= 50 & AGE < 60 & TRL3T >= 28.5 & TRL3T < 33.5 ~ 12,
      AGE >= 50 & AGE < 60 & TRL3T >= 23.5 & TRL3T < 28.5 ~ 13,
      AGE >= 50 & AGE < 60 & TRL3T >= 18.5 & TRL3T < 23.5 ~ 14,
      AGE >= 50 & AGE < 60 & TRL3T >= 13.5 & TRL3T < 18.5 ~ 15,
      AGE >= 50 & AGE < 60 & TRL3T >= 8.5 & TRL3T < 13.5 ~ 16,
      AGE >= 50 & AGE < 60 & TRL3T >= 3.5 & TRL3T < 8.5 ~ 17,
      AGE >= 50 & AGE < 60 & TRL3T > 0 & TRL3T < 3.5 ~ 18,
      
      # Age 60-69
      AGE >= 60 & AGE < 70 & TRL3T >= 105.5 ~ 1,
      AGE >= 60 & AGE < 70 & TRL3T >= 99.5 & TRL3T < 105.5 ~ 2,
      AGE >= 60 & AGE < 70 & TRL3T >= 92.5 & TRL3T < 99.5 ~ 3,
      AGE >= 60 & AGE < 70 & TRL3T >= 85.5 & TRL3T < 92.5 ~ 4,
      AGE >= 60 & AGE < 70 & TRL3T >= 79.5 & TRL3T < 85.5 ~ 5,
      AGE >= 60 & AGE < 70 & TRL3T >= 72.5 & TRL3T < 79.5 ~ 6,
      AGE >= 60 & AGE < 70 & TRL3T >= 65.5 & TRL3T < 72.5 ~ 7,
      AGE >= 60 & AGE < 70 & TRL3T >= 59.5 & TRL3T < 65.5 ~ 8,
      AGE >= 60 & AGE < 70 & TRL3T >= 52.5 & TRL3T < 59.5 ~ 9,
      AGE >= 60 & AGE < 70 & TRL3T >= 45.5 & TRL3T < 52.5 ~ 10,
      AGE >= 60 & AGE < 70 & TRL3T >= 39.5 & TRL3T < 45.5 ~ 11,
      AGE >= 60 & AGE < 70 & TRL3T >= 32.5 & TRL3T < 39.5 ~ 12,
      AGE >= 60 & AGE < 70 & TRL3T >= 25.5 & TRL3T < 32.5 ~ 13,
      AGE >= 60 & AGE < 70 & TRL3T >= 19.5 & TRL3T < 25.5 ~ 14,
      AGE >= 60 & AGE < 70 & TRL3T >= 13.5 & TRL3T < 19.5 ~ 15,
      AGE >= 60 & AGE < 70 & TRL3T >= 9 & TRL3T < 13.5 ~ 16,
      AGE >= 60 & AGE < 70 & TRL3T >= 3.5 & TRL3T < 8.5 ~ 17,
      AGE >= 60 & AGE < 70 & TRL3T > 0 & TRL3T < 3.5 ~ 18,
      
      # Age 70-79
      AGE >= 70 & AGE < 80 & TRL3T >= 120.5 ~ 1,
      AGE >= 70 & AGE < 80 & TRL3T >= 113.5 & TRL3T < 120.5 ~ 2,
      AGE >= 70 & AGE < 80 & TRL3T >= 105.5 & TRL3T < 113.5 ~ 3,
      AGE >= 70 & AGE < 80 & TRL3T >= 98.5 & TRL3T < 105.5 ~ 4,
      AGE >= 70 & AGE < 80 & TRL3T >= 91.5 & TRL3T < 98.5 ~ 5,
      AGE >= 70 & AGE < 80 & TRL3T >= 83.5 & TRL3T < 91.5 ~ 6,
      AGE >= 70 & AGE < 80 & TRL3T >= 76.5 & TRL3T < 83.5 ~ 7,
      AGE >= 70 & AGE < 80 & TRL3T >= 69.5 & TRL3T < 76.5 ~ 8,
      AGE >= 70 & AGE < 80 & TRL3T >= 61.5 & TRL3T < 69.5 ~ 9,
      AGE >= 70 & AGE < 80 & TRL3T >= 54.5 & TRL3T < 61.5 ~ 10,
      AGE >= 70 & AGE < 80 & TRL3T >= 47.5 & TRL3T < 54.5 ~ 11,
      AGE >= 70 & AGE < 80 & TRL3T >= 39.5 & TRL3T < 47.5 ~ 12,
      AGE >= 70 & AGE < 80 & TRL3T >= 32.5 & TRL3T < 39.5 ~ 13,
      AGE >= 70 & AGE < 80 & TRL3T >= 25.5 & TRL3T < 32.5 ~ 14,
      AGE >= 70 & AGE < 80 & TRL3T >= 17.5 & TRL3T < 25.5 ~ 15,
      AGE >= 70 & AGE < 80 & TRL3T >= 10.5 & TRL3T < 17.5 ~ 16,
      AGE >= 70 & AGE < 80 & TRL3T >= 3.5 & TRL3T < 10.5 ~ 17,
      AGE >= 70 & AGE < 80 & TRL3T > 0 & TRL3T < 3.5 ~ 18,
      
      TRUE ~ as.numeric(TRL3TSC)  # Keep existing value when no conditions match
      ),
      
      # Condition 4
      TRL4TSC = case_when(
      # Check for missing values first
      is.na(TRL4T) ~ NA_real_,
      
      # Age 50-59
      AGE >= 50 & AGE < 60 & TRL4T >= 203.5 ~ 1,
      AGE >= 50 & AGE < 60 & TRL4T >= 190.5 & TRL4T < 203.5 ~ 2,
      AGE >= 50 & AGE < 60 & TRL4T >= 177.5 & TRL4T < 190.5 ~ 3,
      AGE >= 50 & AGE < 60 & TRL4T >= 164.5 & TRL4T < 177.5 ~ 4,
      AGE >= 50 & AGE < 60 & TRL4T >= 151.5 & TRL4T < 164.5 ~ 5,
      AGE >= 50 & AGE < 60 & TRL4T >= 138.5 & TRL4T < 151.5 ~ 6,
      AGE >= 50 & AGE < 60 & TRL4T >= 125.5 & TRL4T < 138.5 ~ 7,
      AGE >= 50 & AGE < 60 & TRL4T >= 112.5 & TRL4T < 125.5 ~ 8,
      AGE >= 50 & AGE < 60 & TRL4T >= 99.5 & TRL4T < 112.5 ~ 9,
      AGE >= 50 & AGE < 60 & TRL4T >= 86.5 & TRL4T < 99.5 ~ 10,
      AGE >= 50 & AGE < 60 & TRL4T >= 73.5 & TRL4T < 86.5 ~ 11,
      AGE >= 50 & AGE < 60 & TRL4T >= 60.5 & TRL4T < 73.5 ~ 12,
      AGE >= 50 & AGE < 60 & TRL4T >= 47.5 & TRL4T < 60.5 ~ 13,
      AGE >= 50 & AGE < 60 & TRL4T >= 34.5 & TRL4T < 47.5 ~ 14,
      AGE >= 50 & AGE < 60 & TRL4T >= 21.5 & TRL4T < 34.5 ~ 15,
      AGE >= 50 & AGE < 60 & TRL4T >= 8.5 & TRL4T < 21.5 ~ 16,
      AGE >= 50 & AGE < 60 & TRL4T > 0 & TRL4T < 8.5 ~ 17,
      
      # Age 60-69
      AGE >= 60 & AGE < 70 & TRL4T >= 237.5 ~ 1,
      AGE >= 60 & AGE < 70 & TRL4T >= 223.5 & TRL4T < 237.5 ~ 2,
      AGE >= 60 & AGE < 70 & TRL4T >= 208.5 & TRL4T < 223.5 ~ 3,
      AGE >= 60 & AGE < 70 & TRL4T >= 193.5 & TRL4T < 208.5 ~ 4,
      AGE >= 60 & AGE < 70 & TRL4T >= 179.5 & TRL4T < 193.5 ~ 5,
      AGE >= 60 & AGE < 70 & TRL4T >= 164.5 & TRL4T < 179.5 ~ 6,
      AGE >= 60 & AGE < 70 & TRL4T >= 149.5 & TRL4T < 164.5 ~ 7,
      AGE >= 60 & AGE < 70 & TRL4T >= 135.5 & TRL4T < 149.5 ~ 8,
      AGE >= 60 & AGE < 70 & TRL4T >= 120.5 & TRL4T < 135.5 ~ 9,
      AGE >= 60 & AGE < 70 & TRL4T >= 105.5 & TRL4T < 120.5 ~ 10,
      AGE >= 60 & AGE < 70 & TRL4T >= 91.5 & TRL4T < 105.5 ~ 11,
      AGE >= 60 & AGE < 70 & TRL4T >= 76.5 & TRL4T < 91.5 ~ 12,
      AGE >= 60 & AGE < 70 & TRL4T >= 61.5 & TRL4T < 76.5 ~ 13,
      AGE >= 60 & AGE < 70 & TRL4T >= 47.5 & TRL4T < 61.5 ~ 14,
      AGE >= 60 & AGE < 70 & TRL4T >= 32.5 & TRL4T < 47.5 ~ 15,
      AGE >= 60 & AGE < 70 & TRL4T >= 17.5 & TRL4T < 32.5 ~ 16,
      AGE >= 60 & AGE < 70 & TRL4T >= 3.5 & TRL4T < 17.5 ~ 17,
      AGE >= 60 & AGE < 70 & TRL4T > 0 & TRL4T < 3.5 ~ 18,
      
      # Age 70-79
      AGE >= 70 & AGE < 80 & TRL4T >= 237.5 ~ 2,
      AGE >= 70 & AGE < 80 & TRL4T >= 222.5 & TRL4T < 237.5 ~ 3,
      AGE >= 70 & AGE < 80 & TRL4T >= 207.5 & TRL4T < 222.5 ~ 4,
      AGE >= 70 & AGE < 80 & TRL4T >= 192.5 & TRL4T < 207.5 ~ 5,
      AGE >= 70 & AGE < 80 & TRL4T >= 177.5 & TRL4T < 192.5 ~ 6,
      AGE >= 70 & AGE < 80 & TRL4T >= 162.5 & TRL4T < 177.5 ~ 7,
      AGE >= 70 & AGE < 80 & TRL4T >= 147.5 & TRL4T < 162.5 ~ 8,
      AGE >= 70 & AGE < 80 & TRL4T >= 132.5 & TRL4T < 147.5 ~ 9,
      AGE >= 70 & AGE < 80 & TRL4T >= 117.5 & TRL4T < 132.5 ~ 10,
      AGE >= 70 & AGE < 80 & TRL4T >= 102.5 & TRL4T < 117.5 ~ 11,
      AGE >= 70 & AGE < 80 & TRL4T >= 87.5 & TRL4T < 102.5 ~ 12,
      AGE >= 70 & AGE < 80 & TRL4T >= 72.5 & TRL4T < 87.5 ~ 13,
      AGE >= 70 & AGE < 80 & TRL4T >= 57.5 & TRL4T < 72.5 ~ 14,
      AGE >= 70 & AGE < 80 & TRL4T >= 42.5 & TRL4T < 57.5 ~ 15,
      AGE >= 70 & AGE < 80 & TRL4T >= 27.5 & TRL4T < 42.5 ~ 16,
      AGE >= 70 & AGE < 80 & TRL4T >= 12.5 & TRL4T < 27.5 ~ 17,
      AGE >= 70 & AGE < 80 & TRL4T > 0 & TRL4T < 12.5 ~ 18,
      
      TRUE ~ as.numeric(TRL4TSC)  # Keep existing value when no conditions match
      ),
      
      # Condition 5 - Updated according to SAS code
      TRL5TSC = case_when(
      # Check for missing values first
      is.na(TRL5T) ~ NA_real_,
      
      # Age 50-59
      AGE >= 50 & AGE < 60 & TRL5T >= 90.5 ~ 1,
      AGE >= 50 & AGE < 60 & TRL5T >= 84.5 & TRL5T < 90.5 ~ 2,
      AGE >= 50 & AGE < 60 & TRL5T >= 77.5 & TRL5T < 84.5 ~ 3,
      AGE >= 50 & AGE < 60 & TRL5T >= 70.5 & TRL5T < 77.5 ~ 4,
      AGE >= 50 & AGE < 60 & TRL5T >= 64.5 & TRL5T < 70.5 ~ 5,
      AGE >= 50 & AGE < 60 & TRL5T >= 57.5 & TRL5T < 64.5 ~ 6,
      AGE >= 50 & AGE < 60 & TRL5T >= 50.5 & TRL5T < 57.5 ~ 7,
      AGE >= 50 & AGE < 60 & TRL5T >= 44.5 & TRL5T < 50.5 ~ 8,
      AGE >= 50 & AGE < 60 & TRL5T >= 37.5 & TRL5T < 44.5 ~ 9,
      AGE >= 50 & AGE < 60 & TRL5T >= 30.5 & TRL5T < 37.5 ~ 10,
      AGE >= 50 & AGE < 60 & TRL5T >= 23.5 & TRL5T < 30.5 ~ 11,
      AGE >= 50 & AGE < 60 & TRL5T >= 17.5 & TRL5T < 23.5 ~ 12,
      AGE >= 50 & AGE < 60 & TRL5T >= 12.5 & TRL5T < 17.5 ~ 13,
      AGE >= 50 & AGE < 60 & TRL5T >= 8.5 & TRL5T < 12.5 ~ 14,
      AGE >= 50 & AGE < 60 & TRL5T >= 2.5 & TRL5T < 8.5 ~ 15,
      AGE >= 50 & AGE < 60 & TRL5T > 0 & TRL5T < 2.5 ~ 16,
      
      # Age 60-69
      AGE >= 60 & AGE < 70 & TRL5T >= 103.5 ~ 1,
      AGE >= 60 & AGE < 70 & TRL5T >= 96.5 & TRL5T < 103.5 ~ 2,
      AGE >= 60 & AGE < 70 & TRL5T >= 89.5 & TRL5T < 96.5 ~ 3,
      AGE >= 60 & AGE < 70 & TRL5T >= 81.5 & TRL5T < 89.5 ~ 4,
      AGE >= 60 & AGE < 70 & TRL5T >= 74.5 & TRL5T < 81.5 ~ 5,
      AGE >= 60 & AGE < 70 & TRL5T >= 67.5 & TRL5T < 74.5 ~ 6,
      AGE >= 60 & AGE < 70 & TRL5T >= 59.5 & TRL5T < 67.5 ~ 7,
      AGE >= 60 & AGE < 70 & TRL5T >= 52.5 & TRL5T < 59.5 ~ 8,
      AGE >= 60 & AGE < 70 & TRL5T >= 45.5 & TRL5T < 52.5 ~ 9,
      AGE >= 60 & AGE < 70 & TRL5T >= 37.5 & TRL5T < 45.5 ~ 10,
      AGE >= 60 & AGE < 70 & TRL5T >= 30.5 & TRL5T < 37.5 ~ 11,
      AGE >= 60 & AGE < 70 & TRL5T >= 23.5 & TRL5T < 30.5 ~ 12,
      AGE >= 60 & AGE < 70 & TRL5T >= 15.5 & TRL5T < 23.5 ~ 13,
      AGE >= 60 & AGE < 70 & TRL5T >= 8.5 & TRL5T < 15.5 ~ 14,
      AGE >= 60 & AGE < 70 & TRL5T >= 2.5 & TRL5T < 8.5 ~ 15,
      AGE >= 60 & AGE < 70 & TRL5T > 0 & TRL5T < 2.5 ~ 16,
      
      # Age 70-79
      AGE >= 70 & AGE < 80 & TRL5T >= 114.5 ~ 1,
      AGE >= 70 & AGE < 80 & TRL5T >= 106.5 & TRL5T < 114.5 ~ 2,
      AGE >= 70 & AGE < 80 & TRL5T >= 98.5 & TRL5T < 106.5 ~ 3,
      AGE >= 70 & AGE < 80 & TRL5T >= 91.5 & TRL5T < 98.5 ~ 4,
      AGE >= 70 & AGE < 80 & TRL5T >= 83.5 & TRL5T < 91.5 ~ 5,
      AGE >= 70 & AGE < 80 & TRL5T >= 75.5 & TRL5T < 83.5 ~ 6,
      AGE >= 70 & AGE < 80 & TRL5T >= 68.5 & TRL5T < 75.5 ~ 7,
      AGE >= 70 & AGE < 80 & TRL5T >= 60.5 & TRL5T < 68.5 ~ 8,
      AGE >= 70 & AGE < 80 & TRL5T >= 52.5 & TRL5T < 60.5 ~ 9,
      AGE >= 70 & AGE < 80 & TRL5T >= 45.5 & TRL5T < 52.5 ~ 10,
      AGE >= 70 & AGE < 80 & TRL5T >= 37.5 & TRL5T < 45.5 ~ 11,
      AGE >= 70 & AGE < 80 & TRL5T >= 29.5 & TRL5T < 37.5 ~ 12,
      AGE >= 70 & AGE < 80 & TRL5T >= 22.5 & TRL5T < 29.5 ~ 13,
      AGE >= 70 & AGE < 80 & TRL5T >= 14.5 & TRL5T < 22.5 ~ 14,
      AGE >= 70 & AGE < 80 & TRL5T >= 6.5 & TRL5T < 14.5 ~ 15,
      AGE >= 70 & AGE < 80 & TRL5T > 0 & TRL5T < 6.5 ~ 16,
      
      TRUE ~ as.numeric(TRL5TSC)  # Keep existing value when no conditions match
      )
    )
  
  return(df)
}