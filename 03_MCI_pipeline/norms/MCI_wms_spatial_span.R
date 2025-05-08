# WMS-III Spatial Span normative scoring functions

library(dplyr)

norm_spatial_span <- function(df) {
  # Create default SSPSS column if it doesn't exist
  if (!"SSPSS" %in% names(df)) {
    df$SSPSS <- NA_real_
  }
  
  # Apply rounding to Spatial Span variables if they exist
  if (all(c("SSPFRAW", "SSPBRAW") %in% names(df))) {
    df <- df %>% mutate(
      SSPFRAW = round(SSPFRAW, 0),
      SSPBRAW = round(SSPBRAW, 0)
    )
  }
  
  df <- df %>%
    mutate(
      # Calculate total spatial span score
      SSPRAW = SSPFRAW + SSPBRAW,
      
      # Convert to scaled score based on age norms
      SSPSS = case_when(
        # Age 55-64
        AGE >= 55 & AGE < 65 & is.na(SSPRAW) ~ NA_real_,
        AGE >= 55 & AGE < 65 & SSPRAW == 0 ~ 1,
        AGE >= 55 & AGE < 65 & SSPRAW >= 1 & SSPRAW <= 4 ~ 2,
        AGE >= 55 & AGE < 65 & SSPRAW >= 5 & SSPRAW <= 7 ~ 3,
        AGE >= 55 & AGE < 65 & SSPRAW == 8 ~ 4,
        AGE >= 55 & AGE < 65 & SSPRAW == 9 ~ 5,
        AGE >= 55 & AGE < 65 & SSPRAW == 10 ~ 6,
        AGE >= 55 & AGE < 65 & SSPRAW >= 11 & SSPRAW <= 12 ~ 7,
        AGE >= 55 & AGE < 65 & SSPRAW == 13 ~ 8,
        AGE >= 55 & AGE < 65 & SSPRAW == 14 ~ 10,
        AGE >= 55 & AGE < 65 & SSPRAW == 15 ~ 11,
        AGE >= 55 & AGE < 65 & SSPRAW == 16 ~ 12,
        AGE >= 55 & AGE < 65 & SSPRAW >= 17 & SSPRAW <= 18 ~ 13,
        AGE >= 55 & AGE < 65 & SSPRAW == 19 ~ 14,
        AGE >= 55 & AGE < 65 & SSPRAW == 20 ~ 15,
        AGE >= 55 & AGE < 65 & SSPRAW >= 21 & SSPRAW <= 22 ~ 16,
        AGE >= 55 & AGE < 65 & SSPRAW >= 23 & SSPRAW <= 24 ~ 17,
        AGE >= 55 & AGE < 65 & SSPRAW >= 25 & SSPRAW <= 26 ~ 18,
        AGE >= 55 & AGE < 65 & SSPRAW >= 27 & SSPRAW <= 32 ~ 19,
        
        # Age 65-69
        AGE >= 65 & AGE < 70 & is.na(SSPRAW) ~ NA_real_,
        AGE >= 65 & AGE < 70 & SSPRAW >= 0 & SSPRAW <= 3 ~ 2,
        AGE >= 65 & AGE < 70 & SSPRAW >= 4 & SSPRAW <= 6 ~ 3,
        AGE >= 65 & AGE < 70 & SSPRAW >= 7 & SSPRAW <= 8 ~ 4,
        AGE >= 65 & AGE < 70 & SSPRAW == 9 ~ 5,
        AGE >= 65 & AGE < 70 & SSPRAW == 10 ~ 6,
        AGE >= 65 & AGE < 70 & SSPRAW == 11 ~ 7,
        AGE >= 65 & AGE < 70 & SSPRAW == 12 ~ 8,
        AGE >= 65 & AGE < 70 & SSPRAW == 13 ~ 10,
        AGE >= 65 & AGE < 70 & SSPRAW == 14 ~ 11,
        AGE >= 65 & AGE < 70 & SSPRAW == 15 ~ 12,
        AGE >= 65 & AGE < 70 & SSPRAW == 16 ~ 13,
        AGE >= 65 & AGE < 70 & SSPRAW == 17 ~ 14,
        AGE >= 65 & AGE < 70 & SSPRAW >= 18 & SSPRAW <= 19 ~ 15,
        AGE >= 65 & AGE < 70 & SSPRAW >= 20 & SSPRAW <= 22 ~ 16,
        AGE >= 65 & AGE < 70 & SSPRAW >= 23 & SSPRAW <= 24 ~ 17,
        AGE >= 65 & AGE < 70 & SSPRAW >= 25 & SSPRAW <= 26 ~ 18,
        AGE >= 65 & AGE < 70 & SSPRAW >= 27 & SSPRAW <= 32 ~ 19,
        
        # Age 70-74
        AGE >= 70 & AGE < 75 & is.na(SSPRAW) ~ NA_real_,
        AGE >= 70 & AGE < 75 & SSPRAW >= 0 & SSPRAW <= 3 ~ 2,
        AGE >= 70 & AGE < 75 & SSPRAW >= 4 & SSPRAW <= 6 ~ 3,
        AGE >= 70 & AGE < 75 & SSPRAW == 7 ~ 4,
        AGE >= 70 & AGE < 75 & SSPRAW == 8 ~ 5,
        AGE >= 70 & AGE < 75 & SSPRAW >= 9 & SSPRAW <= 10 ~ 7,
        AGE >= 70 & AGE < 75 & SSPRAW == 11 ~ 8,
        AGE >= 70 & AGE < 75 & SSPRAW == 12 ~ 9,
        AGE >= 70 & AGE < 75 & SSPRAW == 13 ~ 10,
        AGE >= 70 & AGE < 75 & SSPRAW == 14 ~ 11,
        AGE >= 70 & AGE < 75 & SSPRAW == 15 ~ 12,
        AGE >= 70 & AGE < 75 & SSPRAW == 16 ~ 14,
        AGE >= 70 & AGE < 75 & SSPRAW >= 17 & SSPRAW <= 18 ~ 15,
        AGE >= 70 & AGE < 75 & SSPRAW >= 19 & SSPRAW <= 20 ~ 16,
        AGE >= 70 & AGE < 75 & SSPRAW >= 21 & SSPRAW <= 23 ~ 17,
        AGE >= 70 & AGE < 75 & SSPRAW >= 24 & SSPRAW <= 25 ~ 18,
        AGE >= 70 & AGE < 75 & SSPRAW >= 26 & SSPRAW <= 32 ~ 19,
        
        # Age 75-79
        AGE >= 75 & AGE < 80 & is.na(SSPRAW) ~ NA_real_,
        AGE >= 75 & AGE < 80 & SSPRAW >= 0 & SSPRAW <= 3 ~ 2,
        AGE >= 75 & AGE < 80 & SSPRAW >= 4 & SSPRAW <= 6 ~ 3,
        AGE >= 75 & AGE < 80 & SSPRAW == 7 ~ 4,
        AGE >= 75 & AGE < 80 & SSPRAW == 8 ~ 5,
        AGE >= 75 & AGE < 80 & SSPRAW >= 9 & SSPRAW <= 10 ~ 7,
        AGE >= 75 & AGE < 80 & SSPRAW == 11 ~ 8,
        AGE >= 75 & AGE < 80 & SSPRAW == 12 ~ 9,
        AGE >= 75 & AGE < 80 & SSPRAW == 13 ~ 10,
        AGE >= 75 & AGE < 80 & SSPRAW == 14 ~ 11,
        AGE >= 75 & AGE < 80 & SSPRAW == 15 ~ 12,
        AGE >= 75 & AGE < 80 & SSPRAW == 16 ~ 14,
        AGE >= 75 & AGE < 80 & SSPRAW >= 17 & SSPRAW <= 18 ~ 15,
        AGE >= 75 & AGE < 80 & SSPRAW >= 19 & SSPRAW <= 20 ~ 16,
        AGE >= 75 & AGE < 80 & SSPRAW >= 21 & SSPRAW <= 23 ~ 17,
        AGE >= 75 & AGE < 80 & SSPRAW >= 24 & SSPRAW <= 25 ~ 18,
        AGE >= 75 & AGE < 80 & SSPRAW >= 26 & SSPRAW <= 32 ~ 19,
        
        # Age 80-84
        AGE >= 80 & AGE < 85 & is.na(SSPRAW) ~ NA_real_,
        AGE >= 80 & AGE < 85 & SSPRAW >= 0 & SSPRAW <= 2 ~ 2,
        AGE >= 80 & AGE < 85 & SSPRAW >= 3 & SSPRAW <= 4 ~ 3,
        AGE >= 80 & AGE < 85 & SSPRAW == 5 ~ 4,
        AGE >= 80 & AGE < 85 & SSPRAW == 6 ~ 5,
        AGE >= 80 & AGE < 85 & SSPRAW >= 7 & SSPRAW <= 8 ~ 6,
        AGE >= 80 & AGE < 85 & SSPRAW == 9 ~ 7,
        AGE >= 80 & AGE < 85 & SSPRAW == 10 ~ 8,
        AGE >= 80 & AGE < 85 & SSPRAW == 11 ~ 9,
        AGE >= 80 & AGE < 85 & SSPRAW == 12 ~ 10,
        AGE >= 80 & AGE < 85 & SSPRAW == 13 ~ 11,
        AGE >= 80 & AGE < 85 & SSPRAW == 14 ~ 12,
        AGE >= 80 & AGE < 85 & SSPRAW == 15 ~ 13,
        AGE >= 80 & AGE < 85 & SSPRAW == 16 ~ 15,
        AGE >= 80 & AGE < 85 & SSPRAW >= 17 & SSPRAW <= 18 ~ 16,
        AGE >= 80 & AGE < 85 & SSPRAW >= 19 & SSPRAW <= 20 ~ 17,
        AGE >= 80 & AGE < 85 & SSPRAW >= 21 & SSPRAW <= 23 ~ 18,
        AGE >= 80 & AGE < 85 & SSPRAW >= 24 & SSPRAW <= 32 ~ 19,
        
        # Age 85-89
        AGE >= 85 & AGE < 90 & is.na(SSPRAW) ~ NA_real_,
        AGE >= 85 & AGE < 90 & SSPRAW >= 0 & SSPRAW <= 2 ~ 2,
        AGE >= 85 & AGE < 90 & SSPRAW >= 3 & SSPRAW <= 4 ~ 3,
        AGE >= 85 & AGE < 90 & SSPRAW == 5 ~ 4,
        AGE >= 85 & AGE < 90 & SSPRAW == 6 ~ 5,
        AGE >= 85 & AGE < 90 & SSPRAW >= 7 & SSPRAW <= 8 ~ 6,
        AGE >= 85 & AGE < 90 & SSPRAW == 9 ~ 7,
        AGE >= 85 & AGE < 90 & SSPRAW == 10 ~ 8,
        AGE >= 85 & AGE < 90 & SSPRAW == 11 ~ 10,
        AGE >= 85 & AGE < 90 & SSPRAW == 12 ~ 11,
        AGE >= 85 & AGE < 90 & SSPRAW == 13 ~ 12,
        AGE >= 85 & AGE < 90 & SSPRAW == 14 ~ 13,
        AGE >= 85 & AGE < 90 & SSPRAW == 15 ~ 14,
        AGE >= 85 & AGE < 90 & SSPRAW == 16 ~ 15,
        AGE >= 85 & AGE < 90 & SSPRAW >= 17 & SSPRAW <= 18 ~ 16,
        AGE >= 85 & AGE < 90 & SSPRAW >= 19 & SSPRAW <= 20 ~ 17,
        AGE >= 85 & AGE < 90 & SSPRAW >= 21 & SSPRAW <= 23 ~ 18,
        AGE >= 85 & AGE < 90 & SSPRAW >= 24 & SSPRAW <= 32 ~ 19,
        
        TRUE ~ as.numeric(SSPSS)  # Keep existing value if no match
      )
    )
  
  return(df)
}
