# Boston Naming Test normative scoring functions

library(dplyr)

norm_boston_naming <- function(df) {
  # Create default BNTSS column if it doesn't exist
  if (!"BNTSS" %in% names(df)) {
    df$BNTSS <- NA_real_
  }
  
   # Apply rounding to BNT variables
  df <- df %>%
    mutate(
      BNTTOTCOR = round(BNTTOTCOR, 0)
    )

  df <- df %>%
    mutate(
      # Boston Naming Test scaled scores based on raw score
      BNTSS = case_when(
        is.na(BNTTOTCOR) ~ NA_real_,
        BNTTOTCOR == 60 ~ 16,
        BNTTOTCOR == 59 ~ 14,
        BNTTOTCOR == 58 ~ 13,
        BNTTOTCOR == 57 ~ 11,
        BNTTOTCOR == 56 ~ 10,
        BNTTOTCOR >= 54 & BNTTOTCOR <= 55 ~ 9,
        BNTTOTCOR >= 51 & BNTTOTCOR <= 53 ~ 8,
        BNTTOTCOR >= 47 & BNTTOTCOR <= 50 ~ 7,
        BNTTOTCOR >= 45 & BNTTOTCOR <= 46 ~ 6,
        BNTTOTCOR >= 40 & BNTTOTCOR <= 44 ~ 5,
        BNTTOTCOR >= 28 & BNTTOTCOR <= 39 ~ 4,
        BNTTOTCOR >= 26 & BNTTOTCOR <= 27 ~ 3,
        BNTTOTCOR >= 14 & BNTTOTCOR <= 25 ~ 2,
        BNTTOTCOR > 0 & BNTTOTCOR <= 13 ~ 1,
        TRUE ~ as.numeric(BNTSS)  # Keep existing value if no match
      )
    )
  
  return(df)
}
