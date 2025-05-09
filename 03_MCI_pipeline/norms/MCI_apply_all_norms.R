# Master function to apply all normative conversions

library(dplyr)

# Get the directory where this script is located
script_dir <- dirname(sys.frame(1)$ofile)

# Source all individual normative functions from the same directory
source(file.path(script_dir, "MCI_cvlt.R"))
source(file.path(script_dir, "MCI_wasi_matrix.R"))
source(file.path(script_dir, "MCI_wms_logical_memory.R"))
source(file.path(script_dir, "MCI_wms_digit_span.R"))
source(file.path(script_dir, "MCI_wms_letter_number_sequencing.R"))
source(file.path(script_dir, "MCI_dkefs_trailmaking.R"))
source(file.path(script_dir, "MCI_wms_spatial_span.R"))
source(file.path(script_dir, "MCI_stroop.R"))
source(file.path(script_dir, "MCI_letter_category_fluency.R"))
source(file.path(script_dir, "MCI_wms_visual_reproduction.R"))
source(file.path(script_dir, "MCI_boston_naming.R"))

# Master function to apply all normative conversions in correct order
# Now with wave parameter to handle test availability differences
apply_all_norms <- function(df, wave = NULL) {
  # Convert all variable names to uppercase for consistency
  names(df) <- toupper(names(df))
  
  # Determine which tests are available based on the wave
  has_spatial_span <- TRUE
  has_boston_naming <- TRUE
  
  # If wave is specified, adjust test availability accordingly
  if (!is.null(wave)) {
    if (wave == 4) {
      has_spatial_span <- FALSE  # Wave 4 doesn't have Spatial Span
    } else if (wave == 1 || wave == 2) {
      has_boston_naming <- FALSE  # Waves 1 and 2 don't have Boston Naming Test
    }
  } else {
    # If wave is not specified, check for the presence of required raw variables
    if (!"SSPFRAW" %in% names(df) || !"SSPBRAW" %in% names(df)) {
      has_spatial_span <- FALSE
    }
    if (!"BNTTOTCOR" %in% names(df)) {
      has_boston_naming <- FALSE
    }
  }

  
  # Then apply test-specific normative conversions
  df <- norm_cvlt(df)
  df <- norm_wasi_matrix(df)
  df <- norm_logical_memory(df)
  df <- norm_digit_span(df)
  df <- norm_letter_number_sequencing(df)
  df <- norm_trails(df)
  df <- norm_stroop(df)
  df <- norm_fluency(df)
  df <- norm_visual_reproduction(df)
  
  # Apply test-specific normative conversions based on availability
  if (has_spatial_span) {
    df <- norm_spatial_span(df)
  }
  
  if (has_boston_naming) {
    df <- norm_boston_naming(df)
  }
  
  return(df)
}
