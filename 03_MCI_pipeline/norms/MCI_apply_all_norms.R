# Master function to apply all normative conversions

library(dplyr)

# Try to get the directory where this script is located
# First try using sys.frame
try_script_dir <- tryCatch({
  dirname(sys.frame(1)$ofile)
}, error = function(e) {
  NA_character_
})

# If that fails, use a fixed path to where we know the norms are
if (is.na(try_script_dir) || !dir.exists(try_script_dir)) {
  # Look in common locations
  potential_dirs <- c(
    "./code/pe_gee_manuscript/03_MCI_pipeline/norms",
    "~/netshare/M/Projects/PracEffects_GEE/code/pe_gee_manuscript/03_MCI_pipeline/norms",
    "./03_MCI_pipeline/norms"
  )
  
  script_dir <- NULL
  for (dir in potential_dirs) {
    if (dir.exists(dir)) {
      script_dir <- dir
      cat("Using norms directory:", script_dir, "\n")
      break
    }
  }
  
  if (is.null(script_dir)) {
    # Fall back to current directory
    script_dir <- "."
    warning("Could not find norms directory, using current directory")
  }
} else {
  script_dir <- try_script_dir
}

# List of norm scripts to source
norm_scripts <- c(
  "MCI_cvlt.R",
  "MCI_wasi_matrix.R",
  "MCI_wms_logical_memory.R",
  "MCI_wms_digit_span.R",
  "MCI_wms_letter_number_sequencing.R", 
  "MCI_dkefs_trailmaking.R",
  "MCI_wms_spatial_span.R",
  "MCI_stroop.R",
  "MCI_letter_category_fluency.R",
  "MCI_wms_visual_reproduction.R",
  "MCI_boston_naming.R"
)

# Use more robust sourcing - check norm files actually exist
for (norm_script in norm_scripts) {
  norm_path <- file.path(script_dir, norm_script)
  if (file.exists(norm_path)) {
    cat("Sourcing norm script:", norm_path, "\n")
    source(norm_path)
  } else {
    # If can't find in script_dir, try looking in norms directory in various locations
    possible_paths <- c(
      file.path("~/netshare/M/Projects/PracEffects_GEE/code/pe_gee_manuscript/03_MCI_pipeline/norms", norm_script),
      file.path("./code/pe_gee_manuscript/03_MCI_pipeline/norms", norm_script),
      file.path("./03_MCI_pipeline/norms", norm_script)
    )
    
    found <- FALSE
    for (path in possible_paths) {
      if (file.exists(path)) {
        cat("Sourcing norm script from alternative path:", path, "\n")
        source(path)
        found <- TRUE
        break
      }
    }
    
    if (!found) {
      stop("Could not find norm script: ", norm_script, " in any expected location")
    }
  }
}

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
