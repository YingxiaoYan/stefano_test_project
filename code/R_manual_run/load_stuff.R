# ------------------------------------------------------------------------------------------- #
# Shared setup chunk used across multiple Quarto markdown files for `report-ineternal.qmd`
# - Requires:
#   - `params$norm_method`
# ------------------------------------------------------------------------------------------- #
withr::with_dir("..", {
  #setwd("..")
  # Load packages and project local libraries
  options(box.path = "code/") # Path to project local libraries
  box::use(
    SumExp, # Light SummarizedExperiment, to use `[`
    util = projlib / msdial_utils, # Utility functions for MS-Dial data
    projlib / msdial, # Handle MS-Dial files
    projlib / show, # Functions to present data
    ggplot2[...], # Too long to write and usually specific enough
    io = projlib / check_io_exist, # Check input/output files
  )
  
  
  params_yml <- yaml::read_yaml("params.yml")
  

}) 
#setwd("code/")