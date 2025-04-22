# Code 6: Load data imputation ----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path ----
rm(list = ls())
dir <- "Data/Output/Simulate_miss_data"
name_data <- list.files(dir, pattern = "\\.csv$", full.names = FALSE)
path_data <- file.path(dir, name_data)
path_data

# Open test data ----
data_list <- set_names(
  lapply(path_data, readr::read_csv),
  tools::file_path_sans_ext(name_data)
)

names(data_list)
# Verification
#str(data_list, max.level = 1)