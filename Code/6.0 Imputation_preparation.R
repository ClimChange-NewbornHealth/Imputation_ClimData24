# Code 6: Load data imputation ----

## Settings ----
source("Code/0.1 Settings.R")
#source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path ----
#rm(list = ls())
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

# Open validation data ----
load(file=paste0("Data/Output/", "Imputation_train_data", ".RData")) 

# Imputation Components  ----

# daily_log_pm25
# daily_log_o3

# Use 3 missing mechanism: MCAR, MAR, MNAR
# Use 4 porcentual missings: 5%, 10%, 20%, 40%

# Then compare with original serie and estimate adjust index:
# RMSE: Root Mean Squared Error
# MAE: Mean Absolute Error
# PAEM: Percent Absolute Error in Means
# Bias
# IA: Index of Agreement
# CE: Coefficient of Efficiency
# R2: Coefficient of Determination

# Evaluation metrics -----

compute_metrics <- function(original, imputed, variable) {
  df <- tibble(truth = original[[variable]], estimate = imputed[[variable]]) |> 
    filter(!is.na(truth))

  rmse  <- rmse(df, truth = truth, estimate = estimate)$.estimate
  mae   <- mae(df, truth = truth, estimate = estimate)$.estimate
  bias  <- mean(df$estimate - df$truth, na.rm = TRUE)
  paem  <- abs(mean(df$estimate - df$truth) / mean(df$truth)) * 100

  ia_n  <- sum((df$estimate - df$truth)^2)
  ia_d  <- sum((abs(df$estimate - mean(df$truth)) + abs(df$truth - mean(df$truth)))^2)
  ia    <- 1 - ia_n / ia_d

  ce_n  <- sum((df$truth - df$estimate)^2)
  ce_d  <- sum((df$truth - mean(df$truth))^2)
  ce    <- 1 - ce_n / ce_d

  r2    <- cor(df$truth, df$estimate)^2

  return(tibble(RMSE = rmse, MAE = mae, PAEM = paem, Bias = bias, IA = ia, CE = ce, R2 = r2))
}
