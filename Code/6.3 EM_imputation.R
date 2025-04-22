# Code 6.3: Expectation Maximization imputation ----

# Short explanation: imputes missing values using the Expectation-Maximization algorithm, assuming multivariate normality. Implemented using both `norm` (classical EM) and `Amelia` (for time-series-aware EM with multiple imputations).

## Open data ----
source("Code/6.0 Imputation_preparation.R")
rm(dir, path_data, name_data)
names(data_list)

## EM imputation ----
# Use only numeric covariates with multivariate normal distribution 
log_vars <- c("daily_log_pm25", "daily_log_o3", "daily_log_pm10", "daily_log_wspd", "daily_temp", "daily_hum")

data_list_em <- imap(data_list, function(df, name) {
  cols_to_impute <- intersect(names(df), log_vars)
  df_mat <- df[, cols_to_impute]

  # Validar que hay al menos dos columnas para EM multivariado
  if (ncol(df_mat) >= 2 && any(is.na(df_mat))) {
    imputed_df <- tryCatch(
      impute_EM(df_mat, stochastic = FALSE),
      error = function(e) {
        warning(paste("impute_EM failed for", name)); return(df_mat)
      }
    )

    df[, cols_to_impute] <- imputed_df
  }

  return(df)
})

## Calculate metrics ----
results <- list()

for (name in names(data_list_em)) {
  original <- data_imp
  imputed  <- data_list_em[[name]]

  if (grepl("pm25", name)) var <- "daily_log_pm25"
  if (grepl("o3", name))   var <- "daily_log_o3"

  parts <- unlist(strsplit(name, "_"))
  mech <- toupper(parts[4])
  perc <- as.numeric(parts[5])

  res <- compute_metrics(original, imputed, var) |> 
    mutate(Method = "EM-missMethods", Variable = var, Mechanism = mech, Missing = perc)

  results[[name]] <- res
}

## Final table ----
eval_table <- bind_rows(results) |> 
  relocate(Method, Variable, Mechanism, Missing) |> 
  arrange(Variable, Mechanism, Missing)

eval_table

filename <- paste0("EM-missMethods", "_Imputation_Validation", ".xlsx")
writexl::write_xlsx(eval_table, paste0("Output/Models/", filename))

