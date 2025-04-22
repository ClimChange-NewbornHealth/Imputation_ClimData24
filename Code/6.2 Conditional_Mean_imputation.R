# Code 6.2: Conditional Mean imputation ----

# Short explanation: impute missing values using the predicted values from a linear regression model with available covariates. This accounts for conditional structure in the data.

## Open data ----
source("Code/6.0 Imputation_preparation.R")
rm(dir, path_data, name_data)
names(data_list)

## Conditional Mean imputation (via lm on covariates) ----

impute_conditional_mean <- function(df, variable, covariates) {
  complete_cases <- df |> filter(!is.na(.data[[variable]]))

  # Fixed effects as factor
  if ("year" %in% covariates) df$year <- as.factor(df$year)
  if ("season" %in% covariates) df$season <- as.factor(df$season)
  if ("stat_aux" %in% covariates) df$stat_aux <- as.factor(df$stat_aux)

  # Validation numeric variables
  num_covs <- setdiff(covariates, c("year", "season", "stat_aux"))
  valid_covs <- covariates[sapply(complete_cases[num_covs], function(x) !all(is.na(x)) && sd(x, na.rm = TRUE) > 0)]

  if (length(valid_covs) < 2 || nrow(complete_cases) < 10) {
    warning(paste("Skipping", variable, "- insufficient data for model"))
    return(df)
  }

  # Fixed effects models
  formula <- as.formula(paste(variable, "~", paste(covariates, collapse = " + ")))

  model <- tryCatch(lm(formula, data = complete_cases), error = function(e) NULL)

  if (is.null(model)) {
    warning(paste("Model fitting failed for", variable))
    return(df)
  }

  df$predicted <- tryCatch(predict(model, newdata = df), error = function(e) NA)

  df[[variable]] <- ifelse(is.na(df[[variable]]) & !is.na(df$predicted), df$predicted, df[[variable]])
  df <- select(df, -predicted)

  return(df)
}

# Estimate
covariates <- c("daily_log_pm10", "daily_log_wspd", "daily_temp", "daily_hum", "year", "season", "stat_aux")

data_list <- map(data_list, ~ .x |> 
  impute_conditional_mean("daily_log_pm25", covariates) |> 
  impute_conditional_mean("daily_log_o3", covariates))

## Evaluation metrics ----

results <- list()

for (name in names(data_list)) {
  original <- data_imp
  imputed  <- data_list[[name]]

  if (grepl("pm25", name)) var <- "daily_log_pm25"
  if (grepl("o3", name))   var <- "daily_log_o3"

  # Extraer mecanismo y porcentaje del nombre
  parts <- unlist(strsplit(name, "_"))
  mech <- toupper(parts[4])
  perc <- as.numeric(parts[5])

  # Calcular métricas y añadir metadatos
  res <- compute_metrics(original, imputed, var) |> 
    mutate(Variable = var, Mechanism = mech, Missing = perc)

  results[[name]] <- res
}

eval_table <- bind_rows(results) |> 
  mutate(Method = "Conditional Mean") |> 
  arrange(Variable, Mechanism, Missing) |> 
  relocate(Method, Variable, Mechanism, Missing)

# Output table
filename <- paste0("ConditionalMean", "_Imputation_Validation", ".xlsx")
writexl::write_xlsx(eval_table, paste0("Output/Models/", filename))
