# Code 6.9: Novel Adaptative imputation ----

# Short explanation: impute with the mean time series. 

## Open data ----
source("Code/6.0 Imputation_preparation.R")
rm(dir, path_data, name_data)
names(data_list)

## Mean imputation ----
data_list <- map(data_list, ~ .x |> 
  mutate(across(c("daily_log_pm25", "daily_log_o3"), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))))

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
  mutate(Method = "Mean") |> 
  arrange(Variable, Mechanism, Missing) |> 
  relocate(Method, Variable, Mechanism, Missing)

eval_table

filename <- paste0("Mean", "_Imputation_Validation", ".xlsx")
writexl::write_xlsx(eval_table, paste0("Output/Models/", filename))
