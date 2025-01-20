# Code 4: Classical imputation ----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

# Data
#daily_pm25_MCAR_5.csv
#daily_pm25_MCAR_10.csv
#daily_pm25_MCAR_20.csv
#daily_pm25_MCAR_40.csv
#daily_pm25_MAR_5.csv
#daily_pm25_MAR_10.csv
#daily_pm25_MAR_20.csv
#daily_pm25_MAR_40.csv
#daily_pm25_MNAR_5.csv
#daily_pm25_MNAR_10.csv
#daily_pm25_MNAR_20.csv
#daily_pm25_MNAR_40.csv

#daily_o3_MCAR_5.csv
#daily_o3_MCAR_10.csv
#daily_o3_MCAR_20.csv
#daily_o3_MCAR_40.csv
#daily_o3_MAR_5.csv
#daily_o3_MAR_10.csv
#daily_o3_MAR_20.csv
#daily_o3_MAR_40.csv
#daily_o3_MNAR_5.csv
#daily_o3_MNAR_10.csv
#daily_o3_MNAR_20.csv
#daily_o3_MNAR_40.csv

# Imputation tecniques ----

#	1. Imputación Media: Sustitución de valores faltantes por la media.
#	2. Imputación Condicional por Media: Uso de medias condicionadas por otras variables.
#	3. K-Nearest Neighbors (KNN): Imputación basada en la distancia a observaciones cercanas.
#	4. Imputación Múltiple (MI): Imputación iterativa con modelos de regresión.

# Lista de archivos con datos simulados
files <- list.files("simulated_missing_data/", pattern = "\\.csv$", full.names = TRUE)

# Función para aplicar diferentes métodos de imputación a cada archivo
# Lista de archivos con datos simulados
files <- list.files("simulated_missing_data/", pattern = "\\.csv$", full.names = TRUE)

# Función para la imputación condicional por modelo de regresión lineal (LM)
impute_conditional_lm <- function(df, target_var, predictors) {
  # Filtrar filas sin valores faltantes en la variable objetivo
  complete_cases <- df %>%
    filter(!is.na(.data[[target_var]])) %>%
    select(all_of(c(target_var, predictors)))

  # Ajustar el modelo de regresión lineal
  formula <- as.formula(paste(target_var, "~", paste(predictors, collapse = " + ")))
  model <- lm(formula, data = complete_cases)

  # Predecir valores faltantes en el conjunto de datos original
  df <- df %>%
    mutate(predicted_value = predict(model, newdata = df))

  # Reemplazar valores faltantes por las predicciones del modelo
  df[[target_var]] <- ifelse(is.na(df[[target_var]]), df$predicted_value, df[[target_var]])

  # Eliminar la columna de predicción temporal
  df <- select(df, -predicted_value)
  
  return(df)
}

# Función para aplicar diferentes métodos de imputación a cada archivo
impute_and_save <- function(file) {
  df <- read_csv(file)
  
  # Imputación por media
  df_mean <- df %>% 
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  write_csv(df_mean, sub(".csv", "_imputed_mean.csv", file))
  
  # Imputación condicional por media (agrupado por año)
  df_conditional <- df %>% 
    group_by(year) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    ungroup()
  write_csv(df_conditional, sub(".csv", "_imputed_conditional_mean.csv", file))
  
  # Imputación por modelo de regresión lineal (LM)
  target_variable <- str_extract(basename(file), "daily_[a-z0-9]+")  # Extraer la variable objetivo del nombre del archivo
  predictors <- c("daily_hum", "daily_temp", "daily_wspd")
  if (target_variable %in% names(df)) {
    df_lm <- impute_conditional_lm(df, target_variable, predictors)
    write_csv(df_lm, sub(".csv", "_imputed_lm.csv", file))
  }

  # Imputación KNN
  df_knn <- kNN(df, variable = names(df)[sapply(df, is.numeric)], k = 5)
  write_csv(df_knn, sub(".csv", "_imputed_knn.csv", file))
  
  # Imputación Múltiple
  predictor_matrix <- quickpred(df_selected, exclude = c("year", "month", "day", "daily_hum", "daily_temp", "daily_wspd"))

  imputed_mice <- mice(df, m = 5, 
    method = "pmm",
    predictorMatrix = predictor_matrix,
    seed = 123)
  
  df_mice <- complete(imputed_mice)
  write_csv(df_mice, sub(".csv", "_imputed_mi.csv", file))
  
  message("Imputaciones guardadas para: ", file)
}