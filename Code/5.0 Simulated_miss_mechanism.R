# Code 5: Simulated missing data ----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

# Load wide data
load(file=paste0(data_out, "series_daily_cont12h_2000_2023_wide", ".RData"))
#save(data_long, file=paste0(data_out, "series_daily_cont12h_2000_2023_long", ".RData")) # WIDE FORMAT
glimpse(date_series)

## Identify non-missing data ----

complete_series <- date_series |> 
  filter(
    !is.na(daily_pm25),
    !is.na(daily_pm10),
    !is.na(daily_o3),
    !is.na(daily_hum),
    !is.na(daily_temp),
    !is.na(daily_wspd)
  )

glimpse(complete_series)

# Random Sample per day
set.seed(123) # Para reproducibilidad
random_series <- complete_series |> 
  group_by(date) |> 
  slice_sample(n = 1) |> 
  ungroup()

# Identify station-year with complete data
station_year_summary <- date_series |> 
  filter(!is.na(daily_pm25)) |> 
  group_by(station, year) |> 
  summarise(n = n(), .groups = "drop") |> 
  arrange(desc(n))

station_year_summary

find_best_station_year <- function(df, variable) {
  df |>
    filter(!is.na(.data[[variable]])) |>
    group_by(station, year) |>
    summarise(non_missing = n(), .groups = "drop") |>
    arrange(stattion, year, desc(non_missing)) #|>
    #slice(1)
}

date_series |>
  filter(year>=2009 & year<=2016) |> 
  filter(!is.na(daily_pm25)) |>
  group_by(station, year) |>
  summarise(non_missing = n(), .groups = "drop") |>
  arrange(desc(non_missing))

date_series |>
  filter(year>=2009 & year<=2016) |> 
  filter(!is.na(daily_o3)) |>
  group_by(station, year) |>
  summarise(non_missing = n(), .groups = "drop") |>
  arrange(desc(non_missing))

# Select imputation data test
data_imp <- date_series |> 
  filter(stat_aux == "S02-Cerro Navia" & year == 2012)

summary(data_imp)
glimpse(data_imp)

save(data_imp, file=paste0(data_out, "Imputation_train_data", ".RData"))

## Generate simulated data ----

# Missing Process
introduce_missingness <- function(df, column, mechanism, percentage, dependent_cols = NULL) {
  n <- nrow(df)
  missing_indices <- NULL
  
  if (mechanism == "MCAR") {
    # Random process
    missing_indices <- sample(seq_len(n), size = ceiling(n * percentage))
    
  } else if (mechanism == "MAR" && !is.null(dependent_cols)) {
    # MAR: missing depends of observerd covariates
    prob_df <- df[dependent_cols]
    
    # Matrix with NA covariates
    prob_matrix <- prob_df |> 
      mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) |> 
      as.matrix()
    
    # Score with matrix
    score <- rowMeans(scale(prob_matrix))
    score[is.na(score)] <- 0
    prob <- rank(score) / sum(!is.na(score))
    
    missing_indices <- sample(seq_len(n), size = ceiling(n * percentage), prob = prob)
    
  } else if (mechanism == "MNAR") {
    # MNAR: depends on the same variable
    prob <- rank(df[[column]], na.last = "keep") / sum(!is.na(df[[column]]))
    prob[is.na(prob)] <- 0
    missing_indices <- sample(seq_len(n), size = ceiling(n * percentage), prob = prob)
  }

  df[missing_indices, column] <- NA
  return(df)
}

# Generate simulated data
generate_simulated_data <- function(df, variable, mechanisms, percentages, output_dir, mar_covariates = NULL) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  for (mech in mechanisms) {
    for (perc in percentages) {
      simulated_data <- introduce_missingness(
        df = df,
        column = variable,
        mechanism = mech,
        percentage = perc,
        dependent_cols = if (mech == "MAR") mar_covariates else NULL
      )

      filename <- paste0(variable, "_", mech, "_", perc * 100, ".csv")
      write_csv(simulated_data, file.path(output_dir, filename))
    }
  }
}

# Inputs
mar_covs <- c("daily_log_pm10", "daily_temp", "daily_hum", "daily_log_wspd")
mechanisms <- c("MCAR", "MAR", "MNAR")
percentages <- c(0.05, 0.1, 0.2, 0.4)
output_dir <- "Data/Output/Simulate_miss_data/"

# Simulated data PM2.5
generate_simulated_data(data_imp, "daily_log_pm25", mechanisms, percentages, output_dir, mar_covariates = mar_covs)

# Simulated data Ozone
generate_simulated_data(data_imp, "daily_log_o3", mechanisms, percentages, output_dir, mar_covariates = mar_covs)
