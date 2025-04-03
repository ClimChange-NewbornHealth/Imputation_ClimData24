# Code 3: Simulated missing data ----

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

# Identify year with the most complete data 
find_best_station_year <- function(df, variable) {
  df |>
    filter(!is.na(.data[[variable]])) |>
    group_by(station, year) |>
    summarise(non_missing = n(), .groups = "drop") |>
    arrange(desc(non_missing)) |>
    slice(1)
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



# Función para introducir valores faltantes en una columna específica
introduce_missingness <- function(df, column, mechanism, percentage, dependent_col = NULL) {
  n <- nrow(df)
  missing_indices <- NULL
  
  if (mechanism == "MCAR") {
    missing_indices <- sample(seq_len(n), size = floor(n * percentage))
    
  } else if (mechanism == "MAR" && !is.null(dependent_col)) { 
    prob <- rank(df[[dependent_col]], na.last = "keep") / sum(!is.na(df[[dependent_col]]))
    prob[is.na(prob)] <- 0
    missing_indices <- sample(seq_len(n), size = floor(n * percentage), prob = prob)
    
  } else if (mechanism == "MNAR") {
    prob <- rank(df[[column]], na.last = "keep") / sum(!is.na(df[[column]]))
    prob[is.na(prob)] <- 0
    missing_indices <- sample(seq_len(n), size = floor(n * percentage), prob = prob)
  }

  df[missing_indices, column] <- NA
  return(df)
}

# Función para generar datos simulados con missingness
generate_simulated_data <- function(df, variable, mechanisms, percentages, output_dir, dependent_col = NULL) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  for (mech in mechanisms) {
    for (perc in percentages) {
      simulated_data <- introduce_missingness(
        df = df,
        column = variable,
        mechanism = mech,
        percentage = perc,
        dependent_col = dependent_col
      )

      filename <- paste0(variable, "_", mech, "_", perc * 100, ".csv")
      write_csv(simulated_data, file.path(output_dir, filename))
    }
  }
}

# Identificación del año con más datos completos por contaminante
best_year_pm25 <- find_best_station_year(date_series, "daily_pm25")
best_year_o3 <- find_best_station_year(date_series, "daily_o3")

# Filtrar datos del mejor año para cada contaminante
pm25_data <- filter(date_series, year == best_year_pm25)
o3_data <- filter(date_series, year == best_year_o3)

# Definir mecanismos y porcentajes de datos faltantes a simular
mechanisms <- c("MCAR", "MAR", "MNAR")
percentages <- c(0.05, 0.1, 0.2, 0.4)
output_dir <- "simulated_missing_data"

# Generar datos simulados para PM2.5
generate_simulated_data(pm25_data, "daily_pm25", mechanisms, percentages, output_dir, dependent_col = "daily_pm10")

# Generar datos simulados para Ozono
generate_simulated_data(o3_data, "daily_o3", mechanisms, percentages, output_dir, dependent_col = "daily_pm10")

# Confirmación de los archivos generados
message("Simulated data generated and saved in: ", output_dir)