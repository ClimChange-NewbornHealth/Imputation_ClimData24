# Code 1: Explorer missing values ----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

## PM2,5 ---- 

# Station in Santiago
files <- list.files(paste0(data_inp, "PM25"), full.names = TRUE)
files

data_pm25 <- set_names(files, nm = sub("_.*", "", basename(files))) |> 
  imap(~ {
    
    s <- sub("_.*", "", basename(.y))

    import(.x, delim=";") |> 
    janitor::clean_names() |>
    mutate(station=s) |>  
    mutate(pm25=as.numeric(registros_validados)) |> 
    adjust_dates() |> 
    mutate(
      year=year(date),
      month=month(date),
      day=day(date)
    ) |> 
    dplyr::select(station, date, year, month, day, time, datetime, pm25)
  }
)

## 03 ---- 
files <- list.files(paste0(data_inp, "O3"), full.names = TRUE)
files

data_o3 <- set_names(files, nm = sub("_.*", "", basename(files))) |> 
  imap(~ {
    
    s <- sub("_.*", "", basename(.y))

    import(.x, delim=";") |> 
    janitor::clean_names() |>
    mutate(station=s) |>  
    mutate(o3=as.numeric(registros_validados)) |> 
    adjust_dates() |> 
    mutate(
      year=year(date),
      month=month(date),
      day=day(date)
    ) |> 
    dplyr::select(station, date, year, month, day, time, datetime, o3)
  }
)

## Join MP2,5 and O3 ---- 

data_pm25 <- bind_rows(data_pm25)
data_o3 <- bind_rows(data_o3)

glimpse(data_pm25)
glimpse(data_o3)

summary(data_pm25)
summary(data_o3)

# Join all data
full_data <- data_pm25 |> 
  full_join(select(data_o3, station, datetime, o3), by=c("station", "datetime"))

glimpse(full_data)
summary(full_data)

# Save data
save(data_pm25, file=paste0(data_out, "series_pm25_2002_2024", ".RData"))
save(data_o3, file=paste0(data_out, "series_o3_2002_2024", ".RData"))
save(full_data, file=paste0(data_out, "series_full_2002_2024", ".RData"))