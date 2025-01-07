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
    dplyr::select(station, date, year, month, day, time, datetime, pm25) |> 
      filter(year>=2000 & year<=2023)
  }
)

data_pm25 <- bind_rows(data_pm25)

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
    dplyr::select(station, date, year, month, day, time, datetime, o3) |> 
      filter(year>=2000 & year<=2023)
  }
)

data_o3 <- bind_rows(data_o3)

## Covariates ---- 

### PM10 

files <- list.files(paste0(data_inp, "PM10"), full.names = TRUE)

data_pm10 <- set_names(files, nm = sub("_.*", "", basename(files))) |> 
  imap(~ {
    
    s <- sub("_.*", "", basename(.y))

    import(.x, delim=";") |> 
    janitor::clean_names() |>
    mutate(station=s) |>  
    mutate(pm10=as.numeric(registros_validados)) |> 
    adjust_dates() |> 
    mutate(
      year=year(date),
      month=month(date),
      day=day(date)
    ) |> 
    dplyr::select(station, date, year, month, day, time, datetime, pm10) |> 
      filter(year>=2000 & year<=2023)
  }
)

data_pm10 <- bind_rows(data_pm10)

### RHUM

files <- list.files(paste0(data_inp, "RHUM"), full.names = TRUE)

data_hum <- set_names(files, nm = sub("_.*", "", basename(files))) |> 
  imap(~ {
    
    s <- sub("_.*", "", basename(.y))

    import(.x, delim=";") |> 
    janitor::clean_names() |>
    mutate(station=s) |>  
    mutate(hum=as.numeric(v3)) |> 
    adjust_dates() |> 
    mutate(
      year=year(date),
      month=month(date),
      day=day(date)
    ) |> 
    dplyr::select(station, date, year, month, day, time, datetime, hum) |> 
      filter(year>=2000 & year<=2023)
  }
)

data_hum <- bind_rows(data_hum)

### TEMP 

files <- list.files(paste0(data_inp, "TEMP"), full.names = TRUE)

data_temp <- set_names(files, nm = sub("_.*", "", basename(files))) |> 
  imap(~ {
    
    s <- sub("_.*", "", basename(.y))

    import(.x, delim=";") |> 
    janitor::clean_names() |>
    mutate(station=s) |>  
    mutate(temp=as.numeric(v3)) |> 
    adjust_dates() |> 
    mutate(
      year=year(date),
      month=month(date),
      day=day(date)
    ) |> 
    dplyr::select(station, date, year, month, day, time, datetime, temp) |> 
      filter(year>=2000 & year<=2023)
  }
)

data_temp <- bind_rows(data_temp)

### WSPD 

files <- list.files(paste0(data_inp, "WSPD"), full.names = TRUE)

data_wspd <- set_names(files, nm = sub("_.*", "", basename(files))) |> 
  imap(~ {
    
    s <- sub("_.*", "", basename(.y))

    import(.x, delim=";") |> 
    janitor::clean_names() |>
    mutate(station=s) |>  
    mutate(wspd=as.numeric(v3)) |> 
    adjust_dates() |> 
    mutate(
      year=year(date),
      month=month(date),
      day=day(date)
    ) |> 
    dplyr::select(station, date, year, month, day, time, datetime, wspd) |> 
      filter(year>=2000 & year<=2023)
  }
)

data_wspd <- bind_rows(data_wspd)
 

# Join all data ---------
full_data <- data_pm25 |> 
  full_join(select(data_o3, station, datetime, o3), by=c("station", "datetime")) |> 
  full_join(select(data_pm10, station, datetime, pm10), by=c("station", "datetime")) |> 
  full_join(select(data_hum, station, datetime, hum), by=c("station", "datetime")) |> 
  full_join(select(data_temp, station, datetime, temp), by=c("station", "datetime")) |> 
  full_join(select(data_wspd, station, datetime, wspd), by=c("station", "datetime"))

glimpse(full_data)
summary(full_data)

# Save data
save(data_pm25, file=paste0(data_out, "series_pm25_2000_2023", ".RData"))
save(data_o3, file=paste0(data_out, "series_o3_2000_2023", ".RData"))
save(data_pm10, file=paste0(data_out, "series_pm10_2000_2023", ".RData"))
save(data_hum, file=paste0(data_out, "series_hum_2000_2023", ".RData"))
save(data_wspd, file=paste0(data_out, "series_wspd_2000_2023", ".RData"))
save(data_temp, file=paste0(data_out, "series_temp_2000_2023", ".RData"))

# Save complete data 
save(full_data, file=paste0(data_out, "series_full_2000_2023", ".RData"))