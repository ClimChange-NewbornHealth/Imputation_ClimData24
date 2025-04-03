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

# Explorer files
data_pm25 <- set_names(files, nm = sub("_.*", "", basename(files))) |> 
  imap(~ {    
    s <- sub("_.*", "", basename(.y))
    import(.x, delim=";", na = c("", "NA")) |> 
    janitor::clean_names() |>
    mutate(station=s)|> 
    mutate(across(where(is.character), ~ na_if(.x, ""))) 
  })

data_pm25 <- set_names(files, nm = sub("_.*", "", basename(files))) |> 
  imap(~ {
    
    s <- sub("_.*", "", basename(.y))

    import(.x, delim=";", na = c("", "NA")) |> 
    janitor::clean_names() |>
    mutate(station=s) |>  
    mutate(across(where(is.character), ~ na_if(.x, ""))) |> 
    mutate(pm25 = str_replace(registros_validados, pattern = ",", replacement = ".")) |> 
    mutate(pm25=as.numeric(pm25)) |> 
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

glimpse(data_pm25)

map(data_pm25, ~ map_df(.x, ~ sum(is.na(.)), .id = "variable"))

data_pm25 <- bind_rows(data_pm25)

## 03 ---- 
files <- list.files(paste0(data_inp, "O3"), full.names = TRUE)
files

data_o3 <- set_names(files, nm = sub("_.*", "", basename(files))) |> 
  imap(~ {
    
    s <- sub("_.*", "", basename(.y))

    import(.x, delim=";", na = c("", "NA")) |> 
    janitor::clean_names() |>
    mutate(station=s) |>  
    mutate(across(where(is.character), ~ na_if(.x, ""))) |> 
    mutate(o3 = str_replace(registros_validados, pattern = ",", replacement = ".")) |> 
    mutate(o3 = as.numeric(o3)) |> 
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

map(data_o3, ~ map_df(.x, ~ sum(is.na(.)), .id = "variable"))

data_o3 <- bind_rows(data_o3)

## Covariates ---- 

### PM10 

files <- list.files(paste0(data_inp, "PM10"), full.names = TRUE)

data_pm10 <- set_names(files, nm = sub("_.*", "", basename(files))) |> 
  imap(~ {
    
    s <- sub("_.*", "", basename(.y))

    import(.x, delim=";", na = c("", "NA")) |> 
    janitor::clean_names() |>
    mutate(station=s) |>  
    mutate(across(where(is.character), ~ na_if(.x, ""))) |> 
    mutate(pm10= str_replace(registros_validados, pattern = ",", replacement = ".")) |> 
    mutate(pm10 = as.numeric(pm10)) |> 
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

map(data_pm10, ~ map_df(.x, ~ sum(is.na(.)), .id = "variable"))

data_pm10 <- bind_rows(data_pm10)

### HUM

files <- list.files(paste0(data_inp, "RHUM"), full.names = TRUE)

data_hum <- set_names(files, nm = sub("_.*", "", basename(files))) |> 
  imap(~ {
    
    s <- sub("_.*", "", basename(.y))

    import(.x, delim=";", na = c("", "NA")) |> 
    janitor::clean_names() |>
    mutate(station=s) |>  
    mutate(across(where(is.character), ~ na_if(.x, ""))) |> 
    mutate(hum = str_replace(v3, pattern = ",", replacement = ".")) |> 
    mutate(hum = as.numeric(hum)) |> 
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

map(data_hum, ~ map_df(.x, ~ sum(is.na(.)), .id = "variable"))

data_hum <- bind_rows(data_hum)

### TEMP 

files <- list.files(paste0(data_inp, "TEMP"), full.names = TRUE)

data_temp <- set_names(files, nm = sub("_.*", "", basename(files))) |> 
  imap(~ {
    
    s <- sub("_.*", "", basename(.y))

    import(.x, delim=";", na = c("", "NA")) |> 
    janitor::clean_names() |>
    mutate(station=s) |>  
    mutate(across(where(is.character), ~ na_if(.x, ""))) |> 
    mutate(temp = str_replace(v3, pattern = ",", replacement = ".")) |> 
    mutate(temp = as.numeric(temp)) |> 
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

map(data_temp, ~ map_df(.x, ~ sum(is.na(.)), .id = "variable"))

data_temp <- bind_rows(data_temp)

### WSPD 

files <- list.files(paste0(data_inp, "WSPD"), full.names = TRUE)

data_wspd <- set_names(files, nm = sub("_.*", "", basename(files))) |> 
  imap(~ {
    
    s <- sub("_.*", "", basename(.y))

    import(.x, delim=";", na = c("", "NA")) |> 
    janitor::clean_names() |>
    mutate(station=s) |>  
    mutate(across(where(is.character), ~ na_if(.x, ""))) |> 
    mutate(wspd = str_replace(v3, pattern = ",", replacement = ".")) |> 
    mutate(wspd = as.numeric(wspd)) |> 
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

map(data_wspd, ~ map_df(.x, ~ sum(is.na(.)), .id = "variable"))

data_wspd <- bind_rows(data_wspd)
 

# Join all data ---------

stations <- unique(data_pm25$station)

# Grid with time values

attr(data_pm25$datetime, "tzone")
attr(data_o3$datetime, "tzone")
attr(data_pm10$datetime, "tzone")
attr(data_hum$datetime, "tzone")
attr(data_temp$datetime, "tzone")
attr(data_wspd$datetime, "tzone")

full_datetime_series <- expand_grid(
  station = unique(stations),
  datetime = seq.POSIXt(
    from = as.POSIXct("2000-01-01 00:00:00", tz = "UTC"),
    to = as.POSIXct("2023-12-31 23:00:00", tz = "UTC"),
    by = "hour"
  )
)

data_pm25 <- data_pm25 %>%
  mutate(datetime = force_tz(datetime, tzone = "UTC"))

data_o3 <- data_o3 %>%
  mutate(datetime = force_tz(datetime, tzone = "UTC"))

data_pm10 <- data_pm10 %>%
  mutate(datetime = force_tz(datetime, tzone = "UTC"))

data_hum <- data_hum %>%
  mutate(datetime = force_tz(datetime, tzone = "UTC"))

data_temp <- data_temp %>%
  mutate(datetime = force_tz(datetime, tzone = "UTC"))

data_wspd <- data_wspd %>%
  mutate(datetime = force_tz(datetime, tzone = "UTC"))


attr(full_datetime_series$datetime, "tzone")

full_data <- full_datetime_series |> 
  left_join(select(data_pm25, station, datetime, pm25), by=c("station", "datetime")) |> 
  left_join(select(data_o3, station, datetime, o3), by=c("station", "datetime")) |> 
  left_join(select(data_pm10, station, datetime, pm10), by=c("station", "datetime")) |> 
  left_join(select(data_hum, station, datetime, hum), by=c("station", "datetime")) |> 
  left_join(select(data_temp, station, datetime, temp), by=c("station", "datetime")) |> 
  left_join(select(data_wspd, station, datetime, wspd), by=c("station", "datetime"))

glimpse(full_data)
summary(full_data)

# New variables ---------

# Time variables
full_data <- full_data |> 
  mutate(
    year = year(datetime),
    month = month(datetime),
    month_name = month(datetime, label = TRUE, abbr = FALSE, locale = "en_US"), 
    day = day(datetime),
    hour = hour(datetime),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Summer",
      month %in% c(3, 4, 5) ~ "Fall",
      month %in% c(6, 7, 8) ~ "Winter",
      month %in% c(9, 10, 11) ~ "Spring"
    ),
    quarter = paste0("Q", quarter(datetime)), 
    semester = paste0("S", if_else(quarter(datetime) %in% c(1, 2), 1, 2)) 
  ) |> 
  relocate(year, month, month_name, day, hour, season, quarter, semester, .after=datetime)


glimpse(full_data)

# Save all data ---------
save(data_pm25, file=paste0(data_out, "series_pm25_2000_2023", ".RData"))
save(data_o3, file=paste0(data_out, "series_o3_2000_2023", ".RData"))
save(data_pm10, file=paste0(data_out, "series_pm10_2000_2023", ".RData"))
save(data_hum, file=paste0(data_out, "series_hum_2000_2023", ".RData"))
save(data_wspd, file=paste0(data_out, "series_wspd_2000_2023", ".RData"))
save(data_temp, file=paste0(data_out, "series_temp_2000_2023", ".RData"))

# Save complete data 
save(full_data, file=paste0(data_out, "series_full_2000_2023", ".RData"))

