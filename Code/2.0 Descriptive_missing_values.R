## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")


## Data ----
# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

pm25 <- import(paste0(data_out, "series_pm25_2002_2024.RData"))
o3 <- import(paste0(data_out, "series_o3_2002_2024.RData"))


## Visual missing value ----

missing_by_datetime <- pm25 |> 
  group_by(station, datetime) |> 
  summarise(
    missing_pm25 = mean(is.na(pm25)) * 100 
  )

missing_by_date <- pm25 |> 
  group_by(station, date) |> 
  summarise(
    missing_pm25 = mean(is.na(pm25)) * 100  # Porcentaje de NA en pm25
  )


missing_by_date |> 
  ggplot(aes(x=date, y=missing_pm25)) +
  geom_line() +
  facet_wrap(~station) +
  theme_light()
