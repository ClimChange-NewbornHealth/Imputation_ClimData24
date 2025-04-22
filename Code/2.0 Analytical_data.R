# Code 2: Generate Analytical data ----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

# Descriptives series ---- 
series <- rio::import(paste0(data_out, "series_full_2000_2023", ".RData")) 

glimpse(series) # 2.524.608 obs.

## Process analytical data ----

### 1. Filter period 2010 to 2023

series <- series |> 
  filter(year>=2010) 

glimpse(series) # 1.472.544 obs. 

### 2. Replace NA Value in Quilicura with Quilicuria 1

dq <- series %>% 
  filter(station %in% c("Quilicura", "QuilicuraI")) |> 
  pivot_wider(
      names_from = station,
      values_from = c(pm25, o3, pm10, hum, temp, wspd),
      names_sep = "_"
    ) |> 
  mutate(
    pm25_Quilicura = coalesce(pm25_Quilicura, pm25_QuilicuraI),
    o3_Quilicura   = coalesce(o3_Quilicura, o3_QuilicuraI),
    pm10_Quilicura = coalesce(pm10_Quilicura, pm10_QuilicuraI),
    hum_Quilicura  = coalesce(hum_Quilicura, hum_QuilicuraI),
    temp_Quilicura = coalesce(temp_Quilicura, temp_QuilicuraI),
    wspd_Quilicura = coalesce(wspd_Quilicura, wspd_QuilicuraI)
  ) |> 
  select(datetime:semester, ends_with("Quilicura")) %>%
  rename_with(~ gsub("_Quilicura", "", .x), ends_with("_Quilicura")) |> 
  mutate(station = "Quilicura") |> 
  relocate(station, datetime)

series <- series %>% 
  filter(!station %in% c("Quilicura", "QuilicuraI")) |> 
  bind_rows(dq)

rm(dq)

### 3. Rename station, add ID and coordenates 

name_station <- c(
  "CerrillosI"     = "Cerrillos",
  "CerroNavia"     = "Cerro Navia",
  "ElBosque"       = "El Bosque",
  "Independencia"  = "Independencia",
  "LaFlorida"      = "La Florida",
  "LasCondes"      = "Las Condes",
  "ParqueOHiggins" = "Parque O'Higgins",
  "Pudahuel"       = "Pudahuel",
  "PuenteAlto"     = "Puente Alto",
  "Talagante"      = "Talagante",
  "Quilicura"      = "Quilicura"
)

series <- series %>%
  mutate(
    station = recode(station, !!!name_station),
    station_fct = factor(station, levels = sort(unique(station))),
    station_id = str_pad(as.integer(fct_inorder(station_fct)), width = 2, pad = "0"),
    station_id = paste0("S", station_id), 
    station_id = factor(station_id)
  ) |> 
  relocate(station_id, station_fct)

## Add coordinates station

coors_geo <- tibble(
  station = c("Cerrillos", "Cerro Navia", "El Bosque", "Independencia", 
              "La Florida", "Las Condes", "Parque O'Higgins", "Pudahuel", 
              "Puente Alto", "Talagante", "Quilicura"),
  lat = c(-33.4956, -33.4075, -33.5535, -33.4185, -33.5210, -33.4053,
               -33.4606, -33.4197, -33.5947, -33.6653, -33.3601),
  long = c(-70.7196, -70.7510, -70.6674, -70.6412, -70.6032, -70.5381,
                -70.6601, -70.7681, -70.5754, -70.9277, -70.7294)
)

series <- series |> 
  left_join(coors_geo, by="station") |> 
  relocate(station_id, station_fct, station, lat, long)

glimpse(series)

## Filter extreme values and create log transformation ----

# Values with contaminant equal to 0 replace with NA and apply Natural log
series <- series |> 
  mutate(pm25 = if_else(pm25==0, NA_real_, pm25)) |> 
  mutate(pm10 = if_else(pm10==0, NA_real_, pm10)) |> 
  mutate(o3 = if_else(o3==0, NA_real_, o3)) |> 
  mutate(wspd = if_else(wspd==0, NA_real_, pm25)) |> 
  mutate(log_pm25 = log(pm25)) |> 
  mutate(log_pm10 = log(pm10)) |> 
  mutate(log_o3 = log(o3)) |> 
  mutate(log_wspd = log(wspd))

# Extreme reference
p1_log_pm25 <- quantile(series$log_pm25, probs=0.01, na.rm = TRUE)
p99_log_pm25 <- quantile(series$log_pm25, probs=0.99, na.rm = TRUE)

p1_log_pm10 <- quantile(series$log_pm10, probs=0.01, na.rm = TRUE)
p99_log_pm10 <- quantile(series$log_pm10, probs=0.99, na.rm = TRUE)

p1_log_o3 <- quantile(series$log_o3, probs=0.01, na.rm = TRUE)
p99_log_o3 <- quantile(series$log_o3, probs=0.99, na.rm = TRUE)

p1_log_wspd <- quantile(series$log_wspd, probs=0.01, na.rm = TRUE)
p99_log_wspd <- quantile(series$log_wspd, probs=0.99, na.rm = TRUE)

# Replace extremes values with NA
series <- series |> 
  mutate(
    log_pm25 = if_else(log_pm25 < p1_log_pm25 | log_pm25 > p99_log_pm25, NA_real_, log_pm25),
    log_pm10 = if_else(log_pm10 < p1_log_pm10 | log_pm10 > p99_log_pm10, NA_real_, log_pm10),
    log_o3   = if_else(log_o3   < p1_log_o3   | log_o3   > p99_log_o3,   NA_real_, log_o3),
    log_wspd = if_else(log_wspd < p1_log_wspd | log_wspd > p99_log_wspd, NA_real_, log_wspd)
  ) |> 
  mutate(
    pm25 = if_else(is.na(log_pm25), NA, pm25),
    pm10 = if_else(is.na(log_pm10), NA, pm10),
    o3 = if_else(is.na(log_o3), NA, o3),
    wspd = if_else(is.na(log_wspd), NA, wspd)
  ) 

# Save complete data 
save(series, file=paste0(data_out, "analytical_series_full_2000_2023", ".RData"))
