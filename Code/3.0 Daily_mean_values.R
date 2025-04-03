# Code 2: Descriptives series  ----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

# Descriptives series ---- 
series <- rio::import(paste0(data_out, "series_full_2000_2023", ".RData")) 
glimpse(series)

## Daily mean data ---
# Data missing threshold definition 

hour_thresholds <- c(1, 12, 18)

# Function generate figure and descriptive data

missing_series <- function(hours) {
  
  # Series by station, year, month and day 
  date_series <- series |>
    group_by(station, year, month, day) |>
    summarise(
      across(
        starts_with("pm") | starts_with("o") | starts_with("hum") | starts_with("temp") | starts_with("wspd"),
        ~ ifelse(sum(!is.na(.x)) >= hours, mean(.x, na.rm = TRUE), NA),
        .names = "daily_{col}"
      ),
      .groups = "drop"
    )
  
  # Missing values table
  na_sum <- date_series |> 
    drop_na(station, year) |> 
    group_by(station, year) |> 
    summarise(
      total_obs = n(),
      na_pm25_pct = sum(is.na(daily_pm25)) / total_obs * 100,
      na_pm10_pct = sum(is.na(daily_pm10)) / total_obs * 100,
      na_o3_pct   = sum(is.na(daily_o3)) / total_obs * 100,
      na_hum_pct  = sum(is.na(daily_hum)) / total_obs * 100,
      na_temp_pct = sum(is.na(daily_temp)) / total_obs * 100,
      na_wspd_pct = sum(is.na(daily_wspd)) / total_obs * 100,
      .groups = "drop"
    ) |> 
    pivot_longer(
      cols = na_pm25_pct:na_wspd_pct,
      names_to = "metric",
      values_to = "percent_na"
    ) |> 
    mutate(
      metric_label = case_when(
        metric == "na_pm25_pct" ~ "PM2.5",
        metric == "na_pm10_pct" ~ "PM10",
        metric == "na_o3_pct"   ~ "Ozone",
        metric == "na_hum_pct"  ~ "Humidity",
        metric == "na_temp_pct" ~ "Temperature",
        metric == "na_wspd_pct" ~ "Wind Speed",
        TRUE ~ metric
      )
    ) |> 
    mutate(metric_label = factor(metric_label, 
                                levels = c("PM2.5", "Ozone", "PM10", "Humidity", "Temperature", "Wind Speed")))

  # Plot missing values across years
  g1 <- ggplot(na_sum, aes(x = factor(year), y = factor(station), fill = percent_na)) +
    geom_tile(colour = "white") +
    scale_fill_gradientn(
      colours = c("#E0F7FA", "#B2EBF2", "#80DEEA", "#4DD0E1", "#26C6DA", "#00BCD4", "#008BA3", "#005B73", "#003144"), 
      values = scales::rescale(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)),
      breaks = seq(0, 100, by = 10),
      limits = c(0, 100),
      guide = guide_colorbar(
        title = "% Missing Value", 
        barwidth = 15, 
        barheight = 0.5,
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    labs(x = NULL, y = "Station") +
    facet_wrap(~metric_label, scales = "free_x", ncol = 2) +
    theme_light() +
    theme(
      legend.position = "top", 
      plot.margin = margin(t = 10, r = 10, b = 5, l = 10),
      panel.grid = element_blank(),
      strip.text.y = element_text(angle = 0),
      strip.background = element_rect(fill = NA, color = "gray70"), 
      strip.text = element_text(color = "black"),
      strip.text.y.left = element_text(angle = 0)
    )

  # Save plot
  ggsave(
    g1,
    filename = paste0("Output/Descriptives/HEATMAP_missings_", hours, "h.png"), 
    res = 300,
    width = 35,
    height = 20,
    units = 'cm',
    scaling = 0.8,
    device = ragg::agg_png
  )

  # Save table
  na_sum |> 
    select(station, year, metric_label, percent_na) |> 
    write_xlsx(paste0("Output/Descriptives/Missings_", hours, "h.xlsx"))
  
  return(na_sum)
}

# Apply function 
map(hour_thresholds, missing_series)

# Time series across time ----

# Working with 12 hours complete data series 
date_series <- series |>
    group_by(station, year, month, day) |>
    summarise(
      across(
        starts_with("pm") | starts_with("o") | starts_with("hum") | starts_with("temp") | starts_with("wspd"),
        ~ ifelse(sum(!is.na(.x)) >= 12, mean(.x, na.rm = TRUE), NA),
        .names = "daily_{col}"
      ),
      .groups = "drop"
    ) |> 
  mutate(date = make_date(year, month, day))


data_long <- date_series %>%
  pivot_longer(cols = starts_with("daily_"), 
               names_to = "contaminant", 
               values_to = "value") %>%
  mutate(contaminant = case_when(
    contaminant == "daily_pm25"  ~ "PM2.5",
    contaminant == "daily_pm10"  ~ "PM10",
    contaminant == "daily_o3"    ~ "Ozone",
    contaminant == "daily_hum"   ~ "Humidity",
    contaminant == "daily_temp"  ~ "Temperature",
    contaminant == "daily_wspd"  ~ "Wind Speed",
    TRUE                        ~ contaminant
  )) |> 
  mutate(contaminant = factor(contaminant, 
                                levels = c("PM2.5", "Ozone", "PM10", "Humidity", "Temperature", "Wind Speed")))



# Save data frames
save(date_series, file=paste0(data_out, "series_daily_cont12h_2000_2023_wide", ".RData"))
save(data_long, file=paste0(data_out, "series_daily_cont12h_2000_2023_long", ".RData"))


# Plot with time series
s <- unique(data_long$station)

walk(s, function(station_name) {
  
  # Filter station
  data_filtered <- data_long |> 
    filter(station == station_name)
  
  # Plot
  g <- ggplot(data_filtered, aes(x = date, y = value, color = contaminant)) +
    geom_line(size = 0.8, alpha = 0.8, color = "gray30") +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
    labs(
      title = paste0("Estación: ", station_name),
      x = "Date",
      y = "Daily Mean Concentration (12 hours)",
      color = "Contaminant"
    ) +
    facet_wrap(~ contaminant, ncol = 2, nrow = 3, scales = "free") + 
    theme_light() +
    theme(
      legend.position = "none",  # Ocultar leyenda ya que está en facetas
      strip.background = element_rect(fill = NA, color = "black"), 
      strip.text = element_text(color = "black", size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      plot.margin = margin(t = 10, r = 10, b = 5, l = 10)
    )

  # Save the plot 
  ggsave(
    filename = paste0("Output/Descriptives/Series_contaminant_", station_name, ".png"), 
    plot = g,
    res = 300,
    width = 40,
    height = 20,
    units = 'cm',
    scaling = 0.8,
    device = ragg::agg_png
  )

  message("Gráfico guardado para la estación: ", station_name)
})


