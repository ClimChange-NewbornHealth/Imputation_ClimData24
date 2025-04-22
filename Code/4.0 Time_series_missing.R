# Code 2: Descriptives series  ----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

# Open series ---- 
date_series <- rio::import(paste0(data_out, "series_daily_cont12h_2000_2023_wide", ".RData")) 
data_long <- rio::import(paste0(data_out, "series_daily_cont12h_2000_2023_long", ".RData")) 

glimpse(date_series)
glimpse(data_long)

# Time series across time ----

cont <- c("PM2.5", "PM10", "Ozone", "Humidity", "Temperature", "Wind Speed")
let <- LETTERS[1:length(cont)]
panel_labels <- setNames(paste0(let, ". ", cont), cont)

# Plot with time series
s <- unique(data_long$stat_aux)

walk(s, function(station_name) {
  
  # Filter station
  data_filtered <- data_long |> 
    filter(stat_aux == station_name) |>
    filter(contaminant %in% cont) |> 
    mutate(contaminant = recode(as.character(contaminant), !!!panel_labels)) 
  
  # Plot
  g <- ggplot(data_filtered, aes(x = date, y = value, color = contaminant)) +
    geom_line(size = 0.5, alpha = 0.8, color = "gray30") +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
    labs(
      title = paste0(station_name),
      x = "Date",
      y = "Daily Mean Concentration (12 hours)",
      color = "Contaminant"
    ) +
    facet_wrap(~ contaminant, ncol = 2, nrow = 3, scales = "free") + 
    theme_light() +
    theme(
      legend.position = "none",  # Ocultar leyenda ya que está en facetas
      strip.text.y = element_text(angle = 0),
      strip.background = element_rect(fill = NA, color = NA), 
      strip.text = element_text(color = "black", size = 12, hjust = 0),
      strip.text.y.left = element_text(angle = 0),
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

  message("Saving plot: ", station_name)
})

# Time series across time by station and special contaminant ----

data_ts <- data_long |> 
  filter(contaminant %in% c("PM2.5", "Ozone"))

g_all <- ggplot(data_ts, aes(x = date, y = value)) + #  color = stat_aux
  geom_line(alpha = 0.4, size = 0.4) +
  facet_grid(stat_aux ~ contaminant, scales = "free") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    x = NULL, y = "Daily Mean Concentration",
    color = "Station"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    strip.text = element_text(colour = "black"),
    strip.background = element_rect(fill="white", color="black"),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0),
    panel.grid = element_blank()
  )

g_all

ggsave(
  plot = g_all,
  filename = paste0("Output/Descriptives/Series_contaminant.png"), 
  res = 300,
  width = 15,
  height = 20,
  units = 'cm',
  scaling = 0.9,
  device = ragg::agg_png
)

# Complete data

g_full <- data_ts |> 
  group_by(date, contaminant) |> 
  summarise(value=mean(value, na.rm = TRUE)) |> 
  ungroup() |> 
  ggplot(aes(x = date, y = value)) + #  color = stat_aux
  geom_line(alpha = 0.4, size = 0.4) +
  #geom_smooth(method = "loess", se = TRUE, span = 0.1) + # Local regression 
  facet_wrap(~ contaminant, scales = "free", ncol = 1) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    x = NULL, y = "Daily Mean Concentration",
    color = "Station"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    strip.text = element_text(colour = "black"),
    strip.background = element_rect(fill="white", color="black"),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0),
    panel.grid = element_blank()
  )

g_full

ggsave(
  plot = g_full,
  filename = paste0("Output/Descriptives/Series_contaminant_loess.png"), 
  res = 300,
  width = 20,
  height = 15,
  units = 'cm',
  scaling = 0.9,
  device = ragg::agg_png
)
