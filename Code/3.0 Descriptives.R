# Code 2: Descriptives series  ----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

## Exploratory descriptives series ---- 
series <- rio::import(paste0(data_out, "analytical_series_full_2000_2023", ".RData")) 
glimpse(series) # 1.349.832

### Daily mean data ---

# Function generate figure and descriptive exploratory missing data

# Data missing threshold definition 
hour_thresholds <- c(1, 12, 18)

# Apply function 
map(hour_thresholds, missing_series)

## Adjust data -----

# Working with 12 hours complete data series 
ref_data <- series |> 
  select(station_id, station_fct, station, lat, long, year, month, month_name, day, season, quarter, semester)

date_series <- series |>
    group_by(station_id, year, month, day) |>
    summarise(
      across(
        starts_with("pm") | starts_with("o") | starts_with("hum") | starts_with("temp") | starts_with("wspd"),
        ~ ifelse(sum(!is.na(.x)) >= 12, mean(.x, na.rm = TRUE), NA),
        .names = "daily_{col}"
      ),
      .groups = "drop"
    ) |> 
  ungroup() |> 
  left_join(ref_data, by=c("station_id", "year", "month", "day"), multiple = "first") |> 
  mutate(date = ymd(paste(year, month, day))) |> 
  relocate(station_id, station_fct, station, lat, long, year, month, month_name, day, date, season, quarter, semester,
           daily_pm25:daily_wspd) |> 
  mutate(stat_aux = paste0(station_id, "-", station)) 

glimpse(date_series)
summary(date_series)

data_long <- date_series |>
  pivot_longer(cols = starts_with("daily_"), 
               names_to = "contaminant", 
               values_to = "value") |>
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

glimpse(data_long)
summary(data_long)

# Save data frames
save(date_series, file=paste0(data_out, "series_daily_cont12h_2000_2023_wide", ".RData"))
save(data_long, file=paste0(data_out, "series_daily_cont12h_2000_2023_long", ".RData"))

## Daily mean data ---
# Data missing threshold definition 

hour_thresholds <- c(1, 12, 18)

# Function generate figure and descriptive data
map(hour_thresholds, missing_series)

## Descriptive table ----

vars <- c("daily_pm25", "daily_pm10", "daily_o3", "daily_hum", "daily_temp", "daily_wspd")

table_des <- date_series |>
  select(stat_aux, all_of(vars)) |>
  pivot_longer(-stat_aux, names_to = "var", values_to = "value") |>
  group_by(stat_aux, var) |>
  summarise(short_des(value), .groups = "drop") |>
  pivot_longer(cols = N:max, names_to = "stat", values_to = "value") |>
  pivot_wider(names_from = stat_aux, values_from = value) |>
  arrange(var, stat)

table_des_all <- date_series |>
  select(all_of(vars)) |>
  pivot_longer(cols = everything(), names_to = "var", values_to = "value") |>
  group_by(var) |>
  summarise(short_des(value), .groups = "drop") |>
  pivot_longer(cols = N:max, names_to = "stat", values_to = "value") |>
  mutate(stat_aux = "All") |>
  pivot_wider(names_from = stat_aux, values_from = value)

table_des <- table_des_all |>
  bind_cols(table_des |> select(-var, -stat)) |>
  relocate(var, stat) |>
  arrange(var, stat)

write_xlsx(table_des, "Output/Descriptives/Descriptives_stats.xlsx")

## Missing values across time ----

cat_vars <- c("year", "month_name", "season", "quarter", "semester")

tabla_missings_list <- map(cat_vars, table_missing_pct, df = data_long)
names(tabla_missings_list) <- cat_vars
tabla_missings_list

## Upset missing values ----

pretty_names <- c(
  daily_pm25 = "PM2.5",
  daily_pm10 = "PM10",
  daily_o3   = "Ozone",
  daily_hum  = "Humidity",
  daily_temp = "Temperature",
  daily_wspd = "Wind Speed"
)

all_stations <- c("All", unique(as.character(date_series$stat_aux)))
upset_plot_list <- map(all_stations, ~ make_upset_plot(date_series, .x))

upspl <- ggarrange(plotlist = upset_plot_list, ncol = 2, nrow = ceiling(length(upset_plot_list) / 2))

ggsave(
  upspl,
  filename = paste0("Output/Descriptives/UPSET_missings", ".png"), 
  res = 300,
  width = 25,
  height = 28,
  units = 'cm',
  scaling = 0.5,
  device = ragg::agg_png
)

## Relationship covariates ----

data_cor <- date_series |>
  select(all_of(vars)) |>
  drop_na()

my_smooth <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(color = I("gray40"), alpha=0.5, size=0.10) + 
    geom_smooth(method = "lm", color = I("blue"), ...)
}

scatter_matrix <- ggpairs(data_cor, method = "pearson",
        lower = list(continuous=my_smooth),
        diag = list(continuous = wrap("barDiag", bins = 20, fill = "black", color="gray90", alpha=0.25)),
        columnLabels = pretty_names
      ) +
  theme_light() +
  theme(axis.text = element_text(size=6),
        strip.background = element_rect(fill="white", color="black"), 
        strip.text = element_text(colour="black"),
        panel.grid = element_blank())

scatter_matrix 

ggsave(
  scatter_matrix,
  filename = paste0("Output/Descriptives/Scatter_variables", ".png"), 
  res = 300,
  width = 20,
  height = 15,
  units = 'cm',
  scaling = 0.9,
  device = ragg::agg_png
)

# Per station 

for (s in all_stations[-1]) {
  message("Procesando estación: ", s)
  
  data_cor <- date_series |>
    filter(as.character(stat_aux) == s) |>
    select(all_of(names(pretty_names))) |>
    drop_na()

  if (nrow(data_cor) < 10) next  # Saltar si hay muy pocos datos

  scatter_matrix <- ggpairs(
    data_cor,
    lower = list(continuous = my_smooth),
    diag = list(continuous = wrap("barDiag", bins = 20, fill = "black", color = "gray90", alpha = 0.25)),
    columnLabels = pretty_names
  ) +
    labs(title = s) +
    theme_light() +
    theme(
      axis.text = element_text(size = 6),
      strip.background = element_rect(fill = "white", color = "black"),
      strip.text = element_text(colour = "black"),
      panel.grid = element_blank()
    )

  file_name <- str_glue("Output/Descriptives/Scatter_{s}.png") 

 ggsave(
    scatter_matrix,
    filename = file_name,
    res = 300,
    width = 20,
    height = 15,
    units = 'cm',
    scaling = 0.9,
    device = ragg::agg_png
  )

## Distribution plot ----

data_den <- date_series |>
  select(stat_aux, all_of(vars)) |>
  drop_na() |> 
  pivot_longer(-stat_aux, names_to = "var", values_to = "value") |>
  mutate(
    log_value = log(value),  # evitar log(0)
  ) |>
    pivot_longer(cols = c(value, log_value), names_to = "scale", values_to = "val") |>
    mutate(
      scale = recode(scale, value = "Original", log_value = "Log-transformed"),
      scale = factor(scale, levels = c("Original", "Log-transformed"))
    ) |> 
  mutate(var = recode(var, 
      "daily_pm25" = "PM2.5",
      "daily_pm10" = "PM10",
      "daily_o3"   = "Ozone",
      "daily_hum"  = "Humidity",
      "daily_temp" = "Temperature",
      "daily_wspd" = "Wind Speed"
  ))

g1 <- data_den |> 
  filter(scale=="Log-transformed") |> 
  ggplot(aes(x = val, fill = stat_aux)) +
  geom_density(alpha = 0.4, color = "black") +
  facet_grid(var ~ scale, scales = "free") +
  scale_fill_discrete(guide = guide_legend(nrow = 3)) + 
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_light() +
  theme(
    strip.text = element_text(colour="black"),
    legend.position = "top",
    strip.background = element_rect(fill="white", color="black"),
    strip.text.y = element_text(angle = 0)
  )

g2 <- data_den |> 
  filter(scale=="Original") |> 
  ggplot(aes(x = val, fill = stat_aux)) +
  geom_density(alpha = 0.4, color = "black") +
  facet_grid(var ~ scale, scales = "free") +
  scale_fill_discrete(guide = guide_legend(nrow = 3)) + 
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_light() +
  theme(
    strip.text = element_text(colour="black"),
    legend.position = "top",
    strip.background = element_rect(fill="white", color="black"),
    strip.text.y = element_blank()
  )

distribution_plot <- ggarrange(g2, g1, common.legend = TRUE)

ggsave(
  distribution_plot,
  filename = "Output/Descriptives/Distribution_contaminant_station.png",
  res = 300,
  width = 20,
  height = 15,
  units = 'cm',
  scaling = 0.9,
  device = ragg::agg_png
)

## MAP station ----

america <- ne_countries(scale = "medium", continent = "South America", returnclass = "sf")
chile <- ne_states(country = "Chile", returnclass = "sf")
santiago <- chile[chile$name == "Región Metropolitana de Santiago", ]

chile$color <- "gray50"
santiago$color <- "white"

america_centroids <- st_centroid(america) |>
  mutate(long = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) |> 
  filter(!iso_a3 %in% c("FLK", "CHL"))

base_map <- ggplot() +
  geom_sf(data = america, fill = "white", color = "black") +
  geom_sf(data = chile, aes(fill = color), color = "black") +
  geom_sf(data = santiago, fill = "white", color = "black") +  
  geom_text(data = america_centroids, aes(x = long, y = lat, label = iso_a3), size = 4, fontface="bold") + 
  geom_point(aes(x =  st_coordinates(st_centroid(santiago))[1], y =  st_coordinates(st_centroid(santiago))[2]), 
  color = "red", size = 3, shape = 21, fill = "white", stroke = 1.2, alpha=0.5) +
  geom_point(aes(x =  st_coordinates(st_centroid(santiago))[1], y =  st_coordinates(st_centroid(santiago))[2]), 
  color = "red", size = 1, shape = 21, fill = "red") +
  scale_fill_manual(values = c("gray80", "gray50", "white")) +
  #annotation_scale(location = "bl", width_hint = 0.4, text_cex = 0.6, tick_height = 0.3) +
  labs(x=NULL, y=NULL, title = "A.") +
  theme_light() +
  theme(
      plot.title = element_text(size = 16),
      legend.position = "none", 
      #axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1), 
      plot.margin = margin(0, 0, 0, 0),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      strip.text.y = element_text(angle = 0),
      strip.background = element_rect(fill=NA, color="gray70"), 
      strip.text=element_text(color="black"),
      strip.text.y.left = element_text(angle = 0),
      
    )

zoom_map <- ggplot() +
  geom_sf(data = chile, fill = "gray70", color = "black") +
  geom_sf(data = santiago, fill = "white", color = "black") +
  coord_sf(xlim = c(-72, -69.5), ylim = c(-34.5, -32.5)) + 
  geom_sf_text(data=santiago, aes(label = name_en, geometry = geometry), size = 5, fontface="bold", color = "black", stat = "sf_coordinates") +
  labs(x=NULL, y=NULL) +
  theme_light() + 
  theme(
      legend.position = "none", 
      plot.title = element_text(size=11, hjust = 0.5),
      plot.margin = margin(0, 0, 0, 0),
      panel.grid = element_blank(),
      axis.text.y = element_text(size=9),
      axis.text.x = element_text(size=9, angle=45, hjust = 1),
      #axis.ticks = element_blank(),
      strip.text.y = element_text(angle = 0),
      strip.background = element_rect(fill=NA, color="gray70"), 
      strip.text=element_text(color="black"),
      strip.text.y.left = element_text(angle = 0)
    )

zoom_map

map_plot <- base_map + inset_element(zoom_map, 
                                        left = -0.1, 
                                        bottom = 0.2, 
                                        right = 0.5, 
                                        top = 0.5)

map_plot

ggsave(
  filename = paste0("Output/Descriptives/MAP_STGO.png"), 
  res = 300,
  width = 20,
  height = 25,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png) 

stgo <- mapa_comunas |>
  filter(codigo_region == 13) |>
  left_join(
    codigos_territoriales |>
      select(matches("comuna"))
  ) |>
  mutate(codigo_comuna=as.numeric(codigo_comuna)) |>
  mutate(id = 1:n()) |>
  mutate(id_mun = str_pad(as.integer(factor(id)), width = 2, pad = "0"))

# Station_points
points <- date_series |> 
  select(station_id, station, stat_aux, lat, long) |> 
  distinct() |>
  st_as_sf(coords = c("long", "lat"), crs = 4326)
  
# Plot 
stat_points <- stgo |>
  ggplot() +
  geom_sf(aes(geometry = geometry), color = "gray40", fill="white") +
  geom_sf(data = points, aes(shape=stat_aux, color = stat_aux), size = 2) + #shape=stat_aux
  scale_color_manual(values =  brewer.pal(11, "Paired")) +
  scale_shape_manual(values = rep(17,11)) +
  labs(y=NULL, x=NULL, title = "B.") +
  theme_light() +
  theme(
    legend.position = "bottom", 
    legend.key.size = unit(0.6, "cm"), 
    legend.text = element_text(size = 10), 
    legend.title = element_blank(),
    plot.title = element_text(size = 10, hjust = 0),
    strip.background = element_rect(fill = NA, color = "black"),
    strip.text = element_text(color = "black"),
    panel.grid = element_blank()
  )

stat_points

ggsave(stat_points,
  filename = paste0("Output/", "Descriptives/", "MAP_station", ".png"), 
  res = 300,
  width = 18,
  height = 15,
  units = 'cm',
  scaling = 0.8,
  device = ragg::agg_png)  

    


  