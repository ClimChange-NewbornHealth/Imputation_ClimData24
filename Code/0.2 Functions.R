# Functions ----

## Descriptives ----

descriptives <- function(x, data){
  data %>% 
    dplyr::select({{ x }}) %>%    
    #drop_na() %>% 
    summarise(Media_Prop = round(mean({{ x }}, na.rm = TRUE), 3),
              SD = round(sd({{ x }}, na.rm = TRUE), 3),
              Min = min({{ x }}, na.rm = TRUE),
              P5 = round(quantile({{ x }}, probs = 0.05, na.rm = TRUE), 3),
              P10 = round(quantile({{ x }}, probs = 0.1, na.rm = TRUE), 3),
              P25 = round(quantile({{ x }}, probs = 0.25, na.rm = TRUE), 3),
              P50 = round(quantile({{ x }}, probs = 0.50, na.rm = TRUE), 3), # Mediana
              P75 = round(quantile({{ x }}, probs = 0.75, na.rm = TRUE), 3),
              P90 = round(quantile({{ x }}, probs = 0.9, na.rm = TRUE), 3),
              P95 = round(quantile({{ x }}, probs = 0.95, na.rm = TRUE), 3),
              Max = max({{ x }}, na.rm = TRUE),
              N = n(),
              Missing = sum(is.na({{ x }})),
              Pct_miss = round(Missing/N, 4)*100
    ) %>% 
    mutate(Variable={{i}}) %>% 
    relocate(Variable)
}

## Construcción de variables

make_dummies <- function(v, prefix = '') {
  s <- sort(unique(v))
  d <- outer(v, s, function(v, s) 1L * (v == s))
  colnames(d) <- paste0(gsub(".*\\$", "", deparse(substitute(v))), prefix, s)
  d
}

# Adjust dates and time
adjust_dates <- function(df) {
  df %>%
    mutate(
      # Convertir FECHA a formato de fecha correcta asumiendo que YY es desde 2000 hacia adelante
      date = ifelse(nchar(fecha_yymmdd) == 6, ymd(paste0(20, sprintf("%06d", fecha_yymmdd))), NA),
      date = ymd(paste0(20, sprintf("%06d", fecha_yymmdd))),  # Asegurar que FECHA tiene 6 dígitos y agregar "20" para años 2000 en adelante
      
      # Convertir HORA al formato adecuado y unirlo con FECHA
      time = sprintf("%04d", hora_hhmm),  # Asegurar que HORA tiene 4 dígitos
      datetime = ymd_hm(paste(date, time))  # Combinar fecha y hora en formato datetime
    )
}

# Function generate figure and descriptive data

missing_series <- function(hours) {
  
  # Series by station, year, month and day 
  date_series <- series |>
    group_by(station_id, year, month, day) |>
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
    drop_na(station_id, year) |> 
    group_by(station_id, year) |> 
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
        metric == "na_pm25_pct" ~ "A. PM2.5",
        metric == "na_pm10_pct" ~ "C. PM10",
        metric == "na_o3_pct"   ~ "B. Ozone",
        metric == "na_hum_pct"  ~ "D. Humidity",
        metric == "na_temp_pct" ~ "E. Temperature",
        metric == "na_wspd_pct" ~ "F. Wind Speed",
        TRUE ~ metric
      )
    ) |> 
    mutate(metric_label = factor(metric_label, 
                                levels = c("A. PM2.5", "B. Ozone", "C. PM10", 
                                           "D. Humidity", "E. Temperature", "F. Wind Speed")))

  # Plot missing values across years
  g1 <- ggplot(na_sum, aes(x = factor(year), y = factor(station_id), fill = percent_na)) + # fct_rev()
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
      strip.background = element_rect(fill = NA, color = NA), 
      strip.text = element_text(color = "black", size = 12, hjust = 0),
      strip.text.y.left = element_text(angle = 0)
    )

  # Save plot
  ggsave(
    g1,
    filename = paste0("Output/Descriptives/HEATMAP_missings_", hours, "h.png"), 
    res = 300,
    width = 25,
    height = 20,
    units = 'cm',
    scaling = 0.8,
    device = ragg::agg_png
  )

  # Save table
  na_sum |> 
    select(station_id, year, metric_label, percent_na) |> 
    write_xlsx(paste0("Output/Descriptives/Missings_", hours, "h.xlsx"))
  
  return(na_sum)
}

## Short describe
short_des <- function(df) {
  tibble(
    N = sum(!is.na(df)),
    missing_pct = mean(is.na(df)) * 100,
    mean = mean(df, na.rm = TRUE),
    median = median(df, na.rm = TRUE),
    sd = sd(df, na.rm = TRUE),
    min = min(df, na.rm = TRUE),
    p50 = quantile(df, 0.5, na.rm = TRUE),
    max = max(df, na.rm = TRUE),
  )
}

## Missing pct by time 

table_missing_pct <- function(var_cat, df, path = "Output/Descriptives") {
  
  t_stat <- df |> 
    group_by(stat_aux, contaminant, time = .data[[var_cat]]) |> 
    summarise(missing_pct = mean(is.na(value)) * 100, .groups = "drop") |> 
    pivot_wider(names_from = time, values_from = missing_pct)
  
  # (All)
  t_tot <- df |> 
    group_by(contaminant, time = .data[[var_cat]]) |> 
    summarise(missing_pct = mean(is.na(value)) * 100, .groups = "drop") |> 
    pivot_wider(names_from = time, values_from = missing_pct) |> 
    mutate(stat_aux = "All")
  
  # Combinar
  missing_by_station <- bind_rows(t_stat, t_tot) |> 
    relocate(stat_aux)
  
  # Escribir Excel
  write_xlsx(missing_by_station, file.path(path, paste0("Pct_miss_", var_cat, ".xlsx")))
  
  return(missing_by_station)
}

## Upset missing 

make_upset_plot <- function(df, station_label = "All") {
  
  if (station_label != "All") {
    df <- df |> filter(stat_aux == station_label)
  }
  
  df_upset <- df |> 
    select(all_of(names(pretty_names))) |>
    rename_with(~ pretty_names[.x]) |> 
    as_shadow_upset() |> 
    filter(rowSums(across(everything())) > 0)
  
  colnames(df_upset) <- str_remove(colnames(df_upset), pattern = "_NA")
  
  p <- ComplexUpset::upset(
    df_upset,
    set_sizes = FALSE, 
    wrap = FALSE,
    intersect = colnames(df_upset),
    name = "Missing Variables Combinations",
    min_size = 4,
    width_ratio = 0.5,
    base_annotations = list(
      'Intersection size' = intersection_size(
        text = list(size = 2)
      ) +
        ylab("Number of \n missing observation") +
        ggtitle(paste0(station_label)) +
        theme_light() +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    )
  )
  
  return(p)
}

## Smooth plot

my_smooth <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(color = I("gray70"), alpha=0.5, size=0.05) + 
    geom_smooth(method = "lm", color = I("gray30"), ...)
}

my_cor <- function(data, mapping, method = "pearson", use = "complete.obs",
                   ndp = 2,  # número de decimales
                   txt_size = 4,  # tamaño del texto de correlación
                   ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  ct <- cor.test(x, y, method = method, use = use)
  est <- ct$estimate
  pval <- ct$p.value
  
  est_formatted <- formatC(est, digits = ndp, format = "f")
  
  stars <- ""
  if (pval < 0.001) {
    stars <- "***"
  } else if (pval < 0.01) {
    stars <- "**"
  } else if (pval < 0.05) {
    stars <- "*"
  }
  
  label <- paste0(est_formatted, stars)
  
  ggally_text(
    label  = label,
    mapping = mapping,
    xP = 0.5,
    yP = 0.5, 
    size = txt_size,  # tamaño del texto
    ...
  ) +
    theme_void() + 
    theme(panel.background = element_rect(fill = "white"))
}

