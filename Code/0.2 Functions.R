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

# Function install/load packages
install_load <- function(packages){
  for (i in packages) {
    if (i %in% rownames(installed.packages())) {
      library(i, character.only=TRUE)
    } else {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
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

