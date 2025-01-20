# Code 4: Classical imputation ----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

# Imputation tecniques: 
#	Imputación Media: Sustitución de valores faltantes por la media.
#	Imputación Condicional por Media: Uso de medias condicionadas por otras variables.
#	K-Nearest Neighbors (KNN): Imputación basada en la distancia a observaciones cercanas.
#	Imputación Múltiple (MI): Imputación iterativa con modelos de regresión.


