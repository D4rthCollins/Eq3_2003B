# OBJETIVO: Importar y unir las estaciones de la base 2025

# Limpiar el entorno de trabajo
rm(list = ls())

# Cargar librerías
library(readxl)
library(dplyr)
library(purrr)

# Ruta de la base original
ruta_base = "data/raw/BD 2025.xlsx"

# Obtener los nombres de las hojas del archivo
hojas = excel_sheets(ruta_base)

# Mostrar los nombres de las hojas
hojas

# Importar todas las hojas y agregar el nombre de la estación
datos_2025 <- map_dfr(
  hojas,
  function(nombre_hoja) {
    
    read_excel(
      ruta_base,
      sheet = nombre_hoja
    ) %>%
      mutate(
        estacion = nombre_hoja,
        .before = 1
      )
  }
)

# Revisar la base unida
dim(datos_2025)
names(datos_2025)
unique(datos_2025$estacion)
head(datos_2025)
