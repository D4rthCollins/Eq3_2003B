# 1 OBJETIVO: Importar y unir las estaciones de la base 2025

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

# 2 OBJETIVO: Ordenar los datos por estación y fecha
datos_2025 <- datos_2025 %>%
  arrange(estacion, date)

# Revisar las primeras filas después de ordenar
datos_2025 %>%
  select(estacion, date) %>%
  head(10) %>%
  print(width = Inf)

# Revisamos últimas filas también
datos_2025 %>%
  select(estacion, date) %>%
  tail(10) %>%
  print(width = Inf)

# 3 OBJETIVO: Revisar registros duplicados
duplicados = datos_2025 %>%
  count(estacion, date, name = "numero_registros") %>%
  filter(numero_registros > 1)

# Cantidad de combinaciones estación-fecha duplicadas
nrow(duplicados)

# Mostrar los primeros duplicados, si existen
head(duplicados)

# 4 OBJETIVO: Revisar horas faltantes
resumen_horas = datos_2025 %>%
  group_by(estacion) %>%
  summarise(
    fecha_inicial = min(date),
    fecha_final = max(date),
    horas_registradas = n(),
    horas_esperadas = as.numeric(
      difftime(fecha_final, fecha_inicial, units = "hours")
    ) + 1,
    horas_faltantes = horas_esperadas - horas_registradas,
    .groups = "drop"
  )

resumen_horas %>%
  print(n = Inf, width = Inf)

# Identificando faltantes
# Crear todas las combinaciones esperadas de estación y hora
# Se usa UTC para que coincida con las fechas importadas desde Excel
horas_esperadas = expand.grid(
  estacion = unique(datos_2025$estacion),
  date = seq(
    from = as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
    to = as.POSIXct("2025-12-31 23:00:00", tz = "UTC"),
    by = "hour"
  )
)

# Identificar las horas que no aparecen
horas_faltantes = horas_esperadas %>%
  anti_join(
    datos_2025 %>% select(estacion, date),
    by = c("estacion", "date")
  ) %>%
  arrange(estacion, date)

nrow(horas_faltantes)
View(horas_faltantes)
