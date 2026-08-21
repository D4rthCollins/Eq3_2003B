# 1 OBJETIVO: Importar y unir las estaciones de la base 2025

# Limpiar el entorno de trabajo
rm(list = ls())

# Cargar librerías
library(readxl)
library(dplyr)
library(purrr)

# Ruta de la base original
ruta_base = "data/BD 2025.xlsx"

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







#Resolver los registros duplicados encontrados
datos_2025 %>%
  filter(paste(estacion, date) %in% paste(duplicados$estacion, duplicados$date)) %>%
  arrange(estacion, date) %>%
  View()

# Si son duplicados exactos, nos quedamos con una sola copia
n_antes_dup <- nrow(datos_2025)

datos_2025 <- datos_2025 %>%
  distinct(estacion, date, .keep_all = TRUE)

n_despues_dup <- nrow(datos_2025)
n_duplicados_eliminados <- n_antes_dup - n_despues_dup
pct_duplicados_eliminados <- round(100 * n_duplicados_eliminados / n_antes_dup, 3)

n_duplicados_eliminados      # guardar para el reporte
pct_duplicados_eliminados    # guardar para el reporte


#Completar la base con las horas faltantes identificadas
n_filas_antes_completar <- nrow(datos_2025)

datos_2025 <- bind_rows(datos_2025, horas_faltantes) %>%
  arrange(estacion, date)

n_filas_agregadas <- nrow(datos_2025) - n_filas_antes_completar
n_filas_agregadas   

# cada estación debe tener exactamente 8,760 filas
datos_2025 %>%
  count(estacion) %>%
  print(n = Inf)

#Convertir valores no numéricos o  inválidas en NA
columnas_parametros <- setdiff(names(datos_2025), c("estacion", "date"))

#Datos invalidos
banderas_invalidas <- c("ND", "N/D", "NULL", "", "-999", "-99")

datos_2025 <- datos_2025 %>%
  mutate(across(
    all_of(columnas_parametros),
    ~ {
      x <- as.character(.x)
      x[x %in% banderas_invalidas] <- NA
      suppressWarnings(as.numeric(x))
    }
  ))

# Cuántos NA quedaron
datos_2025 %>%
  summarise(across(all_of(columnas_parametros), ~ sum(is.na(.)))) %>%
  print(width = Inf)

#Convertir valores fuera de los rangos operativos de 2025 en NA
# ------------------------------------------------------------
# Requiere el documento de rangos operativos del SIMA como tabla, con
# columnas: parametro, valor_min, valor_max

rangos_operativos <- read_excel("data/rangos_operativos_sima_2025.xlsx")

n_antes_rangos <- datos_2025 %>%
  summarise(across(all_of(columnas_parametros), ~ sum(!is.na(.)))) %>%
  rowSums()

for (p in intersect(rangos_operativos$parametro, columnas_parametros)) {
  min_p <- rangos_operativos$valor_min[rangos_operativos$parametro == p]
  max_p <- rangos_operativos$valor_max[rangos_operativos$parametro == p]
  datos_2025[[p]][datos_2025[[p]] < min_p | datos_2025[[p]] > max_p] <- NA
}

n_despues_rangos <- datos_2025 %>%
  summarise(across(all_of(columnas_parametros), ~ sum(!is.na(.)))) %>%
  rowSums()

n_valores_fuera_de_rango <- n_antes_rangos - n_despues_rangos
n_valores_fuera_de_rango   # guardar para el reporte

#Mantener los outliers  que sean físicamente posibles

resumen_outliers <- datos_2025 %>%
  summarise(across(
    all_of(columnas_parametros),
    list(Q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
         Q3 = ~ quantile(.x, 0.75, na.rm = TRUE))
  ))
resumen_outliers

#crear variables derivadas
library(lubridate)

dias_es <- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")

datos_2025 <- datos_2025 %>%
  mutate(
    anio = year(date),
    mes = month(date),
    dia_semana = factor(dias_es[wday(date)], levels = dias_es),
    hora = hour(date),
    
    
    
    estacion_del_anio = case_when(
      mes %in% c(12, 1, 2) ~ "Invierno",
      mes %in% c(3, 4, 5)  ~ "Primavera",
      mes %in% c(6, 7, 8)  ~ "Verano",
      mes %in% c(9, 10, 11) ~ "Otoño"
    ),
    periodo_del_dia = case_when(
      hora >= 6  & hora < 12 ~ "Mañana",
      hora >= 12 & hora < 19 ~ "Tarde",
      hora >= 19 | hora < 6  ~ "Noche"
    )
    # Si tienes columna de velocidad del viento y NO está en m/s, conviértela aquí:
    # VV_ms = VV / 3.6
  )

#crear un .csv limpio
library(tidyr)

datos_2025_largo <- datos_2025 %>%
  pivot_longer(
    cols = all_of(columnas_parametros),
    names_to = "parametro",
    values_to = "valor"
  )

write.csv(
  datos_2025_largo,
  "data/datos_sima_2025_limpio_largo.csv",
  row.names = FALSE
)


#porcentaje que se modificó o eliminó
n_total_celdas <- nrow(datos_2025) * length(columnas_parametros)
n_na_final <- datos_2025 %>%
  summarise(across(all_of(columnas_parametros), ~ sum(is.na(.)))) %>%
  rowSums()

pct_na_final <- round(100 * n_na_final / n_total_celdas, 2)

resumen_limpieza <- data.frame(
  metrica = c(
    "duplicados_eliminados",
    "pct_duplicados_eliminados",
    "filas_horarias_agregadas",
    "pct_na_final"
  ),
  valor = c(
    n_duplicados_eliminados,
    pct_duplicados_eliminados,
    n_filas_agregadas,
    pct_na_final
  )
)

resumen_limpieza
write.csv(resumen_limpieza, "data/resumen_limpieza_2025.csv", row.names = FALSE)
