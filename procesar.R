library(readxl)
library(tidyverse)
library(tidyr)

datos <- read_excel("datos/datos_originales/5_1_PUEBLOS_ORIGINARIOS.xls", sheet = 4, skip = 2) 

datos1 <- datos |> janitor::clean_names()

censo <- read.csv2("datos/censo_proyecciones_año.csv")

datos2 <- datos1 |> 
  select(nombre_region, codigo_region, nombre_comuna, codigo_comuna, 
         sexo, mapuche:pueblo_ignorado) |> 
  filter(codigo_comuna != "País") |> 
  filter(sexo != "Total Comuna") |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna))

datos3 <- datos2 |> 
  pivot_longer(cols = mapuche:pueblo_ignorado, names_to = "pueblos", values_to = "n")

nombres_pueblos <- tibble(etiqueta = str_to_title(names(datos)),
                          columna = names(datos1)) |> 
  mutate(etiqueta = str_remove(etiqueta, "\n"),
         etiqueta = str_replace(etiqueta, " O ", " o "))

datos4 <- datos3 |> 
  left_join(nombres_pueblos, 
            by = join_by(pueblos == columna)) |> 
  select(-pueblos) |> 
  rename(pueblo = etiqueta)

censo2 <- censo |> 
  tibble() |> 
  select(codigo_comuna = cut_comuna, nombre_comuna = comuna, 
         codigo_region = cut_region, nombre_region = region, 
         año, poblacion_total = población) |> 
  mutate(across(starts_with("codigo"), as.numeric)) |> 
  filter(año == 2017)

orden_regiones <- tribble(~codigo_region, ~orden_region,
                          15, 1,
                          1, 2,
                          2, 3,
                          3, 4,
                          4, 5,
                          5, 6,
                          13, 7,
                          6, 8,
                          7, 9,
                          16, 10,
                          8, 11,
                          9, 12,
                          14, 13,
                          10, 14,
                          11, 15,
                          12, 16)

# nombres de comunas y regiones
datos5 <- datos4 |>
  select(-nombre_region, -nombre_comuna, -codigo_region) |> 
  left_join(censo2, join_by(codigo_comuna)) |> 
  left_join(orden_regiones, join_by(codigo_region)) |> 
  relocate(codigo_comuna, .before = nombre_comuna) |> 
  relocate(n, .after = pueblo)
  
# guardar
readr::write_csv2(datos5, "datos/pueblos_indigenas_chile.csv")







datos5