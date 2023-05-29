library(tidyverse)

# Dataset folder
data_folder <- "data"

# Graph folder
fig_folder <- "fig"

# Files list of datasets folder
list_files <- list.files(data_folder)
# Number of datasets
n <- length(list_files)


# Importing dataset
df_arbolado <- read.csv(
  file.path(
    data_folder,
    list_files),
  dec = ","
  )

## Data types conversion
df_converted <- mutate(
  df_arbolado,
  nro_registro = as.integer(nro_registro),
  calle_altura = as.integer(calle_altura),
  calle_chapa = as.integer(calle_chapa),
  ancho_acera = as.double(ancho_acera),
  diametro_altura_pecho = as.double(diametro_altura_pecho),
  altura_arbol = as.double(altura_arbol),
)

# Quitamos columnas innecesarias
df_converted %>% select(
  -nro_registro,
  -comuna,
  -tipo_activ, 
  -manzana,
  -direccion_normalizada,
  -ubicacion,
  -nivel_plantera,
  -calle_chapa,
  -ubicacion_plantera
)

# Una opción de filtro
df_filtered <- filter(
  df_converted,
  ancho_acera >= 3.5,
  estado_plantera %in% c("Subocupada", "Vacía")
)

# Otra opcion de filtro
df_filtered_2 <- filter(df_converted, ancho_acera >= 3.5) %>%
  group_by(calle_nombre, calle_altura) %>%
  summarise(nro_arboles = n()) %>% filter(nro_arboles <= 15) %>% ungroup()
df_filtered_2b <- inner_join(df_filtered_2, df_converted) %>%
  filter(estado_plantera %in% c("Subocupada", "Vacía")) %>%
  arrange(nro_arboles)