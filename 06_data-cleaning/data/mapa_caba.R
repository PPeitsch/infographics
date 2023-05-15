library(tidyverse)
library(sf)

# Read comunas
# barrio <- st_read("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson",
#                   stringsAsFactors = FALSE)

comunas <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/comunas/comunas.geojson",
                  stringsAsFactors = FALSE)

# Prepare base plot
base_plot <- ggplot() +
    geom_sf(data=comunas, color="white", fill='grey')

base_plot + 
  labs(title = "Ciudad Autónoma de Buenos Aires",
       subtitle = "Con límites de comunas",
       x = "Longitud",
       y = "Latitud",
       caption = "Fuente: Elaboración propia en base a DATA Buenos Aires Ciudad")
