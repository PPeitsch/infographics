library(tidyverse)
ruta <- 'data_tp_final.csv'
df_final <- read.csv(ruta, sep = ';')
view(df_final)
glimpse(df_final)

df_final$comuna[df_final$comuna == 99] <- NA
df_final <- na.omit(df_final)

library(stringr)

df_final$tema <- gsub("<d3>", "Ó", df_final$tema)
df_final$tema <- gsub("<da>", "Ú", df_final$tema)
df_final$tema <- gsub("<c1><81>", "Á", df_final$tema)
df_final$tema <- gsub("<c1><8d>", "Í", df_final$tema)
df_final$tema <- gsub("<c1><ba>", "Ú", df_final$tema)
df_final$tema <- gsub("<c1><91>", "Ñ", df_final$tema)
df_final$tema <- gsub("<c1><89>", "É", df_final$tema)
df_final$tema <- gsub("<c1><b3>", "ó", df_final$tema)
df_final$tema <- gsub("<c1><89>", "É", df_final$tema)
df_final$tema <- gsub("<c1><89>", "É", df_final$tema)

print(df_final$tema)

unique(df_final$tema)


df_ordenado <- df_final[order(-df_final$tfinalizacion), ]

frecuencia_temas <- table(df_final$tema)

tema_max_frecuencia <- names(frecuencia_temas)[which.max(frecuencia_temas)]

vivienda <- df_final %>% group_by(tema) %>% filter(tema == "VIVIENDA")

df3 <- df_final %>%
  filter(tema %in% c("VIVIENDA", "SERVICIOS PÚBLICOS", "DERECHOS DE USUARIOS/AS, CONSUMIDORES/AS y ADMINISTRADOS/AS"))

# Contar la frecuencia de cada tema
frecuencia_temas <- table(df_final$tema)

# Ordenar las frecuencias de manera descendente
frecuencia_ordenada <- sort(frecuencia_temas, decreasing = TRUE)

# Obtener los tres temas con mayor frecuencia
top_3_temas <- names(frecuencia_ordenada)[1:3]
view(top_3_temas)

df3 <- df_final %>%  group_by(tema) %>%  filter(tema == "VIVIENDA", "SERVICIOS PÚBLICOS", "DERECHOS DE USUARIOS/AS, CONSUMIDORES/AS y ADMINISTRADOS/AS")

df_max <- df_final %>%
  filter(estado == "Cerrada" & tfinalizacion > 300)

df_min <- df_final %>%
  filter(estado == "Cerrada" & tfinalizacion < 100)

library(ggplot2)

ggplot(data = df_max, aes(x = tema, y = tfinalizacion, color = tema)) +
  geom_point() +
  labs(x = "Tema", y = "Días de Finalización [>300 Días]") +
  ggtitle("Gráfico de puntos: Tema vs. Días de Finalización") +
  scale_color_discrete()




library(ggplot2)

ggplot(data = df_max, aes(x = tema, y = tfinalizacion, color = tema)) +
  geom_point() +
  labs(x = "Tema", y = "Días de Finalización [>300 Días]") +
  ggtitle("Gráfico de puntos: Tema vs. Días de Finalización") +
  scale_color_brewer(palette ="Set2")




library(dplyr)

top10_temas_por_comuna <- df_final %>%
  group_by(comuna, tema) %>%
  count() %>%
  arrange(comuna, desc(n)) %>%
  group_by(comuna) %>%
  top_n(10)

top10_temas_por_comuna %>% ggplot(aes(x = factor(comuna), fill = tema)) +
                                    geom_bar()

ggplot(df_top_10, aes(x = factor(comuna), fill = tema)) +
  geom_bar(position = "dodge") +
  labs(x = "Comuna", y = "Frecuencia") +
  ggtitle("Top 10 de Temas por Comunas")

glimpse(top10_temas_por_comuna)

df_top_10 <- right_join(df_final, top10_temas_por_comuna, by = c("comuna", "tema"))
