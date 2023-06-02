# dependencies
library(tidyverse)
library(stringr)
library(dplyr)
library(lubridate)

# Datasets folder
data_folder <- "data"
# Graph folder
fig_folder <- "fig"
# Dataset file
file_name <- 'complains2.csv'
# Importing dataset
df_original <- read.csv(file.path(data_folder, file_name), sep=";", encoding = "UTF-8")

# Changing size of plot
options(repr.plot.width=25, repr.plot.height=12)
# Making a copy of dataset to work on it
df_copy <- df_original

# 99 doesn't exist??
df_copy$comuna[df_copy$comuna == 99] <- NA
# 
df_copy <- na.omit(df_copy)

# sorting dataset by creation date
df_ordenado <- arrange(df_copy, fecha_de_creacion)

# frequency of appearance by topic
frecuencia_temas <- table(df_copy$tema)
# sorted 
frecuencia_ordenada <- sort(frecuencia_temas, decreasing = TRUE)
# top 1 topic
tema_max_frecuencia <- names(frecuencia_temas)[which.max(frecuencia_temas)]
# data frame of top 1 topic
vivienda <- group_by(df_copy, tema) %>% filter(tema == tema_max_frecuencia)
# top 4 most common topics
top_4_temas <- names(frecuencia_ordenada)[1:4]
# data frame of top 4 most common topics
df3 <- filter(df_copy, tema %in% top_4_temas)

# getting only closed cases
df_closed_cases <- filter(df_ordenado, estado == "Cerrada")

# case closing time (days), minimum and maximum
df_ordenado$fecha_de_creacion <- dmy(df_ordenado$fecha_de_creacion)
df_ordenado$fecha_cierre <- dmy(df_ordenado$fecha_cierre)
closing_time = df_ordenado$fecha_cierre - df_ordenado$fecha_de_creacion
df_closed_cases <- cbind(df_ordenado, tfinalizacion=closing_time)
maximum_day = 365
minimum_day = 100
df_max <- filter(df_closed_cases, tfinalizacion > maximum_day)
df_med <- filter(df_closed_cases, tfinalizacion < maximum_day & tfinalizacion > minimum_day)
df_min <- filter(df_closed_cases, tfinalizacion < minimum_day)

# plot longer closed cases
p1 <- ggplot(data = subset(df_max, !is.na(tema)), aes(x = tema,
                                y = tfinalizacion,
                                color = tema)) +
  geom_point() +
  labs(x = "Tema", y = "Días de Finalización (t>300)") +
  ggtitle("Gráfico de puntos: Tema vs. Días de Finalización") +
  theme(legend.position="none")

# plot longer closed cases
p2 <- ggplot(data = subset(df_med, !is.na(tema)), aes(x = tema,
                                                      y = tfinalizacion,
                                                      color = tema)) +
  geom_point() +
  labs(x = "Tema", y = "Días de Finalización (100>t<300)") +
  ggtitle("Gráfico de puntos: Tema vs. Días de Finalización") +
  theme(legend.position="none")

# plot faster closed cases
p3 <- ggplot(data = subset(df_min, !is.na(tema)), aes(x = tema,
                                y = tfinalizacion,
                                color = tema)) +
  geom_point() +
  labs(x = "Tema", y = "Días de Finalización (t<300)") +
  ggtitle("Gráfico de puntos: Tema vs. Días de Finalización") +
  theme(legend.position="none")


# top 10 cases by comuna
top10_temas_por_comuna <- df_copy %>%
  group_by(comuna, tema) %>%
  count() %>%
  arrange(comuna, desc(n)) %>%
  group_by(comuna) %>%
  top_n(10)
df_top_10 <- right_join(df_copy, top10_temas_por_comuna, by = c("comuna", "tema"))

# plot top ten cases by comuna
p4 <- ggplot(df_top_10,
             aes(x = factor(comuna),
                 fill = tema)) +
  geom_bar(position = "dodge") +
  labs(x = "Comuna", y = "Frecuencia") +
  ggtitle("Top 10 de Temas por Comunas") +
  theme(legend.position="none")
