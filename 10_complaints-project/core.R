# dependencies
library(tidyverse)
library(modelr)
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


# correction of date format to variables
df_ordenado$fecha_de_creacion <- dmy(df_ordenado$fecha_de_creacion)
df_ordenado$fecha_cierre <- dmy(df_ordenado$fecha_cierre)
df_ordenado$fechanacimiento <- dmy(df_ordenado$fechanacimiento)

# case closing time (days)
closing_time = df_ordenado$fecha_cierre - df_ordenado$fecha_de_creacion

# getting only closed cases
df_closed_cases <- cbind(df_ordenado, tfinalizacion=closing_time) %>%
  filter(estado == "Cerrada")

# getting three groups through maximun and minum arbitrary numbers
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
top10_temas_por_comuna <- group_by(df_copy, comuna, tema) %>%
  count() %>%
  arrange(comuna, desc(n)) %>%
  group_by(comuna) %>%
  top_n(10)
df_top_10 <- right_join(df_copy,
                        top10_temas_por_comuna,
                        by = c("comuna", "tema"))

# plot top ten cases by comuna
p4 <- ggplot(df_top_10,
             aes(x = factor(comuna),
                 fill = tema)) +
  geom_bar(position = "dodge") +
  labs(x = "Comuna", y = "Frecuencia") +
  ggtitle("Top 10 de Temas por Comunas") +
  theme(legend.position="none")


# dataframe for first model
df_comuna <- group_by(df_closed_cases, tema, comuna) %>%
  mutate(n_closed_cases=n()) %>%
  summarize(fecha_cierre, tema, sexo, comuna, tfinalizacion, n_closed_cases) %>%
  ungroup()

# first model
model_1 <- lm(tfinalizacion ~ n_closed_cases + comuna, data = df_comuna)
df_ccases_model1 <- add_predictions(df_comuna, model=model_1) %>% add_residuals(model=model_1)

# first model plot
p5 <- ggplot(data=df_comuna, aes(x=n_closed_cases, y=tfinalizacion)) +
  geom_point(alpha=0.5, size=1.2) +
  geom_line(data=df_ccases_model1, aes(y=pred, color=comuna), size=1) +
  facet_wrap(~comuna) +
  labs(
    x = 'Numero de casos cerrados',
    y = 'Tiempo de finalización',
    title = 'Linear Regression: t ~ n_casos + comuna'
  ) +
  theme(axis.line = element_line(colour = "black", size = 2), text = element_text(size = 20)) +
  theme(legend.position="none")


# dataframe for second model
df_growing_cases <- group_by(df_ordenado, tema, comuna) %>%
  mutate(n_cases=n()) %>%
  summarize(fecha_de_creacion, tema, sexo, comuna, n_cases) %>%
  ungroup()

# second model
model_2 <- lm(n_cases ~ fecha_de_creacion + comuna, data = df_growing_cases)
df_ccases_model2 <- add_predictions(df_growing_cases, model=model_2) %>% add_residuals(model=model_2)

# second model plot
p6 <- ggplot(data=df_growing_cases, aes(x=fecha_de_creacion, y=n_cases)) +
  geom_point(alpha=0.5, size=1.2) +
  geom_line(data=df_ccases_model2, aes(y=pred, color=comuna), size=2) +
  facet_wrap(~comuna) +
  labs(
    y = 'Numero de casos',
    x = 'Fecha de creacion',
    title = 'Linear Regression: n_casos ~ fecha_de_creacion + comuna'
  ) +
  theme(axis.line = element_line(colour = "black", size = 2), text = element_text(size = 20)) +
  theme(legend.position="none")