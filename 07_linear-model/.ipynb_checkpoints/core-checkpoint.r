# Libraries
library(tidyverse)
library(modelr)

# Data and file names
data_folder <- "data"
file_name <- "insurance_filtrado.csv"
fig_folder <- "fig"

# Dataset import
df_seguros <- read.csv(file.path(data_folder, file_name), dec=".")

options(repr.plot.width=36, repr.plot.height=18)


# First Model
modelo = lm(charges ~ age, data=df_seguros)
df_seguros_mod <- add_predictions(df_seguros, model=modelo) %>% add_residuals(model=modelo)

p1 <- ggplot(data=df_seguros_mod, aes(x=age, y=charges)) +
    geom_point(alpha=0.5, size=5) +
    geom_line(data=df_seguros_mod, aes(y=pred), color="blue", size=3) +
    labs(
        x = 'Age',
        y = 'Charges',
        title = 'Linear Regression: charges = age'
    ) +
    theme(axis.line = element_line(colour = "black", size = 2), text = element_text(size = 30))
fig1_name <- "fig1.jpeg"

# Second Model
modelo_2 <- lm(charges ~ I(age**2), data=df_seguros)
df_seguros_mod2 <- add_predictions(df_seguros, model=modelo_2) %>% add_residuals(model=modelo_2)

p2 <- ggplot(data=df_seguros_mod2, aes(x=age, y=charges)) +
    geom_point(alpha=0.5, size=5) +
    geom_line(data=df_seguros_mod2, aes(y=pred, x=age), color="blue", size=3) +
    labs(
        x = 'Age',
        y = 'Charges',
        title = 'Linear Regression: charges = age²'
    ) +
    theme(axis.line = element_line(colour = "black", size = 2), text = element_text(size = 30))
fig2_name <- "fig2.jpeg"

# Thirth Model
modelo_3 <- lm(charges ~ I(age**2) + sex, data=df_seguros)
df_seguros_mod3 <- add_predictions(df_seguros, model=modelo_3) %>% add_residuals(model=modelo_3)

p3 <- ggplot(data=df_seguros_mod3, aes(x=age, y=charges)) +
    geom_point(alpha=0.6, size=5) +
    geom_line(data=df_seguros_mod3, aes(y=pred, x=age), size=3) +
    facet_wrap(~sex, nrow=2) +
    labs(
        x = 'Age',
        y = 'Charges',
        title = 'Multiple Linear Regression: charges = age² + sex'
    ) +
    theme(axis.line = element_line(colour = "black", size = 2), text = element_text(size = 30))
fig3_name <- "fig3.jpeg"