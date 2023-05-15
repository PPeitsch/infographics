# Se importan las librerías necesarias
library(tidyverse)

# Directorio de trabajo y nombre de archivo
data_folder <- "dataset"
file_name <- "Insurance.csv"

# Se importa el dataset
df_seguros <- read.csv(file.path(data_folder, file_name), dec=",")

# Agrego datos sobre obesidad
df_seguros <- mutate(df_seguros,
                     obesidad = bmi >= 30,
                     salud = case_when((obesidad == TRUE & smoker == 'yes') ~ 'TO', 
                                              (obesidad == TRUE & smoker == 'no') ~ 'O', 
                                              (obesidad == FALSE & smoker == 'yes') ~ 'T', 
                                              (obesidad == FALSE & smoker == 'no') ~ 'SP'))

# Cambia tamaño del plot
options(repr.plot.width=18, repr.plot.height=8)
# Plot 1
p1 <- ggplot(data=df_seguros, aes(y=factor(smoker), x=charges, color=obesidad)) + 
    geom_boxplot(outlier.colour = "red", outlier.fill="red", outlier.size=3, notch=TRUE, notchwidth = 0.85) +
    labs(x = 'Costo de seguro medico', color = 'Obesidad') +
    scale_color_discrete(labels=c('BMI<30: NO','BMI>=30: SI')) +
    scale_y_discrete('Tabaquismo',labels=c('NO','SI')) +
    coord_cartesian(xlim=c(0, 58000)) +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 20))

# Cantidad de habitantes por zona
metricas_df2 <- group_by(df_seguros, region) %>% summarise(habitantes=n())
# Metricas por region, obesidad y smoker
metricas_df3 <- group_by(df_seguros, salud, region) %>% 
    summarise(median_charge=median(charges), mean_charge=mean(charges), cantidad=n())

metricas_merge <- merge(metricas_df2, metricas_df3, by='region') %>% 
    mutate(poblacion_pje=round(cantidad/habitantes*100, 2), 
    ganancia_grupo=(10000*0.70*0.25)*median_charge,
    ganancia_extrap=(10000*0.70*0.25*poblacion_pje/100)*median_charge)

# Cambia tamaño del plot
options(repr.plot.width=18, repr.plot.height=10)
# Plot 2
p2 <- ggplot(metricas_merge, aes(fill=salud, y=poblacion_pje, x=region)) + 
    geom_bar(position="fill", stat="identity") +
    labs(fill = "Salud") +
    xlab("Region") +
    ylab("Grupos poblacionales (%)") +
    scale_fill_discrete(labels=c('Tabaquismo', 'Tabaquismo y Obesidad', 'Obesidad', 'Sin cond. preexistentes')) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_text(aes(label = paste(poblacion_pje,"%")), colour = "black", position = position_fill(vjust = 0.5)) +
    theme(axis.line = element_line(colour = "black", size = 0.5), text = element_text(size = 20))

modelo_1 <- lm(charges ~ age + smoker + obesidad, data=df_seguros)

# Agregar datos del modelo
grid_1 <- modelr::data_grid(df_seguros, age, smoker, obesidad)
# Agregar predicciones del modelo
grid_1 <- modelr::add_predictions(grid_1, modelo_1)

p3 <- ggplot(df_seguros, aes(x=age, y=charges, color=smoker, shape=obesidad)) + geom_point(size=2) + 
    geom_line(data=grid_1, mapping=aes(y=pred, linetype=obesidad), size=1.5) +
    labs(color = "Tabaquismo", linetype = "Obesidad", shape = "Obesidad") +
    xlab("Edad") +
    ylab("Cargos anuales (usd)") +
    theme(axis.line = element_line(colour = "black", size = 0.5), text = element_text(size = 20))

modelo_2 <- lm(charges ~ age*smoker + age*obesidad + age*obesidad*smoker, data=df_seguros)

# Agregar datos del modelo
grid_2 <- modelr::data_grid(df_seguros, age, smoker, obesidad)
# Agregar predicciones del modelo
grid_2 <- modelr::add_predictions(grid_2, modelo_2)

# Gráfico del modelo
p4 <- ggplot(df_seguros, aes(x=age, y=charges, color=smoker, shape=obesidad)) + geom_point(size=2) + 
    geom_line(data=grid_2, mapping=aes(y=pred, linetype=obesidad), size=1.5) +
    labs(color = "Tabaquismo", linetype = "Obesidad", shape = "Obesidad") +
    xlab("Edad") +
    ylab("Cargos anuales (usd)") +
    theme(axis.line = element_line(colour = "black", size = 0.5), text = element_text(size = 20))