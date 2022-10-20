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
                     rango_etario = case_when((18 <= age & age < 30) ~ '18 a 29', 
                               (30 <= age & age < 40) ~ '30 a 39', 
                               (40 <= age & age < 50) ~ '40 a 49', 
                               (50 <= age & age < 60) ~ '50 a 59', 
                               (60 <= age) ~ '60 <='),
                     salud = case_when((obesidad == TRUE & smoker == 'yes') ~ 'FO', 
                                              (obesidad == TRUE & smoker == 'no') ~ 'O', 
                                              (obesidad == FALSE & smoker == 'yes') ~ 'F', 
                                              (obesidad == FALSE & smoker == 'no') ~ 'S'))

# Cambia tamaño del plot
options(repr.plot.width=18, repr.plot.height=8)
# Plot 1
p1 <- ggplot(data=df_seguros, aes(y=factor(smoker), x=charges, color=obesidad)) +
    geom_boxplot(outlier.colour = "red", outlier.fill="red", outlier.size=3, notch=TRUE, notchwidth = 0.85) +
    labs(x = 'Costo de seguro médico', color = 'Obesidad') +
    scale_color_discrete(labels=c('BMI<30: NO','BMI>=30: SI')) +
    scale_y_discrete('Fumador',labels=c('NO','SI')) +
    coord_cartesian(xlim=c(0, 58000)) +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 20))

# Summarise de cantidad de habitantes por zona
df_seguros_2 <- group_by(df_seguros, region)
metricas_df2 <- summarise(df_seguros_2, habitantes=n())

# Agrupo para hacer tabla de metricas por region, obesidad y smoker
df_seguros_3 <- group_by(df_seguros, salud, region)
metricas_df3 <- summarise(df_seguros_3,
          median_charge=median(charges),
          mean_charge=mean(charges),
          cantidad=n())

metricas_merge <- merge(metricas_df2, metricas_df3, by='region')
metricas_merge <- mutate(metricas_merge,
                         poblacion_pje=round(cantidad/habitantes*100, 2),
                         ganancia_grupo=(10000*0.70*0.25)*median_charge,
                         ganancia_extrap=(10000*0.70*0.25*poblacion_pje/100)*median_charge)

# Calculo ganancias promedio por region
metricas_merge2 <- group_by(metricas_merge, region)
metricas_merge2 <- summarise(metricas_merge2,
                             gciaGrupo_mean=mean(ganancia_grupo),
                             gciaGrupo_median=median(ganancia_grupo),
                             gciaTotal=sum(ganancia_extrap))