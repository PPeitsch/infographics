# Matriz de correlacion
cor_data <- select(combined_data, Close_btc, Close_oro, Close_brent, Close_wti, Close_sp500, Close_nasdaq)
correlation_matrix <- cor(cor_data)
print(correlation_matrix)


# Convertir la matriz de correlación en un dataframe para facilitar el gráfico
cor_df <- as.data.frame(as.table(correlation_matrix))
cor_df <- cor_df %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq)

# Gráfico de barras de la matriz de correlación
ggplot(cor_df, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red",
                      limits = c(0, 1),
                      na.value = "transparent") +
  labs(x = "Activos", y = "Activos", fill = "Correlación", title = "Matriz de Correlación", subtitle = "del Precio Cierre de los Activos") +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Carga de dataset
orodf <- read_csv(file.path(data_folder,'GOLD/GC=F_hist.csv'))
btcdf <- read_csv(file.path(data_folder,'BTC/BTC-USD_hist.csv'))
glimpse(orodf)

## Operaciones
orodf <- orodf %>% mutate(Diff = High - Low)
btcdf <- btcdf %>% mutate(Diff = High - Low)

## Oro
ggplot(orodf, aes(x = Date, y = Diff)) +
  geom_point() +
  stat_smooth() +
  labs(title = "Gráfico de diferencia de precios de Oro",
       subtitle = "(High - Low)",
       x = "Fecha [Diario]",
       y = "Diferencia de precios [USD]")

## BTC
ggplot(btcdf, aes(x = Date, y = Diff)) +
  geom_point() +
  stat_smooth() +
  coord_cartesian(ylim = c(0, 5000)) +
  labs(title = "Gráfico de diferencia de precios de BTC",
       subtitle = "(High - Low)",
       x = "Fecha [Diario]",
       y = "Diferencia de precios [USD]")