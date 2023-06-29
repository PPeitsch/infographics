# GRAPHICS
# Bitcoin vs time
g1 <- ggplot(combined_data, aes(x = Date, y = Close_btc)) + 
        geom_line(color = 'Red', size = 1.5) +
        labs(x = "Fecha", y = "Precio BTC [USD]", title = "Precio del Bitcoin, periodo 2014-2023") +
        theme(axis.line = element_line(colour="black", size=1), text = element_text(size=30),
              plot.title = element_text(color="Black", size=30, face="bold"))

# SP500 vs time
g2 <- ggplot(combined_data, aes(x = Date, y = Close_sp500)) + 
        geom_line(color = 'Blue', size = 1.5) +
        labs(x = "Fecha", y = "Precio BTC [USD]", title = "Precio del Índice SP500, periodo 2014-2023") +
        theme(axis.line = element_line(colour="black", size=1), text = element_text(size=30),
              plot.title = element_text(color="Black", size=30, face="bold"))

# Bitcoin vs time
g3 <- ggplot(combined_data, aes(x = Date, y = Close_nasdaq)) + 
        geom_line(color = 'Green', size = 1.5) +
        labs(x = "Fecha", y = "Precio BTC [USD]", title = "Precio del Nasdaq, período 2014-2023") +
        theme(axis.line = element_line(colour="black", size=1), text = element_text(size=30),
              plot.title = element_text(color="Black", size=30, face="bold"))

# Founds Rate vs time
g4 <- ggplot(combined_data_fed, aes(x = DATE, y = tasa_FED)) +
        geom_line(color = 'Orange', size = 1.5) +
        labs(x = "Fecha", y = "Tasa de interés", title = "Tasa de interés de referencia en EEUU, periodo 2014-2023") +
        theme(axis.line = element_line(colour="black", size=1), text = element_text(size=30),
              plot.title = element_text(color="Black", size=30, face="bold"))

# GDP vs time
g5 <- ggplot(combined_data_fed, aes(x = DATE, y = PBI_FED)) +
        geom_line(color = 'Black', size = 1.5) +
        labs(x = "Fecha", y = "Producto Bruto Interno", title = "Producto Bruto Interno de EEUU, periodo 2014-2023") +
        theme(axis.line = element_line(colour="black", size=1), text = element_text(size=30),
              plot.title = element_text(color="Black", size=30, face="bold"))

# GDP vs time
g6 <- ggplot(combined_data_fed, aes(x = DATE, y = desempleo_FED)) +
        geom_line(color = 'Blue', size = 1.5) +
        labs(x = "Fecha", y = "Tasa de desempleo", title = "Tasa de desempleo en EEUU, periodo 2014-2023") +
        theme(axis.line = element_line(colour="black", size=1), text = element_text(size=30),
              plot.title = element_text(color="Black", size=30, face="bold"))

g7 <- ggplot(combined_data_fed) +
        geom_line(aes(x = DATE, y = tasa_FED), color = 'Red', size = 1.5) +
        geom_line(aes(x = DATE, y = desempleo_FED), color = 'Blue', size = 1.5) +
        labs(x = "Fecha", y = "Tasa de desempleo", title = "Datos macroeconomicos de EEUU") +
        theme(axis.line = element_line(colour="black", size=1), text = element_text(size=30),
              plot.title = element_text(color="Black", size=30, face="bold"))