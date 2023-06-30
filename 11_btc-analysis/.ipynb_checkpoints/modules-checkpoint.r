source("core.r")

# GRAPHICS
# Bitcoin vs time
g1 <- ggplot(combined_data) + 
        geom_line(aes(x = Date, y = Close_btc), color = 'Red', size = 1.5) +
        labs(x = "Fecha", y = "Precio BTC [USD]", title = "Precio del Bitcoin, periodo 2014-2023") +
        theme(axis.line = element_line(colour="black", size=1), text = element_text(size=30),
              plot.title = element_text(color="Black", size=30, face="bold"))
g1_name <- "g1.jpeg"

# SP500 vs time
g2 <- ggplot(combined_data) + 
        geom_line(aes(x = Date, y = Close_sp500), color = 'Blue', size = 1.5) +
        labs(x = "Fecha", y = "Precio SP500 [USD]", title = "Precio del Indice SP500, periodo 2014-2023") +
        theme(axis.line = element_line(colour="black", size=1), text = element_text(size=30),
              plot.title = element_text(color="Black", size=30, face="bold"))
g2_name <- "g2.jpeg"

# Nasdaq vs time
g3 <- ggplot(combined_data) + 
        geom_line(aes(x = Date, y = Close_nasdaq), color = 'Green', size = 1.5) +
        labs(x = "Fecha", y = "Precio Nasdaq [USD]", title = "Precio del Nasdaq, periodo 2014-2023") +
        theme(axis.line = element_line(colour="black", size=1), text = element_text(size=30),
              plot.title = element_text(color="Black", size=30, face="bold"))
g3_name <- "g3.jpeg"

# Founds Rate vs time
g4 <- ggplot(combined_data_fed) +
        geom_line(aes(x = DATE, y = tasa_FED), color = 'Orange', size = 1.5) +
        labs(x = "Fecha", y = "Tasa de interes", title = "Tasa de interes de referencia en EEUU, periodo 2014-2023") +
        theme(axis.line = element_line(colour="black", size=1), text = element_text(size=30),
              plot.title = element_text(color="Black", size=30, face="bold"))
g4_name <- "g4.jpeg"

# GDP vs time
g5 <- ggplot(combined_data_fed) +
        geom_line(aes(x = DATE, y = PBI_FED), color = 'Black', size = 1.5) +
        labs(x = "Fecha", y = "Producto Bruto Interno", title = "Producto Bruto Interno de EEUU, periodo 2014-2023") +
        theme(axis.line = element_line(colour="black", size=1), text = element_text(size=30),
              plot.title = element_text(color="Black", size=30, face="bold"))
g5_name <- "g5.jpeg"

#  vs time
g6 <- ggplot(combined_data_fed) +
        geom_line(aes(x = DATE, y = desempleo_FED), color = 'Blue', size = 1.5) +
        labs(x = "Fecha", y = "Tasa de desempleo", title = "Tasa de desempleo en EEUU, periodo 2014-2023") +
        theme(axis.line = element_line(colour="black", size=1), text = element_text(size=30),
              plot.title = element_text(color="Black", size=30, face="bold"))
g6_name <- "g6.jpeg"

# Founds Rate and Unemployment vs time
g7 <- ggplot(combined_data_fed) +
        geom_line(aes(x = DATE, y = tasa_FED), color = 'Red', size = 1.5) +
        geom_line(aes(x = DATE, y = desempleo_FED), color = 'Blue', size = 1.5) +
        labs(x = "Fecha", y = "Tasa (%)", title = "Datos macroeconomicos de EEUU") +
        theme(axis.line = element_line(colour="black", size=1), text = element_text(size=30),
              plot.title = element_text(color="Black", size=30, face="bold"))
g7_name <- "g7.jpeg"



# First model
m1 <- ggplot(data=df_model_1) + 
        geom_point(aes(x=Date, y=Close_btc)) +
        geom_line(data=df_model_1, aes(x=Date, y=pred), color='Blue', size=1.5) +
        theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 30),
              plot.title = element_text(color="Black", size=30, face="bold")) +
        labs(x="Fecha (años)", y="Precio (u$s)", title="Regresion lineal: modelo 1")
m1_name <- "m1.jpeg"
# Residual graph
m2 <- ggplot(data=df_model_1, aes(y = resid)) +
        geom_boxplot() +
        labs(title='Boxplot: Residuals', y='Residuals value') +
        theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 40),
              plot.title = element_text(color="Black", size=40, face="bold"))
m2_name <- "m2.jpeg"


# Second model
m3 <- ggplot(data=df_model_2) + 
        geom_point(aes(x=Date, y=Close_btc)) +
        geom_line(data=df_model_2, aes(x=Date, y=pred), color='Blue', size=1.5) +
        theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 30),
              plot.title = element_text(color="Black", size=30, face="bold")) +
        labs(x="Fecha (años)", y="Precio (u$s)", title="Regresion lineal: modelo 2")
m3_name <- "m3.jpeg"
# Residual graph
m4 <- ggplot(data=df_model_2, aes(y = resid)) +
        geom_boxplot() +
        labs(title='Boxplot: Residuals', y='Residuals value') +
        theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 40),
              plot.title = element_text(color="Black", size=40, face="bold"))
m4_name <- "m4.jpeg"


# Third model
m5 <- ggplot(data=df_model_3) + 
        geom_point(aes(x=Date, y=Close_btc)) +
        geom_line(data=df_model_3, aes(x=Date, y=pred), color='Blue', size=1.5) +
        theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 30),
              plot.title = element_text(color="Black", size=30, face="bold")) +
        labs(x="Fecha (años)", y="Precio (u$s)", title="Regresion lineal: modelo 3")
m5_name <- "m5.jpeg"
# Residual graph
m6 <- ggplot(data=df_model_3, aes(y = resid)) +
        geom_boxplot() +
        labs(title='Boxplot: Residuals', y='Residuals value') +
        theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 40),
              plot.title = element_text(color="Black", size=40, face="bold"))
m6_name <- "m6.jpeg"