# Dependencies
library(tidyverse)
library(modelr)
library(lubridate)

# directories
data_folder <- "data"

# dataframes
btc_data <- read_csv(file.path(data_folder, 'BTC/BTC-USD_hist.csv'))
oro_data <- read_csv(file.path(data_folder, 'GOLD/GC=F_hist.csv'))
brent_data <- read_csv(file.path(data_folder, 'BRENT/BZ=F_hist.csv'))
wti_data <-read_csv(file.path(data_folder, 'WTI/CL=F_hist.csv'))
nasdaq_data <- read_csv(file.path(data_folder, 'NASDAQ/^IXIC_hist.csv'))
sp500_data <- read_csv(file.path(data_folder, 'SP500/^GSPC_hist.csv'))
fed_data <- read_csv(file.path(data_folder, 'FED/FEDFUNDS.csv'))
pbi_data <- read_csv(file.path(data_folder, 'FED/GDP.csv'))
desempleo <- read_csv(file.path(data_folder, 'FED/unemployment.csv'))

# discarding innecesary columns
btc_data <- subset(btc_data, select = c("Date", "Open", "High", "Low", "Close", "Volume"))
oro_data <- subset(oro_data, select = c("Date", "Open", "High", "Low", "Close", "Volume"))
brent_data <- subset(brent_data, select = c("Date", "Open", "High", "Low", "Close", "Volume"))
wti_data <- subset(wti_data, select = c("Date", "Open", "High", "Low", "Close", "Volume"))
sp500_data <- subset(sp500_data, select = c("Date", "Open", "High", "Low", "Close", "Volume"))
nasdaq_data <- subset(nasdaq_data, select = c("Date", "Open", "High", "Low", "Close", "Volume"))

# changing types
btc_data$Date <- as.Date(btc_data$Date)
oro_data$Date <- as.Date(oro_data$Date)
brent_data$Date <- as.Date(brent_data$Date)
wti_data$Date <- as.Date(wti_data$Date)
nasdaq_data$Date <- as.Date(nasdaq_data$Date)
sp500_data$Date <- as.Date(sp500_data$Date)

# joining dataframes
temp_data <- inner_join(btc_data, oro_data, by = "Date", suffix = c("_btc", "_oro"))
combined_data <- inner_join(temp_data, brent_data, by = "Date", suffix = c("", "_brent"))
combined_data <- inner_join(combined_data, wti_data, by = "Date", suffix = c("", "_wti"))
combined_data <- inner_join(combined_data, sp500_data, by = "Date", suffix = c("", "_sp500"))
combined_data <- inner_join(combined_data, nasdaq_data, by = "Date", suffix = c("", "_nasdaq"))

# some problems with brent suffix, so renaming
names(combined_data)[names(combined_data) == "Open"] <- "Open_brent"
names(combined_data)[names(combined_data) == "High"] <- "High_brent"
names(combined_data)[names(combined_data) == "Low"] <- "Low_brent"
names(combined_data)[names(combined_data) == "Close"] <- "Close_brent"
names(combined_data)[names(combined_data) == "Volume"] <- "Volume_brent"

## 
combined_data_fed <- inner_join(fed_data, pbi_data, by = "DATE", suffix = c("_fed", "_pbi"))
combined_data_fed <- inner_join(combined_data_fed, desempleo, by = "DATE", suffix = c("", "_desemp"))

# some problems with fed suffix, so renaming
names(combined_data_fed)[names(combined_data_fed) == "GDP"] <- "PBI_FED"
names(combined_data_fed)[names(combined_data_fed) == "UNRATE"] <- "desempleo_FED"
names(combined_data_fed)[names(combined_data_fed) == "FEDFUNDS"] <- "tasa_FED"

# data conversion
combined_data <- data.frame(combined_data)
combined_data_fed <- data.frame(combined_data_fed)


# First model
df_model_1 <- subset(combined_data, select=c("Date", "Close_btc", "Close_oro", "Close_brent", "Close_nasdaq"))
model_1 = lm( Close_btc + Close_oro + Close_brent + Close_nasdaq ~ Date, data=df_model_1)
df_model_1 <- add_predictions(data=df_model_1, model=model_1) %>% add_residuals(model=model_1)

# Second model
df_model_2 <- df_model_1
model_2 = lm(Close_btc ~ Date + Close_oro + Close_brent + Close_nasdaq, data=df_model_2)
df_model_2 <- add_predictions(data=df_model_2, model=model_2) %>% add_residuals(model=model_2)

