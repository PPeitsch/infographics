# Library imports
library(tidyverse)


# Datasets folder
data_folder <- "data"

# Graph folder
fig_folder <- "fig"

# Files list of datasets folder
list_files <- list.files(data_folder)
# Number of datasets
n <- length(list_files)
# Changin size of plot
options(repr.plot.width=25, repr.plot.height=12)


# Importing dataset
df_titanic <- read.csv(file.path(data_folder, list_files), dec=",")

# Name of columns (variables)
variables <- colnames(df_titanic)

# Changing long names
variables[3] <- "siblings"
variables[4] <- "parents"
variables[6] <- "port"
colnames(df_titanic) <- variables

# Sorting dataset by age
df_titanic_st <- arrange(df_titanic, age)

# Statistics of dataset
min_ticket <- min(df_titanic$fare)
max_ticket <- max(df_titanic$fare)
mean_ticket <- mean(df_titanic$fare)



#df_titanic-st <- summarise(df_titanic, min())
