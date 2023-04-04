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

# Summary of passengers and survivors
df_titanic_st <- group_by(df_titanic, survived, class) %>%
  summarize(n_pasenger = n(),
            mean_age = mean(age, na.rm = TRUE))

# Graphic of survivors
ggplot(data = df_titanic_st, aes(x = class, y = n_pasenger, fill = survived)) +
  geom_bar(stat = "identity", color = "white", alpha = 0.8) +
  theme_minimal() +
  labs(x = 'Categoría de pasaje', y = 'Cantidad de personas', title = "Estadística breve de sobrevivientes del Titanic") +
  theme(legend.position = "right")


# Sorting dataset by age
#df_titanic_st <- arrange(df_titanic, age)

# Statistics of dataset
#min_ticket <- min(df_titanic$fare)
#max_ticket <- max(df_titanic$fare)
#mean_ticket <- mean(df_titanic$fare)

#min_age <- min(df_titanic$age)
#max_age <- max(df_titanic$age)
#mean_age <- mean(df_titanic$age)

#df_titanic_st <- summarise(df_titanic, min())

  #survivors = count(survived == 'yes')/n_pasenger * 100,
  #gender = mean(sex == "male") * 100,
  #first_class = count(class == 'yes')/n_pasenger * 100,