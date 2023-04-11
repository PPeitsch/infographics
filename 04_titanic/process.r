# Library imports
library(tidyverse)
library(ggthemes)

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
# Graphic of survivors by CLASS
p1 <- ggplot(data = df_titanic_st, aes(x=class, y=n_pasenger, fill=survived)) +
  geom_col(position="dodge") +
  geom_text(aes(label=n_pasenger), colour="white", vjust = 3, position = position_dodge(.9)) +
  theme_minimal() +
  labs(x = 'Categoría de pasaje', y = 'Cantidad de personas') +
  theme(legend.position = "right") +
  theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 20))


# Sorting dataset by age
df_titanic_st2 <- group_by(df_titanic, survived, sex) %>%
  summarize(n_pasenger = n(), age)
# Graphic of survivors by SEX
p2 <- ggplot(data=df_titanic_st2, aes(x=sex, y=n_pasenger, fill=survived)) +
  geom_col(position="dodge") +
  geom_text(aes(label=n_pasenger), colour="white", vjust = 3, position = position_dodge(.9)) +
  theme_minimal() +
  labs(x='Sexo', y='Cantidad de personas') +
  theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 20))


# Graphic of survivors by AGE
p3 <- ggplot(data=df_titanic_st2, aes(x=age, fill=sex, colour=sex)) +
  geom_density(alpha=0.5, linewidth=1.5) +
  theme_foundation() +
  facet_wrap(~survived) +
  theme(legend.position = "top") +
  labs(x='Edad', y='Densidad de personas') +
  theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 20))