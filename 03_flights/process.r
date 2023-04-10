# Importing librearies
library(tidyverse)
library(nycflights13)
#library(ggridges)
library(reshape2)

# Folder name
fig_folder <- "fig"

# Removing NA data
#flights <- flights %>% filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
flights <- flights %>% filter_all(all_vars(!is.infinite(.)))
#flights <- flights[!is.infinite(rowSums(flights)),]

# Creating a list of unique carriers and months
flights_grouped <- group_by(flights, month) %>% select(month, dep_delay, arr_delay, carrier)
months_ = unique(month.abb[sort(flights_grouped$month)])
carriers_ = unique(sort(flights_grouped$carrier))

# Grouping by month
flights_melted_1 <- subset(flights_grouped, select= -c(carrier)) %>% melt(id="month")
flights_grouped_month <- group_by(flights_melted_1, variable)

# Ploting departure delay vs month
fig1_name <- "fig1.jpeg"
p1 <- ggplot(data=flights_grouped_month,
       aes(
           x=value,
           y=factor(month),
           color=factor(variable)
       )) +
    # Colores y forma del boxplot
    geom_boxplot() +
    labs(
        x = 'Departure delay (s)',
        color = 'Delay'
        ) +
    scale_y_discrete('Month', label=months_) +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 30))
