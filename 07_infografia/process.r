library(tidyverse)
library(nycflights13)
library(ggridges)
library(reshape2)
library(gridExtra)

fig_folder <- "fig"

#flights <- flights %>% filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
flights <- flights %>% filter_all(all_vars(!is.infinite(.)))
#flights <- flights[!is.infinite(rowSums(flights)),]

flights_grouped <- summarise(flights, .groups="month", month, departure=dep_delay, arrival=arr_delay, carrier)
months_ = unique(month.abb[sort(flights_grouped$month)])
carriers_ = unique(sort(flights_grouped$carrier))

flights_melted_1 <- subset(flights_grouped, select= -c(month)) %>% melt(id="carrier")
flights_grouped_carrier <- group_by(flights_melted_1, variable)
p1 <- ggplot(data=flights_grouped_carrier,
       aes(
           y=value,
           x=factor(carrier),
           color=factor(variable)
       )) +
    # Colores y forma del boxplot
    geom_violin(lwd=1.5) +
    coord_cartesian(ylim=c(-1, 0.75)) +
    labs(
        y = 'Retraso (min)',
        color = 'Tipo de retraso'
        ) +
    scale_x_discrete('Aerolínea', label=carriers_) +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 25))
fig1_name <- "fig1.jpeg"

flights_melted_2 <- melt(flights_grouped, id=c("carrier", "month"))
flights_grouped_ <- group_by(flights_melted_2, month)
p2 <- ggplot(data=flights_grouped_, aes(y=factor(carrier), x=value, color=factor(month))) +
    # Colores y forma del boxplot
    geom_density_ridges(alpha=0.05, lwd=1.5) +
    coord_cartesian(xlim=c(-1.5, 5)) +
    labs(
        x = 'Retrasos (min)',
        color = 'Etiqueta'
        ) +
    scale_color_discrete(label=months_) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0), 'Aerolínea', label=carriers_) +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 25))
fig2_name <- "fig2.jpeg"


flights_reduced <- filter(flights_grouped, carrier==c('OO', 'HA', 'FL', 'F9', 'AS', '9E'))
#flights_reduced <- filter(flights_melted_3, carrier==c('OO', 'YV', 'HA', 'FL', 'F9', 'AS', '9E'))
p3 <- ggplot(data=flights_reduced) +
        geom_boxplot(aes(y=departure, x=factor(carrier), color=factor(month))) +
        coord_cartesian(ylim=c(-0.2, 2)) +
        labs(y='Retraso de partida (min)', color = 'Etiqueta') +
        scale_color_discrete(label=months_) +
        theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 25), axis.title.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank())
p4 <- ggplot(data=flights_reduced) +
        geom_boxplot(aes(y=arrival, x=factor(carrier), color=factor(month))) +
        coord_cartesian(ylim=c(-1,2.5)) +
        labs(y='Retraso de arribo (min)', color = 'Etiqueta') +
        scale_color_discrete(label=months_) +
        scale_x_discrete('Aerolínea', label=c('OO', 'HA', 'FL', 'F9', 'AS', '9E')) +
        theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 25))
fig3_name <- "fig3.jpeg"


flights_melted_4 <- flights_grouped[complete.cases(flights_grouped), ]
flights_metricas <- filter(flights_melted_4, carrier==c('FL')) %>% group_by(month, carrier) %>%
                    summarise(
                                media_arr_delay=mean(arrival),
                                media_dep_delay=mean(departure),
                                mediana_arr_delay=median(arrival),
                                mediana_dep_delay=median(departure),
                                sd_arr_delay=sd(arrival),
                                sd_dep_delay=sd(departure)
                                #iqr_arr_delay=iqr(arr_delay),
                                #iqr_dep_delay=iqr(dep_delay),
                                )# %>% filter(carrier==c('AS', 'FL', 'YV')) %>% melt(id=c("carrier", "month"))

flights_grouped2 <- summarise(flights, .groups="month", month, departure=dep_delay/60, arrival=arr_delay/60, carrier, origin,dest)
flights_reduced2 <- filter(flights_grouped2, carrier==c('OO', 'HA', 'FL', 'F9', 'AS', '9E'))
flights_OO <- filter(flights_reduced2, carrier==c('OO'))
flights_HA <- filter(flights_reduced2, carrier==c('HA'))
flights_FL <- filter(flights_reduced2, carrier==c('FL'))
flights_F9 <- filter(flights_reduced2, carrier==c('F9'))
flights_AS <- filter(flights_reduced2, carrier==c('AS'))
flights_9E <- filter(flights_reduced2, carrier==c('9E'))