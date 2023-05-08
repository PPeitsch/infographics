library(tidyverse)
library(nycflights13)
library(ggridges)
library(reshape2)
#library(gridExtra)

fig_folder <- "fig"

#flights <- flights %>% filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
flights_correction <- flights %>% filter_all(all_vars(!is.infinite(.)))
#flights <- flights[!is.infinite(rowSums(flights)),]

flights_grouped <- filter(flights_correction, dep_delay>0) %>%
    summarise(.groups="month", month, mean_delay=mean(dep_delay, na.rm=TRUE), mean_dist=mean(distance, na.rm=TRUE), carrier)
months_ = unique(month.abb[sort(flights_grouped$month)])
carriers_ = unique(sort(flights_grouped$carrier))

#flights_carrier <- subset(flights_grouped, select= -c(month)) %>% melt(id="carrier") %>% group_by(variable)
p1 <- ggplot(data=flights_grouped, aes(x=factor(carrier), y=mean_delay)) +
    geom_bar(stat="identity") +
    labs(y = 'Retraso medio (min)') +
    scale_x_discrete('Aerolínea', label=carriers_) +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 26))
fig1_name <- "fig1.jpeg"


p2 <- ggplot(data=flights_correction, aes(x=carrier, y=distance)) +
    geom_bar(stat="identity") +
    labs(y = 'Distancia (km)') +
    scale_x_discrete('Aerolínea', label=carriers_) +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 26), axis.title.x=element_blank(),
    axis.text.x=element_blank(),axis.ticks.x=element_blank())
fig2_name <- "fig2.jpeg"



flights_season <- mutate(flights_grouped, season = ifelse(month %in% 3:5, "Primavera",
    ifelse(month %in% 6:8, "Verano", ifelse(month %in% 9:11, "Otoño", "Invierno"))))
p3 <- ggplot(data=flights_season, aes(x=season, y=mean_dist, fill=factor(carrier)))+ geom_col() +
    labs(y = 'Distancia (km)', x= "Temporada", fill = 'Compañia') +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 36))
fig3_name <- "fig3.jpeg"



airports_filtered <- filter(airports, lon<0)
p4 <- ggplot(data=airports_filtered, aes(x=lon, y=lat, color="red")) + 
    geom_point(size=3) +
    labs(x='Longitud (°)', y="Latitud (°)", color="Aeropuertos") +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 45))
fig4_name <- "fig4.jpeg"



flights_airports <- inner_join(flights, airports, by = c("dest" = "faa")) %>%
    group_by(dest, lat, lon) %>%
    summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE), count_dep = n())

p5 <- ggplot(data=flights_airports, aes(x=lon, y=lat, color=avg_dep_delay, size=count_dep)) + 
    geom_point() +
    labs(x='Longitud (°)', y="Latitud (°)", color="Retrasos(min)", fill="#Retrasos") +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 35))
fig5_name <- "fig5.jpeg"