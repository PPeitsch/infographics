library(tidyverse)
library(nycflights13)
library(ggridges)
library(reshape2)
library(maps)
library(mapdata)
library(viridis)
library("RColorBrewer")

fig_folder <- "fig"

#flights <- flights %>% filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
flights_correction <- flights %>% filter_all(all_vars(!is.infinite(.)))
#flights <- flights[!is.infinite(rowSums(flights)),]

flights_grouped <- group_by(flights_correction, carrier) %>%
                    mutate(n_flights=n()) %>%
                    filter(dep_delay>0, arr_delay<0) %>%
                    summarise(month, dep_delay, arr_delay, acum_dep_delay=sum(dep_delay, na.rm=TRUE),
                                acum_arr_delay=sum(arr_delay, na.rm=TRUE), acum_dist=sum(distance, na.rm=TRUE),
                                media=mean(dep_delay), mediana=median(dep_delay), carrier, distance, n_flights) %>%
                    ungroup()
months_ = unique(month.abb[sort(flights_grouped$month)])
carriers_ = unique(sort(flights_grouped$carrier))




acumulated_delays <- summarise(flights_grouped, .groups="carrier", acum_dep_delay, acum_arr_delay*-1, carrier) %>%
                    ungroup() %>%
                    gather("delay", "value", -carrier)
p1 <- ggplot(data=flights_grouped, aes(x=reorder(carrier, n_flights), y=acum_dep_delay/60)) +
        geom_bar(stat="identity", position="dodge") +
        geom_text(aes(x=carrier,
                      y=acum_dep_delay/60,
                      label=round(acum_dep_delay/60),
                      hjust="center",
                     ),
                  color="white",
                  vjust=1.5,
                  size=6
                 ) +
        geom_text(aes(x=carrier,
                      y=acum_dep_delay/60,
                      label=n_flights,
                      hjust="center",
                     ),
                  color="black",
                  vjust=-0.8,
                  size=6.5
                 ) +
        geom_text(aes(x=carrier,
                      y=acum_dep_delay/60,
                      label="#flights:",
                      hjust="center",
                     ),
                  color="black",
                  vjust=-2,
                  size=6.5
                 ) +
        coord_cartesian(ylim=c(0, 1500)) +
        scale_y_sqrt() +
        labs(y = 'Retraso anual (h)', fill="Tipo de retraso") +
        scale_x_discrete('Aerolínea') +
        theme(axis.line = element_line(colour = "black", size = 1.5), text = element_text(size = 32), legend.position = c(0.15, 0.80))
fig1_name <- "fig1.jpeg"




p2 <- ggplot(data=flights_grouped, aes(x=reorder(carrier, n_flights), y=acum_dist)) +
        geom_bar(stat="identity", position="dodge") +
        geom_text(aes(x=carrier,
                      y=acum_dist,
                      label=round(acum_dist/1000, 2),
                      hjust="center",
                     ),
                  color="white",
                  vjust=1.2,
                  size=5
                 ) +
        geom_text(aes(x=carrier,
                      y=acum_dist,
                      label="mil",
                      hjust="center",
                     ),
                  color="white",
                  vjust=2.2,
                  size=5
                 ) +
        geom_text(aes(x=carrier,
                      y=acum_dist,
                      label=n_flights,
                      hjust="center",
                     ),
                  color="black",
                  vjust=-0.8,
                  size=6.5
                 ) +
        geom_text(aes(x=carrier,
                      y=acum_dist,
                      label="#flights:",
                      hjust="center",
                     ),
                  color="black",
                  vjust=-2,
                  size=6.5
                 ) +
        coord_cartesian(ylim=c(0, 20000000)) +
        scale_y_sqrt() +
        labs(y = 'Reorrido anual (km)') +
        scale_x_discrete('Aerolínea') +
        theme(axis.line = element_line(colour = "black", size = 1.5), text = element_text(size = 32))
        #theme(axis.line = element_line(colour = "black", size = 1.5), text = element_text(size = 30), axis.title.x=element_blank(),
        #axis.text.x=element_blank(),axis.ticks.x=element_blank())
fig2_name <- "fig2.jpeg"




p3 <- ggplot(data=flights_grouped) +
        geom_boxplot(aes(x=reorder(carrier, mediana), y=dep_delay), outlier.shape = NA) +
        labs(y = 'Retraso (min)', fill="Tipo de retraso") +
        scale_x_discrete('Aerolínea') +
        coord_cartesian(ylim=c(2, 25)) +
        theme(axis.line = element_line(colour = "black", size = 1.5), text = element_text(size = 36))
fig3_name <- "fig3.jpeg"




flights_season <- mutate(flights_grouped, season = ifelse(month %in% 3:5, "Primavera",
    ifelse(month %in% 6:8, "Verano", ifelse(month %in% 9:11, "Otoño", "Invierno"))))
flights_season_selected <- filter(flights_season, carrier==c('9E', 'F9', 'DL'))
p4 <- ggplot(data=flights_season_selected, aes(x=reorder(season, acum_dist), y=acum_dist, fill=factor(carrier))) +
        geom_col() +
        labs(y = 'Distancia (km)', x= "Temporada", fill = 'Compañia') +
        theme(axis.line = element_line(colour = "black", size = 1.5), text = element_text(size = 32))
fig4_name <- "fig4.jpeg"



airports_filtered <- filter(airports, lon<(-70)&lon>-130, lat<50&lat>25)
flights_airports <- inner_join(flights, airports, by = c("dest" = "faa")) %>%
    group_by(dest, lat, lon) %>%
    summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE), count_dep = n()) %>%
    filter(lon<(-70)&lon>-130, lat<50&lat>25) %>%
    ungroup()
usa <- map_data("usa")
p5 <- ggplot() + 
        geom_polygon(data=usa, aes(x=long, y=lat, group=group), fill="white", color="black") +
        geom_point(data=flights_airports, aes(x=lon, y=lat), color="orange", size=15, position=position_jitter(h=0.15,w=0.15)) +
        #coord_cartesian(ylim=c(26, 48.5), xlim=c(-123, -68)) +
        labs(x='Longitud (°)', y="Latitud (°)", color="Aeropuertos") +
        theme(axis.line = element_line(colour = "black", size = 1.5), text = element_text(size = 32))
fig5_name <- "fig5.jpeg"




flights_season_selected <- filter(flights_season, carrier==c('9E', 'F9', 'DL'))
states <- map_data("state")
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
p6 <- ggplot() + 
        geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="gray", color="black", size=2) +
        geom_point(data=flights_airports, aes(x=lon, y=lat, color=count_dep), size=15) +
        scale_colour_gradientn(colours = myPalette(100), na.value = NA) +
        labs(x='Longitud (°)', y="Latitud (°)", color="# retrasos") +
        theme(axis.line = element_line(colour = "black", size = 1.5), text = element_text(size = 36), legend.position = c(0.9, 0.2))
fig6_name <- "fig6.jpeg"





flights_airports_selected <- inner_join(flights, airports, by = c("dest" = "faa")) %>%
    group_by(dest, lat, lon) %>%
    ungroup()
flights_another <- filter(flights_airports_selected, carrier==c('F9', 'DL', '9E'))
flights_another_2 <- group_by(flights_another, dest, lat, lon) %>%
    summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE), count_dep=n(), lat, lon, carrier, dest) %>%
    ungroup()
delayed_airport <- distinct(flights_another_2) %>%
    filter(carrier=='DL') %>%
    arrange(-count_dep)
delayed_airport_3 <- head(delayed_airport, 3)
p7 <- ggplot() + 
        geom_polygon(data=states, aes(x=long, y=lat, group=group), fill="gray", color="black", size=2) +
        geom_point(data=flights_another_2, aes(x=lon, y=lat, color=count_dep), size=15) +
        geom_label(data=delayed_airport_3, aes(label=dest, x=lon, y=lat),
                   vjust=1.5,
                   color="dark violet", 
                   hjust=-0.15, 
                   angle=0, 
                   size=8
                ) +
        scale_colour_gradientn(colours = myPalette(100), na.value = NA) +
        labs(x='Longitud (°)', y="Latitud (°)", color="# retrasos") +
        theme(axis.line = element_line(colour = "black", size = 1.5), text = element_text(size = 36), legend.position = c(0.9, 0.2))
fig7_name <- "fig7.jpeg"