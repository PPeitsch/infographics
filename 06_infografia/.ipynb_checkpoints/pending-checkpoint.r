#library(rnaturalearth)
#library(sp)

# MAPA
eeuu <- ne_countries(scale = "medium", returnclass = "sf")
p6 <- ggplot(data=eeuu) + geom_sf() +
    geom_sf(data=flights_airports, aes(color = ID)) +
    scale_y_continuous(name = "", limits = c(min(flights_airports$lat)-1, max(flights_airports$lat)+1)) +
    scale_x_continuous(name = "", limits = c(min(flights_airports$lon)-1, max(flights_airports$lon)+1)) +
    theme_light() +
    theme(legend.position = "none")


# LINEAL
flights_reduced <- filter(flights_correction, dep_delay>0, distance<3000, carrier==c('B6', 'Dl', 'EV', 'UA'))
p7 <- ggplot(data=flights_reduced, aes(y=dep_delay, x=distance)) + 
    geom_point() +
    labs(x='Distancia (km)', y="Retrasos (min)") +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 36))
fig7_name <- "fig7.jpeg"



# METRICAS
#flights_melted <- flights_grouped[complete.cases(flights_reduced), ] %>%
flights_metricas <- filter(flights_correction, carrier==c('B6', 'Dl', 'EV', 'UA')) %>% group_by(month, carrier) %>%
                    summarise(
                                media_dep_delay=mean(dep_delay),
                                mediana_dep_delay=median(dep_delay),
                                sd_dep_delay=sd(dep_delay)
                                #iqr_arr_delay=iqr(arr_delay),
                                #iqr_dep_delay=iqr(dep_delay),
                                )# %>% filter(carrier==c('AS', 'FL', 'YV')) %>% melt(id=c("carrier", "month"))

###