library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(rnaturalearth)

# MAPA

#download a shapefile with ALL states
tmp_dl <- tempfile()
download.file("http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_state_20m.zip", tmp_dl)
unzip(tmp_dl, exdir=tempdir())
ST <- readOGR(tempdir(), "cb_2013_us_state_20m")

latlong2state <- function(pointsDF) {
    # Just copied the earlier code with some key changes
    states <- ST

    # Convert pointsDF to a SpatialPoints object 
    # USING THE CRS THAT MATCHES THE SHAPEFILE
    pointsCRS <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
    pointsSP <- SpatialPoints(pointsDF, proj4string=CRS(pointsCRS))

    # Use 'over' to get _indices_ of the Polygons object containing each point 
    indices <- over(pointsSP, states)

    # Return the state names of the Polygons object containing each point
    as.vector(indices$NAME)
}