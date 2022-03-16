library(sf)
library(data.table)
source("./0-env-download.R")

asos <- read_station_list()
sulf <- get_annual_epa()

a <- st_as_sf(asos, coords = c("LON", "LAT"), crs = st_crs(4326))
b <- st_as_sf(sulf, coords = c("LON", "LAT"), crs = st_crs(4327))

dist <- data.table(st_as_sf(a, b))
