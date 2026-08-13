## code to prepare `basemap` dataset goes here
#library(brazilmaps)
library(sf)
library(geobr)
library(rmapshaper)
#Mapa municipios
#basemap <- get_brmap("City")
# basemap$codmun <- as.numeric(as.character(trunc(basemap$City/10)))

basemap <- geobr::read_municipality(year=2020)|> ms_simplify(keep=0.01,keep_shapes = TRUE)
basemap$codmun <- as.numeric(as.character(trunc(basemap$code_muni/10)))


basemap <- basemap |>st_cast('MULTIPOLYGON')|>st_cast('POLYGON')
basemap <- st_as_sf(basemap)|>st_transform(4326)

basemap <- sf::st_make_valid(basemap)

usethis::use_data(basemap, overwrite = TRUE)
