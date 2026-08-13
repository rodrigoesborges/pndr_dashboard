load("data/basemap.rda")
municipios <- basemap|>sf::st_drop_geometry()|>
  dplyr::select("name_muni","code_muni")
munif <- municipios$code_muni
names(munif) <- municipios$name_muni
munif

usethis::use_data(munif,overwrite=T)
