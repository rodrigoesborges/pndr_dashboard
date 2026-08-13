##infra4 siconfi despesa habitação

files <- list.files(path = "coleta/cache/infra4_aedi_s/",
                    full.names = TRUE,
                    recursive = TRUE,
                    pattern = ".csv$")


reading_data <- function(x){
  data <- data.table::fread(x) |>
    dplyr::filter(coluna == "Despesas Pagas") |>
    dplyr::filter(stringr::str_detect(conta, "^15 -") | stringr::str_detect(conta, "^18.543 -")) |>
    dplyr::rename(codmun = cod_ibge,
           value = valor) |>
    dplyr::group_by(codmun,ano=exercicio) |>
    dplyr::summarise(value = sum(value, na.rm = TRUE)) |>
    dplyr::mutate(
           variavel = "infra4_aedi")
}


infra4_aedi <- purrr::map_dfr(.x = files, .f = reading_data)

saveRDS(infra4_aedi,'coleta/cache/infra4_aedi_s/infra4_aedi.rds')
