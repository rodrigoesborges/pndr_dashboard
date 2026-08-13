siconfi_ufrec <- \(local,dirsaida='coleta/cache/gov4_uf') {
  dadoslocal <- data.table::rbindlist(lapply(2014:2024,\(x) siconfir::get_annual_acc(year = x, cod = local,annex = 'DCA-Anexo I-C')))
  readr::write_csv2(dadoslocal,paste0(dirsaida,'/uf_',local,"_2014_2024_annex_dca_i_c.csv"))
}



ufs_lista <- sf::st_drop_geometry(geobr::read_state())

lapply(ufs_lista$code_state,siconfi_ufrec)


calcula_ifsm_uf <- \(uf) {

  ufarquivo <-
    list.files(
      path = "coleta/cache/gov4_uf",
      pattern=paste0("uf_",uf,"_2014_2024_annex_dca_i_c.csv"),
      full.names = TRUE)

  ifsm_teste <- data.table::fread(ufarquivo)


  #"siconfi-cor:RO1.1.0.0.00.00.00"
  contas <- c(
  "RO1.1.0.0.00.00.00", #" - Receita Tributária"
#  "RI7.1.0.0.00.00.00" #- Receita Tributária
  "TotalReceitas")

  ifsm_teste <- ifsm_teste|>janitor::clean_names()

  ifsm_teste <-
    ifsm_teste[cod_conta %in% contas & coluna == "Receitas Brutas Realizadas",
               .(ano=exercicio,uf=cod_ibge,cod_conta,valor)]

  ifsm_teste <-
    ifsm_teste|>
    dplyr::group_by(uf,ano)|>
    tidyr::pivot_wider(
      names_from = cod_conta,
      values_from=valor,
      values_fill=0)|>
    dplyr::ungroup()


  ifsm_teste <-
    ifsm_teste |>
    dplyr::group_by(uf,ano)|>
    dplyr::mutate(across(`TotalReceitas`:`RO1.1.0.0.00.00.00`, ~tidyr::replace_na(.x, 0))) |>
    dplyr::transmute(total_receitas = TotalReceitas,
                     arrecadacao_propria = `RO1.1.0.0.00.00.00`,
                     value = 100*arrecadacao_propria/`TotalReceitas`) |>
    dplyr::mutate(variavel = "governativas4",
                  value = ifelse(!is.finite(value), NA, value)) |>
    dplyr::ungroup()

  return(ifsm_teste)
}

ifsm_uf <- lapply(ufs_lista$code_state,
                             calcula_ifsm_uf)
ifsm_uf <-
  data.table::rbindlist(
    ifsm_uf,fill=TRUE)



saveRDS(ifsm_uf,'coleta/cache/gov4_uf/ifsm_ufs.rds')
