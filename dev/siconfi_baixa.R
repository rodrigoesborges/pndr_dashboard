siconficoleta_dpaghab <- \(local,dirsaida='coleta/cache/infra4_aedi_s/'){
  dadoslocal <- data.table::rbindlist(lapply(2015:2024,\(x) siconfir::get_annual_acc(year = x, cod = local,annex = 'DCA-Anexo I-E')))
  readr::write_csv2(dadoslocal,paste0(dirsaida,'mun_',local,"_2015_2024_annex_dca_i_e.csv"))
}


municipios <- geobr::read_municipality(year=2022)

municipios <- municipios|>sf::st_drop_geometry()

municipios$grupo_muni <- substr(municipios$code_muni,1,3)

lapply(unique(malhacenso$CD_MUN)[unique(malhacenso$CD_MUN)>2615409],
       siconficoleta_dpaghab)

grupos_muni <- (municipios|>dplyr::distinct(code_muni)|>dplyr::distinct(a=substr(code_muni,1,3)))$a

jabaixados <- as.numeric(gsub("mun_(\\d+)_.*","\\1",list.files("coleta/cache/infra4_aedi_s/")))

library(doParallel)
cl <- makeCluster(6,type="FORK")
registerDoParallel(cl,6)




parLapply(cl,municipios[municipios$grupo_muni>312,]$code_muni,
       siconficoleta_dpaghab)


tentanovamente <- municipios[!municipios$code_muni %in% jabaixados,]$code_muni
parLapply(cl,tentanovamente,
          siconficoleta_dpaghab)


##Brasília - pega DF

siconfir::find_cod('Distrito Federal')
siconficoleta_dpaghab(53)

#copiado mun_53 para mun_5300108
bsb <- readr::read_csv2('coleta/cache/infra4_aedi_s/mun_5300108_2015_2024_annex_dca_i_e.csv')
bsb$cod_ibge <- 5300108
readr::write_csv2(bsb,'coleta/cache/infra4_aedi_s/mun_5300108_2015_2024_annex_dca_i_e.csv')
# tic()
# pacote2015_2024_teste <- siconfir::get_annual_acc(year=2015:2024,cod=municipios[municipios$grupo_muni==312,]$code_muni,annex='DCA-Anexo I-E')
# toc()
#
#
# tic()
# pacote2015_2024_teste2 <- siconfir::get_annual_acc(year=2015,cod=municipios[municipios$grupo_muni==313,]$code_muni,annex='DCA-Anexo I-E')
# toc()
#
#
# tic()
# pacote2015_2024_teste2 <- parallel::parLapply(cl,2015:2020,\(x) {siconfir::get_annual_acc(year=x,cod=municipios[municipios$grupo_muni==313,]$code_muni,annex='DCA-Anexo I-E')})
# toc()
