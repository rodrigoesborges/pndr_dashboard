jabaixados <- readRDS("dadostat/jabaixadosiconfi.rds")

municipios <- geobr::read_municipality(year=2022)
municipios <- municipios|>sf::st_drop_geometry()

siconficoleta_receitas <- \(local,dirsaida='.'){
  dadoslocal <- data.table::rbindlist(lapply(2014:2024,\(x) siconfir::get_annual_acc(year = x, cod = local,annex = 'DCA-Anexo I-C')))
  if(length(local)==1) {
  readr::write_csv2(dadoslocal,paste0(dirsaida,'/mun_',local,"_2015_2024_annex_dca_i_c.csv"))
  } else {
    readr::write_csv2(dadoslocal,paste0(dirsaida,'/mun_',local[1],"_a_",local[length(local)],"_2014_2024_annex_dca_i_c.csv"))
  }
}

comproblemascr <- c(1200328,1200435,1300029,1300060,
                    1300102,1300201,1300409,1301159,
                    1301506,1302108,1302207,1302306,
                    1303700,1304005,1304237,1304401,
                    1400027,1400050,1400233,1400282,
                    1400407,1400506,1400605,1400704,
                    1500701,1500958,1501006,1501105,
                    1501253,1501576,1501600,1501956,
                    1502905,1503101,1503408,1504109,
                    2305209,2308203,2903003,2903235,
                    2903706,2905156,2908200,2911253,
                    2919009,2919900

)

municipios_faltantes <- municipios[!(municipios$code_muni %in% jabaixados),]$code_muni
municipios_faltantes <- c(municipios_faltantes,comproblemascr)

tictoc::tic()
testebatch <- siconficoleta_receitas(municipios_faltantes[2001:2100])
tictoc::toc()


tictoc::tic()
testebatch2 <- data.table::rbindlist(lapply(municipios_faltantes[2101:2200],siconficoleta_receitas))
tictoc::toc()
