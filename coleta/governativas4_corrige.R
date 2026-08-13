siconfi_rec <- \(local,dirsaida='coleta/cache/governativas4_aedi'){
  dadoslocal <- data.table::rbindlist(lapply(2014:2024,\(x) siconfir::get_annual_acc(year = x, cod = local,annex = 'DCA-Anexo I-C')))
  readr::write_csv2(dadoslocal,paste0(dirsaida,'/mun_',local,"_2014_2024_annex_dca_i_c.csv"))
}

calcula_ifsm <- \(codigoibge) {
  if(codigoibge==2605459){
    fnoronha <- data.frame(
      codmun=2605459,
      ano=2014:2024,
      value=NA,
      variavel='governativas4'

    )
    return(fnoronha)
  }
ifsm_teste <-
  data.table::fread(
    paste0("coleta/cache/governativas4_aedi/mun_",
           codigoibge,
    "_2014_2024_annex_dca_i_c.csv"))

contas <- c("RO1.1.1.2.02.00.00",
            "RO1.1.1.2.08.00.00",
            "RO1.1.1.3.05.00.00",
            "RO1.1.1.2.50.0.0",
            "RO1.1.1.2.53.0.0",
            "RO1.1.1.4.51.1.0",
            "RO1.1.1.8.01.1.0",
            "RO1.1.1.8.01.4.0",
            "RO1.1.1.8.02.3.0",
            "TotalReceitas")

ifsm_teste <- ifsm_teste|>janitor::clean_names()

ifsm_teste <-
  ifsm_teste[cod_conta %in% contas & coluna == "Receitas Brutas Realizadas",
             .(ano=exercicio,codmun=cod_ibge,cod_conta,valor)]

ifsm_teste <-
  ifsm_teste|>
  dplyr::group_by(codmun,ano)|>
  tidyr::pivot_wider(
    names_from = cod_conta,
    values_from=valor,
    values_fill=0)#|>
#  dplyr::ungroup()
colunazerada <- \(nomecol="RO1.1.1.2.02.00.00") {
if(!(nomecol %in% names(ifsm_teste) )) {
  ifsm_teste[[nomecol]] <- 0
}
  ifsm_teste
  }

for(conta in contas) {
  assign("ifsm_teste",colunazerada(conta))
}

ifsm_teste <-
  ifsm_teste |>
  dplyr::group_by(codmun,ano)|>
  dplyr::mutate(across(`TotalReceitas`:`RO1.1.1.4.51.1.0`, ~tidyr::replace_na(.x, 0))) |>
  dplyr::transmute(total_receitas = TotalReceitas,
    arrecadacao_propria =  dplyr::case_when(ano < 2018 ~ (`RO1.1.1.2.02.00.00`+`RO1.1.1.2.08.00.00`+`RO1.1.1.3.05.00.00`),
                                                           ano < 2022 ~ (`RO1.1.1.8.01.1.0`+`RO1.1.1.8.01.4.0`+`RO1.1.1.8.02.3.0`),
                                                           ano >= 2022 ~ (`RO1.1.1.2.50.0.0`+`RO1.1.1.2.53.0.0`+`RO1.1.1.4.51.1.0`)),
    value = dplyr::case_when(ano < 2018 ~ (`RO1.1.1.2.02.00.00`+`RO1.1.1.2.08.00.00`+`RO1.1.1.3.05.00.00`)/`TotalReceitas`*100,
                              ano < 2022 ~ (`RO1.1.1.8.01.1.0`+`RO1.1.1.8.01.4.0`+`RO1.1.1.8.02.3.0`)/`TotalReceitas`*100,
                              ano >= 2022 ~ (`RO1.1.1.2.50.0.0`+`RO1.1.1.2.53.0.0`+`RO1.1.1.4.51.1.0`)/`TotalReceitas`*100)) |>
  dplyr::mutate(variavel = "governativas4",
         value = ifelse(!is.finite(value), NA, value)) |>
  dplyr::ungroup()

return(ifsm_teste)
}
#siconfi_rec(53)
#mv mun_53_2014_2024_annex_dca_i_c.csv mun_5300108_2014_2024_annex_dca_i_c.csv

jabaixados <- as.numeric(gsub("mun_(\\d+)_.*","\\1",list.files("coleta/cache/governativas4_aedi/",pattern="*.csv")))
jabaixados <- sort(jabaixados)

ifsm_e_rec_propria <- lapply(jabaixados,
                             calcula_ifsm)
ifsm_e_rec_propria <-
  data.table::rbindlist(
    ifsm_e_rec_propria,fill=TRUE)


saveRDS(ifsm_e_rec_propria,'coleta/cache/governativas4_aedi/ifsm_receita-propria.rds')

















##Resolvido adicionando colunas zeradas quando necessário:

# comproblemascr <- c(1200328,1200435,1300029,1300060,
#                     1300102,1300201,1300409,1301159,
#                     1301506,1302108,1302207,1302306,
#                     1303700,1304005,1304237,1304401,
#                     1400027,1400050,1400233,1400282,
#                     1400407,1400506,1400605,1400704,
#                     1500701,1500958,1501006,1501105,
#                     1501253,1501576,1501600,1501956,
#                     1502905,1503101,1503408,1504109,
#                     1504505,1504901,1504950,1505007,
#                     1506112,1506302,1506401,1506906,
#                     1507102,1507409,1507466,1507474,
#                     1600105,1600204,1600212,1600238,
#                     1600253,1600402,1600501,1600709,
#                     1600808,1703883,1705557,1707405,
#                     1709807,1710706,1711951,1715259,
#                     1718709,1720200,1720309,1721109,
#                     2101731,2102606,2103158,2105104,
#                     2105658,2106003,2107209,2108454,
#                     2109304,2110609,2111789,2200954,
#                     2201051,2201919,2202059,2202117,
#                     2202539,2202737,
#
#                     2203230,2203750,2205599,2206654,
#                     2207777,2208106,2209658,2210383,
#                     2210953,2400901,2401651,2402907,
#                     2404002,2404101,2405108,2405405,
#                     2405504,2405900,2406007,2407906,
#                     2410009,2410256,2410504,2410801,
#                     2411700,2411908,2515203,2515401,
#                     2517407,2605459,2608453,2609154,
#                     2609204,2609808,2702108,2702801,
#                     2703502,2703809,2707305,2708709,
#                     2708907,2800704,
#                     2802007,2803906,2900504,
#                     2924207,2924405,2924678,2929354,
#                     2929750,
#                     3144656,3146255,3301207,3305703,
#                     3512605,4112702,4123600,
#                     4201653,4301701,4308052,4312203,
#                     4320206,5107800,5204607,5209408,
#                     5209457,5213772,5222302,5300108,
#
#
#                     2305209,2308203,2903003,2903235,
#                     2903706,2905156,2908200,2911253,
#                     2919009,2919900
#
# )

