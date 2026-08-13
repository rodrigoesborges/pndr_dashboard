library(RPostgres)
bdn <- "painelpndr"
ubd <- "usr_cggi_admin"
bdh <- "10.214.50.169"
bds <- "anbhdbregrvdf@2024"

conmdr <- DBI::dbConnect(
  RPostgres::Postgres(),user=ubd,
  password=bds,dbname=bdn,host = bdh)

conrais <- DBI::dbConnect(
  RPostgres::Postgres(),user='mte_rais',
  password='aEd1#man@gRpublicrais',dbname='mte_rais')

rais2022pub <-
  DBI::dbGetQuery(
    conrais,
    "select municipio, cbo_ocupacao_2002, cnae_2_0_classe, escolaridade_apos_2005,tipo_vinculo from
    rais_vinculo_2022 where natureza_juridica < 2000 and vinculo_ativo_31_12 = 1")

rais2023pub <-
  DBI::dbGetQuery(
    conrais,
    "select municipio, cbo_ocupacao_2002, cnae_2_0_classe, escolaridade_apos_2005,tipo_vinculo from
    rais_vinculo_2023 where natureza_juridica < 2000 and vinculo_ativo_31_12 = 1")

totgrupo <- \(tabrais){
  resultado <- tabrais|>
    dplyr::count(municipio,tipo_vinculo %in% c(30,31,35),cbo_ocupacao_2002,cnae_2_0_classe,escolaridade_apos_2005)
  names(resultado)[2] <- 'vinculo_servidor'
  resultado
}

saldopub2322 <- totgrupo(rais2023pub)|>
  dplyr::left_join(totgrupo(rais2022pub),
                   by = c("municipio","vinculo_servidor","cbo_ocupacao_2002","cnae_2_0_classe","escolaridade_apos_2005"))|>
  dplyr::mutate(dplyr::across(dplyr::matches("^n\\."),\(x) {tidyr::replace_na(x,replace=0)}),
                saldo= `n.x`-`n.y`)

rais2022priv <-
  DBI::dbGetQuery(
    conrais,
    "select municipio,tipo_vinculo, cbo_ocupacao_2002, cnae_2_0_classe, escolaridade_apos_2005, count(*) from
    rais_vinculo_2022 where natureza_juridica > 2000 and vinculo_ativo_31_12 = 1
    GROUP BY municipio,tipo_vinculo, cbo_ocupacao_2002, cnae_2_0_classe,escolaridade_apos_2005")

rais2023priv <-
  DBI::dbGetQuery(
    conrais,
    "select municipio,tipo_vinculo, cbo_ocupacao_2002, cnae_2_0_classe, escolaridade_apos_2005, count(*) from
    rais_vinculo_2023 where natureza_juridica > 2000 and vinculo_ativo_31_12 = 1
    GROUP BY municipio,tipo_vinculo, cbo_ocupacao_2002, cnae_2_0_classe,escolaridade_apos_2005")

rais2022priv_nestat <-
  rais2022priv[!(rais2022priv$tipo_vinculo %in% c(30,31,35)),]

rais2023priv_nestat <-
  rais2023priv[!(rais2023priv$tipo_vinculo %in% c(30,31,35)),]

saldopriv2322_nestat <-
  rais2023priv_nestat|>
  dplyr::left_join(rais2022priv_nestat,
                   by = c("municipio","tipo_vinculo",
                          "cbo_ocupacao_2002",
                          "cnae_2_0_classe",
                          "escolaridade_apos_2005"))|>
  dplyr::mutate(
    dplyr::across(dplyr::matches("^count\\."),\(x) {tidyr::replace_na(x,replace=0)}),
    saldo=count.x-count.y)



novocagedprep <- \(arquivo) {
# Fator multiplicador para tratar as exclusões
fatorMultiplicador <- ifelse(grepl("EXC", arquivo), -1, 1)

caged <- arquivo|>
  archive::archive_read()|>
  read.csv2()|>
  data.table::as.data.table()

caged[,`:=` (
  admitidos = (saldomovimentação == 1) * fatorMultiplicador,
  desligados = (saldomovimentação == -1) * fatorMultiplicador,
  saldomovimentação = saldomovimentação * fatorMultiplicador,
  ano = stringr::str_sub(competênciamov,end=4)
)]
caged[,.(ano,município,subclasse,cbo2002ocupação,graudeinstrução,
         admitidos,desligados,saldomovimentação,salário)]
return(caged)
}



cagedj <-
  data.table::rbindlist(
    lapply(
      list.files(path="coleta/cache/novocaged/2023",pattern="*.7z",
                     recursive=TRUE,full.names=TRUE)[7:18],
      novocagedprep),fill=TRUE)


