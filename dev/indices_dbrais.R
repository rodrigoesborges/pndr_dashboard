# DEPRECATED (2026-08-25): use raisqlr::rais_create_indexes(years) — mesmo
# conjunto de indices, IF NOT EXISTS e opcao CONCURRENTLY. Este script
# historico contem os loops 2013:2023 originais.
### Cria indexes nas tabelas de vínculos do banco postgresql da RAIS criado

userais="mte_rais"
passwordrais="aEd1#man@gRpublicrais"
hostprais="38.242.154.34"

con <-
  DBI::dbConnect(  RPostgreSQL::PostgreSQL(), dbname=unique( catalog[ , "dbfile" ] )[[1]], user=userais,password=passwordrais,
              host=hostprais,port=5432     )

geraindice <- \(x){
  bnm <- paste0("idx_rais_vinculo_",x,"_municipio")
  DBI::dbExecute(con,
  paste0(
    "DROP INDEX IF EXISTS ",bnm,";\n",
    "CREATE INDEX ",bnm,
    " ON rais_vinculo_",
    x,
    " (vinculo_ativo_31_12,municipio)"
  ))
}

lapply(2013:2023,geraindice)

geraindice_c <- \(x){
  bnm <- paste0("idx_rais_vinculo_",x,"_municipio_cnae")
  DBI::dbExecute(con,
                 paste0(
                   "DROP INDEX IF EXISTS ",bnm
                 ))
  DBI::dbExecute(con,
                 paste0(
                   "CREATE INDEX CONCURRENTLY ",bnm,
                   " ON rais_vinculo_",
                   x,
                   " (vinculo_ativo_31_12,municipio,cnae_2_0_classe);"
                 ))
}

lapply(2016:2023,geraindice_c)

geraindice_escolaridade <- \(x){
  bnm <- paste0('idx_rais_vinculo_',x,"_municipio_escolaridade")
  DBI::dbExecute(con,paste0(
    "DROP INDEX IF EXISTS ",bnm
  ))
  DBI::dbExecute(con,
                 paste0(
                   "CREATE INDEX  CONCURRENTLY ",bnm,
                   " ON rais_vinculo_",x,
                   " (vinculo_ativo_31_12,municipio,escolaridade_apos_2005);"
                 ))
}

lapply(2013:2023,geraindice_escolaridade)


geraindice_cbo <- \(x){
  DBI::dbExecute(con,
                 paste0(
                   "CREATE INDEX idx_rais_vinculo_",
                   x,"_municipio_cbo ON rais_vinculo_",
                   x,
                   " (vinculo_ativo_31_12,municipio,TRUNC(cbo_ocupacao_2002 /1000))"
                 ))
}

lapply(c(2014:2023),geraindice_cbo)

geraindice_cbog <- \(x){
  DBI::dbExecute(con,
                 paste0(
                   "CREATE INDEX idx_rais_vinculo_",
                   x,"_municipio_cbog ON rais_vinculo_",
                   x,
                   " (vinculo_ativo_31_12,municipio,cbo_ocupacao_2002)"
                 ))
}

lapply(c(2013:2023),geraindice_cbog)



geraindice_estab <- \(x){
  DBI::dbExecute(con,
                 paste0(
                   "CREATE INDEX idx_rais_estabelecimento_",
                   x,"_municipio ON rais_estabelecimento_",
                   x,
                   " (municipio)"
                 ))
}

lapply(2014:2023,geraindice_estab)

geraindice_estab_c <- \(x){
  DBI::dbExecute(con,
                 paste0(
                   "CREATE INDEX idx_rais_estabelecimento_",
                   x,"_municipio_cnae ON rais_estabelecimento_",
                   x,
                   " (municipio,cnae_2_0_classe)"
                 ))
}

lapply(c(2014:2023),geraindice_estab_c)

