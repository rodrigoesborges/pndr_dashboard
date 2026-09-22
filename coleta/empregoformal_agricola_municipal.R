# empregoformal_agricola_municipal (mdata 98): vinculos ativos na agricultura
# (CNAE classe < 4000) por municipio. Fonte direta mte_rais. Padrao A5b.

if (!exists("rais") || !inherits(rais, "DBIConnection"))
  rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = Sys.getenv("mte_rais"), user = "mte_rais",
                         password = Sys.getenv("pwdrais"),
                         host = Sys.getenv("hostraispsql"))

pega_agricola <- \(ano) {
  a <- DBI::dbGetQuery(rais,
    paste0("SELECT municipio local, COUNT(*) qtd_vinculos FROM rais_vinculo_",
           ano, " WHERE vinculo_ativo_31_12 = 1 AND cnae_2_0_classe < 4000",
           " GROUP BY municipio"))
  a$ano <- ano
  a
}

# apenas anos com CNAE 2.0 (2000-2002 usam cnae_95_classe e quebrariam o filtro)
anos <- AEDi:::anos_rais(rais)
tem_cnae2 <- as.numeric(gsub("\\D", "", DBI::dbGetQuery(rais, paste(
  "SELECT DISTINCT c.table_name FROM information_schema.columns c",
  "WHERE c.table_schema='public' AND c.column_name='cnae_2_0_classe'",
  "AND c.table_name ~ '^rais_vinculo_[0-9]+$'"))$table_name))
ag <- data.table::rbindlist(lapply(anos[anos %in% tem_cnae2], pega_agricola))
readr::write_csv(ag, "coleta/cache/empregoformal_agricola_municipal/empregoformal_agricola_municipal.csv")

AEDi:::gravar_serie_dw("empregoformal_agricola_municipal",
  data.frame(local = ag$local,
             periodo = as.Date(paste0(ag$ano, "-12-31")),
             valor = ag$qtd_vinculos))
DBI::dbDisconnect(rais)
