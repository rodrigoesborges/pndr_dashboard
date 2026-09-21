# pequenas_empresas_biotecsaude_mun (101) e citec1_aedi (102):
# estabelecimentos de pequeno porte (tamanho < 6) em setores de biotecnologia
# e saude (CNAE 211,212,266,325) por municipio; citec1 = 1e6*estab/pop.
# Fonte direta mte_rais (rais_estabelecimento_). Padrao A5b.

if (!exists("rais") || !inherits(rais, "DBIConnection"))
  rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = Sys.getenv("mte_rais"), user = "mte_rais",
                         password = Sys.getenv("pwdrais"),
                         host = Sys.getenv("hostraispsql"))

anos_estab <- sort(as.numeric(gsub("\\D", "", grep("^rais_estabelecimento_[0-9]+$",
  DBI::dbGetQuery(rais, "SELECT table_name FROM information_schema.tables
                   WHERE table_schema='public' AND table_name ~ '^rais_estabelecimento_[0-9]+$'")$table_name,
  value = TRUE))))

peq <- data.table::rbindlist(lapply(anos_estab, \(x) {
  # colunas e granularidade variam por era (raisqlr resolve por ano):
  # CNAE 2.0 grupos 211,212,266,325 equivalem aos grupos 233,245,331 do 95
  col20 <- raisqlr::rais_coluna(x, "estabelecimento", "cnae_2_0")
  porte <- raisqlr::rais_coluna(x, "estabelecimento", "porte")
  # tamestab antigo tem 0 = sem informacao: restringe a 1-5
  cond_porte <- if (porte == "tamestab") "BETWEEN 1 AND 5" else "< 6"
  filtro <- if (is.na(col20)) {
    paste0("AND trunc(", raisqlr::rais_coluna(x, "estabelecimento", "cnae_95"), "/100) IN (",
           paste(raisqlr::cnae_equivalentes(c(211, 212, 266, 325), "2.0", "1.0",
                                            nivel = "grupo"), collapse = ","), ")")
  } else {
    paste0("AND trunc(", col20, "/", raisqlr::rais_divisor(x, "estabelecimento", "grupo"),
           ") IN (211,212,266,325)")
  }
  p <- DBI::dbGetQuery(rais,
    paste0("SELECT municipio, COUNT(*) estabelecimento FROM rais_estabelecimento_",
           x, " WHERE ", porte, " ", cond_porte, " ", filtro,
           " GROUP BY municipio"))
  p$ano <- x
  p
}), fill = TRUE)
readr::write_csv(peq, "coleta/cache/pequenas_empresas_biotecsaude_mun/pequenas_empresas_biotecsaude_mun.csv")
DBI::dbDisconnect(rais)

AEDi:::gravar_serie_dw("pequenas_empresas_biotecsaude_mun",
  data.frame(local = peq$municipio,
             periodo = as.Date(paste0(peq$ano, "-12-31")),
             valor = peq$estabelecimento))

# citec1_aedi: per capita (popmun de julho do mesmo ano -> refdate 31/12)
con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))
pop <- DBI::dbGetQuery(con, "SELECT trunc(l.geoloc_id/10) codmun,
        extract(year from d.refdate)::int ano, d.value pop
  FROM data_values d JOIN mdata m USING (mdata_id) JOIN local l USING (local_id)
 WHERE m.orig_name = 'datasus_popmun'")
DBI::dbDisconnect(con)

c1 <- peq |>
  dplyr::left_join(pop, by = c("municipio" = "codmun", "ano" = "ano")) |>
  dplyr::filter(!is.na(pop), pop > 0) |>
  dplyr::transmute(local = municipio,
                   periodo = as.Date(paste0(ano, "-12-31")),
                   citec1_aedi = 1e6 * estabelecimento / pop)

AEDi:::gravar_serie_dw("citec1_aedi",
  data.frame(local = c1$local, periodo = c1$periodo, valor = c1$citec1_aedi))
