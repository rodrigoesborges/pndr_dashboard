# C0 do roadmap_compostos_2025.md: cria mdata proprios _v0 com a versao
# ORIGINAL (calculada fora da estrutura AEDi) dos compostos pendentes,
# ANTES de qualquer replace do ciclo 2025:
#   comp_objetivo1_v0 (104) <- mdata 62 do REMOTO (v0 so existe la)
#   comp_objetivo3_v0 (105) <- mdata 64 do REMOTO (idem)
#   comp_desprod_v0   (106) <- mdata 15 do LOCAL  (excluido do A5b, v0 local)
# Nao toca nos mdata originais (15/62/64) — sao eles que as matviews
# referenciam. Metadados copiados dos originais (mdata_exts; grupo 3 apenas
# para o desprod, como no original).

suppressMessages(library(DBI))

loc <- dbConnect(RPostgres::Postgres(),
                 user = "aedi", password = "aEd1#man@gR",
                 host = "127.0.0.1", dbname = "aedidb")
rem <- dbConnect(RPostgres::Postgres(),
                 user = "usr_cggi_admin", password = Sys.getenv("passwddbdev"),
                 host = "10.214.50.169", dbname = "painelpndr")

# novo_id, orig_name_v0, data_name_v0, origem (loc/rem), mdata_id_origem, grupo
specs <- list(
  list(104, "comp_objetivo1_v0", "Indicador Composto do Objetivo 1 (v0)",
       "rem", 62, NA),
  list(105, "comp_objetivo3_v0", "Indicador Composto do Objetivo 3 (v0)",
       "rem", 64, NA),
  list(106, "comp_desprod_v0",
       "Índice Composto de Desenvolvimento Produtivo (v0)",
       "loc", 15, 3)
)

DESC_V0 <- paste("v0: serie original calculada fora da estrutura AEDi",
                 "(dadostat), preservada antes do recalculo do ciclo 2025.")

for (s in specs) {
  novo_id <- s[[1]]; nome <- s[[2]]; dname <- s[[3]]
  origem <- s[[4]]; orig_id <- s[[5]]; grupo <- s[[6]]

  ja <- dbGetQuery(loc, "SELECT mdata_id FROM mdata WHERE orig_name=$1",
                   params = list(nome))$mdata_id
  if (length(ja) > 0) { cat(nome, ": ja existe (mdata_id", ja, ") — skip\n"); next }

  con <- if (origem == "rem") rem else loc
  dados <- dbGetQuery(con,
    "SELECT local_id, refdate, value FROM data_values WHERE mdata_id=$1",
    params = list(orig_id))
  stopifnot(nrow(dados) > 0)

  dbBegin(loc)
  tryCatch({
    dbExecute(loc,
      "INSERT INTO mdata (mdata_id, orig_name, data_name, data_desc)
       VALUES ($1,$2,$3,$4)",
      params = list(novo_id, nome, dname, DESC_V0))
    dbExecute(loc,
      "INSERT INTO mdata_exts (mdata_id, data_class_id, data_freq_id,
                               dataunit_num, dataunit_den, data_type_id,
                               datasource_id)
       SELECT $1, data_class_id, data_freq_id, dataunit_num, dataunit_den,
              data_type_id, datasource_id
         FROM mdata_exts WHERE mdata_id=$2",
      params = list(novo_id, orig_id))
    if (!is.na(grupo))
      dbExecute(loc,
        "INSERT INTO mdata_group (mdata_id, datagroup_id) VALUES ($1,$2)",
        params = list(novo_id, grupo))
    dbAppendTable(loc, "data_values",
                  data.frame(mdata_id = novo_id,
                             local_id = dados$local_id,
                             refdate = dados$refdate,
                             value = dados$value))
    dbExecute(loc,
      "INSERT INTO mdata_timetable (mdata_id, last_refdate, last_update)
       VALUES ($1, $2, current_date)",
      params = list(novo_id, max(dados$refdate)))
    dbCommit(loc)
    cat(sprintf("%-20s mdata_id %d <- %s mdata %d: %d pontos (%s a %s)\n",
                nome, novo_id, origem, orig_id, nrow(dados),
                format(min(dados$refdate)), format(max(dados$refdate))))
  }, error = function(e) {
    dbRollback(loc); stop(nome, ": ", conditionMessage(e))
  })
}

cat("\n== verificacao ==\n")
print(dbGetQuery(loc, "
  SELECT m.mdata_id, m.orig_name, count(*) n,
         min(d.refdate)::text mn, max(d.refdate)::text mx,
         round(sum(d.value)::numeric,2) soma
    FROM data_values d JOIN mdata m USING (mdata_id)
   WHERE m.orig_name LIKE 'comp_%_v0' GROUP BY 1,2 ORDER BY 1"), row.names = FALSE)

dbDisconnect(loc); dbDisconnect(rem)
