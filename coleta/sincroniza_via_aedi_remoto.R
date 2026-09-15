# Sincroniza os indicadores _via_aedi locais para os mdata antigos do remoto
# (ex.: objetivo1_1_via_aedi local -> objetivo1_1 remoto)
suppressMessages(library(DBI))

loc <- dbConnect(RPostgres::Postgres(),
                 user = "aedi", password = "aEd1#man@gR",
                 host = "127.0.0.1", dbname = "aedidb")
rem <- dbConnect(RPostgres::Postgres(),
                 user = "usr_cggi_admin",
                 password = Sys.getenv("passwddbdev"),
                 host = "10.214.50.169", dbname = "painelpndr")

# pares: nome local (_via_aedi) -> nome remoto (antigo)
pares <- list(
  c("objetivo1_1_via_aedi", "objetivo1_1"),
  c("objetivo1_2_via_aedi", "objetivo1_2"),
  c("objetivo1_3_via_aedi", "objetivo1_3"),
  c("objetivo2_2_via_aedi", "objetivo2_2"),
  c("objetivo2_3_via_aedi", "objetivo2_3"),
  c("objetivo3_1_via_aedi", "objetivo3_1"),
  c("objetivo3_2_via_aedi", "objetivo3_2"),
  c("objetivo3_3_via_aedi", "objetivo3_3")
)

for (p in pares) {
  local_name <- p[1]; rem_name <- p[2]
  lid <- dbGetQuery(loc, "SELECT mdata_id FROM mdata WHERE orig_name=$1",
                    params = list(local_name))$mdata_id
  rid <- dbGetQuery(rem, "SELECT mdata_id FROM mdata WHERE orig_name=$1",
                    params = list(rem_name))$mdata_id
  if (length(lid) == 0 || length(rid) == 0) { cat(local_name, "-> SKIP\n"); next }

  dados <- dbGetQuery(loc,
    "SELECT local_id, refdate, value FROM data_values WHERE mdata_id=$1",
    params = list(lid))
  if (nrow(dados) == 0) { cat(local_name, "-> sem dados\n"); next }

  max_rem <- dbGetQuery(rem,
    "SELECT max(refdate) FROM data_values WHERE mdata_id=$1",
    params = list(rid))[[1]]
  if (!is.na(max_rem) && max_rem >= max(dados$refdate)) {
    cat(sprintf("%-28s -> %-20s remoto ja em %s — skip\n",
                local_name, rem_name, format(max_rem)))
    next
  }

  dbBegin(rem)
  tryCatch({
    dbExecute(rem, "DELETE FROM data_values WHERE mdata_id=$1", params = list(rid))
    dbAppendTable(rem, "data_values",
      data.frame(mdata_id = rid, local_id = dados$local_id,
                 refdate = dados$refdate, value = dados$value))
    dbExecute(rem,
      "UPDATE mdata_timetable SET last_refdate=$2, last_update=current_date
        WHERE mdata_id=$1", params = list(rid, max(dados$refdate)))
    dbCommit(rem)
    cat(sprintf("%-28s -> %-20s %d pontos -> %s\n",
                local_name, rem_name, nrow(dados), format(max(dados$refdate))))
  }, error = function(e) {
    try(dbRollback(rem), silent = TRUE)
    cat(sprintf("%-28s ERRO: %s\n", local_name, substr(conditionMessage(e), 1, 50)))
  })
}

# refresh das matviews objetivo*
for (obj in 1:4) {
  for (ano in 2013:2025) {
    mv <- sprintf("objetivo%d_%d", obj, ano)
    try(dbExecute(rem, sprintf("REFRESH MATERIALIZED VIEW %s", mv)), silent = TRUE)
  }
}
dbExecute(rem, "REFRESH MATERIALIZED VIEW valoresmeta")
dbExecute(rem, "REFRESH MATERIALIZED VIEW named_datavalues")

cat("\nVerificação objetivo1_2025:\n")
print(dbGetQuery(rem, "SELECT count(*) FILTER (WHERE objetivo1_1 IS NOT NULL) ind1,
       count(*) FILTER (WHERE objetivo1_2 IS NOT NULL) ind2,
       count(*) FILTER (WHERE objetivo1_3 IS NOT NULL) ind3,
       count(*) FILTER (WHERE comp_objetivo1 IS NOT NULL) comp
  FROM objetivo1_2025"))

dbDisconnect(loc); dbDisconnect(rem)
cat("FIM\n")
