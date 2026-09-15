# Sincroniza os indicadores atualizados do aedidb LOCAL para o painelpndr
# REMOTO (10.214.50.169). Nao cria tabelas/bancos novos: apenas DELETE +
# INSERT nos indicadores que ja existem no remoto. Os _via_aedi e novos
# compostos que so existem localmente sao pulados.

suppressMessages({library(DBI); library(dplyr)})

loc <- dbConnect(RPostgres::Postgres(),
                 user = "aedi", password = "aEd1#man@gR",
                 host = "127.0.0.1", dbname = "aedidb")
rem <- dbConnect(RPostgres::Postgres(),
                 user = "usr_cggi_admin",
                 password = Sys.getenv("passwddbdev"),
                 host = "10.214.50.169", dbname = "painelpndr")

rem_mdata <- dbGetQuery(rem, "SELECT mdata_id, orig_name FROM mdata")
loc_mdata <- dbGetQuery(loc, "SELECT mdata_id, orig_name FROM mdata")

comum <- inner_join(
  rem_mdata |> rename(rem_id = mdata_id),
  loc_mdata |> rename(loc_id = mdata_id),
  by = "orig_name")

cat("indicadores no remoto:", nrow(rem_mdata),
    "| no local:", nrow(loc_mdata),
    "| em comum:", nrow(comum), "\n")

for (i in seq_len(nrow(comum))) {
  ind <- comum$orig_name[i]
  rid <- comum$rem_id[i]
  lid <- comum$loc_id[i]

  dados <- dbGetQuery(loc, "SELECT local_id, refdate, value FROM data_values
                       WHERE mdata_id = $1", params = list(lid))
  if (nrow(dados) == 0) next

  max_loc <- max(dados$refdate)
  max_rem <- dbGetQuery(rem, "SELECT max(refdate) FROM data_values
                        WHERE mdata_id = $1", params = list(rid))[[1]]

  if (!is.na(max_rem) && max_rem >= max_loc) {
    cat(sprintf("%-25s remoto ja em %s (local %s) — skip\n",
                ind, format(max_rem), format(max_loc)))
    next
  }

  dbBegin(rem)
  tryCatch({
    dbExecute(rem, "DELETE FROM data_values WHERE mdata_id = $1",
              params = list(rid))
    dbAppendTable(rem, "data_values",
                  data.frame(mdata_id = rid,
                            local_id = dados$local_id,
                            refdate = dados$refdate,
                            value = dados$value))
    dbExecute(rem,
      "UPDATE mdata_timetable SET last_refdate = $2, last_update = current_date
        WHERE mdata_id = $1", params = list(rid, max_loc))
    dbCommit(rem)
    cat(sprintf("%-25s %d pontos -> %s\n", ind, nrow(dados), format(max_loc)))
  }, error = function(e) {
    try(dbRollback(rem), silent = TRUE)
    cat(sprintf("%-25s ERRO: %s\n", ind, substr(conditionMessage(e), 1, 60)))
  })
}

dbDisconnect(loc); dbDisconnect(rem)
cat("FIM\n")
