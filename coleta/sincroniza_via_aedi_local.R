# Replica LOCALMENTE o push feito no remoto (sincroniza_via_aedi_remoto.R):
# os dados _via_aedi substituem as series dos builders (mdata antigos),
# deixando o aedidb local igual ao painelpndr (10.214.50.169).
# Antes do DELETE, as series antigas dos builders sao salvas em
# coleta/cache/backup_builders_pre_push_via_aedi/ (seguranca extra; os
# compostos "v0" nao-AEDi NAO sao tocados por este script).

suppressMessages(library(DBI))

loc <- dbConnect(RPostgres::Postgres(),
                 user = "aedi", password = "aEd1#man@gR",
                 host = "127.0.0.1", dbname = "aedidb")

# pares: nome origem (_via_aedi) -> nome destino (builder)
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

bkp_dir <- "coleta/cache/backup_builders_pre_push_via_aedi"
dir.create(bkp_dir, showWarnings = FALSE, recursive = TRUE)

for (p in pares) {
  via <- p[1]; alvo <- p[2]
  vid <- dbGetQuery(loc, "SELECT mdata_id FROM mdata WHERE orig_name=$1",
                    params = list(via))$mdata_id
  aid <- dbGetQuery(loc, "SELECT mdata_id FROM mdata WHERE orig_name=$1",
                    params = list(alvo))$mdata_id
  if (length(vid) == 0 || length(aid) == 0) { cat(via, "-> SKIP\n"); next }

  antigo <- dbGetQuery(loc,
    "SELECT local_id, refdate, value FROM data_values WHERE mdata_id=$1",
    params = list(aid))
  novo <- dbGetQuery(loc,
    "SELECT local_id, refdate, value FROM data_values WHERE mdata_id=$1",
    params = list(vid))
  if (nrow(novo) == 0) { cat(via, "-> sem dados\n"); next }

  saveRDS(antigo, file.path(bkp_dir, paste0(alvo, ".RDS")))

  dbBegin(loc)
  tryCatch({
    dbExecute(loc, "DELETE FROM data_values WHERE mdata_id=$1", params = list(aid))
    dbAppendTable(loc, "data_values",
                  data.frame(mdata_id = aid,
                             local_id = novo$local_id,
                             refdate = novo$refdate,
                             value = novo$value))
    dbExecute(loc,
      "UPDATE mdata_timetable SET last_refdate=$2, last_update=current_date
        WHERE mdata_id=$1", params = list(aid, max(novo$refdate)))
    dbCommit(loc)
    cat(sprintf("%-24s -> %-14s mdata_id %d: %d pontos (%s a %s) | backup RDS ok\n",
                via, alvo, aid, nrow(novo),
                format(min(novo$refdate)), format(max(novo$refdate))))
  }, error = function(e) {
    dbRollback(loc); stop(alvo, ": ", conditionMessage(e))
  })
}
dbDisconnect(loc)
