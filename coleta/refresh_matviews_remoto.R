# Refresh das materialized views do painelpndr dev (10.214.50.169) +
# criação das views objetivo*_2025 (mesma estrutura das de 2024, mdata_id
# do remoto, ano=2025). Roda direto via psql (mais rápido que DBI).

suppressMessages(library(DBI))

PWD_DEV <- Sys.getenv("passwddbdev")
con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = "usr_cggi_admin", password = PWD_DEV,
                      host = "10.214.50.169", dbname = "painelpndr")

# 1. criar views objetivoN_2025
for (obj in 1:4) {
  mds <- dbGetQuery(con, sprintf(
    "SELECT mdata_id, orig_name FROM mdata
     WHERE orig_name IN ('objetivo%d_1','objetivo%d_2','objetivo%d_3','comp_objetivo%d')
     ORDER BY 2", obj, obj, obj, obj))
  if (nrow(mds) != 4) { cat("obj", obj, ": faltam indicadores\n"); next }

  # obter definicao da view de 2024 e trocar o ano
  def <- dbGetQuery(con,
    "SELECT definition FROM pg_matviews WHERE matviewname = $1",
    params = list(sprintf("objetivo%d_2024", obj)))$definition
  if (length(def) == 0 || is.na(def)) { cat("obj", obj, ": sem view 2024\n"); next }

  def2025 <- gsub("2024", "2025", def)
  # o gsub acima troca 2024 nos dois lugares: no date_part compare e no alias
  # mas precisamos garantir que so o ano muda, nao mdata_ids
  mv <- sprintf("objetivo%d_2025", obj)
  dbExecute(con, sprintf("DROP MATERIALIZED VIEW IF EXISTS %s", mv))
  dbExecute(con, sprintf("CREATE MATERIALIZED VIEW %s AS %s", mv, def2025))
  cat(mv, "criada\n")
}

# 2. refresh de todas as matviews
mvs <- dbGetQuery(con, "SELECT matviewname FROM pg_matviews
                    WHERE schemaname='public' ORDER BY 1")$matviewname
for (mv in mvs) {
  dbExecute(con, sprintf("REFRESH MATERIALIZED VIEW %s", mv))
  cat(mv, "refresh\n")
}

# 3. criar indices nas novas views (se nao existirem)
for (obj in 1:4) {
  mv <- sprintf("objetivo%d_2025", obj)
  dbExecute(con, sprintf(
    "CREATE INDEX IF NOT EXISTS idx_%s_codigo ON %s (codigo_ibge)", mv, mv))
}

dbDisconnect(con)
cat("FIM\n")
