# RAIS -> PostgreSQL via pacote {raisqlr} (2026-08-25, fase B4 do roadmap_rais)
# Substitui dev/build_dbrais.R, dev/temp_consolida_catalogo_pgsql.R e
# dev/indices_dbrais.R (mantidos apenas como historico).
#
# Ciclo padrao para incorporar um ano novo:
#
#   remotes::install_github("rodrigoesborges/raisqlr")
#   cat   <- raisqlr::rais_catalog(years = 2026)
#   cat   <- raisqlr::rais_download(cat, dir = "coleta/cache/mte_rais_2026",
#                                   workers = 7)   # FTP limita ~60 KB/s/conexao
#   cache <- raisqlr::rais_build(cat, dir = "coleta/cache/mte_rais_2026_cache")
#   raisqlr::rais_pg_load(cache)   # DROP+CREATE+COPY transacional; dbrais/mte_rais/pwdrais/hostraispsql
#   raisqlr::rais_create_indexes(2026)
#   raisqlr::rais_narrow_types()   # smallint/integer nas categoricas
#
# Credenciais: mesmas env vars do AEDi (dbrais, mte_rais, pwdrais, hostraispsql).
# Notas de campo (dialeto COMT 2024+, releases preliminares, hack setor publico):
# ver pndr_coord/roadmap_rais.md e o README do pacote.

# Exemplo executavel (2025 preliminar, ja carregado em ago/2026):
if (FALSE) {
  cat <- raisqlr::rais_catalog(years = 2025)
  print(cat[, c("file", "subtype", "year", "situacao", "size_bytes")])
}
