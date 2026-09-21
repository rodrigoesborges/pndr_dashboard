# desprod4 (14): Escala Produtiva = massa salarial (sem adm publica) /
# numero de estabelecimentos (sem adm publica). Fonte mte_rais.
# A massa salarial vem do desprod3 (ja calculada em cache/DW); a contagem
# de estabelecimentos vem de rais_estabelecimento_ com filtro CNAE != adm pub.
# Padrao A5b.

if (!exists("rais") || !inherits(rais, "DBIConnection"))
  rais <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = Sys.getenv("mte_rais"), user = "mte_rais",
                         password = Sys.getenv("pwdrais"),
                         host = Sys.getenv("hostraispsql"))

# massa salarial sem adm publica por municipio-ano (mesma query do desprod3)
# adm publica = divisao 84 (2.0), que equivale a divisao 75 do CNAE 95
massa <- data.table::rbindlist(lapply(AEDi:::anos_rais(rais), \(ano) {
  filtro <- if (is.na(raisqlr::rais_coluna(ano, "vinculo", "cnae_2_0"))) {
    paste0("AND trunc(", raisqlr::rais_coluna(ano, "vinculo", "cnae_95"), "/1000) NOT IN (",
           paste(raisqlr::cnae_equivalentes(84, "2.0", "1.0"), collapse = ","), ")")
  } else {
    paste0("AND trunc(", raisqlr::rais_coluna(ano, "vinculo", "cnae_2_0"), "/",
           raisqlr::rais_divisor(ano, "vinculo", "divisao"), ") != 84")
  }
  a <- DBI::dbGetQuery(rais,
    paste0("SELECT municipio local, SUM(vl_remun_dezembro_nom) massa_salarial
            FROM rais_vinculo_", ano,
           " WHERE vinculo_ativo_31_12 = 1 ", filtro,
           " GROUP BY municipio"))
  a$ano <- ano
  a
}))

# estabelecimentos sem adm publica por municipio-ano
anos_estab <- sort(as.numeric(gsub("\\D", "", grep("^rais_estabelecimento_[0-9]+$",
  DBI::dbGetQuery(rais, "SELECT table_name FROM information_schema.tables
                   WHERE table_schema='public' AND table_name ~ '^rais_estabelecimento_[0-9]+$'")$table_name,
  value = TRUE))))

estab <- data.table::rbindlist(lapply(anos_estab, \(ano) {
  col20 <- raisqlr::rais_coluna(ano, "estabelecimento", "cnae_2_0")
  filtro <- if (is.na(col20)) {
    paste0("trunc(", raisqlr::rais_coluna(ano, "estabelecimento", "cnae_95"), "/1000) NOT IN (",
           paste(raisqlr::cnae_equivalentes(84, "2.0", "1.0"), collapse = ","), ")")
  } else {
    paste0("trunc(", col20, "/",
           raisqlr::rais_divisor(ano, "estabelecimento", "divisao"), ") != 84")
  }
  a <- DBI::dbGetQuery(rais,
    paste0("SELECT municipio local, COUNT(*) qtd_estabelecimentos
            FROM rais_estabelecimento_", ano,
           " WHERE ", filtro,
           " GROUP BY municipio"))
  a$ano <- ano
  a
}))
DBI::dbDisconnect(rais)

readr::write_csv(estab, "coleta/cache/desprod4_aedi/estabelec_sem_admpu_mun.csv")

# desprod4 = massa / estabelecimentos
d4 <- massa |>
  dplyr::inner_join(estab, by = c("local", "ano")) |>
  dplyr::filter(qtd_estabelecimentos > 0) |>
  dplyr::transmute(local,
                   periodo = as.Date(paste0(ano, "-12-31")),
                   desprod4_aedi = massa_salarial / qtd_estabelecimentos)

cat("desprod4:", nrow(d4), "municipios-ano |",
    "anos:", paste(range(as.numeric(format(unique(d4$periodo), "%Y"))), collapse="-"), "\n")

AEDi:::gravar_serie_dw("desprod4",
  data.frame(local = d4$local, periodo = d4$periodo, valor = d4$desprod4_aedi))
