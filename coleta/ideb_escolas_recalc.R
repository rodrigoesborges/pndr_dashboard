# ideb_media_basico_redep (75), ideb_basico_rede_mediana (76) e educ4 (4):
# recalculo "fino" a partir do arquivo IDEB POR ESCOLA do INEP, no padrao do
# metodo antigo (pipeline mdr/dadostat): media aritmetica simples do IDEB
# observado (1 casa decimal) das escolas publicas do municipio em cada etapa
# (anos iniciais/finais) e, depois, media das duas etapas. Municipio sem IDEB
# escolar em alguma etapa fica sem ponto (mesma regra da serie publicada).
# Restaura a precisao que a serie-base perdeu ao passar a derivar do arquivo
# municipal (1 casa), causa dos zeros por quantizacao no objetivo1_2.
#
# Fonte: download.inep.gov.br/ideb/resultados/divulgacao_anos_{iniciais,finais}_escolas_2023.zip
#   (xlsx por escola, cabecalho na linha 10; arquivo so tem escolas publicas;
#   "-" vira NA; CO_MUNICIPIO 7 digitos -> %/% 10). Cache: coleta/cache/ideb_escolas_recalc/.
# Cobertura: as edicoes 2005-2023 do arquivo, cada uma replicada no refdate do
#   ano seguinte (padrao das series-base). A edicao 2025 (ainda sem arquivo por
#   escola) permanece a derivacao quantizada do municipal (ideb_derivados_recalc.R).
# Orquestrador: ordem alfabetica ja garante ideb_censo_update -> educ4_ideb_dw
#   ->este-> objetivo1_diferenciais_recalc. Se ideb_derivados_recalc.R rodar
#   manualmente (replace quantizado), rodar este script DEPOIS.
# Aviso de vintage: o INEP muda o conjunto de escolas entre edicoes do arquivo,
#   entao os valores 2005-2021 NAO reproduzem exatamente a serie fina historica
#   (cor ~0,97; MAE ~0,10-0,16). Serie fina original preservada em
#   coleta/cache/backup_ideb_escolas_20260922/.

suppressMessages(library(DBI))

edicao <- 2023
anos <- seq(2005, edicao, by = 2)

cache <- file.path("coleta", "cache", "ideb_escolas_recalc")
dir.create(cache, showWarnings = FALSE, recursive = TRUE)

baixar <- function(etapa) {
  xlsx <- file.path(cache, sprintf("divulgacao_anos_%s_escolas_%d.xlsx", etapa, edicao))
  if (file.exists(xlsx)) return(xlsx)
  zipf <- sub("\\.xlsx$", ".zip", xlsx)
  url <- sprintf("https://download.inep.gov.br/ideb/resultados/divulgacao_anos_%s_escolas_%d.zip",
                 etapa, edicao)
  op <- options(download.file.method = "curl",
                download.file.extra = "-k -L --retry 3 --retry-delay 10")
  on.exit(options(op), add = TRUE)
  download.file(url, zipf, mode = "wb")
  utils::unzip(zipf, exdir = cache)
  stopifnot(file.exists(xlsx))
  xlsx
}

# media simples do IDEB observado (1 casa) das escolas publicas, por municipio
# (IBGE 6d) e edicao bienal, para uma etapa do fundamental
media_escolas <- function(etapa) {
  base <- readxl::read_excel(baixar(etapa), skip = 9, col_types = "text",
                             .name_repair = "minimal")
  cols <- grep("^VL_OBSERVADO_[0-9]{4}$", names(base), value = TRUE)
  anosc <- as.integer(sub("^VL_OBSERVADO_", "", cols))
  cols <- cols[anosc %in% anos]
  anosc <- anosc[anosc %in% anos]
  stopifnot(length(cols) == length(anos))
  codmun <- suppressWarnings(as.numeric(base[["CO_MUNICIPIO"]])) %/% 10
  do.call(rbind, lapply(seq_along(cols), function(i) {
    v <- suppressWarnings(as.numeric(base[[cols[i]]]))
    ok <- !is.na(v) & !is.na(codmun)
    agg <- tapply(v[ok], codmun[ok], mean)
    data.frame(codmun6 = as.numeric(names(agg)), ano = anosc[i],
               valor = as.numeric(agg))
  }))
}

ini <- media_escolas("iniciais")
fin <- media_escolas("finais")
names(ini)[3] <- "ai"; names(fin)[3] <- "af"

mb <- merge(ini, fin, by = c("codmun6", "ano"))
mb$valor <- (mb$ai + mb$af) / 2
mb <- mb[!is.na(mb$valor), ]
stopifnot(nrow(mb) > 40000)
cat("pontos bienais (munis com as duas etapas):", nrow(mb), "\n")
print(table(mb$ano))

# replica cada edicao no refdate do ano seguinte (padrao das series-base)
serie75 <- do.call(rbind, lapply(split(mb, mb$ano), function(v) {
  a <- v$ano[1]
  rbind(
    data.frame(local = v$codmun6, periodo = as.Date(sprintf("%d-12-31", a)), valor = v$valor),
    data.frame(local = v$codmun6, periodo = as.Date(sprintf("%d-12-31", a + 1)), valor = v$valor))
}))

AEDi:::gravar_serie_dw("ideb_media_basico_redep", serie75, modo = "append")

# mediana nacional por refdate da media basico (semantica do builder),
# recalculada sobre a serie ja fina; 2025 fica com a mediana quantizada
con <- dbConnect(RPostgres::Postgres(),
                 user = Sys.getenv("user", "aedi"),
                 password = Sys.getenv("password", "aEd1#man@gR"),
                 host = Sys.getenv("host", "127.0.0.1"),
                 dbname = Sys.getenv("dbname", "aedidb"))
refs <- sort(unique(serie75$periodo))
dbd <- dbGetQuery(con, "SELECT * FROM named_datavalues
                   WHERE orig_name = 'ideb_media_basico_redep'")
med <- dbd |>
  dplyr::filter(refdate %in% refs) |>
  dplyr::group_by(refdate) |>
  dplyr::mutate(ideb_basico_rede_mediana = median(value, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::transmute(local = local_id, periodo = refdate,
                   valor = ideb_basico_rede_mediana) |>
  dplyr::distinct()
dbDisconnect(con)

AEDi:::gravar_serie_dw("ideb_basico_rede_mediana",
  data.frame(local = med$local, periodo = med$periodo, valor = med$valor),
  modo = "append")

# educ4 (indicador do painel): refdates bienais, mesma serie fina
AEDi:::gravar_serie_dw("educ4",
  data.frame(local = mb$codmun6,
             periodo = as.Date(sprintf("%s-12-31", mb$ano)),
             valor = mb$valor),
  modo = "append")

cat("medianas por edicao (fino):\n")
print(aggregate(valor ~ ano, mb, median))
