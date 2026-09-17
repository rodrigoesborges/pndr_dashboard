#desprod1 - ICE-R municipal (complexidade economica regional) pela metodologia
#do TD 657 (Freitas, Romero, Britto, Stein & Torres 2023, CEDEPLAR/UFMG)
#
# Recalcula o desprod1 direto da RAIS crua (banco mte_rais): massa salarial
# (remuneracao nominal de dezembro dos vinculos ativos em 31/12) agregada no
# SQL em municipio x CNAE 2.0 classe, QL/VCR de Balassa (eq 1), matriz binaria
# M (eq 2) e metodo dos reflexos espectral (Hidalgo et al 2009; eqs 3-7b do
# TD): ICE-R = z-score do 2o autovetor. Validado contra a Tabela 1 do TD em
# 2021 (top-10 na MESMA ordem, Manaus 3.329) e contra a serie do DW (cor
# ~0.999; a cor historica contra o ECI do CEDEPLAR era 0.9992).
#
# ICE-R e z-score DENTRO de cada ano: valores NAO sao comparaveis entre anos,
# apenas posicoes no ranking (alerta explicito da secao 3 do TD).
#
# Cache por ano em coleta/cache/desprod1_icer/ (a extracao agrega tabelas de
# ~70M vinculos, ~1-2 min/ano): com cache completo o script recalcula sem
# tocar na VPS. Para reextrair, apague os CSV do cache. Credenciais RAIS nas
# env vars mte_rais/pwdrais/hostraispsql (.Renviron).

if (file.exists(".Renviron")) readRenviron(".Renviron")
# fallback: credenciais RAIS vivem no .Renviron do AEDi (o do pndr_dashboard
# nao as tem); o mte_rais canonico e o LOCAL (roadmap_rais)
if (!nzchar(Sys.getenv("pwdrais")) &&
    file.exists("/home/wlvdbaj/pRojetos/AEDi/.Renviron")) {
  readRenviron("/home/wlvdbaj/pRojetos/AEDi/.Renviron")
  Sys.setenv(hostraispsql = "127.0.0.1")
}

# autocontencao (padrao A5b): conexoes e objetos de sessao. A conexao `rais`
# so e aberta se faltar cache de algum ano (ver .abre_rais abaixo).
if (!exists("con") || !inherits(con, "DBIConnection")) con <- DBI::dbConnect(RPostgres::Postgres(),
                                                                             user=Sys.getenv("user","aedi"), password=Sys.getenv("password","aEd1#man@gR"),
                                                                             host=Sys.getenv("host","127.0.0.1"), dbname=Sys.getenv("dbname","aedidb"))
if (!exists("mdr") || !inherits(mdr, "DBIConnection")) mdr <- con
if (!exists("locgeoloc") || !is.data.frame(locgeoloc)) locgeoloc <- DBI::dbGetQuery(con,
                                                                                    "select local_id, local_name, geoloc_id from local")

.rais_nossa <- FALSE
.abre_rais <- function() {
  if (!exists("rais") || !inherits(rais, "DBIConnection")) {
    rais <<- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                            dbname=Sys.getenv("mte_rais"), user="mte_rais",
                            password=Sys.getenv("pwdrais"),
                            host=Sys.getenv("hostraispsql", "127.0.0.1"))
    .rais_nossa <<- TRUE
  }
  rais
}

pega_massa_mun_cnae <- \(ano) {
  # cubo W_rs do TD 657 (secao 3). Municipio nesta base: IBGE de 6 digitos
  # SEM digito verificador (110001-530010; 999999="ignorado"); CNAE classe
  # sem o zero a esquerda (a01113 = 1113).
  arq <- sprintf("coleta/cache/desprod1_icer/massa_mun_cnae_%d.csv", as.integer(ano))
  if (file.exists(arq)) return(readr::read_csv(arq, show_col_types = FALSE))
  a <- DBI::dbGetQuery(.abre_rais(), sprintf(
    "SELECT municipio local, cnae_2_0_classe setor, SUM(vl_remun_dezembro_nom) massa
       FROM rais_vinculo_%d
      WHERE vinculo_ativo_31_12 = 1
        AND vl_remun_dezembro_nom > 0
        AND municipio BETWEEN 110001 AND 599999
        AND cnae_2_0_classe BETWEEN 1113 AND 99097
      GROUP BY 1, 2", as.integer(ano)))
  dir.create("coleta/cache/desprod1_icer", recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(a, arq)
  a
}

# ---- nucleo metodologico (equacoes 1-7b do TD 657; repo de origem com suite
# ---- de testes sinteticos: cada equacao reproduz conta de mao do TD)

# eqs 7b/8: ICE-R/ICA-R sao autovetores padronizados como z-score anual
padronizar <- \(x) (x - mean(x)) / sd(x)

# eq 1: quociente locacional de Balassa (com massa salarial, VCR == QL):
# participacao do setor na regiao dividida pela participacao do setor no pais
calcular_ql <- \(d) {
  tot_r <- tapply(d$massa, d$local, sum)
  tot_s <- tapply(d$massa, d$setor, sum)
  W <- sum(d$massa)
  d$ql <- (d$massa / tot_r[as.character(d$local)]) /
    (tot_s[as.character(d$setor)] / W)
  d
}

# eq 2: M_rs = 1 se QL_rs >= 1; regiao/setor sem VCR alguma sai fora
# (k = 0 quebraria as divisoes das eqs 5-7)
matriz_m <- \(d_ql) {
  r <- unique(as.character(d_ql$local))
  s <- unique(as.character(d_ql$setor))
  M <- matrix(0L, length(r), length(s), dimnames = list(r, s))
  M[cbind(as.character(d_ql$local), as.character(d_ql$setor))] <-
    as.integer(d_ql$ql >= 1)
  M[rowSums(M) > 0, colSums(M) > 0, drop = FALSE]
}

# eqs 3-7b: diversificacao (k_r0), ubiquidade (k_s0) e reflexos espectrais.
# A matriz dos reflexos dos SETORES (670x670, ponta barata) entrega o 2o
# autovetor; o das regioes (5570) sai pela identidade AB/BA sem eig 5570^2.
icer_ano <- \(d) {
  M <- matriz_m(calcular_ql(d))
  k_r0 <- rowSums(M)
  k_s0 <- colSums(M)
  Ms <- sweep(crossprod(M, M / k_r0), 1, k_s0, "/")
  ev <- eigen(Ms, symmetric = FALSE)
  idx <- order(Re(ev$values), decreasing = TRUE)  # 1o autovalor = 1 (Perron, trivial)
  S2 <- Re(ev$vectors[, idx[2]])
  R2 <- (M %*% S2) / k_r0
  # sinal do autovetor e arbitrario: fixa-se o que correlaciona ICE-R
  # positivamente com a diversificacao k_r0, como na prosa do TD
  cr <- suppressWarnings(cor(R2, k_r0))
  if (is.finite(cr) && cr < 0) { S2 <- -S2; R2 <- -R2 }
  data.frame(local = rownames(M), valor = padronizar(R2))
}

# ---- calculo ano a ano

# anos do cache; se vazio, descobre no banco (CNAE 2.0 classe integral >= 2013)
anos <- sort(as.integer(gsub("\\D", "",
                             list.files("coleta/cache/desprod1_icer", "^massa_mun_cnae"))))
if (!length(anos)) {
  aa <- AEDi:::anos_rais(.abre_rais())
  anos <- sort(aa[aa >= 2013])
}

desprod1_icer <- data.table::rbindlist(lapply(anos, \(ano) {
  df <- icer_ano(pega_massa_mun_cnae(ano))
  # z-score anual (media 0, dp 1) e invariante ALGEBRICA da eq 7b: falhar
  # aqui e bug de pipeline, nao variacao dos dados
  stopifnot(abs(mean(df$valor)) < 1e-8, abs(sd(df$valor) - 1) < 1e-8)
  if (abs(nrow(df) - 5570) > 100)
    warning("desprod1_icer: ano ", ano, " com ", nrow(df),
            " municipios (esperado ~5570)")
  message("desprod1_icer: ano ", ano, " - ", nrow(df), " municipios ok")
  df$ano <- as.integer(ano)
  df
}))

# Tabela 1 do TD 657 (2021): top-10 = Manaus, Paulinia, Barueri, Diadema, SP,
# Guarulhos, S. Bernardo, Rio, Araucaria, Curitiba (codigos de 6 digitos)
if (2021 %in% anos) {
  s21 <- desprod1_icer[desprod1_icer$ano == 2021, ]
  ordem <- as.numeric(s21$local[order(-s21$valor)])
  td_top10 <- c(130260,353650,350570,351380,355030,351880,354870,330455,410180,410690)
  message("desprod1_icer 2021: sobreposicao top-10 com a Tabela 1 do TD = ",
          sum(td_top10 %in% head(ordem, 10)), "/10; Manaus em ",
          match(130260, ordem), "o lugar")
}

readr::write_csv(desprod1_icer, "coleta/cache/desprod1_icer/desprod1_icer.csv")

# serie antiga capturada ANTES do replace: mdr aponta para o MESMO DW local
# onde gravar_serie_dw escreve, e depois dele a consulta devolveria a serie
# nova (cor trivialmente 1)
desprod1_orig <- DBI::dbGetQuery(mdr,"select refdate, trunc(c.geoloc_id/10) codmun, value from data_values a left join mdata b on a.mdata_id = b.mdata_id left join local c on a.local_id = c.local_id where orig_name like 'desprod1%'")

# serie no DW (recalculo completo, padrao A5b; gravar_serie_dw mapeia sozinho
# o IBGE de 6 digitos da RAIS para o geoloc_id de 7 digitos do DW)
AEDi:::gravar_serie_dw("desprod1",
                       data.frame(local = desprod1_icer$local,
                                  periodo = as.Date(paste0(desprod1_icer$ano, "-12-31")),
                                  valor = desprod1_icer$valor))
if (.rais_nossa) DBI::dbDisconnect(rais)

#Conferencia: contra a serie antiga do desprod1 no DW (ECI CEDEPLAR/DataViva)
desprod1_compara <- desprod1_orig|>
  dplyr::mutate(codmun=as.numeric(codmun))|>
  dplyr::inner_join(desprod1_icer|>
                      dplyr::transmute(codmun=as.numeric(local),
                                       refdate=as.Date(paste0(ano,"-12-31")), icer=valor),
                    by=c("codmun","refdate"))
cor(desprod1_compara$value,desprod1_compara$icer,use="complete.obs")
#0.999... (cor historica contra o ECI do CEDEPLAR: 0.9992)
