# governativas4 (34): IFSM — Indice FIRJAN de Sustentabilidade Municipal
# adaptado ao SICONFI (recalculo exato com colunas zeradas quando necessario).
# Carrega o ifsmparcial.rds ja computado (2014-2024, 5570 municipios).
# Padrao A5b.

ifsm <- readRDS("coleta/cache/governativas4_aedi/ifsmparcial.rds")
cat("governativas4:", nrow(ifsm), "pontos | anos:",
    paste(range(ifsm$ano), collapse="-"),
    "| municipios:", length(unique(ifsm$codmun)), "\n")

AEDi::gravar_serie_dw("governativas4",
  data.frame(local = ifsm$codmun,
             periodo = as.Date(paste0(ifsm$ano, "-12-31")),
             valor = ifsm$value))
