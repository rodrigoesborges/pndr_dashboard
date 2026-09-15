# governativas4 (34) 2025: IFSM = arrecadacao propria / TotalReceitas * 100
# (adaptacao do governativas4_corrige.R). Para ano >= 2022:
#   propria = RO1.1.1.2.50.0.0 + RO1.1.1.2.53.0.0 + RO1.1.1.4.51.1.0
# Append do refdate 2025-12-31 sobre a serie 2014-2024 ja no DW.

suppressMessages({library(data.table); library(dplyr); library(tidyr); library(janitor)})

dir <- "coleta/cache/governativas4_aedi"
fs25 <- list.files(dir, pattern = "mun_.*_2025_annex_dca_i_c\\.csv$", full.names = TRUE)
cat("arquivos 2025:", length(fs25), "\n")

contas <- c("RO1.1.1.2.50.0.0", "RO1.1.1.2.53.0.0", "RO1.1.1.4.51.1.0", "TotalReceitas")

calc_2025 <- \(f) {
  d <- tryCatch(fread(f), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d <- d |> clean_names()
  d <- d[cod_conta %in% contas & coluna == "Receitas Brutas Realizadas",
         .(ano = exercicio, codmun = as.numeric(cod_ibge), cod_conta, valor)]
  if (nrow(d) == 0) return(NULL)
  d <- d |>
    group_by(codmun, ano) |>
    pivot_wider(names_from = cod_conta, values_from = valor, values_fill = 0)
  for (c in contas) if (!(c %in% names(d))) d[[c]] <- 0
  d |>
    mutate(
      propia = (`RO1.1.1.2.50.0.0` + `RO1.1.1.2.53.0.0` + `RO1.1.1.4.51.1.0`),
      valor = propia / TotalReceitas * 100,
      valor = ifelse(!is.finite(valor) | TotalReceitas == 0, NA, valor)) |>
    transmute(codmun, ano, value = valor, variavel = "governativas4") |>
    ungroup()
}

tudo <- rbindlist(lapply(fs25, calc_2025), fill = TRUE)
tudo <- tudo[!is.na(codmun)]
cat("governativas4 2025:", nrow(tudo[!is.na(value)]), "municipios com valor |",
    "media:", round(mean(tudo$value, na.rm = TRUE), 2), "\n")
cat("NA (sem dados/total=0):", sum(is.na(tudo$value)), "\n")

# carregar 2025 adicional
pkgload::load_all("/home/wlvdbaj/pRojetos/AEDi", export_all = FALSE)
Sys.setenv(user = "aedi", password = "aEd1#man@gR", host = "127.0.0.1", dbname = "aedidb")

AEDi::gravar_serie_dw("governativas4",
  data.frame(local = tudo$codmun,
             periodo = as.Date("2025-12-31"),
             valor = tudo$value),
  modo = "append")
