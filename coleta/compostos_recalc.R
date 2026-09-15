# Compostos municipais (comp_educ, comp_citec, comp_infra, comp_dessoc,
# comp_sust, comp_governativas, comp_objetivo1-4).
# Recalculo completo (replace). Padrao A5b. Exclui desprod e pnadc.

suppressMessages({library(dplyr); library(readr); library(lubridate)})

con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))

sentido <- read_csv(
  "/home/wlvdbaj/pRojetos/AEDi/inst/extdata/indicadores_sentido.csv",
  show_col_types = FALSE
) |>
  mutate(orig_name = ifelse(grepl("obj", orig_name),
                            paste0(orig_name, "_", n_indicador),
                            paste0(orig_name, n_indicador)))

grupos <- list(
  comp_educ         = c("educ1", "educ2", "educ3", "educ4"),
  comp_citec        = c("citec1", "citec2", "citec3", "citec4"),
  comp_infra        = c("infra1", "infra2", "infra3", "infra4"),
  comp_dessoc       = c("dessoc1", "dessoc2", "dessoc3", "dessoc4"),
  comp_sust         = c("sust1", "sust2", "sust3", "sust4"),
  comp_governativas = c("governativas1", "governativas2", "governativas3", "governativas4"),
  comp_objetivo1    = c("objetivo1_1", "objetivo1_2", "objetivo1_3"),
  comp_objetivo2    = c("objetivo2_1", "objetivo2_2", "objetivo2_3"),
  comp_objetivo3    = c("objetivo3_1", "objetivo3_2", "objetivo3_3"),
  comp_objetivo4    = c("objetivo4_1", "objetivo4_2", "objetivo4_3")
)

for (nome_comp in names(grupos)) {
  inds <- grupos[[nome_comp]]
  sent <- sentido |> filter(orig_name %in% inds) |> select(orig_name, maior_melhor)

  sql <- sprintf(
    "SELECT d.refdate, d.local_id, m.orig_name, d.value
       FROM data_values d JOIN mdata m USING (mdata_id)
      WHERE m.orig_name IN ('%s')", paste(inds, collapse = "','"))
  base <- DBI::dbGetQuery(con, sql)

  base <- base |>
    inner_join(sent, by = "orig_name") |>
    group_by(refdate, orig_name) |>
    mutate(rank_val = rank(value * maior_melhor, ties.method = "min", na.last = TRUE)) |>
    ungroup()

  # carry-forward ate o ano maximo global
  max_ano <- max(year(base$refdate))
  extras <- list()
  for (ind in unique(base$orig_name)) {
    sub <- base[base$orig_name == ind, ]
    ultimo_ano <- max(year(sub$refdate))
    if (ultimo_ano < max_ano) {
      ultimo <- sub[year(sub$refdate) == ultimo_ano, ]
      for (a in (ultimo_ano + 1):max_ano) {
        extras[[length(extras) + 1]] <- ultimo |>
          mutate(refdate = as.Date(paste0(a, "-12-31")))
      }
    }
  }
  if (length(extras) > 0) base <- bind_rows(base, bind_rows(extras))

  composto <- base |>
    group_by(refdate, local_id) |>
    summarise(indsmedia = mean(rank_val), .groups = "drop") |>
    group_by(refdate) |>
    mutate(value = (indsmedia - min(indsmedia)) / (max(indsmedia) - min(indsmedia))) |>
    ungroup() |>
    select(local_id, refdate, value)

  cat(nome_comp, ":", nrow(composto), "pontos | anos:",
      paste(range(year(composto$refdate)), collapse = "-"), "\n")

  AEDi::gravar_serie_dw(nome_comp,
    data.frame(local = composto$local_id,
               periodo = composto$refdate,
               valor = composto$value))
}
DBI::dbDisconnect(con)
