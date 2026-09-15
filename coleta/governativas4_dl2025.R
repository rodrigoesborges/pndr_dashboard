# Download SICONFI 2025 por municipio (anexo DCA-I-C)
# Salva em coleta/cache/governativas4_aedi/mun_<cod>_2025_annex_dca_i_c.csv
# ~5570 municipios; com pausa de 0.3s = ~28 min

suppressMessages({library(siconfir); library(data.table)})

dir <- "coleta/cache/governativas4_aedi"
codigos <- readRDS(file.path(dir, "ifsmparcial.rds"))$codmun |> unique() |> sort()
ja <- list.files(dir, pattern = "mun_.*_2025_") |>
  gsub("mun_(.*)_2025.*", "\\1", x = _) |> as.numeric()
todo <- setdiff(codigos, ja)
cat("total:", length(codigos), "| já baixados:", length(ja), "| faltam:", length(todo), "\n")

for (i in seq_along(todo)) {
  cod <- todo[i]
  dest <- file.path(dir, sprintf("mun_%d_2025_annex_dca_i_c.csv", cod))
  if (file.exists(dest)) next
  tryCatch({
    d <- siconfir::get_annual_acc(year = 2025, cod = cod, annex = "DCA-Anexo I-C")
    fwrite(d, dest)
    if (i %% 100 == 0) cat(format(Sys.time(), "%H:%M:%S"), cod, "(", i, "/", length(todo), ")\n")
  }, error = function(e) NULL)
  Sys.sleep(0.3)
}
cat("FIM:", length(list.files(dir, pattern = "mun_.*_2025_")), "arquivos 2025\n")
