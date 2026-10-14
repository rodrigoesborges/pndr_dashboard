# dessoc2 (22): % de pessoas em familias com renda per capita de ate meio
# salario-minimo (Pobreza + Baixa renda) no CadUnico sobre a populacao
# municipal.
# Fonte: CSV exportado do portal CadUnico/MDS (visdata3). O link de download
# (engenharia reversa — URL de exportacao DataTables, sem cookie/token) esta
# em `linkreversengineer`; bloqueado desta maquina pelo CDN gov.br — baixar
# manualmente ou via VPS brasileira. Padrao A5b.
linkreversengineer <- paste0(
  "https://aplicacoes.cidadania.gov.br/vis/data3/v.php",
  "?q[]=oNOclsLerpibuKep3bWDhbNe09Gv17lljax%2FYWyAYmqqdH9%2BaWOEkWuXbWTZ",
  "67Knr6enoO2Yg4BpaL3Cn92ibsPcuaehg3Cg2qXAs3Joytagja2Y0O6spqK8lHCra6x",
  "%2FaWGLnJnLqabCtrOVqLuadbSfrrqqkpKcpt%2BqVs3gwKebupuu2Gp%2FgmuMiJlp",
  "oHiZvufAmXeulqbsnoiJnY7D1JileKbS6HCkobuomeufwa1oZY2XbtCen9DgiJqdtKi",
  "ftHSzr6OgvJxu3bKggOuyp6%2Bnp5%2Fnna6tq5zLwp%2FJsJjK2r%2BZr7iUndqdiLS",
  "YmcrGbtCen9DgiG%2BiqaGt3nSIwaya06%2F2JKqYz%2BptmKFopZ%2FsrLyvqk26wpf",
  "LsKfP3LGVr2ijqZl8rrKYoMvToooAzcvksKNcraJa35q6EeSZwMKmiqCiypu%2Fmaqsl",
  "lrtqMGvo027xlPLsfYGm35Ur6mh%2FRqrtr1kmhoOodOqooDJEO6praepmZ2ybqeSyt",
  "Siy7BTwNyxla%2B8p5vdmsBupZx3pJTOnqbR7bxU%2F%2BKjo9yobbOkTb3CoC3qn8b",
  "cwFSft6Ja6567sphNy9Cny6lTwunBpqFoZlreWX9uqo7DJNTcpqLQqLr36baep%2Bis",
  "cJz658TGpdldl8KbvZmvu6Sb7Fmwr5uOytWly6GU0Ju7o1yLlp7arMHApk0a%2B6HTo",
  "KJ94LpUoqmi%2FSaltq%2BqTbrQoIqvmMvfrlSwt6mb5VmyvKufvIFliqJTkJvAlagL",
  "1qziqMB7pPAEz5zXrKaAyRDuqa2nqZmdsm6nksrUosuwU8DcsZWvvKeb3ZrAbqWcd6S",
  "Uzp6m0e28VP%2Fio6PcqG2zpE29wqAt6p%2FG3MBUn7eiWuueu7KYTcvQp8upU8rctq",
  "OuaKav3lmAbqqOwyTU3Kai0Ki69%2Bm2nqforHCc%2BufExqXZXZfCm72Zr7ukm%2BxZ",
  "sK%2BbjsrVpcuhlNCbu6Nci5ae2qzBwKZNGvuh06CifeC6VKKpov0mpbavqk3KxqCK",
  "pqHD6r%2BhnQvc%2FRyobbKcTcnGoc6eU9HqwZWoxKV19bXJiQ%3D%3D",
  "&ag=m&wt=json&tp_funcao_consulta=0&draw=2",
  "&start=0&length=3147483647",
  "&export=1&export_data_comma=1&export_tipo=csv")

# usa TODOS os CSVs do cache (o export do portal vem fragmentado por periodo;
# mais recente primeiro para vencer duplicatas) e acha a coluna "ate meio
# salario" PELO NOME (a posicao muda entre edicoes do portal)
arqs <- sort(list.files("coleta/cache/dessoc2_aedi", pattern = "^visdata3.*\\.csv$"),
             decreasing = TRUE)
stopifnot(length(arqs) > 0)
ler_pessoas <- function(arq) {
  x <- data.table::fread(file.path("coleta/cache/dessoc2_aedi", arq), encoding = "UTF-8")
  j <- setdiff(grep("meio sal", names(x), ignore.case = TRUE),
               grep("acima", names(x), ignore.case = TRUE))
  stopifnot(length(j) == 1)
  x[, .(codigo = as.numeric(.SD[[1]]), ref = as.character(.SD[[2]]),
        pessoas_meio_sm = suppressWarnings(as.numeric(.SD[[3]]))),
    .SDcols = c(1, 4, j)]
}
dessoc2 <- data.table::rbindlist(lapply(arqs, ler_pessoas))
dessoc2 <- dessoc2[!duplicated(dessoc2, by = c("codigo", "ref"))]
cat("fontes:", paste(arqs, collapse = " + "), "| linhas:", nrow(dessoc2), "\n")
cat("referencias:", paste(tail(sort(unique(dessoc2$ref)), 5), collapse = " "), "\n")

dez <- dessoc2[grepl("^12", ref)] |>
  dplyr::transmute(codigo_ibge = as.numeric(codigo),
                   ano = as.numeric(sub(".*/", "", ref)),
                   pessoas_ate_meio_sm = as.numeric(pessoas_meio_sm))

# populacao do DW (julho de cada ano -> refdate 31/12)
con <- DBI::dbConnect(RPostgres::Postgres(),
                      user = Sys.getenv("user", "aedi"),
                      password = Sys.getenv("password", "aEd1#man@gR"),
                      host = Sys.getenv("host", "127.0.0.1"),
                      dbname = Sys.getenv("dbname", "aedidb"))
pop <- DBI::dbGetQuery(con, "SELECT trunc(l.geoloc_id/10) codigo_ibge,
        extract(year from d.refdate)::int ano, d.value populacao
  FROM data_values d JOIN mdata m USING (mdata_id) JOIN local l USING (local_id)
 WHERE m.orig_name = 'datasus_popmun'")
DBI::dbDisconnect(con)

serie <- dez |>
  dplyr::inner_join(pop, by = c("codigo_ibge", "ano")) |>
  dplyr::filter(populacao > 0) |>
  dplyr::transmute(local = codigo_ibge,
                   periodo = as.Date(paste0(ano, "-12-31")),
                   valor = pessoas_ate_meio_sm / populacao)

cat("dessoc2:", nrow(serie), "municipios-ano | anos:",
    paste(range(as.numeric(format(unique(serie$periodo), "%Y"))), collapse = "-"), "\n")

AEDi:::gravar_serie_dw("dessoc2",
  data.frame(local = serie$local, periodo = serie$periodo, valor = serie$valor))
