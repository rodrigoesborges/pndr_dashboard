library(RPostgres)


# bdn <- "aedidb"
# ubd <- "aedi"
# bdh <- "127.0.0.1"
# bds <- "aEd1#man@gR"

mdrc <- DBI::dbConnect(
  RPostgres::Postgres(),
  user=Sys.getenv('userdb'),
  password = Sys.getenv('passwddbdev'),
  host = Sys.getenv('hostdbdev'),
  dbname=Sys.getenv('tdbname'))
###Vamos gerar a consulta da VIEW

resultadod <- readr::read_csv2("dadostat/2015.csv")

ncolfin <- names(resultadod)


#geoloc <- dbGetQuery(con,"SELECT * from geoloc")


#datavalues <- dbGetQuery(con,"SELECT * from data_values")


#mdata <- dbGetQuery(con,"SELECT * from mdata")

# DBI::dbExecute(con,'BEGIN TRANSACTION;')
# DBI::dbExecute(con,"
#   SELECT FORMAT('DROP VIEW IF EXISTS %I.%I;',v.schemaname, v.viewname) drop_views
#   FROM pg_views v WHERE
#   v.schemaname NOT IN (
#     'pg_catalog','information_schema') AND
#   v.viewname NOT IN ('pg_stat_statements','geography_columns','geometry_columns');")
# DBI::db(con,'\\gexec')
# DBI::dbExecute(con,'END TRANSACTION;')

gerap <- \(indicador="objetivo1") {
  view_obj1 <- dbGetQuery(mdrc,paste0(
    "SELECT mdata_id from mdata WHERE orig_name LIKE '%",indicador,"%'"))$mdata_id

  anosdisp <- dbGetQuery(
    mdrc,
    paste0("SELECT DISTINCT(EXTRACT('Year' FROM refdate)) FROM data_values WHERE ",
           "mdata_id IN (",paste0(view_obj1,collapse=","),")")
  )

  anosdisp <- sort(anosdisp[[1]])


  baseind <-
    dbGetQuery(
      mdrc,
      paste0("SELECT EXTRACT('Year' FROM refdate) ano,
      geoloc_id as codigo_ibge,local_name municipio, orig_name,
      value as valor from data_values c left join mdata a
      on c.mdata_id = a.mdata_id
      LEFT JOIN
      local b on c.local_id = b.local_id
      WHERE orig_name LIKE '%",indicador,"%' AND EXTRACT('Month' FROM refdate) = 12 AND b.local_id > 6260 AND b.local_id < 6293"))

  data.table::setDT(baseind)
  baseind <- baseind[ano>2012]

  saveRDS(baseind,paste0("coleta/cache/indicadores_atualizados/",indicador,"uf.rds"))
  basefplanilha <-
    baseind|>tidyr::pivot_wider(names_from=c(ano,orig_name),names_sep = "_",values_from=valor)


  writexl::write_xlsx(basefplanilha,paste0('coleta/cache/indicadores_atualizados/',indicador,"uf.xlsx"))

}

indicadores_planilhas <- unique(gsub("(?<!vo)\\d","\\1",unique(gsub("_[^_]+$","",dbGetQuery(
  mdrc , "SELECT DISTINCT(orig_name) from mdata")$orig_name)),perl=TRUE))

indicadores_planilhas<- sort(indicadores_planilhas[indicadores_planilhas!='comp' & indicadores_planilhas!='pnadc'])


lapply(indicadores_planilhas,gerap)
