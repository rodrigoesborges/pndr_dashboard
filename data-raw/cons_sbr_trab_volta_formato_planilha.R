library(RPostgres)




con <- DBI::dbConnect(
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

geraq <- \(indicador="objetivo1_1") {
  view_obj1 <- dbGetQuery(con,paste0(
    "SELECT mdata_id from mdata WHERE orig_name LIKE '%",indicador,"%'"))$mdata_id

  anosdisp <- dbGetQuery(
    con,
    paste0("SELECT DISTINCT(EXTRACT('Year' FROM refdate)) FROM data_values WHERE ",
           "mdata_id = ",view_obj1)
  )

  anosdisp <- sort(anosdisp[[1]])

  anosdisp <- anosdisp[anosdisp>2013]


  baseind <-
    dbGetQuery(
      con,
      paste0("SELECT EXTRACT('Year' FROM refdate) ano,
      geoloc_id as codigo_ibge,orig_name,
      value as valor from data_values c left join mdata a
      on c.mdata_id = a.mdata_id
      LEFT JOIN
      local b on c.local_id = b.local_id
      WHERE orig_name LIKE '%",indicador,"%'"))



  tabprimano <- paste0(gsub("_\\d+$","_",indicador),max(anosdisp[1],2015))

  colsbase <- names(dbGetQuery(con,
                         paste0("select * from ",
                                tabprimano,
                                " limit 1"
                         )))

  colsbase <- colsbase[-(length(colsbase)-5):-(length(colsbase)-1)]

  tabela_apelido <- data.frame(ano=anosdisp,ref=letters[1:length(anosdisp)])

  tab_enxuta <- paste0("(SELECT codigo_ibge, ",indicador,
                       " FROM ",gsub("_\\d$","_",indicador),anosdisp,") ",tabela_apelido$ref)

  tab_enxuta[[1]] <- paste(tabprimano,tabela_apelido$ref[1])
  parteon <- paste0(tabela_apelido$ref[1],
         ".codigo_ibge = ",
         tabela_apelido$ref[2:nrow(tabela_apelido)],".codigo_ibge")

  left_joins <- gsub("a ON ","a",paste(paste(paste(tab_enxuta,c("",parteon),sep = " ON "),collapse=" LEFT JOIN ")))

    consulta <- paste0(paste0("SELECT a.",paste0(colsbase,collapse=", "),", ",
         paste(paste0("CAST(",tabela_apelido$ref,".",indicador," AS real) ano",tabela_apelido$ano),collapse=", "),
         " FROM ", left_joins)
         )
#
#     testaconsulta <-
#       dbGetQuery(con,consulta)

  DBI::dbExecute(con,paste0("CREATE MATERIALIZED VIEW cons_",indicador," AS ",consulta))
}


lapply(paste0("objetivo1_",1:3),geraq)


dbDisconnect(con)
