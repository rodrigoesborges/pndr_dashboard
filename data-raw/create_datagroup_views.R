library(RPostgres)

bdn <- "painelpndr"
ubd <- "usr_cggi_admin"
bdh <- "10.214.50.169"
bds <- "anbhdbregrvdf@2024"

# bdn <- "aedidb"
# ubd <- "aedi"
# bdh <- "127.0.0.1"
# bds <- "aEd1#man@gR"

con <- DBI::dbConnect(RPostgres::Postgres(),user=ubd,password = bds,host = bdh,dbname=bdn)
###Vamos gerar a consulta da VIEW

resultadod <- readr::read_csv2("dadostat/2015.csv")

ncolfin <- names(resultadod)


###OBJETIVO: VIEW COM
# codigo_ibge
# indicador
# subindicador
# ano
# pivot wider daqueles do mesmo objetivo 1


grupos_objs <- dbGetQuery(con,"SELECT datagroup_id FROM datagroup WHERE datagroup_name LIKE '%bj% %';")$datagroup_id

objs_inds <-
  DBI::dbGetQuery(
    con,
    paste('SELECT * FROM mdata LEFT JOIN mdata_group ON',
    'mdata.mdata_id = mdata_group.mdata_id',
    'WHERE mdata_group.datagroup_id IN (',
    paste0(grupos_objs,collapse=','),
    ')'))

consultageobase <- '(SELECT * FROM recortes_geograficos) viewbase'
adiciona_valor <- \(indicador_id = objs_inds$mdata_id[1],
                    baseq = consultageobase,
                    ano = 2015,
                    viewbase= paste0(gsub("_.*","",objs_inds$orig_name[1]),"_")) {


  ##Prepare data for JOIN
  newcolname <- objs_inds[objs_inds$mdata_id==indicador_id,]$orig_name


  valorind <-
    paste("(SELECT geoloc.geoloc_id codigo_ibge,",
          "CONCAT(datagroup_name, ' - ', datagroup_desc) grupo_indicador, ",
          "EXTRACT('Year' FROM data_values.refdate) ano, ",
          "data_values.value" ,newcolname,
          "FROM local LEFT JOIN geoloc ON",
          "local.geoloc_id = geoloc.geoloc_id",
          "LEFT JOIN data_values on local.local_id = data_values.local_id",
          "LEFT JOIN mdata_group on data_values.mdata_id = mdata_group.mdata_id",
          "LEFT JOIN datagroup on mdata_group.datagroup_id = datagroup.datagroup_id",
          "WHERE local.local_id <",numero_municipios+1,
          "AND data_values.mdata_id = ",indicador_id,
          "AND (SELECT EXTRACT('Year' FROM data_values.refdate)  = ",ano,
          ")",
          ") As", newcolname)

  ###JOIN

  paste0(baseq,
         ' LEFT JOIN ',
         valorind,
         ' ON viewbase.codigo_ibge = ',
         newcolname,'.codigo_ibge')


}

query_objetivo <- \(indicadoress = 9,anoq=2015) {
centro_query <- \(indicadoress =  indicadoress) Reduce(
  function(q, adic_indicador) {
    adiciona_valor(indicador_id = adic_indicador, baseq = q,ano=anoq)
  },
  objs_inds[objs_inds$datagroup_id == indicadoress,]$mdata_id,
  init = consultageobase

)

colselect <- paste(
  "SELECT viewbase.codigo_ibge,",
  "viewbase.longitude, viewbase.latitude,",
  "viewbase.município,",
  'estado, região,',
  paste0(previos_recortes_nmcol,collapse=", "),
  ", ",paste0(objs_inds[objs_inds$datagroup_id == indicadoress,]$orig_name[1],".ano,"),
  paste0(objs_inds[objs_inds$datagroup_id == indicadoress,]$orig_name[1],'.grupo_indicador,'),
  paste0(objs_inds[objs_inds$datagroup_id == indicadoress,]$orig_name,collapse=", "),
  'FROM '
)


consulta <- paste0(colselect,centro_query(indicadoress))

datagruponome <- dbGetQuery(con,paste0('select datagroup_name from datagroup where datagroup_id = ',
                            indicadoress))
nomedatagroup <-
  paste0(tolower(gsub(" ","",datagruponome)),"_",anoq)

DBI::dbExecute(con,
               paste('DROP MATERIALIZED VIEW IF EXISTS',
                     nomedatagroup, 'CASCADE'))

DBI::dbExecute(con,
               paste0("CREATE MATERIALIZED VIEW ",
                      nomedatagroup," AS ",consulta))
}



mapply(query_objetivo,rep(grupos_objs,each=9),anoq=2015:2023,USE.NAMES=F)

