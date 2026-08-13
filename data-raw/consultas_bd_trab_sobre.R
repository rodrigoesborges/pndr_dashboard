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

geraq <- \(indicador="objetivo1",ano=2015) {
  view_obj1 <- dbGetQuery(con,paste0(
    "SELECT mdata_id from mdata WHERE orig_name LIKE '%",indicador,"%'"))$mdata_id

  ##TBD - 15 subst by DBI::dbGetQuery(con,"select datagroup_id from datagroup where datagroup_Name like "Tipologia%")
  ##TBD - and 672 subst by DBI::dbGetQuery(con,"select datagroup_id from datagroup where datagroup_Name like "Faixa de Fronteira")
#             b.data_name Subindicador_desc,
  consulta <- paste0(
    "SELECT  d.geoloc_id codigo_ibge,
             ST_X(ST_Centroid(d.geometry)) longitude,
             ST_Y(ST_Centroid(d.geometry)) latitude,
             c.local_name município,
             e.regiao_imediata,
             eee.regiao_intermediaria,
             e.tipologia,
             f.local_name Estado,
             g.local_name Região,
             t.faixa_de_fronteira,
             tttt.participacao_semiarido,
             ttt.participacao_amazonia_legal,
             hhhh.indicador,
             hhhh.subindicador SubIndicador,
             EXTRACT('Year' FROM a.refdate) ano,
             CAST(a.value AS real) Objetivo1_1,
             CAST(i.value AS real) Objetivo1_2,
             CAST(j.value AS real) Objetivo1_3,
             CAST(k.value AS real) Indice_Composto,
             d.geometry geometry\n",

    "FROM data_values a \n",
    "LEFT JOIN \n",
    "mdata b on a.mdata_id = b.mdata_id \n",
    "LEFT JOIN \n",
    "local c on a.local_id = c.local_id \n",
    "LEFT JOIN \n",
    "geoloc d on c.geoloc_id = d.geoloc_id \n",
    "LEFT JOIN \n",
    "(SELECT aa.local_id,
    aa.datagroup_id,cc.datagroup_name regiao_imediata, dd.datagroup_name indicador, \n",
    "ee.tipologia \n",
    "FROM \n",
    "local_group aa \n",
    "LEFT JOIN \n",
    "local bb ON aa.local_id=bb.local_id \n",
    "LEFT JOIN \n",
    "datagroup cc ON aa.datagroup_id = cc.datagroup_id \n",
    "LEFT JOIN \n",
    "(SELECT aaa.datagroup_parentid,aaa.datagroup_id,bbb.datagroup_name FROM group_parent aaa LEFT JOIN datagroup bbb ON aaa.datagroup_parentid = bbb.datagroup_id) dd ON \n",
    "cc.datagroup_id = dd.datagroup_id \n",
    "LEFT JOIN \n",
    "(SELECT bb.local_id, cc.datagroup_desc, cc.datagroup_name tipologia from \n",
    "local_group bb \n",
    "LEFT JOIN datagroup cc ON bb.datagroup_id = cc.datagroup_id \n",
    "LEFT JOIN group_parent dd ON bb.datagroup_id=dd.datagroup_id \n",
    "WHERE dd.datagroup_parentid = 15) ee ON aa.local_id = ee.local_id \n",
    "WHERE dd.datagroup_parentid = 25) e ON  \n",
    "a.local_id  = e.local_id \n",
    "LEFT JOIN \n",
    "(SELECT bb.local_id, cc.datagroup_desc, cc.datagroup_name faixa_de_fronteira from \n",
    "local_group bb \n",
    "LEFT JOIN datagroup cc ON bb.datagroup_id = cc.datagroup_id \n",
    "LEFT JOIN group_parent dd ON bb.datagroup_id=dd.datagroup_id \n",
    "WHERE dd.datagroup_parentid = 672) t ON  \n",
    "a.local_id  = t.local_id \n",
    "LEFT JOIN \n",
    "(SELECT bb.local_id, cc.datagroup_desc, cc.datagroup_name participacao_amazonia_legal from \n",
    "local_group bb \n",
    "LEFT JOIN datagroup cc ON bb.datagroup_id = cc.datagroup_id \n",
    "LEFT JOIN group_parent dd ON bb.datagroup_id=dd.datagroup_id \n",
    "WHERE dd.datagroup_parentid = 678) ttt ON  \n",
    "a.local_id  = ttt.local_id \n",
    "LEFT JOIN \n",
    "(SELECT bb.local_id, cc.datagroup_desc, cc.datagroup_name participacao_semiarido from \n",
    "local_group bb \n",
    "LEFT JOIN datagroup cc ON bb.datagroup_id = cc.datagroup_id \n",
    "LEFT JOIN group_parent dd ON bb.datagroup_id=dd.datagroup_id \n",
    "WHERE dd.datagroup_parentid = 675) tttt ON  \n",
    "a.local_id  = tttt.local_id \n",
    "LEFT JOIN \n",
    "(SELECT ab.local_id,ba.datagroup_desc regiao_intermediaria FROM \n",
    "local_group ab ",
    "LEFT JOIN \n",
    "datagroup ba ON ab.datagroup_id = ba.datagroup_id \n",
    "WHERE ab.datagroup_id>538 AND  ab.datagroup_id < 672",
    ") eee ON a.local_id = eee.local_id \n",
    "LEFT JOIN \n",
    "(SELECT geoloc_id,local_name FROM local WHERE geoloc_id<100 AND geoloc_id>9) f ON \n",
    " SUBSTR(c.geoloc_id::text,1,2)::INTEGER = f.geoloc_id \n",
    "LEFT JOIN \n",
    "(SELECT geoloc_id,local_name FROM local WHERE geoloc_id<10) g ON \n",
    " SUBSTR(c.geoloc_id::text,1,1)::INTEGER = g.geoloc_id \n",
    "LEFT JOIN \n",
    "(SELECT aa.mdata_id, cc.datagroup_desc subindicador, \n",
    "cc.datagroup_name indicador from mdata aa \n",
    "LEFT JOIN mdata_group bb ON aa.mdata_id=bb.mdata_id \n",
    "LEFT JOIN datagroup cc ON bb.datagroup_id = cc.datagroup_id\n",
    ") hhhh ON a.mdata_id = hhhh.mdata_id \n",
    "LEFT JOIN \n",
    "(SELECT ab.mdata_id,local_id,refdate,value FROM data_values ab \n",
    "LEFT JOIN \n",
    "mdata ba ON ab.mdata_id=ba.mdata_id \n",
    "WHERE ab.mdata_id =",view_obj1[2],
    " ) i ON a.local_id=i.local_id AND a.refdate=i.refdate \n",
    "LEFT JOIN \n",
    "(SELECT ab.mdata_id,local_id,refdate,value FROM data_values ab \n",
    "LEFT JOIN \n",
    "mdata ba ON ab.mdata_id=ba.mdata_id \n",
    "WHERE ab.mdata_id =",view_obj1[3],
    " ) j ON a.local_id=j.local_id AND a.refdate=j.refdate \n",
    "LEFT JOIN \n",
    "(SELECT ab.mdata_id,local_id,refdate,value FROM data_values ab \n",
    "LEFT JOIN \n",
    "mdata ba ON ab.mdata_id=ba.mdata_id \n",
    "WHERE ab.mdata_id =",view_obj1[4],
    " ) k ON a.local_id=k.local_id AND a.refdate=k.refdate \n",
    "WHERE a.mdata_id =",view_obj1[1],
    " AND \n",
    "(SELECT EXTRACT('Year' FROM a.refdate)  = ",ano,")"
  )

  #


#  consulta
  DBI::dbExecute(con,paste0("CREATE MATERIALIZED VIEW ",indicador,"_",ano," AS ",consulta))
}

#oqvirou <- dbGetQuery(con,geraq())

mapply(geraq,rep(paste0("objetivo",1:4),each=10),ano=2014:2024,USE.NAMES=F)

oqvirouview <- dbGetQuery(con,"SELECT * FROM objetivo1_2022")
