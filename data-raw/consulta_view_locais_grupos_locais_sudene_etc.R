library(RPostgres)

# bdn <- "painelpndr"
# ubd <- "usr_cggi_admin"
# bdh <- "10.214.50.169"
# bds <- "anbhdbregrvdf@2024"

bdn <- "aedidb"
ubd <- "aedi"
bdh <- "127.0.0.1"
bds <- "aEd1#man@gR"

con <- DBI::dbConnect(RPostgres::Postgres(),user=ubd,password = bds,host = bdh,dbname=bdn)
geraq <- \(x) {

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
             sudene.participacao_sudene,
             d.geometry geometry\n",

    "FROM local c \n",
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
    "c.local_id  = e.local_id \n",
    "LEFT JOIN \n",
    "(SELECT bb.local_id, cc.datagroup_desc, cc.datagroup_name faixa_de_fronteira from \n",
    "local_group bb \n",
    "LEFT JOIN datagroup cc ON bb.datagroup_id = cc.datagroup_id \n",
    "LEFT JOIN group_parent dd ON bb.datagroup_id=dd.datagroup_id \n",
    "WHERE dd.datagroup_parentid = 672) t ON  \n",
    "c.local_id  = t.local_id \n",
    "LEFT JOIN \n",
    "(SELECT bb.local_id, cc.datagroup_desc, cc.datagroup_name participacao_amazonia_legal from \n",
    "local_group bb \n",
    "LEFT JOIN datagroup cc ON bb.datagroup_id = cc.datagroup_id \n",
    "LEFT JOIN group_parent dd ON bb.datagroup_id=dd.datagroup_id \n",
    "WHERE dd.datagroup_parentid = 678) ttt ON  \n",
    "c.local_id  = ttt.local_id \n",
    "LEFT JOIN \n",
    "(SELECT bb.local_id, cc.datagroup_desc, cc.datagroup_name participacao_sudene from \n",
    "local_group bb \n",
    "LEFT JOIN datagroup cc ON bb.datagroup_id = cc.datagroup_id \n",
    "LEFT JOIN group_parent dd ON bb.datagroup_id=dd.datagroup_id \n",
    "WHERE dd.datagroup_parentid = 681) sudene ON  \n",
    "c.local_id  = sudene.local_id \n",
    "LEFT JOIN \n",
    "(SELECT bb.local_id, cc.datagroup_desc, cc.datagroup_name participacao_semiarido from \n",
    "local_group bb \n",
    "LEFT JOIN datagroup cc ON bb.datagroup_id = cc.datagroup_id \n",
    "LEFT JOIN group_parent dd ON bb.datagroup_id=dd.datagroup_id \n",
    "WHERE dd.datagroup_parentid = 675) tttt ON  \n",
    "c.local_id  = tttt.local_id \n",
    "LEFT JOIN \n",
    "(SELECT ab.local_id,ba.datagroup_desc regiao_intermediaria FROM \n",
    "local_group ab ",
    "LEFT JOIN \n",
    "datagroup ba ON ab.datagroup_id = ba.datagroup_id \n",
    "WHERE ab.datagroup_id>538 AND  ab.datagroup_id < 672",
    ") eee ON c.local_id = eee.local_id \n",
    "LEFT JOIN \n",
    "(SELECT geoloc_id,local_name FROM local WHERE geoloc_id<100 AND geoloc_id>9) f ON \n",
    " SUBSTR(c.geoloc_id::text,1,2)::INTEGER = f.geoloc_id \n",
    "LEFT JOIN \n",
    "(SELECT geoloc_id,local_name FROM local WHERE geoloc_id<10) g ON \n",
    " SUBSTR(c.geoloc_id::text,1,1)::INTEGER = g.geoloc_id \n",
    "WHERE length(d.geoloc_id::text)=7"
  )

  #


  #  consulta
  DBI::dbExecute(con,paste0("CREATE MATERIALIZED VIEW recortes_geograficos AS ",consulta))
}

geraq()

DBI::dbExecute(con,
               paste0("CREATE UNIQUE INDEX IF NOT EXISTS codibge_index ON
                      public.recortes_geograficos USING btree
                      (codigo_ibge ASC NULLS LAST) WITH (FILLFACTOR=90)
                      TABLESPACE pg_default;"))
DBI::dbDisconnect(con)
