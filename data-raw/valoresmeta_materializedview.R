con <- DBI::dbConnect(
  RPostgres::Postgres(),
  user=Sys.getenv('userdb'),
  password = Sys.getenv('passwddbdev'),
  host = Sys.getenv('hostdbdev'),
  dbname=Sys.getenv('tdbname'))
###Vamos gerar a consulta da VIEW


# con <- DBI::dbConnect(RPostgres::Postgres(),
#                       user="aedi",
#                       password="aEd1#man@gR",
#                       host="127.0.0.1",
#                       dbname="aedidb")

novaview <- "SELECT
    v.mdata_id,
    v.local_id,
    v.refdate,
    v.value,
    m.orig_name,
    m.data_name,
    l.geoloc_id,
    l.local_name
FROM
    painelpndr.public.data_values AS v
INNER JOIN
    painelpndr.public.mdata AS m
ON
    v.mdata_id = m.mdata_id
INNER JOIN
    painelpndr.public.local AS l
ON
    v.local_id = l.local_id"


DBI::dbExecute(con,paste0("CREATE MATERIALIZED VIEW valoresmeta AS ",novaview))

DBI::dbExecute(con,'DROP INDEX IF EXISTS valoresmeta.geolocid_index')

DBI::dbExecute(con,
               paste0("CREATE UNIQUE INDEX IF NOT EXISTS geoloc_mdata_refdate_index ON
                      public.valoresmeta USING btree
                      (geoloc_id ASC NULLS LAST,mdata_id ASC NULLS LAST,refdate ASC NULLS LAST)
                      WITH (FILLFACTOR=90)
                      TABLESPACE pg_default;"))

DBI::dbDisconnect(con)


