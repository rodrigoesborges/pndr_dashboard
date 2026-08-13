# bdn <- "aedidb"
# ubd <- "aedi"
# bdh <- "127.0.0.1"
# bds <- "aEd1#man@gR"

con <- DBI::dbConnect(RPostgres::Postgres(),user=ubd,password = bds,host = bdh,dbname=bdn)
###Vamos gerar a consulta da VIEW
objetivo3_nayra <- lapply(2015:2022,\(x) {dbGetQuery(con,paste0("SELECT * FROM objetivo3_",x))})

names(objetivo3_nayra) <- paste0("objetivo3_",2015:2022)
