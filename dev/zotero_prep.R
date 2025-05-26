library(RSQLite)

con <- dbConnect(SQLite(),dbname = "/media/usb1/Zotero-Refs/2024-09-07-zotero.sqlite")

tabelas <- dbGetQuery(con,"SELECT * FROM sqlite_master where type='table';")


zoterocsv <- read.csv("dev/2024-09-Produto-1/bib/2024-desenvolvimento_regional.csv")
testenotas <- dbGetQuery(con, "SELECT * from itemNotes")

notas <- testenotas[grepl("Citations",testenotas$note),]
notas <- notas[-1:-2,]
pegachavesnotas <- dbGetQuery(con,
                              paste("SELECT * FROM items WHERE ItemID in (",paste0(notas$parentItemID[-1:-2],collapse=", "),")"))




formacita <- \(x){
  jsonify::from_json(rvest::html_text(rvest::html_elements(xml2::read_xml(notas$note[x]),xpath="//pre")),simplify=T)$item
}

elems <- lapply(3:nrow(notas),formacita)

juntabibnumalinha <- \(notasimp) {

  testa <- try(notasimp[[1]]$creators,silent=T)
  if (inherits(testa, 'try-error')) {
    return("")
  }
ajcre <-
  \(x){
    df <- notasimp[[x]]$creators
    if (is.null(dim(df))) {
      df <- data.frame(firstName="Author",lastName="Unknown",creatorType = "author")
    }
    paste(gsub("^([^,]+, [^,]+), ([^,]+)","\\1 (\\2)",
               apply(df,1,\(y) {paste(y,collapse=", ")})),
          collapse=" and ")
               }
criadores <- sapply(1:length(notasimp),ajcre)

for (i in 1:length(notasimp)) {
  notasimp[[i]]$creators <- criadores[i]
  }


ajcoll <-
  \(x){
    df <- notasimp[[x]]$collections
    if (is.null(dim(df))) {
      df <- data.frame(firstName="Collection",lastName="Unknown",colType = "collection")
    }
    paste(gsub("^([^,]+, [^,]+), ([^,]+)","\\1 (\\2)",
               apply(df,1,\(y) {paste(y,collapse=", ")})),
          collapse=" and ")
  }
colecoes <- sapply(1:length(notasimp),ajcoll)

for (i in 1:length(notasimp)) {
  notasimp[[i]]$collections <- colecoes[i]
}

junta <- data.table::rbindlist(notasimp,use.names=T,fill = T,ignore.attr=T)

paracampo <- paste0(apply((apply(junta,1,\(x) {gsub("([A-Z])\\.","\\1",paste(x[c("creators","date","title")]))})),2,\(x){paste(x,collapse=",")}),collapse="\n   ")
paracampo
}

notas1linha <- sapply(1:50,\(x)juntabibnumalinha(elems[[x]]))

notas$note <- paste0("CR ",notas1linha)


### associar zkey com bbtkey
juntazkbbtk <- unlist(strsplit(
  readr::read_file("dev/2024-09-Produto-1/bib/2024-desenvolvimento_regional.txt"),", "))

zoterocsv$bbtkey <- juntazkbbtk

zoterocsv$totalcount <- as.numeric(gsub("GSCC:[^0-9]*([0-9]+).*","\\1",zoterocsv$Extra))
zoterocsv[is.na(zoterocsv$totalcount),]$totalcount <- 0

ztbbtkey <- zoterocsv[c("Key","bbtkey","totalcount")]

dadositem <- dbGetQuery(con,"select * from items")
dadositem <- dadositem|>dplyr::filter(itemID %in% notas$parentItemID)

dadositem <- dadositem|>dplyr::left_join(ztbbtkey,by=c("key"="Key"))

dadositem <- dadositem|>dplyr::left_join(notas[c("parentItemID","note")],by=c("itemID"="parentItemID"))


#adiciona nas linhas corretas
zoterob <- bib2df::bib2df("dev/2024-09-Produto-1/bib/fdesenvolvimento_regional.bibtex")

zoterob$YEAR <- as.numeric(zoterob$YEAR)

zoterob[is.na(zoterob$YEAR),]$YEAR <- 1971

zoterob[zoterob$YEAR==23,]$YEAR <- 2023

zoterob <- zoterob|>dplyr::left_join(ztbbtkey,by=c("BIBTEXKEY"="bbtkey"))

zoterob$UT <- paste0("ZOT:",zoterob$BIBTEXKEY)

zoterob <- zoterob|>dplyr::left_join(dadositem[c("key","note","bbtkey")],by=c("Key"="key","BIBTEXKEY"="bbtkey"))

##palavras chave geradas via python keyword-spacy
palavras <- read.csv("dev/2024-09-Produto-1/palavraschave.csv",header=F)
palavras <- as.data.frame(apply(palavras,c(1,2),\(x){gsub("\\\\n","",x)}))

zoterob <- zoterob[-82,]

zoterob$palavraschave <- apply(palavras,1,\(x){paste(x,collapse = ";")})

zoterob$citations <- zoterob$totalcount

zoterob <-
  zoterob|>
  dplyr::mutate(affiliation=
                  dplyr::case_when(
                    grepl("MI",INSTITUTION,ignore.case=T) ~ "Ministério/União",
                    grepl("Brasil",AUTHOR,ignore.case=T)~ "Ministério/União",
                    grepl("Federal",AUTHOR,ignore.case=T)~ "Ministério/União",
                    T ~ "instituição de pesquisa"
                  ),
                country =
                  dplyr::case_when(
                    grepl("MI",INSTITUTION,ignore.case=T) ~ "Brazil",
                    grepl("Brasil",AUTHOR,ignore.case=T)~ "Brazil",
                    grepl("Santin",AUTHOR,ignore.case=T)~ "Portugal",
                    T ~ "USA")
)
bib2df::df2bib(zoterob,"dev/2024-09-Produto-1/bib/utdr.bib")

rbibutils::bibConvert(
  "dev/2024-09-Produto-1/bib/utdr.bib",
  outfile = "dev/2024-09-Produto-1/bib/woszot.bib",
  informat="bibtex",outformat = "isi")

adicionaut <- readr::read_file("dev/2024-09-Produto-1/bib/woszot.bib")

adicionaut <- gsub("PT Unknown","PT Book",adicionaut)

#adicionaut <- paste0(stringr::str_replace_all(adicionaut,"PY",paste0("CR ",zoterob$note,"\nUT ",zoterob$UT,"\nPY")),collapse="\n")

regmatches(adicionaut,gregexpr("PY ",adicionaut))[[1]] <-
  paste0("DT ",zoterob$CATEGORY,
         "\nC1 ",zoterob$country,
         "\nTC ",zoterob$totalcount,
         "\nID ",zoterob$palavraschave,
         "\nCR ",zoterob$note,
         "\nUT ",zoterob$UT,"\nPY ")

readr::write_file(adicionaut,"dev/2024-09-Produto-1/bib/woszot.txt")




tesdb <- bibliometrix::convert2df("dev/2024-09-Produto-1/bib/woszot.txt")


saveRDS(tesdb,"dev/2024-09-Produto-1/bib/woszot.rds")


# ntabib <- \(x) {
#   f <- tempfile()
#   rbibutils::charToBib(x,informat="bibtex",outfile=f,outformat="bibtex")
#   tab <- bib2df::bib2df(f)
#   tab
# }
#
#
# rbibutils::charToBib(zoterocsv$Notes,informat="bib")
#
#
# bib2df::df2bib(zoterob,"dev/2024-09-Produto-1/bib/utdr.bib")
#
# rbibutils::bibConvert("dev/2024-09-Produto-1/bib/utdr.bib",outfile = "dev/2024-09-Produto-1/bib/woszot.bib",informat="bibtex",outformat = "isi")

# nota1 <- bib2df::bib2df("dev/2024-09-Produto-1/bib/Citações exportadas.bib")
# aleaUT <- \(x) {
#   nid <- abs(trunc(rnorm(n=1)*1e5))
#   paste0(x,"\n\tUT = {",nid,"},")
# }
#
#
# sink(file="desreg.bib", split=TRUE)
# cat(stringr::str_replace_all(sbrl,"(@.*),",aleaUT))
# sink()

