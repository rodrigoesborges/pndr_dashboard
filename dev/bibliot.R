library(bibliometrix)
library(openalexR)
options(openalexR.mailto="rodrigo@borges.net.br")

query_oa <- 'pndr OR polycentr* OR ("central places" AND network)'

oa_biblio <- oa_fetch(entity="works",
                      default.search=query_oa,
                      is_paratext="false",
                      verbose=T,mailto=oa_email())

##adiciona keywords
#aselec <- c(names(oa_biblio),"keywords")

oa_biblio <- oa_fetch(entity="works",
                      default.search=query_oa,
                      is_paratext="false",
                      verbose=T,mailto=oa_email())


##ADiciona autoria completa de obras indicadas como truncadas em lista de autores
oa_detalhe <- c("W2097137004", "W2151464048", "W2482077161", "W4400221903", "W4307537905", "W2588962111")

oa_comp <- lapply(oa_detalhe,\(x) oa_fetch(identifier=x,
                                           mailto = oa_email()))

oa_comp <- data.table::rbindlist(oa_comp,fill=T)

oa_biblio <- data.table::rbindlist(list(oa_biblio|>dplyr::filter(!(id %in% paste0("https://openalex.org/",oa_detalhe))),
                                        oa_comp),fill=T)
saveRDS(oa_biblio,"dadostat/oa_biblio.rds")

saveRDS(oa2bibliometrix(oa_biblio),"dadostat/oa_biblio_x.rds")

oaxbal <- biblioAnalysis(oax)

keywords <- names(oaxbal$ID)


###WOS
wosdf <- bibliometrix::convert2df(
  list.files(path="dadostat/WosCategoriesRefino",
             pattern="savedrecs.*\\.txt",full.names=T),
  dbsource = "wos",format="plaintext")

wosdfr <- bibliometrix::convert2df(
  list.files(path="dadostat/poli_centr_apenas",
             pattern="savedrecs.*\\.txt",full.names=T),
  dbsource = "wos",format="plaintext")

saveRDS(wosdfr,"dadostat/wosbrx.rds")
