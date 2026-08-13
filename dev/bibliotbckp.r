library(bibliometrix)
library(openalexR)
options(openalexR.mailto="rodrigo@borges.net.br")
#Sys.setenv("CHROMOTE_CHROME"="opt/google/chrome-linux64/chrome")
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

oabibx <- readRDS("dadostat/oa_biblio_x.rds")
oabibr <- readRDS("dadostat/oa_biblio.rds")
oabibx <- openalexR::oa2bibliometrix(oabibr)

oabibxf <- oabibx|>dplyr::filter(!grepl("COMPUTER SCIENCE",ID), !grepl("BIOLOGY",ID), !grepl("MEDICINE",ID),!grepl("GENETICS",ID),!grepl("CHEMISTRY",ID),
                                 !grepl("ASTRONOMY",ID),!grepl("PHYSICS",ID))

oabibxf <- oabibx|>dplyr::filter(grepl("ECONO",ID) | grepl("SOCI",ID) | grepl("URBAN",ID) |
                                   grepl("PUBLIC",ID) | grepl("POLIT",ID) |
                                 grepl("PLAN",ID))

##Arbitrary, for getting when sustained production started
oabibxf <- oabibxf|>dplyr::filter(PY>1930)

oabibxf <- oabibxf|>dplyr::distinct(across(id_oa),.keep_all=T)

saveRDS(oabibxf,"dadostat/oa_biblio_xf.rds")
##translating to portuguese all keywords
work_kw <- tibble::tibble(workid= oabibxf$id_oa, keywordsai=oabibxf$ID)

##flow - serialize,factorize,get unique levels for translation, revert process
work_kww <- work_kw|>
  tidyr::separate_wider_delim(keywordsai,delim=";",too_few="align_start",names_sep="_")|>
  select(workid,paste0("keywordsai_",1:5))|>
  tidyr::pivot_longer(-1,names_to="kwn",values_to = "keyword")

work_kww <- work_kww|>mutate(across(keyword,forcats::as_factor))

kwlevels <- levels(work_kww$keyword)

kwlevels <- data.frame(kworigs=paste0(kwlevels,collapse=";"))|>
  tidyr::separate_longer_position(kworigs,width=49995)

kwlevels <- kwlevels|>mutate(across(kworigs,\(x){gsub(";","; ",x)}))

kwlevels[4,] <- substr(kwlevels$kworigs[1],49996,nchar(kwlevels$kworigs[1]))

#kwlevels[6,] <- substr(kwlevels$kworigs[3],49981,nchar(kwlevels$kworigs[3]))

kwlevels$kworigs[1] <- substr(kwlevels$kworigs[1],1,49994)

kwlevels$kworigs[2] <- substr(kwlevels$kworigs[2],1,49989)


googlesheets4::gs4_auth()

langdest <- "pt-BR"

coltrad <- \(x,db=base) {
  b <- db|>
    transmute(a =
                paste0('=googletranslate(A:A;"en";"',x,'")'))
  renom <- paste0("label_",x)
  b <- rename_with(b,~ gsub("a",renom,.x),starts_with("a"))
}
#2) Envia para a tradução automática do Google
kwlevels <- bind_cols(kwlevels,coltrad(langdest,kwlevels))

kwlevels <- kwlevels|>mutate(across(contains("label_"),googlesheets4::gs4_formula))


googlesheets4::gs4_create(paste0(format.Date(Sys.Date(),"%Y%m%d"),"-bibliometrixpndr"),sheets = kwlevels)

trad <- googlesheets4::gs4_get(
  googlesheets4::gs4_find(
    paste0(format.Date(Sys.Date(),"%Y%m%d"),"-bibliometrixpndr"))) |>
  googlesheets4::read_sheet()

nkwlevels <- data.frame(kwtrad =paste0(toupper(trad$`label_pt-BR`[c(1,4,2,5,3)]),collapse=""))

nkwlevels <- nkwlevels|>
  tidyr::separate_longer_delim(kwtrad,delim=";")

nkwlevels[372,1] <- "Preço de Oferta(Vendedor)"

nkwlevels$kwtrad <- stringr::str_trim(nkwlevels$kwtrad)

subforcatslevels <- levels(work_kww$keyword)
names(subforcatslevels) <- nkwlevels$kwtrad

work_kww <- work_kww|>
  mutate(across(keyword,\(x){as.character(forcats::fct_recode(x,!!!subforcatslevels))}))

fwork_kw <- work_kww|>tidyr::pivot_wider(names_from=kwn,values_from=keyword)
fwork_kw <- fwork_kw|>tidyr::unite(col="keywordsai",keywordsai_1:keywordsai_5,sep=";")

#check if equal

if (nrow(fwork_kw)==sum(fwork_kw$workid==oabibxf$id_oa)) {
  oabibxf$ID <- fwork_kw$keywordsai
}

saveRDS(oabibxf,"dadostat/oabxfpt.rds")
#workid= oabibxf$id_oa, keywordsai=oabibxf$ID)


rscopus::set_api_key("52288cf6330cfba55e4a8b4820f14e4c")
rscopus::elsevier_authenticate(headers = c("Origin" = "https://salas.grupo.pro.br"))

autenelse <- function (api_key = NULL, api_key_error = TRUE, choice = NULL,
                       verbose = TRUE, headers = NULL, ...)
{
  api_key = get_api_key(api_key, error = api_key_error)
  content_type = "authenticate"
  http = "https://api.elsevier.com"
  http = paste0(paste(http, content_type, sep = "/"),"?platform=scopus")
  if (verbose) {
    parsed_url = httr::parse_url(http)
    parsed_url$query$APIKey = NULL
    parsed_url = httr::build_url(parsed_url)
    message(paste0("HTTP specified is: ", parsed_url, "\n"))
  }
  qlist = list()
  qlist$apiKey = api_key
  qlist$choice = choice
  hdrs = do.call(httr::add_headers, args = as.list(headers))
  r = httr::GET(http, query = qlist, hdrs, ...)
  cr = httr::content(r)
  L = list(get_statement = r, content = cr)
  token = cr$`authenticate-response`$authtoken
  class(token) = "token"
  L$token = token
  L$auth_type = cr$`authenticate-response`$`@type`
  return(L)
}


result <- rscopus::scopus_search(
  'TITLE-ABS-KEY(pndr OR polycentr* OR ("central places" AND network) OR ("medium sized cities" AND ("policy" or "policies")))',
  headers= c(Origin = "https://salas.grupo.pro.br"),api_key = get_api_key(),
  view = "STANDARD",  count =25)


ZOPP <- rscopus::scopus_search(
  'TITLE-ABS-KEY(zopp AND method*)',
  headers= c(Origin = "https://salas.grupo.pro.br"),api_key = get_api_key(),
  view = "STANDARD",  count =25,max_count=4790)
