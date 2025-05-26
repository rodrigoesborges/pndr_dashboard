oalist <- readRDS("dadostat/oabxfpt.rds")
#oanal <- readRDS("dadostat/oa_analisis_bmx.rds")
juntanuma <- readRDS("dadostat/oa_zot.rds")
library(bibliometrix)
oanal <- bibliometrix::biblioAnalysis(juntanuma)
saveRDS(oanal,"dadostat/oa_analisis_bmx.rds")
#desreg <- bib2df::bib2df("dev/2024-09-Produto-1/bib/desreg.bib")

#desreg <- desreg[colSums(!is.na(desreg))>0]

#drraw <- bibliometrix:::importFiles("dev/2024-09-Produto-1/bib/desenvreg.bib")

#bibliometrix::convert2df("dev/2024-09-Produto-1/bib/desreg.bib",dbsource="isi",format="bibtex")

#saveRDS(M,"dev/2024-09-Produto-1/bib/zotero_bibliometrix.rds")
oalist <- readRDS("dadostat/oabxfpt.rds")
tesdb <- readRDS("dev/2024-09-Produto-1/bib/woszot.rds")
#analzot <- biblioAnalysis(juntanuma)
juntanuma <- data.table::rbindlist(list(oalist,tesdb),fill=T)
#geral <- bibliometrix::mergeDbSources(tesdb,oa_biblio_x)

saveRDS(juntanuma,"dadostat/oa_zot.rds")
saveRDS(biblioAnalysis(juntanuma),"dadostat/oa_zot_an.rds")


#bibliometrix::plotThematicEvolution(
oathemev <-   bibliometrix::thematicEvolution(juntanuma,
                                  field="ID",
                                  years=c(1985,1995,2005,2015),
                                  remove.terms = c("na","de"))

evol_them <- plotThematicEvolution(oathemev[["Nodes"]],oathemev[["Edges"]])


oanal <- readRDS("dadostat/oa_zot_an.rds")

sumanal <- summary(oanal)

gptchatteR::chatter.auth(Sys.getenv("OPENAI_API_KEY"))

gptchatteR::chatter.create(model="gpt-4o-mini",temperature=0,max_tokens=16384)


chatter.feed("Use o R. Não inclua '<code>' na resposta. Só responda com código.\n")
complete_response <- chatter.rechat(
  paste0("Traduza para o português a seguinte tabela:\n",
         gptr::dataframe_to_text(sumanal$MainInformationDF)),
  feed = T,
  return_response=T
)

eval(parse(text = complete_response$choices$message.content))

principais_infos <- data.frame(
  Descrição = Description,
  Resultados = Results
)

save(list=c("principais_infos","oathemev","evol_them"),file = "dev/2024-09-Produto-1/data/biblio01.rda")

gptmodel="gpt-4o-mini"
library(chattr)
library(askgpt)
Sys.setenv("OPENAI_RETURN_LANGUAGE" = "pt-BR")
Sys.setenv("OPENAI_TEMPERATURE" = "0.2")
Sys.setenv("OPENAI_MODEL" = "gpt-4o-mini")
Sys.setenv("OPENAI_MAX_TOKENS" = "2048")

resposta <- chatgpt::explain_code()


# explcollectb <- chattr(paste0("Explique para inserir em um relatório o procedimento feito no script "),stream=F)

#
# explique_codigo <- \(code=clipr::read_clip(allow_non_interactive = T)) {
#   code <- paste(gsub("\"","'",code),collapse="\n")
#   prompt <- paste0("Explique em até 4 parágrafos o seguinte código em linguagem R:\"",
#                    code,"\"")
#   chatgpt:::gpt_get_completions(prompt)
# }
