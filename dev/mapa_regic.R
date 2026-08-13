library(RSIDRA)
pega_regic <- sf::st_read("dadostat/regic_camadas/REGIC2018_hierarquia_final/REGIC2018_hierarquia_final.shp")

charconv <- \(x){
  iconv(x,from="iso8859-13",to="utf8")
}
pega_regic <-
  pega_regic|>
  mutate(across(where(is.character),charconv))

pega_regic$cod_cidade <-
  as.numeric(pega_regic$cod_cidade)


base_regic <- municipios|>left_join(pega_regic|>sf::st_drop_geometry(),by=c("code_muni" = "cod_cidade"))


ggplot(base_regic)+geom_sf(aes(fill=classe))+
  annotation_north_arrow(location = "br",which_north = "true",
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_orienteering,width = unit(1.5, "cm"),
                         height = unit(1.5, "cm"))+
  ggspatial::annotation_scale(line_width=1,bar_cols=c("white","white"),height=unit(0.001,"cm"),tick_height=2000)+
  scale_fill_discrete(type=c("#e7e7e7","#e7e7e7","#ffcc01","#ffebb0","#e34c05","#e60000","#01724c","#98e500","#e7e7e7","#e7e7e7","#e7e7e7","#e7e7e7"))+
  theme_classic()+theme(legend.position = "bottom")



###tipologia 2018
library(tabulapdf)

pegatab <- \(x,pag=1,pagf=get_n_pages(x),varias=0,...){
  print(paste0("Extraindo da página ",pag, " até a página ",pagf ))
  if(varias == 0) {
  tabulapdf::extract_tables(x,pages=pag:pagf,output="tibble",method="stream",...)[[1]]
  } else {
    tabulapdf::extract_tables(x,pages=pag:pagf,output="tibble",method="stream",...)
  }
}
tipologia2018_capa <-
#  data.table::rbindlist(
    lapply(list.files("dadostat/tipologia_vigente_portaria/",pattern="*.pdf",full.names = T),
           pegatab,pag=15,pagf=15,varias=0
    )
tipologia2018_capa <- tipologia2018_capa[[1]]

#,use.names = F
#  )

nomes_cols <- data.frame(prim=gsub("\\.\\.\\.[0-9]","",names(tipologia2018_capa)))
nomes_cols$sec <- t(tipologia2018_capa[1,])
nomes_cols$ter <- t(tipologia2018_capa[2,])
nomes_cols$qua <- t(tipologia2018_capa[3,])
nomes_cols$qui <- t(tipologia2018_capa[4,])
nomes_cols[is.na(nomes_cols)] <- ""
nomes_cols<- apply(nomes_cols,1,paste,collapse=" ")

nomes_cols <- nomes_cols[-3]
nomes_cols <- trimws(nomes_cols)

tipologia2018_capa <- tipologia2018_capa[-1:-4,-4]

names(tipologia2018_capa) <- nomes_cols

tipologia2018_demais <-
#  data.table::rbindlist(
    lapply(list.files("dadostat/tipologia_vigente_portaria/",pattern="*.pdf",full.names = T),
           pegatab,pag=16 ,pagf=100,varias=1,col_names=F)


tipologia2018_demais2 <-
  #  data.table::rbindlist(
  lapply(list.files("dadostat/tipologia_vigente_portaria/",pattern="*.pdf",full.names = T),
         pegatab,pag=101 ,pagf=215,varias=1,col_names=F)

tip2018 <-
  data.table::rbindlist(list(tipologia2018_capa,
                             data.table::rbindlist(tipologia2018_demais[[1]]),
                             data.table::rbindlist(tipologia2018_demais2[[1]])),use.names = F,fill=T)

#,use.names = F
  #)
tip2018 <- tip2018|>mutate(across(`TIPOLOGIA SUB REGIONAL`,as_factor))

tip2018 <- municipios|>left_join(tip2018,by= c("name_muni"="MUNICÍPIO","abbrev_state"="UF"))


ggplot(tip2018|>filter(!is.na(`TIPOLOGIA SUB REGIONAL`)))+geom_sf(aes(fill=`TIPOLOGIA SUB REGIONAL`),lwd = 0)+
  annotation_north_arrow(location = "br",which_north = "true",
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_orienteering,width = unit(1.5, "cm"),
                         height = unit(1.5, "cm"))+
  geom_sf(data=regioes_imediatas,fill="#ffffff",alpha=0)+
  ggspatial::annotation_scale(line_width=1,bar_cols=c("white","white"),height=unit(0.001,"cm"),tick_height=2000)+
  scale_fill_discrete(type=c("#e60000","#e34c05","#ffebb0","#98e500","#01724c","#ffcc01","#686868","#e7e7e7","#ffffff","#a1a1a1","#a1a1a1"))+
  theme_classic()+theme(legend.position = "bottom")




###Tipologia 2018 em 2021
#=> em 2018 utilizou 2012 a 2014!!

pibsmunicipais <- RSIDRA::API_SIDRA(5938, cod_nivel="6",periodo = c(2008:2011:12,2018:2021))

#https://apisidra.ibge.gov.br/values/t/5938/n6/all/v/37/p/2008,2009,2010,2011,2019,2020,2021/d/v37%200
metapibsmunicipais <- read_html("https://sidra.ibge.gov.br/tabela/5938#/n6/all/v/37/p/2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021/d/v37%200/l/v,p,t/resultado")

pibsmunicipais <-
  html_table(
    read_html("https://sidra.ibge.gov.br/geratabela?format=html&name=tabela5938.html&terr=N&rank=-&query=t/5938/n6/all/v/37/p/2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021/d/v37%200/l/v,p,t")
  )
obspibsmunicipais <- pibsmunicipais[[2]]
pibsmunicipais <- pibsmunicipais[[1]]

names(pibsmunicipais) <- as.character(pibsmunicipais[3,])

pibsmunicipais <- pibsmunicipais[-1:-3,]
pibsmunicipais <- pibsmunicipais[-nrow(pibsmunicipais),]

pibsmunicipais <-
  pibsmunicipais|>
  separate_wider_regex(Município,c(municipio=".*"," \\(",uf=".*","\\)"))


pibsmicrorregionais <-
  html_table(
    read_html("https://sidra.ibge.gov.br/geratabela?format=html&name=tabela5938.html&terr=N&rank=-&query=t/5938/n9/all/v/37/p/2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021/d/v37%200/l/v,p,t")
  )[[1]]

names(pibsmicrorregionais) <- as.character(pibsmicrorregionais[3,])
pibsmicrorregionais <- pibsmicrorregionais[-1:-3,]
pibsmicrorregionais <- pibsmicrorregionais[-nrow(pibsmicrorregionais),]

pibsmicrorregionais <-
  pibsmicrorregionais|>
  separate_wider_regex(`Microrregião Geográfica`,c(microrregiao=".*"," \\(",uf=".*","\\)"))

f <- tempfile()

##PIB ótica da renda UF
download.file("https://ftp.ibge.gov.br/Contas_Regionais/2021/xls/PIB_Otica_Renda_UF.xls",f)

pibrendaufs <- readxl::read_xls(f,"Tabela1")

regiao_tabela <- \(x=1){
  readxl::read_xls(f,paste0("Tabela",x))$`CONTAS REGIONAIS DO BRASIL - 2010-2021`[6]
}

regioes_ufs <- sapply(1:33,regiao_tabela)

uf_tabela <- \(x=3){
  df <- readxl::read_xls(f,paste0("Tabela",x),skip=8)
  df$UF <- regioes_ufs[x]
  df
}

ufs_pibs <- lapply(c(3:9,11:19,21:24,26:28,30:33),uf_tabela)

ufs_pibs <- data.table::rbindlist(ufs_pibs)

ufs_pibs <- ufs_pibs[-nrow(ufs_pibs),]

percentualiza <- \(x){
  100*as.numeric(x)
}

ufs_pibsarrumado <-
  ufs_pibs|>select(-14:-37)|>
  pivot_longer(2:13,names_to="ano",values_to="valor",names_transform=as.character)|>
  mutate(tipo_valor="valor_absoluto")

ufs_pibsarrumadoperc <-
  ufs_pibs|>select(c(1,14:25,38))|>
  pivot_longer(2:13,names_to="ano",values_to="valor",names_transform=as.character)|>
    mutate(tipo_valor="perc_comp_total_UF")

ufs_pibsarrumadopercbr <-
  ufs_pibs|>select(c(1,26:38))|>
  pivot_longer(2:13,names_to="ano",values_to="valor",names_transform=as.character)|>
  mutate(tipo_valor="perc_comp_perc_c_BR")

ufpib <- data.table::rbindlist(mget(ls(pattern="ufs_pibsarr.*")))

names(ufpib)[1] <- "Componente do PIB"

ufpib$ano <- as.numeric(substr(ufpib$ano,1,4))

download.file("https://ftp.ibge.gov.br/Contas_Regionais/2021/xls/Especiais_2010_2021_xls.zip",paste0(f,324,".zip"))

utils::unzip(paste0(f,324,".zip"),files = "tab03.xls",exdir = "data-raw/")
# libreoffice --headless --convert-to xlsx tab03.xls

ufvolume <- read_xlsx("data-raw/tab03.xlsx",skip=3)
names(ufvolume)[1] <- "uf_regiao"
ufvolume <- ufvolume[-1,]
ufvolume <-
  ufvolume|>
  pivot_longer(-1,names_to="ano",values_to="pib_volume")

##Indice var valor corrente PIB ótica produção
ufindicecorr <- ufpib|>
  filter(grepl("Produção$",`Componente do PIB`),
         tipo_valor=="valor_absoluto")|>
  mutate(indice=100*as.numeric(valor)/as.numeric(lag(valor)))

ufindicecorr[is.na(ufindicecorr)] <- 100


ufdeflator <- ufindicecorr|>select(UF,ano,valor,indice)|>
  left_join(ufvolume|>mutate(across(ano,as.numeric)),by = c("UF" = "uf_regiao","ano"))

ufdeflator$deflator <- ufdeflator$indice/ufdeflator$pib_volume

deflatoruf <-
  ufdeflator|>select(UF,ano,deflator)|>
  pivot_wider(names_from=ano,values_from=deflator)


###Pegar renda domiciliar censo ibge 2010 e 2022
censo10 <- "data/censo/2010"
microdadosBrasil::download_sourceData("CENSO",2010,root_path=censo10)

###find . -type f  -exec bash -c 'mv "$1" "$(iconv -f iso8859-1 -t utf8  <<< $1 | sed 's/Æ/çã/g' - | sed 's/µ/Á/g' - | sed 's/\\\([[:alpha:]]\\\) \\\([[:alpha:]]\\\)/\1á\2/g' - | sed 's/¡/í/g' - | sed 's/£/ú/g' - | sed 's/ä/çõ/g' -)"' -- {} \;

pessoas <- microdadosBrasil::read_CENSO(ft="pessoas",root_path = censo10,i=2010,vars_subset = c("V0001","V0002","V0011","V0010","V5070"))


areas_ponderacao <- read_xls("data/censo/2010/Microdados/Documentacao/Documentação/Áreas de Ponderação/Lista das Áreas de Ponderação.xls")

dicmicrodados <- read_xls("data/censo/2010/Microdados/Documentacao/Documentação/Layout/Layout_microdados_Amostra.xls",skip=1,sheet=2)

dicmicrodados <-
  dicmicrodados|>
  mutate(NOME=gsub("\\\n",";",NOME))

dicmicrodados <-
  dicmicrodados|>
  separate_wider_delim(NOME,";",names=c("nome_var",paste0("resposta_",1:27)),
                       too_few="align_start")|>
  mutate(across(nome_var,str_squish))

dicmicrodados$nome_var <- gsub(":","",dicmicrodados$nome_var)

nomes_vp <- dicmicrodados$nome_var
#names(pessoas) <- dicmicrodados$nome_var

dicmicrodados <-
  dicmicrodados|>
  pivot_longer(contains("resposta_"),names_to="reposta",values_to="cod_trad_resposta")

dicmicrodados <- dicmicrodados|>filter(!is.na(cod_trad_resposta))

dicmicrodados <-
  dicmicrodados|>
  separate_wider_regex(cod_trad_resposta,c(codresposta=".*","-[ ]*",valoresposta=".*"),
                       too_few = "align_start")

dicmicrodados <-
  dicmicrodados|>
  mutate(valoresposta = ifelse(!is.na(valoresposta),valoresposta,codresposta),
         codresposta = ifelse(!is.na(as.numeric(codresposta)),codresposta,""))

#(AM, PA, AP, RR, AC).


pessoas <-
  pessoas|>
  group_by(V0011)|>
  mutate(qtdPessoasAP = n())|>
  ungroup()


library(survey)
design <- svydesign(
  ids = ~1,
  strata = ~V0011,
  fpc = ~qtdPessoasAP,
  weights = ~V0010,
  data = pessoas|>select(-V0001)
)

library(srvyr)
desenho <- srvyr::as_survey_design(design)
pormunicipio <- desenho|>
  srvyr::group_by(V0001,V0002)|>summarize(rendimento_dom_ph <- survey_mean(V5070,na.rm=T))
