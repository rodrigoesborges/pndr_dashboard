##instrumentos de financiamento
library(ckanr)
library(scriptName)
library(tidyverse)
library(rapiclient)
library(XML)
library(ggspatial)
library(sf)
library(ggthemes)
library(cowplot)

##Mapa inicial
regioes <- brazilmaps::get_brmap("Region")


fonte <- "https://dadosabertos.mdr.gov.br/"

ckanr_setup(fonte)



datasets <- tribble(
  ~dataset,
  "fundos-constitucionais-de-financiamento",
  "tci")

fcf <- package_show(datasets[1,]$dataset)

fcfids <- sapply(1:fcf$num_resources,\(x) {fcf$resources[[x]]$id})

fcfurls <- sapply(1:fcf$num_resources,\(x) {fcf$resources[[x]]$url})


mdrda <- "dadostat/mdr_dadosabertos/"
narqfcfs <- URLdecode(gsub(".*/([^/]*)$","\\1",fcfurls))


download.file(fcfurls,paste0(mdrda,narqfcfs),method="libcurl")

# ##trace edit CKAN_VERB para ssl.verifypeer = FALSE
#package_show(datasets[1,]$dataset)
# datasets <- ckanr::package_list()
#

lapply(narqfcfs[1:5],\(x){
  system(paste0("cd ",mdrda," && iconv -f cp1252 -t utf8 ",x," -o utf8",x))
})


fcfdata <- data.table::rbindlist(lapply(paste0(mdrda,"utf8",narqfcfs)[1:5],readr::read_csv2),fill=T)

fcf2024 <- readODS::read_ods(paste0(mdrda,narqfcfs[6]))

fcfdata <- data.table::rbindlist(list(fcf2024,fcfdata[,1:15]),use.names = F,fill=T)

fcfdata[is.na(fcfdata$num_ano),]$num_ano <- 2023

fcfan <- fcfdata|>group_by(cod_municipio,num_ano)|>summarize(across(vlr_contrato,sum,na.rm=T))







##Populações municipais TCU
"https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2024/estimativa_dou_2024.ods"

tcupopurl <- \(i){
  sprintf("https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_%2d/estimativa_dou_%2d.ods",i,i)
}


popurls <- sapply(c(2019,2020,2021,2024),tcupopurl)

download.file(popurls,paste0("dadostat/ibge/tcupop/",gsub(".*/([^/]*)$","\\1",popurls)),method="libcurl")

##2023
download.file(
  "https://ftp.ibge.gov.br/Informacoes_Gerais_e_Referencia/Relacao_da_Populacao_dos_Municipios_para_publicacao_no_DOU_em_2023/POP_DOU_2023_Municipios_POP2022_Malha2023.ods",
  "dadostat/ibge/tcupop/estimativa_dou_2023.ods"
)
#2022
download.file(
  "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Previa_da_Populacao/POP2022_Municipios_20230622.xls",
  "dadostat/ibge/tcupop/POP2022_Municipios_20230622.xls"
)
##2022 retirada de sobrescritos notas de rodapé
ajustapop22 <-
  readxl::read_xls("dadostat/ibge/tcupop/POP2022_Municipios_20230622.xls",skip=1)

ajustapop22$POPULAÇÃO <- as.integer(gsub("\\(.*\\)","",ajustapop22$POPULAÇÃO))

readODS::write_ods(ajustapop22, "dadostat/ibge/tcupop/estimativa_censo_2022.ods")


popmun_of <- \(x) {
  ano <- gsub("[^0-9]","",x)
  if (ano == 2022) {
  df <- readODS::read_ods(x,range="A1:E5572")
  } else if (ano==2023) {
    df <- readODS::read_ods(x,range="A2:E5572")
  } else if (ano!=2020){
    df <- readODS::read_ods(x,sheet=2,range="A2:E5572")
  } else {
    df <- readODS::read_ods(x,sheet=2,range="A1:E5572")
  }
  df[[ncol(df)]] <- as.integer(trimws(gsub("\\.","",gsub("\\(.*\\)","",df[[ncol(df)]]))))
  df$ano <- as.numeric(ano)

  df
}
pmarq <- list.files("dadostat/ibge/tcupop","^estimativa.*ods",full.names = T)
popmuns <- data.table::rbindlist(lapply(pmarq,popmun_of),use.names = F)

popmunsaj <-
  popmuns|>
  transmute(ano,
            cod_municipio=as.numeric(paste0(`COD. UF`,`COD. MUNIC`)),
            populacao=as.numeric(POPULAÇÃO))

financ_mun <- fcfan|>rename(ano=num_ano)|>
  left_join(popmunsaj
   )
pibsmunicipais <-
  data.table::rbindlist(
    lapply(
      2019:2021,\(x) RSIDRA::API_SIDRA(5938,nivel=6,periodo = as.character(x),variavel =  37)))


ipeadatar::search_series("IGP-M")

igpm <- ipeadatar::ipeadata("IGP12_IGPM12")

igpm$value <- igpm$value*100/igpm[igpm$date=='2024-10-01',]$value

igpm <- igpm|>filter((month(date)==12 & year(date)>2017 ) | date==last(date))|>
  transmute(ano=year(date),inflacao_i=value)

financ_mun <-
  financ_mun|>
  left_join(igpm,by="ano")

financ_mun <-
  financ_mun|>
  left_join(pibsmunicipais|>
              transmute(ano=`Ano (Código)`,
                        cod_municipio=`Município (Código)`,
                        pib_milhoes=as.numeric(Valor)/1e3))


financ_mun$vlr_real <- financ_mun$vlr_contrato*100/financ_mun$inflacao_i

financ_mun$pib_real <- financ_mun$pib_milhoes*100/financ_mun$inflacao_i

financ_mun$credpc <- financ_mun$vlr_real/financ_mun$populacao
financ_mun$pci <- financ_mun$cod_municipio %in% unique(munimapa$code_muni)

financ_mun$credppib <- financ_mun$vlr_contrato/(1e6*financ_mun$pib_milhoes)

financ_mun|>filter(ano<2022)|>
  group_by(ano,pci)|>summarize(
    partic_credito=sum(vlr_real),
    credito_pib = sum(vlr_contrato)/sum(pib_milhoes)/1e6,
    pibpc = sum(pib_real)*1e6/sum(populacao)
  )|>
  mutate(partic_credito=partic_credito/sum(partic_credito))|>
  group_by(pci)|>
  mutate(crecpibpc=100*(pibpc/lag(pibpc)-1))


vtotal_fcf <- financ_mun|>
  group_by(ano)|>
  summarize(across(vlr_real,sum))|>
  mutate(vlr_real=vlr_real/1e6,
         cresc=ifelse(ano!=2024,
                      100*(vlr_real/lag(vlr_real)-1),
                      100*(4*vlr_real/lag(vlr_real*3)-1)))
fcf_crescmedio <-
  100*((last(vtotal_fcf$vlr_real)/first(vtotal_fcf$vlr_real))^(1/5)-1)



vtotal_pci <- financ_mun|>filter(pci)|>
  group_by(ano)|>
  summarize(across(vlr_real,sum))|>
  mutate(vlr_real=vlr_real/1e6,
         cresc=ifelse(ano!=2024,
                      100*(vlr_real/lag(vlr_real)-1),
                      100*(4*vlr_real/lag(vlr_real*3)-1)))

pcifcf_crescmedio <-
  100*((last(vtotal_pci$vlr_real)/first(vtotal_pci$vlr_real))^(1/5)-1)


financ_mun <- financ_mun|>
  group_by(ano)|>
  mutate(anocpc=sum(vlr_real,na.rm=T)/sum(populacao,na.rm=T))
financ_mun <- ungroup(financ_mun)
financ_mun <- group_by(financ_mun,uf=substr(cod_municipio,1,2))
financ_mun$indcpc <- financ_mun$credpc*100/financ_mun$anocpc

financ_mun <- financ_mun|>filter(!is.na(cod_municipio))

financ_mun <- ungroup(financ_mun)
fmgeo <- munimapa|>right_join(financ_mun,by=c("code_muni"="cod_municipio"))

ufmapa <- brazilmaps::get_brmap("State")

fmgeouf <- ufmapa|>transmute(code_state=State,geometry)|>
  right_join(sf::st_drop_geometry(fmgeo))

fmgeose <-
  fmgeo|>
  ungroup()|>
  group_by(ano,`Região Imediata`)|>
  summarize(min=min(indcpc,na.rm=T),max=max(indcpc,na.rm=T),
            media=mean(indcpc,na.rm=T),
            mediana=median(indcpc,na.rm=T),
            perc_sobremedia = 100*(1-ecdf(indcpc)(100)),lonc=first(lonc),latc=first(latc),
            code_region=first(code_region))|>
  mutate(across(where(is.numeric) & contains("m"),\(x){round(x,1)}))|>ungroup()


plotafcf <- \(regiao=1,anno=2019,zooma=F,eh=0.2,ocaj=1.5,acaj=0.8) {
  mapa_estados <-
    brazilmaps::get_brmap("State",geo.filter=list("Region" = regiao)) |>
    st_transform(st_crs(4326))

  basemmm <- fmgeo|>
    filter(ano == anno,code_region==regiao,!is.na(name_region))


  basere <- fmgeose|>filter(ano ==anno,code_region==regiao)


    gq <- \(x=1) {
      geom_rect(fill=NA,color="black",size=0.1,
                   xmin=st_bbox(basere[x,])[[1]],
                   xmax=st_bbox(basere[x,])[[3]],
                   ymin=st_bbox(basere[x,])[[2]],
                   ymax=st_bbox(basere[x,])[[4]])
    }
    geraquad <- \(){
      lapply(1:nrow(fmgeose),gq)
    }



  nomeregeo <- munimapa|>filter(code_region== regiao)|>
    mutate(`Região Imediata`=gsub(" - ","/\\\n",`Região Imediata`))|>
    group_by(`Região Imediata`)|>summarize_all(first)

  limites <- \(x=zooma) {
    a <- sf::st_bbox(basere)
    if (x) {
    a[1] <- a[1]-eh
    a[3] <- a[3]+eh

    } else {
      a[1] <- a[1]-40
      a[3] <- a[3]+40
      a[2] <- a[2]-20
      a[4] <- a[4]+20
    }
    a
  }
    posleg <- ifelse(zooma,"bottom","none")

    aceiling <- \(x){
      a <- floor(log10(x))
      10^a*ceiling(x/(10^a))
    }

    quebras <- c(0,
      aceiling(min(basere$min)*4),
      aceiling(mean(basere$media)),
      aceiling(max(basere$max)/4),
      aceiling(max(basere$max)/2),
      aceiling(max(basere$max)))

  zoomapa <- \(x=zooma){
    if (!x) {
      ggplot(mapa_estados)+
        geom_sf(fill="#fafafa")+
        geom_sf(data=munimapa|>filter(code_region==regiao),stroke=0.2,linetype="dashed")+
        geraquad()
    } else {
      ggplot(fmgeose,aes(fill=fct(`Região Imediata`)))+
        geom_sf(data=munimapa|>filter(code_region==regiao),stroke=0.2,linetype="dashed")+
        ggtitle(paste0("Volume de Crédito Per Capita - Cidades Selecionadas - ","Região ",
                       str_to_title(regioes[regioes$Region==regiao,]$desc_rg)," - ",anno),
                subtitle="(Índice - média da UF = 100)")+
        geom_label(data=basere|>mutate(across(where(is.numeric) & contains("m"),\(x){formatC(x,decimal.mark = ",",big.mark=".")})),
                   aes(label=paste0("min:",min,"\nmax: ",max,"\nmédia: " ,media,"\nQtd % > 100: ",perc_sobremedia),
                       x=lonc+ocaj,y=latc-acaj),size=2.5,label.size=0.05,size.unit="mm",
                   alpha = .5, color = alpha('black', .5))+
        annotation_north_arrow(location = "br",which_north = "true",
                               pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                               style = north_arrow_orienteering,width = unit(0.7, "cm"),
                               height = unit(0.7, "cm"))+
        ggspatial::annotation_scale(
          line_width=1,bar_cols=c("white","white"),
          height=unit(0.001,"cm"),tick_height=2000)+
        coord_sf(
          xlim=limites()[c(1,3)],
          ylim=limites()[c(2,4)],
          expand=F
        )

    }
  }

  zoomapa()+
    geom_point(data=basemmm,
      aes(x=lonc,y=latc,size=indcpc),
      alpha=0.1,fill="blue",pch=21)+
    scale_size_continuous(breaks=quebras)+
    geom_text(
      data=nomeregeo,
      aes(x=jitter(lonc,1),y=latc+jitter(0,1),label=`Região Imediata`),size=7,size.unit="pt")+
    theme_map()+theme(strip.background = element_blank(),legend.position=posleg,
                      legend.justification = "center",
                      legend.title=element_text(size=unit(12,"pt")),
                      legend.text = element_text(size=unit(7,"pt")))+
    labs(x="",y="",size="Crédito contratado per capita \n(% da média UF)",fill="Região I.")

}





#Região Norte
nortejunto <-
  plot_grid(ggdraw(plotafcf(zooma=T,eh=2.8,ocaj=3.3,acaj=-0.43))+draw_plot(plotafcf(zooma=F),width=0.27,height = 0.29,y=0.25,x=0.4),
           ggdraw(plotafcf(anno=2023,zooma=T,eh=2.8,ocaj=3.3,acaj=-0.43))+draw_plot(plotafcf(anno=2023,zooma=F),width=0.27,height = 0.29,y=0.25,x=0.4),
           ncol=1)


##nordeste
plot_grid(
  ggdraw(plotafcf(regiao=2,zooma=T,eh=1.7))+draw_plot(plotafcf(regiao=2,zooma=F),width=0.35,height = 0.35,y=0.34,x=0.31),
  ggdraw(plotafcf(anno=2023,regiao=2,zooma=T,eh=1.7))+draw_plot(plotafcf(anno=2023,regiao=2,zooma=F),width=0.35,height = 0.35,y=0.34,x=0.31),
  ncol=1)

##sudeste
plot_grid(
  ggdraw(plotafcf(regiao=3,zooma=T,eh=1,ocaj=0.8,acaj=0.2))+draw_plot(plotafcf(regiao=3,zooma=F),width=0.4,height = 0.4,y=0.13,x=0.25),
  ggdraw(plotafcf(anno=2023,regiao=3,zooma=T,eh=1,ocaj=0.8,acaj=0.2))+draw_plot(plotafcf(anno=2023,regiao=3,zooma=F),width=0.4,height = 0.4,y=0.13,x=0.25),
  ncol=1)


##centroeste
plot_grid(
  ggdraw(plotafcf(regiao=5,zooma=T,eh=1.6,ocaj=1.7,acaj=-0.2))+draw_plot(plotafcf(regiao=5,zooma=F),width=0.5,height = 0.5,y=0.23,x=0.28),
  ggdraw(plotafcf(anno=2023,regiao=5,zooma=T,eh=1.7,ocaj=1.6,acaj=-0.2))+draw_plot(plotafcf(anno=2023,regiao=5,zooma=F),width=0.5,height = 0.5,y=0.23,x=0.28),
  ncol=1)

save(list=c("financ_mun","vtotal_fcf","vtotal_pci","fcf_crescmedio","pcifcf_crescmedio"),file="dev/2024-09-Produto-1/data/fcfdata.rda")

#
# fonte <- "https://dados.gov.br/"
#
#
#
#
#   dadosapi <- "https://dados.gov.br/v3/api-docs"
#
# dadosgov_api <- get_api(dadosapi,ext="json")
#
# operations <- get_operations(dadosgov_api)

###FNE
sudene_da <- "http://ftp.sudene.gov.br"
fne_da <- "/dados_abertos/FNE"

arqsfne <- rvest::read_html(paste0(sudene_da,fne_da))|>
  rvest::html_elements(xpath="//a")|>
  rvest::html_attr("href")
##Filtra apenas ods
arqsfne <- arqsfne[grepl(".ods",arqsfne)]

destino <- "dadostat/financiamento/fundos/fne"
narqfne <- URLdecode(gsub(".*/([^/]*)$","\\1",arqsfne))

dnfne <- paste0(destino,"/",narqfne)

download.file(paste0(sudene_da,arqsfne),dnfne,method="libcurl")



