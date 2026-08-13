library(tidyverse)
library(sf)
library(geobr)
library(RSIDRA)
library(ipeadatar)


##################

consolida_tr_funcao <- \(ano){

  dados <- data.table::rbindlist(lapply(1:12,\(x) download_transferencias_uniao(ano,x)|>dplyr::mutate(ano=trunc(ano_mes/100))))

  dados|>
    dplyr::mutate(privadopub=ifelse(grepl("Privadas",nome_modalidade_aplicacao_despesa),'inst_privadas','publico'))|>
    dplyr::group_by(ano,codigo_ibge,tipo_transferencia,privadopub,nome_funcao)|>
    dplyr::summarize(valor_transferido=sum(valor_transferido,na.rm=T))|>
    tidyr::pivot_wider(names_from=c('tipo_transferencia','privadopub'),values_from='valor_transferido',values_fill = 0)
  #|>
  #dplyr::rename('governo_e_publico'='FALSE','privadas'='TRUE')

}



transfpubs_14_24 <-
  data.table::rbindlist(lapply(2014:2024,\(x) consolida_tr_funcao(x)))


names(transfpubs_14_24)[4:5] <- c('governo_e_publico','privadas')

transfpubswide <- transfpubs_14_24|>
  dplyr::select(-privadas)|>
  tidyr::pivot_wider(names_from=nome_funcao,values_from=governo_e_publico,values_fill=0)|>
  dplyr::mutate(transftotal=rowSums(dplyr::across(-c(ano,codigo_ibge))))


gastostrib <- "https://www.gov.br/receitafederal/pt-br/centrais-de-conteudo/publicacoes/relatorios/renuncia/gastos-tributarios-bases-efetivas/dgt-bases-efetivas-2022-serie-2020-a-2025-quadros.xlsx/@@download/file"
gtrib <- tempfile(fileext = ".xlsx")
download.file(gastostrib,gtrib,method="wget",extra = "--no-check-certificate")
gtrip <- readxl::read_xlsx(gtrib,skip=4)

##################

user="aedi"
dbname="aedidb"
dbpass="aEd1#man@gR"
dbhost="38.242.154.34"
dbport=5432

aedidb <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                 user=user,password=dbpass,dbname=dbname,host=dbhost)


massa_salarial <- DBI::dbGetQuery(aedidb,"select refdate ano,geoloc_id codigo_ibge,value massa_salarial from data_values a left join local b on a.local_id = b.local_id  where mdata_id = 89")

massa_salarial$ano <- year(massa_salarial$ano)

massa_salarial$massa_salarial <- massa_salarial$massa_salarial/1e3

pop_municipios <- DBI::dbGetQuery(aedidb,"select refdate ano,geoloc_id codigo_ibge,value populacao from data_values a left join local b on a.local_id = b.local_id  where mdata_id = 66")

# download.file("https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2025/estimativa_dou_2025.xls","data/estimativa_dou_2025.xls")
# pop2024 <- readxl::read_excel("data/estimativa_dou_2025.xls",skip=1,sheet=2)
# pop2024$codigo_ibge <- as.numeric(paste0(pop2024$`COD. UF`,pop2024$`COD. MUNIC`))
# pop2024 <- pop2024[!is.na(pop2024$codigo_ibge),]
#
# pop_municipios <- pop_municipios|>bind_rows(pop2024|>transmute(ano=as.Date('2024-07-01'),codigo_ibge,populacao=`POPULAÇÃO ESTIMADA`))

pibsmunicipais <-
  data.table::rbindlist(
    lapply(
      2014:2023,\(x) RSIDRA::API_SIDRA(5938,nivel=6,periodo = as.character(x),variavel =  37)|>
        transmute(ano=as.numeric(Ano),codigo_ibge= `Município (Código)`,
                  unidade=`Unidade de Medida`,pib=as.numeric(Valor))))



pibextrapolado <- left_join(massa_salarial|>mutate(ano=year(ano)),pibsmunicipais)|>
  mutate(pib_massa_salarial=pib/massa_salarial)

fator_municipio <- pibextrapolado|>filter(ano>2017,ano<2022)|>group_by(codigo_ibge)|>
  summarize(fator_municipio=min(pib_massa_salarial))

pibextrapolado <-
  pibextrapolado|>left_join(fator_municipio)

pibextrapolado <-
  pibextrapolado|>
  mutate(
    fator_municipio = case_when(
      ano < 2022 ~ pib_massa_salarial,
      T ~ fator_municipio))|>
  group_by(codigo_ibge)|>
  mutate(
    pib  = case_when(
      ano < 2022 ~ pib,
      T ~ max(massa_salarial)*fator_municipio*(1.02^(ano-2022))
    )
  )


igpm <- ipeadatar::ipeadata("IGP_IGPMG",language="br")

igpm <- igpm|>mutate(indice_igpm=100*cumprod(1+value/100))|>transmute(ano=year(date),indice_igpm=indice_igpm*100/dplyr::nth(indice_igpm,-1))

mapabrasil <- geobr::read_municipality(year=2020)


estadosbr <- geobr::read_state(year=2020)

mapabrasil <- mapabrasil|>
  dplyr::left_join(
    bind_rows(transfpubs_14_24|>
                mutate(total=(governo_e_publico+privadas)/1e3),
              transfpubswide|>select(ano,codigo_ibge,transftotal)|>pivot_longer(-c(ano,codigo_ibge),values_to="total",names_to="nome_funcao")|>
                dplyr::mutate(governo_e_publico=NA,privadas=NA,total=total/1e3)
    ),by=c("code_muni"="codigo_ibge"))|>
  left_join(pibextrapolado,by=c("code_muni"="codigo_ibge","ano"))|>
  left_join(pop_municipios|>mutate(ano=year(ano)),by=c("code_muni"="codigo_ibge","ano"))|>
  left_join(igpm)|>
  mutate(transfpib=total/pib,transf_massa_sal=total/massa_salarial,transfpc=total*1e3*100/indice_igpm/populacao)



plotatransf <- \(anop=2019){
ggplot(mapabrasil|>filter(ano==anop,nome_funcao=="transftotal"),aes(fill=transfpc) )+
  geom_sf(linewidth=0.05, color="white")+
  geom_sf(data=estadosbr,linewidth=0.2, color="grey",fill=NA)+
  theme_minimal()+
  labs(fill="Transferências p.c.",
       title = paste0('Recursos Transferidos para Municípios \nper capita - ', anop,' - (em R$ de 2024)'))+
  scale_fill_viridis_b(alpha = 0.5,breaks=c(1,2,3,5,8,13)*1e3,
                       labels = scales::number_format(big.mark=".",decimal.mark=",",accuracy = 0.01,prefix="R$ ",suffix="/pc"))
}

lista_mapas_transf <- map(2014:2024,plotatransf)

library(cowplot)
cowplot::plot_grid(plotlist = lista_mapas_transf,nrow=4,ncol=3,labels="AUTO")

library(gganimate)
library(gifski)
library(av)

pgga <- ggplot(mapabrasil|>filter(nome_funcao=="transftotal"),aes(fill=transfpc) )+
  geom_sf(linewidth=0.05, color="white")+
  geom_sf(data=estadosbr,linewidth=0.2, color="grey",fill=NA)+
  coord_sf()+
  theme_minimal()+
  labs(fill="Transferências p.c.",
       title = paste0('Recursos Transferidos para Municípios \nper capita - ano: {closest_state} - (em R$ de 2024)'))+
  scale_fill_viridis_b(alpha = 0.5,breaks=c(1,2,3,5,8,13)*1e3,
                       labels = scales::number_format(big.mark=".",decimal.mark=",",accuracy = 0.01,prefix="R$ ",suffix="/pc"))

# 2. Tell gganimate which column holds the time dimension
anim <- pgga +
  transition_states(ano, transition_length = 1, state_length = 2) +
  enter_fade() +
  exit_fade()
# 3. Render
# 3a. GIF
animate(anim,
        fps        = 5,          # 5 frames/sec → 2 s total
        width      = 800,
        height     = 500,
        renderer   = gifski_renderer("mapa_anos.gif"))

# 3b. MP4 (H.264, plays everywhere)
animate(anim,
        fps        = 5,
        width      = 800,
        height     = 500,
        renderer   = av_renderer("map_years.mp4"))


# +
#   scale_fill_viridis_b(alpha = 0.5,breaks=c(0.05,0.10,0.3,0.45))


ggplot(mapabrasil|>filter(ano==2021,nome_funcao=="transftotal"),aes(fill=transfpib) )+
  geom_sf()+theme_minimal()+
  scale_fill_viridis_b(alpha = 0.5,breaks=c(0.05,0.10,0.3,0.45))+
  labs(fill="Transferências (% PIB)")
