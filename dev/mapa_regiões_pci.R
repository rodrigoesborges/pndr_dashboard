library(geobr)
library(tidyverse)
library(ggspatial)
library(ggthemes)
regioes_imediatas <- geobr::read_immediate_region()

municipios <- geobr::read_municipality(year=2022)

regioes_imediatas <- geobr::read_immediate_region(year=2020)
#Fontes 5449360 - minuta de resolução do programa PCI
#https://www.gov.br/secom/pt-br/assuntos/noticias/2024/09/governo-federal-lanca-programa-cidades-intermediadoras-para-reduzir-desigualdades-regionais
pcicid <- readODS::read_ods("dadostat/PCI-cidades_ri_intermediarias.ods")

# pcicid <-
#   pcicid|>
#   dplyr::mutate(across(Município,stringr::str_to_title))|>
#   dplyr::left_join(municipios|>
#               dplyr::select(code_muni,name_muni,abbrev_state)|>
#                 dplyr::mutate(across(name_muni,stringr::str_to_title)),
#             by=c("Município" = "name_muni", "UF" = "abbrev_state"))

munimapa <- municipios|>mutate(across(name_muni,str_to_title))|>
  left_join(pcicid|>mutate(across(Município,str_to_title)),
              by=c( "name_muni"="Município" , "abbrev_state"="UF"))|>
  filter(!is.na(`Região Imediata`))|>
           mutate(centroides=sf::st_centroid(geom),
                  lonc=sf::st_coordinates(centroides)[,1],
                  latc=sf::st_coordinates(centroides)[,2])

muniri <- munimapa|>
  rowwise()|> filter(grepl(name_muni,str_to_title(`Região Imediata`),ignore.case=T))|>ungroup()
#munimapa[is.na(munimapa$cor_principal),]$cor_principal <-"#eff2ef"

mapa_pci <- ggplot(data=brazilmaps::get_brmap(geo=c("State")),aes())+
  scale_y_continuous(position="right")+
  geom_sf(fill="#e7e7e7",alpha=0.6)+
  annotation_north_arrow(location = "br",which_north = "true",
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_orienteering,width = unit(1.5, "cm"),
                         height = unit(1.5, "cm"))+
  ggspatial::annotation_scale(line_width=1,bar_cols=c("white","white"),height=unit(0.001,"cm"),tick_height=2000)+
  geom_sf(data=munimapa,fill=munimapa$cor_principal)+
  geom_sf(data=muniri,aes(fill=cor_principal))+
  geom_text(data=muniri|>group_by(`Região Imediata`)|>summarize_all(first)|>arrange(code_region,`Região Imediata`)|>
              mutate(nmapa=sprintf("%02d",1:n())),aes(x=lonc,y=latc-1,label=nmapa))+
  labs(x="",y="",fill="Região Imediata - PCI")+
  scale_fill_discrete(
    labels=
      (muniri|>group_by(`Região Imediata`)|>summarize_all(first)|>arrange(code_region,`Região Imediata`)|>
         mutate(nmapa=sprintf("%02d",1:n()),regleg=paste0(nmapa,") ",`Região Imediata`)))$regleg)+
  theme_classic()+theme(legend.position = "bottom")

