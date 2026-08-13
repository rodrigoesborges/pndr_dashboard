##Panorama atual
library(cowplot)

manualhtml <- "dadostat/Painel de Indicadores/Cálculo Painel de Indicadores/Manual_Painel.html"


obj_manual <-
  rvest::read_html(manualhtml)|>
  rvest::html_elements(xpath="//div[@id='objetivos']/ul/li/a")|>
  rvest::html_attr("href")

obj_manual <- gsub("#","",obj_manual)

manualpleg <- \(objetivo=2,lobj=obj_manual) {


  coluna <- rvest::read_html(manualhtml)|>
  rvest::html_elements(xpath=paste0("//div[@id='",lobj[objetivo],"']//h4"))|>
    rvest::html_text()
  coluna=    gsub("\\r\\n","",coluna)
  vezes <- length(coluna)
  df <- data.frame(n_objetivo=rep(objetivo,vezes), codigo=rep(obj_manual[objetivo],vezes),
                   indicador=coluna)|>
    tidyr::separate_wider_regex(indicador,patterns=c(n_indicador="^[^ ]*","[^ ] ",indicador=".*"))
  df
}

indicadores_obj <-
  data.table::rbindlist(lapply(1:4,manualpleg))


obj2 <- readRDS("dadostat/Painel de Indicadores/Cálculo Painel de Indicadores/9_ind_objetivo_2.RDS")
obj2x <- readxl::read_excel("dadostat/Painel de Indicadores/Cálculo Painel de Indicadores/9_ind_objetivo_2.xlsx")

obj2 <- obj2|>
  dplyr::mutate(across(variavel,\(x){gsub("objetivo(.*)_(.*)","\\1.\\2",x)}))

obj2 <-
  obj2|>dplyr::left_join(indicadores_obj,by=c("variavel" = "n_indicador"))

obj2norm <- obj2|>mutate(uf=trunc(codmun/1e4))|>group_by(uf,variavel,ano)|>
  mutate(ma=max(value),normalizado=value/ma)|>group_by(uf,variavel)|>mutate(multiplic=ma/max(ma),normgeral=multiplic*normalizado)


###1) Normalizar pelo maior valor da série inteira (todos anos) o índice de
## centralidade

###1.1) Fazer nova variável = 'multiplicador' da centralidade

juntano <- \(ano=2015,indicador="2.1"){
df <-
    munimapa|>
    mutate(codmun=as.numeric(substr(code_muni,1,6)))|>
    left_join(obj2norm[obj2norm$ano==ano & obj2norm$variavel==indicador,],by="codmun")
df
}


obj2geo <-
  bind_rows(mapply(juntano,ano=rep(2015:2021), indicador=rep(paste0("2.",1:3),7),SIMPLIFY = F,USE.NAMES = T))



##Mapa inicial
regioes <- brazilmaps::get_brmap("Region")

### Escalas baixo médio alto cf. painel de indicadores
bmalto <- function(x) {
  baixo <- 0.25
  alto <- 0.75
  scales::rescale(ifelse(x<alto,
         ifelse(
           x<0.25,0.01,0.499),
         ifelse(is.na(x),NA,0.99)),to=to)
  }


mapa_geral_fixo <-\(regiao,anou){
  ggplot(
    data=cbind(brazilmaps::get_brmap("State"),
               indicador="Primazia Econômica",ano=anou))+
  geom_sf(fill="#f7f7f7",alpha=0.6)+
  geom_sf(data=obj2geo|>filter(grepl("zia Eco",indicador,ano==anou)),
          aes(fill=normgeral))+
    scale_fill_viridis_b(alpha = 0.5,breaks=c(0.25,0.75))+
    scale_y_continuous(position="right")+
    geom_rect(xmin=sf::st_bbox(brazilmaps::get_brmap("State",geo.filter=c("Region"=regiao)))[[1]],
              ymin=sf::st_bbox(brazilmaps::get_brmap("State",geo.filter=c("Region"=regiao)))[[2]],
              xmax=sf::st_bbox(brazilmaps::get_brmap("State",geo.filter=c("Region"=regiao)))[[3]],
              ymax=sf::st_bbox(brazilmaps::get_brmap("State",geo.filter=c("Region"=regiao)))[[4]],
              fill=NA,color="black",size=0.1)+
    ggtitle("Primazia Econômica\nEstadual") +
  theme_minimal()+
    theme(legend.position="none",
          axis.text  = element_blank(),
          panel.grid = element_blank(),
          title = element_text(size=6),
          plot.background = element_rect(color = "black", linewidth = 0.5,fill=NA))
}


plotatudo <- \(i,anou,posy=0.52,deslocx=2,deslocy=2) {

  limites_mapa_maior <-
    sf::st_bbox(brazilmaps::get_brmap("State",geo.filter=c("Region"=i)))

  estreg <-
    unique(municipios$code_state)[grepl(paste0("^",i),unique(municipios$code_state))]

  regim <-
    sf::st_as_sf(
      data.table::rbindlist(
        lapply(estreg,\(x){geobr::read_immediate_region(x)})))

mapaprin <- ggplot(regioes[i,])+
  geom_sf(data=brazilmaps::get_brmap("State",geo.filter=c("Region"=i)),linewidth=0.8)+
  geom_sf(fill="#e7e7e7",alpha=0.6)+
  annotation_north_arrow(location = "br",which_north = "true",
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_orienteering,width = unit(0.7, "cm"),
                         height = unit(0.7, "cm"))+
  ggspatial::annotation_scale(
    line_width=1,bar_cols=c("white","white"),
    height=unit(0.001,"cm"),tick_height=2000)+

  scale_y_continuous(position="right")+
  geom_sf(data=obj2geo|>filter(grepl("entra",indicador),ano ==anou,code_region==i),
          aes(fill=normgeral),linewidth=0.1)+
  theme_minimal()+
  scale_fill_viridis_b(alpha=0.5,limits=c(0,1),
                       breaks=c(0,0.25,0.75,1),labels=c("baixo","médio","alto",""))+
  geom_label(
    data=obj2geo|>filter(code_region==i,ano==anou,grepl("entrali",indicador),
                          !is.na(variavel),!is.na(value))|>
      mutate(centroides=sf::st_centroid(geom),
             lonc=sf::st_coordinates(centroides)[,1],
             latc=sf::st_coordinates(centroides)[,2])|>
      group_by(`Região Imediata`)|>summarize_all(first),
    aes(x=jitter(lonc+deslocx,1),y=latc+jitter(deslocy,1),
        label=gsub(" - ","\n",`Região Imediata`)),fill="#ffffff66",size=8,size.unit="pt",
    label.r=unit(0.5,"lines"),colour="#333333",label.size = 0,fontface="bold")+
  labs(x="",y="",fill="")+
  coord_sf(xlim=c(limites_mapa_maior[1]-7.5,
                  limites_mapa_maior[3]+5),
           ylim=c(limites_mapa_maior[2],
                  limites_mapa_maior[4]+3.75))+
  ggtitle(label=paste("Índice de Centralidade",anou,
                      paste("Região",str_to_title(regioes$desc_rg[i])),sep=" - "),
          subtitle = "Regiões Imediatas Selecionadas")+
  theme(panel.background = element_rect(color = "black", linewidth = 0.5,fill=NA,),
        panel.spacing=margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 3,  # Bottom margin
                             l = 0,  # Left margin
                             unit = "cm"),legend.position="right",
        legend.text = element_text(vjust=-1.5))




if (anou==2021) {
  ggdraw(mapaprin)+
    draw_plot(mapa_geral_fixo(i,anou),x=0.07,y=posy,width=0.27,height=0.17)
} else {
  ggdraw(mapaprin+
           theme(legend.key = element_rect(fill = "white",linewidth=0),
                 legend.box = element_blank(),
                 legend.text = element_text(color = "white"),
                 legend.title = element_text(color = "white"))+
           guides(fill=guide_legend(override.aes = list(color = NA,fill = NA))))+
  draw_plot(mapa_geral_fixo(i,anou),x=0.07,y=posy,width=0.27,height=0.17)


}
}

plotamapa <- \(i,p=0.52,deslocx=2,deslocy=2) {
mapas1 <- lapply(c(2015,2021),\(x){plotatudo(i,x,p,deslocx,deslocy)})
plot_grid(mapas1[[1]],mapas1[[2]])
}

plotamapa(1,p=0.5,deslocx=2,deslocy=1)

plotamapa(2,p=0.565,deslocx=0.6,deslocy=0.6)

plotamapa(3,p=0.5,deslocx=1.3,deslocy=0.55)

plotamapa(4,p=0.55,deslocx=1.6,deslocy=0.9)

plotamapa(5,p=0.55,deslocx=2.8,deslocy=1)
