cgpac1l <- "https://www.in.gov.br/en/web/dou/-/resolucao-cgpac-n-1-de-19-de-dezembro-de-2023-532316160"


novopac_leva1 <- data.table::rbindlist(rvest::html_table(
  rvest::read_html(cgpac1l)
),use.names=F,fill=T)


novopac_leva1 <- novopac_leva1[stats::complete.cases(novopac_leva1),]

names(novopac_leva1) <- as.character(novopac_leva1[1,])
novopac_leva1 <- novopac_leva1[-1,]
novopac_leva1 <-
  novopac_leva1|>
  filter(Localização!="")

total_obrasl1 <- nrow(novopac_leva1)

novopac_leva1 <-
  novopac_leva1|>
  mutate(across(Localização,\(x){gsub("/([^/]*)/([^/]{2}$)","\\2;\\1/\\2", x)}))|>
  separate_longer_delim(Localização,"\\n")|>
  separate_longer_delim(Localização,";")

novopac_leva1 <-
  novopac_leva1|>
  mutate(across(Localização,str_squish))

diretomun <- novopac_leva1|>filter(grepl("/",Localização))|>
  count(Localização)|>
  arrange(desc(n))


 diretomun$Localização <-
   gsub("/AL ","",diretomun$Localização)

## Contorna problema de dois municípios indicados
 doisdm <- diretomun|>dplyr::filter(grepl(".*/.*/",Localização))|>tidyr::separate_wider_delim(Localização,"/",names=c("municipio1","municipio2","uf"))|>mutate(municipio2=gsub("[A-Z]{2} ","",municipio2))|>tidyr::pivot_longer(-c("uf","n"),names_to = "tm",values_to="municipio")|>select("municipio","uf","n")


diretomun <-
  diretomun[!grepl(".*/.*/",diretomun$Localização),]|>
  tidyr::separate_wider_delim(Localização,"/",names = c("municipio","uf"))|>
  bind_rows(doisdm)

obras_dirmun <- sum(diretomun$n)

diretomun <- diretomun|>left_join(pcicid|>select(UF,`Região Imediata`,Município),
                                  by=c("municipio"= "Município", "uf" = "UF"))

tabela_novopac <- diretomun|>
  group_by(!is.na(`Região Imediata`))|>
  summarize(obras_diretas=sum(n))|>mutate(proporcao=prop.table(obras_diretas))|>
  select(-1)|>mutate(
    PCIv1=c("Não","Sim"),
    cidades=c(5570-nrow(pcicid),nrow(pcicid)))|>
  mutate(propcidades=prop.table(cidades),
         prop_obras_cidades=proporcao/propcidades)|>
  arrange(desc(PCIv1))|>
  select(PCIv1,obras_diretas,proporcao,cidades,propcidades,prop_obras_cidades)

write_csv2(tabela_novopac,"dev/2024-09-Produto-1/data/tabelanovopac.csv")

