library(data.table)
malhacenso <-
  fread('coleta/cache/censo2022/BR_setores_CD2022.csv')

basicosetores <-
  fread('coleta/cache/censo2022/Agregados_por_setores_basico_BR_20250417.csv')

caracdomsetores1 <-
  fread('coleta/cache/censo2022/Agregados_por_setores_caracteristicas_domicilio1_BR.csv')

caracdomsetores2 <-
  fread('coleta/cache/censo2022/Agregados_por_setores_caracteristicas_domicilio2_BR_20250417.csv')

caracdomsetores3 <-
  fread('coleta/cache/censo2022/Agregados_por_setores_caracteristicas_domicilio3_BR_20250417.csv')



setores_pess_dom <- basicosetores[,.(CD_SETOR,CD_MUN,SITUACAO,v0001,v0002,v0003,v0007)]

dom1geral <- caracdomsetores1[,.(CD_setor,V00001,V00002,V00003,V00004,V00005,V00006,V00007)]
colforca <- names(dom1geral)[sapply(dom1geral,is.character)]
dom1geral <- dom1geral[,(colforca):= lapply(.SD,as.integer),.SDcols = colforca]

#dom1geral <- dom1geral,CD_setor :=
acessoaguadom <- caracdomsetores2[,.(setor, V00111,V00464)]

infoagua <- setores_pess_dom[acessoaguadom,on = list(CD_SETOR = setor)]
names(infoagua)[-1:-3] <- c(
  'total_pessoas',
  'total_dom',
  'total_dpp',
  'total_dppo',
  'dppo_acesso_rede_geral_agua',
  'dppo_sem_acesso_rede_geral_agua'
)

infoagua <- infoagua[dom1geral,on = list(CD_SETOR = CD_setor)]

infoagua[,(names(infoagua)[8:9]):= lapply(.SD,as.integer),.SDcols = names(infoagua)[8:9]]

infoagua[,`:=` ("pessoas_dpo_acesso_rede_geral"=round(dppo_acesso_rede_geral_agua*V00005/V00004,0),
                  "pessoas_dpo_sem_aguageral" = round(dppo_sem_acesso_rede_geral_agua*V00005/V00004,0))]

somasna <- \(x){sum(x,na.rm=T)}

infoagua <- infoagua[SITUACAO == 'Urbana',lapply(.SD,\(x){sum(x,na.rm=T)}),by=CD_MUN,.SDcols = names(infoagua)[c(13,14,17,18)]]

infoagua[,atendimento_agua_urbana:=100*pessoas_dpo_acesso_rede_geral/V00005]
infoagua[,refdate:= as.Date('2022-12-31')]
infra1 <-infoagua[,.(refdate,CD_MUN,atendimento_agua_urbana)]

names(infra1)[2:3] <- c('geoloc_id','infra1')

infra1[,geoloc_id:=as.numeric(geoloc_id)]

#https://indicadores-sinisa-2025.cidades.gov.br/f9486ad8-9676-4ef0-8f73-11157f8fd3eb

infragua23 <- readxl::read_excel('coleta/cache/sinisa2025/relatorio_indicadores_Brasil.xlsx',sheet=2,skip=5)

infragua23 <-
  infragua23|>
  dplyr::select(Cod_IBGE,IAG0002)|>
  dplyr::filter(Cod_IBGE>999999)|>
  dplyr::transmute(refdate=as.Date('2023-12-31'),geoloc_id=Cod_IBGE,infra1=
                     as.numeric(gsub(",",".",IAG0002)))


compara_censo_sinisa <- infra1|>dplyr::select(-refdate)|>
  dplyr::left_join(infragua23|>dplyr::select(-refdate),by="geoloc_id")

infra1_aedi <- rbind(infra1,infragua23)

