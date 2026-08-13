### Cria indexes nas tabelas de vínculos do banco postgresql da RAIS criado

# userais="mte_rais"
# passwordrais="aEd1#man@gRpublicrais"
# hostprais="38.242.154.34"

# con <-
#   DBI::dbConnect(  RPostgreSQL::PostgreSQL(), dbname=unique( catalog[ , "dbfile" ] )[[1]], user=userais,password=passwordrais,
#                    host=hostprais,port=5432     )
#
#
# #### recálculo indicador de centralidade
#
# vinculos_ano_setor <- \(ano) {
#   a <- dbGetQuery(con,paste0("SELECT municipio, cnae_2_0_classe setor,  COUNT(*) qtd_vinc from rais_vinculo_",ano,
#                       " GROUP BY municipio, setor"))
#   a$ano <-  ano
#   a
# }
#
#
# #emprego_por_setormun <- vinculos_ano_setor(2014)
#
# emprego_por_cnae_mun <- data.table::rbindlist(lapply(2013:2023,vinculos_ano_setor))


objetivo4_3_aedi <- emprego_por_cnae_mun|>

  dplyr::mutate(
         uf = trunc(municipio/10000),
         setor = trunc(setor/1000))|>
  dplyr::group_by(municipio, ano, setor, uf) |>
  dplyr::summarise(vinc_setor = sum(qtd_vinc, na.rm = TRUE)) |>
  dplyr::group_by(municipio, ano) |>
  dplyr::mutate(vinc_munic = sum(vinc_setor, na.rm = TRUE)) |>
  dplyr::group_by( setor, ano) |>
  dplyr::mutate(vinc_setor_br = sum(vinc_setor, na.rm = TRUE)) |>
  dplyr::group_by(ano) |>
  dplyr::mutate(vinc_br = sum(vinc_setor, na.rm = TRUE),
         value = abs((vinc_setor/vinc_munic)-(vinc_setor_br/vinc_br))) |>
  dplyr::group_by(municipio, ano) |>
  dplyr::summarise(value = sum(value, na.rm = TRUE)/2) |>
  dplyr::mutate(variavel = "objetivo4_3") |>
  dplyr::rename(codmun = municipio) |>
  dplyr::select(ano, codmun, variavel, value) |>
  dplyr::ungroup()

saveRDS(objetivo4_3_aedi,'coleta/cache/objetivo4_3_aedi/objetivo4_3_aedi.rds')

##Conferência

objetivo4_3_orig <- readxl::read_excel('coleta/cache/objetivo4_3_aedi/9_ind_objetivo_4.xlsx')
names(objetivo4_3_orig) <- gsub("__","_",names(objetivo4_3_orig))
objetivo4_3_orig <- objetivo4_3_orig|>
  tidyr::pivot_longer(-1:-5,names_sep="_",names_to = c("objetivo","n_indicador","ano"),values_to="valor")

objetivo4_3_orig <- objetivo4_3_orig |>
  dplyr::filter(n_indicador==3)|>
  dplyr::transmute(codmun=as.numeric(code_muni6),ano=as.integer(ano),valor)

obj4_3_compara <- objetivo4_3_orig|>
  dplyr::left_join(objetivo4_3_aedi)

cor(obj4_3_compara$valor,obj4_3_compara$value,use='complete.obs')
#0.9207802
summary(obj4_3_compara)
#
