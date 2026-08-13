microdados_cursos_2022_esup <-
  readr::read_csv2('coleta/cache/censo_educ_sup/microdados_educacao_superior_2022/dados/microdados_cadastro_cursos_2022.csv')


ardic <- "coleta/cache/censo_educ_sup/microdados_educacao_superior_2022/Anexos/ANEXO I - Dicionario de Dados/dicionario_dados_educacao_superior.xlsx"
dicdados <- readxl::read_excel(ardic, skip = 4,sheet = 2)


#QT_MAT - quantidade de matrículas
#TP_GRAU_ACADEMICO -  "1. Bacharelado\r\n2. Licenciatura\r\n3. Tecnológico\r\n4. Bacharelado e Licenciatura\r\n(.) Não aplicável (cursos com nível acadêmico igual a sequencial de formação específica ou cursos de área básica de Ingresso)"

#TP_NIVEL_ACADEMICO - 1. Graduação / 2. sequencial de formação específica ou cursos de área basíca de Ingresso

matriculas_curso_tecnologico <- microdados_cursos_2022_esup|>dplyr::filter(TP_GRAU_ACADEMICO==3)|>
  dplyr::group_by(NU_ANO_CENSO,CO_MUNICIPIO)|>
  dplyr::summarize(matriculas=sum(QT_MAT,na.rm=T))

censo2019 <-
  data.table::fread('coleta/cache/censo_educ_sup/microdados_educacao_superior_2019/dados/MICRODADOS_CADASTRO_CURSOS_2019.CSV')

matec2019 <- censo2019|>
  dplyr::filter(TP_GRAU_ACADEMICO==3)|>
  dplyr::group_by(NU_ANO_CENSO,CO_MUNICIPIO)|>
  dplyr::summarize(matriculas=sum(QT_MAT,na.rm=T))

saveRDS(matec2019,'coleta/cache/educ3_aedi/matec2019.rds')
