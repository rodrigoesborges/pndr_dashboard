
le_mesanocnes <- \(mesano){
  arquivos <- list.files(path=paste0("coleta/cache/datasus_cnes/",mesano),
                         pattern="*.dbc",
                         full.names = TRUE)
  data.table::rbindlist(
    lapply(arquivos,read.dbc::read.dbc))
}

explora_cnes <- read.dbc::read.dbc("coleta/cache/datasus_cnes/201401/cnes-pf_201401-sp_20190515.dbc")


cnes201401 <- le_mesanocnes(201401)

##dieese ocupações da saúde anuário 2018
dicdados_cbosaude <- data.table::fread('coleta/cache/datasus_cnes/cnes-anuario-p25-27-ocupacoes_saude_sus.csv')

cbos_saude_ocsaude <- dicdados_cbosaude[Tipo == "Ocupações da saúde",`Códigos das famílias ocupacionais (CBO 2002)`]
cbos_saude_sem5 <- sort(cbos_saude_ocsaude[!grepl("^[1]",cbos_saude_ocsaude)])
# - PROF SUS CPF UNICO - 1.194.265 - 201401
# no cnes_profissionais.csv - 969.662

prosaudepmun <- \(dt) {
  dt[
  CPFUNICO==1,.N,
  by= .(grepl(paste0("^(",paste0(cbos_saude_sem5,collapse="|"),")"),CBOUNICO),CODUFMUN)][
    grepl == TRUE][
      order(as.character(CODUFMUN)),.(as.character(CODUFMUN),N)]
}


recalc_obj1_3 <- \(anomes){
  lido <- le_mesanocnes(anomes)
  pspc <- prosaudepmun(lido)
  anoe <- substr(anomes,1,4)

  if(anoe=='2024'){
    datasuspop <- datasus::ibge_popt2024br_mun()
  } else {
  datasuspop <- datasus::ibge_poptbr_mun(periodo=anoe)
  }

  datasuspop <-
    datasuspop|>tidyr::separate_wider_delim(Município,names=c('cd_mun','nm_mun'),delim=" ",too_many = 'merge',too_few='align_end')


  obj1_3_takefinal <- datasuspop[-1,]|>dplyr::left_join(profsaude_pmun,by=c("cd_mun"="V1"))|>
    dplyr::mutate(profsaude_p_mun=ifelse(is.na(N),0,N)/`População estimada`,
                  objetivo1_3_via_aedi=profsaude_p_mun-median(profsaude_p_mun,na.rm=TRUE))|>
    dplyr::transmute(refdate=as.Date(paste0(anoe,"-12-31")),geoloc_idd=cd_mun,
              profissionais_de_saude_pc=profsaude_p_mun,
              objetivo1_3_via_aedi)
}

obj1_3_takefinal <-
  data.table::rbindlist(
    lapply(paste0(2014:2024,'01'),recalc_obj1_3)
  )


