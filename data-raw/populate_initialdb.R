#' Importa previos PNDR
#'
#' Prepara dados já calculados do Painel da PNDR
#'
#' @export
pega_pndr_previo <- \(){
  #populate static tables
  con <- DBI::dbConnect(RSQLite::SQLite(),"dashboard_db.sqlite")
  ##GEOLOC
  muncodes <- read.csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/master/csv/municipios.csv")

  geoloc <- brazilmaps::get_brmap(
    geo="City",
    geo.filter=list(City = sort(muncodes$codigo_ibge)),
    class = 'sf'
  )
  geoloc<- sf::st_transform(geoloc,crs = "+proj=longlat +datum=WGS84 +no_defs")

  geolocb <- as.data.frame(
    cbind(
      geoloc_id=geoloc$City,
      geometry=
        sf::st_as_text(sf::st_sfc(geoloc$geometry))))


  DBI::dbAppendTable(con,"geoloc",geolocb)

  ##local
  local <- data.frame(local_id = 1:nrow(geoloc),local_name=geoloc$nome,geoloc_id=geoloc$City)

  DBI::dbAppendTable(con,"local",local)

  ##datagroup
  urlma <- "dadostat/Painel de Indicadores/Cálculo Painel de Indicadores/Manual_Painel.html"
  manualhtml <- rvest::read_html(urlma)

  get_datagroup <- \(pathx="//*[@id='ind-municipais-desenvolvimento']/ul/li",grpbn="Eixo",
                     delimi=") ") {
  grupo <- gsub("\\\r\\\n"," ",rvest::html_elements(manualhtml,
  xpath=pathx)|>rvest::html_text())
  grupo <- grupo|>
    tibble::as_tibble()|>
    tidyr::separate_wider_delim(value,names=c("datagroup_id","datagroup_desc"),delim=delimi)

  grupo$datagroup_id <- as.numeric(gsub("[^\\d]* ","",grupo$datagroup_id))
  grupo$datagroup_name <- paste(grpbn,grupo$datagroup_id)

  grupo
  }

  grupos <- get_datagroup()
  grupos <- rbind(grupos,
                  data.frame(
                    datagroup_id=8,
                    datagroup_desc="Indicadores de desenvolvimento por estratos da PNAD(não municipais)",
                    datagroup_name="Estratos PNAD"
                  ))

  pathxobj= "//*[@id='objetivos']/ul/li"

  grupos <- dplyr::bind_rows(grupos, get_datagroup(pathxobj,
                      grpbn="Objetivo",delimi=": ")|>
                        dplyr::mutate(datagroup_id=max(grupos$datagroup_id)+datagroup_id))



  DBI::dbAppendTable(con,"datagroup",grupos)


  ##Data values and mdata
  rdsbpath <- "dadostat/Painel de Indicadores/Cálculo Painel de Indicadores/"
  alvos <- list.files(path=rdsbpath,pattern="*.RDS",full.names = T)
  lapply(alvos,\(x){
      assign(gsub(".*/([^/]+)\\.RDS$","\\1",x),readRDS(x),envir=.GlobalEnv)
    })

  ##fix pnad_c with extracts with different colname
  `8_pnadc_estratos` <-
    `8_pnadc_estratos` |>dplyr::rename(codmun=cod_estrato)|>
    dplyr::select(2,1,3,4)


  nmarqind <- ls(pattern="\\d_ind|\\d_pnadc")
  lista_ind <- lapply(nmarqind,
                      \(x){(get(x))})
  names(lista_ind) <- nmarqind


  tiguais <-   all(
      sort(
        Reduce(
          intersect,
          lapply(lista_ind,names))) ==
            sort(names(lista_ind[[1]])))

  if (tiguais) {


    lista_ind <- data.table::rbindlist(lista_ind,use.names = T)




  }

  inds <- unique(lista_ind$variavel)


  ##fix compound indicators not found in rds files:
  calvos <- list.files(path=rdsbpath,pattern="*.xlsx",full.names = T)

  retrieve_comp <- \(x){
    x <- readxl::read_xlsx(x)

    if(!("code_muni6" %in% names(x))) {
      x <- dplyr::select(x,Cdgdest,contains("comp_"))
    } else {
      x <- dplyr::select(x,code_muni6,contains("comp_"))
    }
    x <- x |>
      tidyr::pivot_longer(-1,names_pattern = "(.*)_([^_]*$)",names_to=c("variavel","ano"),values_to="value")
    x <- x|>dplyr::rename(codmun=1)
    }
lista_compostos <- data.table::rbindlist(lapply(calvos,retrieve_comp))

lista_compostos <- lista_compostos|>
  dplyr::mutate(ano=as.numeric(ano))

compind <- data.frame("ind"=inds)|>tidyr::separate(1,into=c("var","id"),sep=-1,remove=T)|>
  dplyr::mutate(across(id,as.numeric))|>
  dplyr::group_by(var)|>dplyr::filter(id==max(id))|>
  dplyr::mutate(id=id+1)

compind <- rbind(compind|>dplyr::mutate(var=ifelse(var=="pnadc","pnadc1",var)),
                 data.frame(
                   var=paste0("pnadc",2:7),
                   id=9:14
                 ))

lista_compostos <- lista_compostos|>
  dplyr::mutate(var=gsub("comp_","",variavel))|>
  dplyr::left_join(compind)|>
  dplyr::transmute(codmun,ano,variavel=paste0(variavel,id),value)

lista_ind <- dplyr::bind_rows(lista_ind,lista_compostos)

  inds <- unique(lista_ind$variavel)

  inds <- data.frame("mdata_id" = 1:length(inds),"orig_name"=inds)

  manual_indnpath <- "div.section.level4 h4"
  nomes_ind <-
  rvest::html_elements(manualhtml,css=manual_indnpath)|>
    rvest::html_text()

  ## Fix objectives numbering on manual
  nomes_ind[48:54] <- paste("8.",1:7,")",nomes_ind[48:54])

  ## Fix linebreakes
  nomes_ind <- gsub("\\\r(\\\n)*"," ",nomes_ind)


  nomes_ind <- data.frame(
    data_name = nomes_ind
  )    |>tidyr::separate(
    col="data_name",into=c("datagroup_id","orig_name_id","data_name"),
    sep="[.)]",convert=T, extra="merge"
  )

  ##Adjust group_id of objectives according to convention determined above

  nomes_ind[36:47,]$datagroup_id <- rep(9:12,each=3)

  orig_names <- data.frame("orig_name"=sort(unique(lista_ind$variavel)))|>
    dplyr::mutate(orig_name_id = as.numeric(substr(orig_name,nchar(orig_name)-grepl("1[01234]$",orig_name),nchar(orig_name))),
                  datagroup_id = dplyr::case_when(
      grepl("educ",orig_name) ~ 1,
      grepl("citec",orig_name) ~ 2,
      grepl("desprod",orig_name) ~ 3,
      grepl("infra",orig_name) ~ 4,
      grepl("dessoc",orig_name) ~ 5,
      grepl("sust",orig_name) ~ 6,
      grepl("governativas",orig_name) ~ 7,
      grepl("adicionais",orig_name) ~ 7,
      grepl("objetivo1",orig_name) ~ 9,
      grepl("objetivo2",orig_name) ~ 10,
      grepl("objetivo3",orig_name) ~ 11,
      grepl("objetivo4",orig_name) ~ 12,
      T ~ 8
    ))|>dplyr::arrange(datagroup_id)

  nomes_ind <- nomes_ind|>dplyr::left_join(orig_names)

  nomes_ind <- nomes_ind|>dplyr::mutate(mdata_id=1:nrow(nomes_ind),data_desc="")|>
    dplyr::select(mdata_id,orig_name,data_name,data_desc)

  DBI::dbAppendTable(con,"mdata",nomes_ind)

  ##mdata_group
  nomes_ind <- nomes_ind|>dplyr::left_join(orig_names)

  mdata_group <- nomes_ind|>dplyr::distinct(mdata_id,datagroup_id)

  DBI::dbAppendTable(con,"mdata_group",mdata_group)
  ##data_values
  lista_ind <- lista_ind|>dplyr::left_join(inds,by=c("variavel"="orig_name"))

  lista_ind$codmun <- as.numeric(lista_ind$codmun)

  lista_indt <-
    lista_ind |>
  dplyr::left_join(local|>
                     dplyr::mutate(codmun=as.integer(substr(geoloc_id,1,6))),
                   by="codmun")|>
    dplyr::mutate(refdate=as.Date(paste0("31/12/",.data[["ano"]]),"%d/%m/%Y"))|>
    dplyr::select(mdata_id,local_id,refdate,value)


  DBI::dbAppendTable(con,"data_values",lista_indt)

  DBI::dbDisconnect(con)
  }
