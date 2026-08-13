##Criar camadas do geoserver
library(geosapi)
# Configuração da conexão com o GeoServer
gsurl <- "https://geoserver.mdr.gov.br"
usuario <- "rodrigo.borges"
senha <- "pndr*678"
gsapi <- paste0(gsurl,"/geoserver/rest/")
###Publicação de todos os ft como camadas via curl  e feature.json baixado de
gsrestftapi <- paste0(gsapi,
                      "workspaces/pndr/",
                      "datastores/painel_pndr_bd/featuretypes")
gsrestlayerapi <- paste0(gsapi,
                      "layers/pndr:")

f <- paste0(tempfile(),".json")
download.file(
  paste0(gsrestftapi,"/objetivo1_2015_camada.json"),
f,method="libcurl")

ftobj115 <- jsonify::from_json(f)


publica_camada <- \(ano) {
  atano <- \(x){
    gsub("2015",ano,x)
  }

  ftpub <- ftobj115
  ftpub$featureType$name <- atano(ftpub$featureType$name)
  ftpub$featureType$nativeName <- atano(ftpub$featureType$nativeName)
  ftpub$featureType$title <- atano(ftpub$featureType$title)

  print(ftpub$featureType$name)

  f <- paste0(tempfile(),".json")

  jsonlite::write_json(ftpub,f)
  httr::POST(url = gsrestftapi,
             config = list(httr::authenticate(usuario,senha),
                           httr::content_type_json()),
            body=httr::upload_file(f)
            )
}

lapply(2019:2022,publica_camada)

##ajusta estilo da camada

gs <- geosapi::GSManager$new(gsurl, usuario, senha)

store <- "painel_pndr_bd"
workspace <- "pndr"


# Nomes de features e camadas
obj <- "objetivo"
nomesfeatures <- sort(paste0(obj,apply(expand.grid(1:4,2015:2022),1,paste0,collapse="_")))
nomescamadas <- paste0(nomesfeatures,"_camada")
camadabase <- gs$getLayer(nomescamadas[1])

sldpad <- camadabase$defaultStyle

atualizaestilo <- \(ano){
   f <- paste0(tempfile(),".json")
   jsonat <- jsonlite::parse_json(paste0('{"layer": {"defaultStyle": "',sldpad$name,'"}}'))
   jsonlite::write_json(jsonat,f)
  # download.file(
  #   paste0(gsrestlayerapi,"objetivo1_2015_camada.json"),f,method="libcurl")
  #
  camadat <- nomescamadas[grepl(paste0("objetivo1_",ano),nomescamadas)]
  # jsonbase <- jsonify::from_json(f)
  # jsonat <- gsub("2015","2018",jsonbase)
  # jsonlite::write_json(jsonat,f)
  httr::PUT(url = paste0(gsrestlayerapi,camadat,".json"),
             config = httr::config(httr::authenticate(usuario,senha),
                           httr::content_type_json()),
             body=httr::upload_file(f)
  )

}

lapply(2019:2022,atualizaestilo)
#Função para publicar camadas com as tabelas - ft - dos objetivos

geosapi::GSLayer()

geosapi::GSLayerManager()

geosapi::GSLayerGroup()

geosapi::GSStyle()

geosapi::GSStyleManager()
