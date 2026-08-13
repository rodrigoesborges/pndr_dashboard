#' convergência UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom leaflet leafletOutput renderLeaflet leaflet addWMSTiles leafletProxy
#' @importFrom leaflet addProviderTiles addMapPane addPolygons leafletOptions
#' @importFrom leaflet providers providerTileOptions pathOptions WMSTileOptions
#' @importFrom ggdark dark_theme_minimal
#' @importFrom ggplot2 aes theme_minimal ggplot geom_line theme element_rect
#' @importFrom ggplot2 element_text element_line
#'
##Global do módulo

tema_escuro <-theme_minimal(base_family = "Arial") +
  theme(
    panel.background = element_rect(fill = "#272b30", color = NA),  # Fundo escuro (tom de cinza do Slate)
    plot.background = element_rect(fill = "#272b30", color = NA),   # Fundo do gráfico
    panel.grid.major = element_line(color = "#3b3f44"),             # Grades principais em cinza escuro
    panel.grid.minor = element_line(color = "#444950"),             # Grades menores em tom mais escuro
    axis.text = element_text(color = "#d3d3d3", size = 12),         # Texto dos eixos em cinza claro
    axis.title = element_text(color = "#f8f9fa", size = 14, face = "bold"), # Títulos em branco
    plot.title = element_text(color = "#f8f9fa", size = 16, face = "bold", hjust = 0.5), # Título centralizado
    legend.background = element_rect(fill = "#272b30", color = NA), # Fundo da legenda
    legend.text = element_text(color = "#d3d3d3"),                 # Texto da legenda
    legend.title = element_text(color = "#f8f9fa", face = "bold")   # Título da legenda
  )



mod_convergência_ui <- function(id){
  ns <- NS(id)

  tagList(
    h1("Objetivo 1 - Convergência"),

    fluidRow(
      shiny::column(4,sliderInput(ns('ano1'),"Ano",2015,2022,2022,animate=T,ticks=F,animationOptions(interval=2500,loop=F,playButton = icon('play')))),
    shiny::column(4,shiny::selectizeInput(ns('municipios1'),label="Municipio",choices=NULL))),
  fluidRow(
    shiny::column(7,leafletOutput(ns("mapabasse"), width = "100%", height = paste0("calc(100vh - ", 230, "px)"))),
  shiny::column(5,plotly::plotlyOutput(ns("graficobj1"))))
  )
}

#' convergência Server Functions
#'
#' @noRd
mod_convergência_server <- function(id){
  moduleServer(id,function(input,output,session){
    ns <- session$ns
    updateSelectizeInput(session, 'municipios1', choices = munif,server = TRUE)


    output$graficobj1 <- plotly::renderPlotly({

      if(!is.null(input$municipios1) &
         !is.na(input$municipios1) &
         input$municipios1 != 'NA') {
        munselec <- as.numeric(input$municipios1)
      }
        if(is.na(as.numeric(munselec))){
          munselec <- 5300108
        }
      con <- DBI::dbConnect(RPostgres::Postgres(),
                            user="aedi",
                            password="aEd1#man@gR",
                            host="127.0.0.1",
                            dbname="aedidb")

      obj1graf <- DBI::dbGetQuery(con,
                             paste0("select refdate ano,value valor, data_name nome_indicador from data_values LEFT JOIN
                              local on data_values.local_id = local.local_id
                             LEFT JOIN mdata ON data_values.mdata_id = mdata.mdata_id
                             WHERE geoloc_id = ",munselec," AND orig_name = 'objetivo1_1' "))

      DBI::dbDisconnect(con)
      insert_at_third_space <- function(text, char) {
        spaces <- gregexpr(" ", text)[[1]]  # Localiza os espaços

        if (length(spaces) < 3) {
          return(text)  # Retorna a string original se houver menos de 3 espaços
        }

        pos <- spaces[3]  # Posição do terceiro espaço

        # Insere o caractere na posição encontrada
        paste0(substr(text, 1, pos), char, substr(text, pos + 1, nchar(text)))
      }

      indicadorleg <- insert_at_third_space(obj1graf$nome_indicador[1],"\n")

      plotly::ggplotly(
        ggplot2::ggplot(obj1graf,ggplot2::aes(x=ano,y=valor))+
          ggplot2::geom_smooth(color="gray")+tema_escuro+
          ggplot2::ggtitle(
            paste0(names(munif[munif==munselec]),"\n",indicadorleg))
      )

    })|>bindCache(input$municipios1)

      output$mapabasse <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        zoomControl = FALSE,
        boxZoom = TRUE,
        doubleClickZoom = FALSE,
        zoomSnap = 0,
        zoomDelta = 0.25,
        maxZoom = 18,
        minZoom = 3,
        maxBoundsViscosity = 1,
        preferCanvas = TRUE,
        worldCopyJump = FALSE
      )) |>
      addMapPane("base", zIndex = 5) |>
      addMapPane("polygons", zIndex = 10) |>
      addMapPane("labels", zIndex = 15) |>
      addProviderTiles(
        providers$CartoDB.DarkMatterNoLabels,
        options = providerTileOptions(
#          maxZoom = 18,
          pane = "base"
        )
      ) |>
      leaflet::setView(lng = -53.633308, lat = -13.550520, zoom = 4)  |>
      leaflet::setMaxBounds(-77,-38,-27,10) |>
      leaflet::fitBounds(-71,-34,-30,5.47)|>
      # leaflet::addPolygons(
      #          data=basemap,
      #          color = "#FCFCFC",
      #          opacity=0.1,
      #          label = basemap$name_muni,
      #          weight = 1,
      #          highlightOptions = leaflet::highlightOptions(
      #            color = "red",
      #            fillOpacity = 0.7,
      #            bringToFront = TRUE),
      #          options = leaflet::pathOptions(
      #            pane = "highlightes"
      #          )
#              )|>
#       addProviderTiles(
#         providers$CartoDB.PositronOnlyLabels,
#         options = providerTileOptions(
# #          maxZoom = 18,
#           pane = "labels"
#         )
#      )|>
    addWMSTiles("https://geoserver.mdr.gov.br/geoserver/pndr/wms",
                     layers =
                  "objetivo1_2015_camada",
                  #"pndr:Objetivo 1 - Indicador 1 - 2016 - PNDR",
                     layerId="testecamada",
                     options = c(leaflet::pathOptions(pane="polygons"),
                                 WMSTileOptions(format = "image/png",transparent = TRUE,
                                                styles="pndr:desnivelmedio2",
                                                version="1.3.0",
                                                crs="EPSG:4326"),
                                 pathOptions(pane = "base"))
      )|>
      addPolygons(data=basemap,
                  fillOpacity=0,
                  stroke=F,
                  options = pathOptions(pane = "polygons"))


  })

  observe({
    input$ano1
    leafletProxy("mapabasse")|>
      addWMSTiles("https://geoserver.mdr.gov.br/geoserver/pndr/wms",
                  layers =
                    paste0("pndr:objetivo1_",input$ano1,"_camada"),
                  #"pndr:Objetivo 1 - Indicador 1 - 2016 - PNDR",
                  layerId="testecamada",
                  options = c(leaflet::pathOptions(pane="base"),
                              WMSTileOptions(format = "image/png",transparent = TRUE,
                                             styles="pndr:desnivelmedio2",
                                             version="1.3.0",
                                             crs="EPSG:4326"))
      )
  })
  observeEvent(input$mapabasse_click, {
    print("clicou no mapa")
    municipio <- input$mapabasse_click
    if (!is.null(municipio)) {
      ponto <- sf::st_sfc(sf::st_point(c(municipio$lng, municipio$lat)), crs = sf::st_crs(basemap))
    }
    municipio <- basemap$code_muni[sf::st_nearest_feature(ponto,basemap)]

    updateSelectizeInput(
      inputId = "municipios1",
      choices=munif,
      selected = municipio,
      server=T)
  })
})
}
