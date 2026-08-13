#' competitividade_regional UI Function
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

obj3 <-  readRDS("data-raw/9_ind_objetivo_3.RDS")
mod_competitividade_regional_ui <- function(id){
  ns <- NS(id)
  tagList(
      h1("Objetivo 3 - Competitividade - Leaflet puro"),
    sliderInput(ns('ano3'),"Ano",2015,2022,2022,animate=T,ticks=F,animationOptions(interval=2500,loop=F,playButton = icon('play'))),
    selectInput(ns("indicador3"),"Indicador",choices = unique(obj3$variavel),selected = ""),
    div(shinybusy::add_busy_spinner(spin = "fading-circle", position = "bottom-right",timeout=200),
        leaflet::leafletOutput(ns("mapabase3"), width = "100%", height = paste0("calc(100vh - ", 350, "px)")))
  )
}

#' competitividade_regional Server Functions
#'
#' @noRd
mod_competitividade_regional_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$mapabase3 <- leaflet::renderLeaflet({
      leaflet::leaflet(
        options = leaflet::leafletOptions(
          zoomControl = FALSE,
          boxZoom = TRUE,
          doubleClickZoom = FALSE,
          zoomSnap = 0,
          zoomDelta = 0.25,
          maxZoom = 10,
          minZoom = 2,
          maxBoundsViscosity = 1,
          preferCanvas = TRUE,
          worldCopyJump = FALSE
        )) |>
      leaflet::addMapPane("base", zIndex = 5) |>
        leaflet::addMapPane("polygons", zIndex = 400) |>
        leaflet::addMapPane("highlights", zIndex = 413)|>
        leaflet::addMapPane("labels", zIndex = 415) |>
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.PositronNoLabels,
        options = leaflet::providerTileOptions(
     #     maxZoom = 10,
          pane = "base"
        )
      ) |>
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.PositronOnlyLabels,
        options = leaflet::providerTileOptions(
 #         maxZoom = 10,
          pane = "labels"
        )
      )|>
        # leaflet::addPolygons(
        #   data=basemap,
        #   color = "#FCFCFC",
        #   opacity=0.1,
        #   label = basemap$name_muni,
        #   weight = 1,
        #   highlightOptions = leaflet::highlightOptions(
        #     color = "red",
        #     fillOpacity = 0.7,
        #     bringToFront = TRUE),
        #   options = leaflet::pathOptions(
        #     pane = "highlightes"
        #   )
        # )|>
        leaflet::setMaxBounds(-74,6,-40,-34) #|>
  #      leaflet::setView(-47.9292, -15.7801, zoom = 5)
    })

    metemapa <- reactive({
      indicador <- input$indicador3
      ano <- input$ano3
      req(indicador)
      req(ano)
      dados <- basemap|>dplyr::left_join(obj3[obj3$variavel == indicador & obj3$ano == ano,])
    })|>bindCache(input$indicador3,input$ano3)

    pallet <- reactive({

      indicador <- input$indicador3
      map_data <- metemapa()
      print(head(map_data$value))
      tpaleta <- mypallet(map_data$value,indicador)
      tpaleta
    })

    observe({
         camada <- metemapa()

         proxy <- leaflet::leafletProxy("mapabase3")
         #colourvalues::color_values_rgb(camada$value,include_alpha = F)
         pal <- leaflet::colorNumeric(palette = "Reds", domain = NULL,na.color = "transparent")
         print("adicionando_camada competencia regional")
         # memoise::memoise(proxy|> leafgl::addGlPolygons(
         #   data = camada,
         #   fillColor =    ~pal(camada$value),
         #
         #   fillOpacity = 0.5,
         #   smoothFactor = 0.5,
         #    color = "lightgray",
         #   opacity=0.8,
         #   weight = 1,
         #   options = list(zIndex=6,leaflet::pathOptions(pane='polygons',zIndex=10))
         # )
         proxy|> leaflet::addPolygons(
           data = camada,
           fillColor =    ~pal(camada$value),

           fillOpacity = 0.5,
           smoothFactor = 0.5,
           color = "lightgray",
           opacity=0.8,
           weight = 1,
           options = leaflet::pathOptions(pane='highlights')
         )
               })

  })
}

## To be copied in the UI
# mod_competitividade_regional_ui("competitividade_regional_1")

## To be copied in the server
# mod_competitividade_regional_server("competitividade_regional_1")
