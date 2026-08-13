#' convergência UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom leaflet leafletOutput renderLeaflet leaflet
#'
##Global do módulo


mod_convergência_ui <- function(id){
  ns <- NS(id)

  tagList(
    h1("Objetivo 1 - Convergência"),

  leafletOutput("mapabase", width = "100%", height = paste0("calc(100vh - ", 120, "px)")),
  )
}

#' convergência Server Functions
#'
#' @noRd
mod_convergência_server <- function(id){
  moduleServer(id,function(input,output,session,RV){

  output$mapabase <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        zoomControl = FALSE,
        boxZoom = TRUE,
        doubleClickZoom = FALSE,
        zoomSnap = 0,
        zoomDelta = 0.25,
#        maxZoom = 18,
#        minZoom = 15,
        maxBoundsViscosity = 1,
        preferCanvas = TRUE,
        worldCopyJump = FALSE
      )) |>
      addMapPane("base", zIndex = 5) |>
      addMapPane("polygons", zIndex = 10) |>
      addMapPane("labels", zIndex = 15) |>
      addProviderTiles(
        providers$CartoDB.PositronNoLabels,
        options = providerTileOptions(
#          maxZoom = 18,
          pane = "base"
        )
      ) |>
      addProviderTiles(
        providers$CartoDB.PositronOnlyLabels,
        options = providerTileOptions(
#          maxZoom = 18,
          pane = "labels"
        )
      )

  })
})
}
