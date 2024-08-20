#' competitividade_regional UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_competitividade_regional_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' competitividade_regional Server Functions
#'
#' @noRd
mod_competitividade_regional_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_competitividade_regional_ui("competitividade_regional_1")

## To be copied in the server
# mod_competitividade_regional_server("competitividade_regional_1")
