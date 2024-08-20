#' cadeias_sustentaveis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cadeias_sustentaveis_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' cadeias_sustentaveis Server Functions
#'
#' @noRd 
mod_cadeias_sustentaveis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_cadeias_sustentaveis_ui("cadeias_sustentaveis_1")
    
## To be copied in the server
# mod_cadeias_sustentaveis_server("cadeias_sustentaveis_1")
