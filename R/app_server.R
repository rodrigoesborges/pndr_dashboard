#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  RV <- reactiveValues()
  mod_setup_dashboard_server("setup_dashboard_1")
  mod_LandingPage_server("LandingPage_1")
#  mod_convergência_server("convergência_1")
  mod_rede_policêntrica_server("rede_policêntrica_1")
  mod_competitividade_regional_server("competitividade_regional_1")
  mod_cadeias_sustentaveis_server("cadeias_sustentaveis_1")
  # Your application server logic

}


