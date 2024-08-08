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
  mod_rede_policêntrica_server("rede_policêntrica")
  mod_competitividade_regional_server("competitividade_regional")
  mod_cadeias_sustentaveis_server("cadeias_sustentaveis")
  # Your application server logic
  observeEvent(input$Eixos, {
    updateTabsetPanel(session, "pagina", "convmarra")
  })
}


