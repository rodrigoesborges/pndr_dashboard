#' framegov UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_framegov_ui <- function(id) {
  ns <- NS(id)
#  tagList(
  shiny::fluidPage(
    theme = bslib::bs_theme(preset ="slate"),
  shinyGovBRstyle::header("Painel da PNDR","Protótipo",logo="www/pndr-sologo.png"),
   shinyGovBRstyle::gov_main_layout(
#     shinyGovBRstyle::govTabs("abas",)
     shiny::tabsetPanel(
#      shiny::tabPanel("Rede Policêntrica",mod_rede_policêntrica_ui("rede_policêntrica_1")),
     shiny::tabPanel("Convergência",mod_convergência_ui("convergência_1"))#,
#     shiny::tabPanel("Competitividade",mod_competitividade_regional_ui("competitividade_regional_1"))
       ),
  shinyGovBRstyle::footer()
     ),

  )
#  )
}

#' framegov Server Functions
#'
#' @noRd
mod_framegov_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_framegov_ui("framegov_1")

## To be copied in the server
# mod_framegov_server("framegov_1")
