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
    shinyGovBRstyle::use_govbr(),
    header_marca(),
   shinyGovBRstyle::br_layout(
#     shinyGovBRstyle::govTabs("abas",)
     shinyGovBRstyle::br_tabs("abas",c("Rede Policêntrica","Convergência","Painel"),
      shiny::tabPanel("Rede Policêntrica",mod_rede_policêntrica_ui("rede_policêntrica_1")),
     shiny::tabPanel("Convergência",mod_convergência_ui("convergência_1")),
     shiny::tabPanel("Painel",
       mod_panel_map_ui("panel_map_1"),
       shiny::hr(),
       mod_panel_series_ui("panel_series_1"))#,
       ),
  shinyGovBRstyle::br_footer()
     ),
  contato_rodape_tag(),

  )
#  )
}

#' framegov Server Functions
#'
#' @noRd
mod_framegov_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    mod_panel_map_server("panel_map_1")
    mod_panel_series_server("panel_series_1")
  })
}

## To be copied in the UI
# mod_framegov_ui("framegov_1")

## To be copied in the server
# mod_framegov_server("framegov_1")
