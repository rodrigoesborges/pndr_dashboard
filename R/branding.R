# Marca configuravel do painel PNDR (2026-09-17)
#
# Titulo, subtitulo, logo e contato do rodape sao configuraveis por
# variaveis de ambiente (use o .Renviron do projeto), com fallback para
# o padrao atual:
#
#   pndr_titulo      titulo do header. Default: "Painel da PNDR"
#   pndr_subtitulo   subtitulo do header. Default: "Protótipo"
#   pndr_logo        logo do header, caminho relativo a inst/app/www/.
#                    Default: www/pndr-sologo.png
#   pndr_contato     contato do rodape, campos "nome|telefone|email"
#                    separados por "|". Default: Distintive

#' Header do painel com a marca configuravel (usa pndr_titulo/subtitulo/logo)
#' @keywords internal
header_marca <- function() {
  shinyGovBRstyle::header(
    Sys.getenv("pndr_titulo", "Painel da PNDR"),
    Sys.getenv("pndr_subtitulo", "Protótipo"),
    logo = Sys.getenv("pndr_logo", "www/pndr-sologo.png"))
}

#' Linha de contato do rodape (usa pndr_contato)
#' @keywords internal
contato_rodape_tag <- function() {
  campos <- strsplit(Sys.getenv(
    "pndr_contato",
    "Distintive|61-XXXX-XXXX|apps@distintive.com.br"), "|", fixed=TRUE)[[1]]
  length(campos) <- 3
  campos[is.na(campos)] <- ""
  shiny::tags$div(
    shiny::tags$hr(),
    shiny::p(
      shiny::icon("envelope"),
      shiny::a(campos[3], href = paste0("mailto:", campos[3])),
      " | ",
      shiny::icon("phone"), campos[2],
      " | ",
      shiny::tags$b(campos[1])
    ),
    align = "center",
    style = "padding: 10px;"
  )
}
