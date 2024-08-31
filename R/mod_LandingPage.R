#' LandingPage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
obj2 <- readRDS("dadostat/Painel de Indicadores/Cálculo Painel de Indicadores/9_ind_objetivo_2.RDS")

mypallet <- function(data, indicator) {
  # If data has positive and negative values, pallet is divergent and centred in 0

  qtt <- data |> length()


    negative <- grDevices::colorRampPalette(colors = c("#0000FF", "#C8C8FF"))(qtt)
    positive <- grDevices::colorRampPalette(colors = c("#FFC8C8", "#FF0000"))(qtt)

  onlypositive <- grDevices::colorRampPalette(colors = c("#FFC8C8", "#FF0000"))(qtt)


  suppressWarnings({
    maximum <- max(data, na.rm = TRUE) *1.1
    minimum <- min(data, na.rm = TRUE) *1.1
  })

    dominium <- c(0,maximum)
    colours <- onlypositive

  if (maximum |> is.infinite()) {
    function (i) "transparent"
  } else {
    leaflet::colorNumeric(
      colours,
      domain = dominium,
      na.color="transparent")
  }
}

mod_LandingPage_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$script(src="https://code.jquery.com/ui/1.13.0/jquery-ui.js"),
                                tags$script("
                                            $('#splashmdr video').bind('ended', function(){
                 $(this).parent().hide('fade',90)
              })"),
              tags$style(HTML("@keyframes fade2 {
                   to {
                   visibility: hidden;
                   opacity:0;
                   z-index: 1;
                   transition: visibility 0s 2s, opacity 2s linear z-index 5s linear;
                   }
                  };"))),
   div(id=ns("splashmdr"),
        tags$video(height='100%',width='100%',
                   id=ns("splash"),poster='www/pndr-sologo.png',
                   muted='true',autoplay='true',
                   preload="auto",
                   tags$source(src="www/mdr_vinheta.mp4",type="video/mp4"),
                    style='animation: fade2 1s 9s ease-in forwards; z-index: 999999; position:absolute; background-color: white;'
        )
    )
  )
}

#' LandingPage Server Functions
#'
#' @noRd
mod_LandingPage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
#    jqui_hide('#LandingPage_1-splashmdr', effect = 'fade', duration = 1000  )
  })

}

