#' rede_policêntrica UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
obj2 <-  readRDS("data-raw/9_ind_objetivo_2.RDS")

mod_rede_policêntrica_ui <- function(id){
  ns <- NS(id)
  tagList(
      h1("Objetivo 2 - Rede Policêntrica - maplibreGL"),
   # fluidPage(
      sliderInput(ns('ano'),"Ano",2015,2022,2022,animate=T,ticks=F,animationOptions(interval=2500,loop=F,playButton = icon('play'))),
      selectInput(ns("indicador"),"indicador",choices = unique(obj2$variavel),selected = ""),
      div(shinybusy::add_busy_spinner(spin = "fading-circle", position = "bottom-right",timeout=200),
          #leafgl::leafglOutput(ns("mapabase"),height='99vh')
          mapgl::maplibreOutput(ns("mapabase")))
#    )

  )
}

#' rede_policêntrica Server Functions
#'
#' @noRd
mod_rede_policêntrica_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    output$mapabase <- mapgl::renderMaplibre({
      mapgl::maplibre(style = mapgl::carto_style("positron")) |>
        mapgl::fit_bounds(basemap, animate = TRUE) #|>
      # mapgl::add_line_layer(id = "polines",
      #                source = basemap,
      #                line_color = "black",
      #                line_sort_key=15,
      #                line_opacity = 0.9,
      #                line_width=0.5,
      #                hover_options = list(
      #                  line_color = "red",
      #                  line_opacity = 1
      #                ))
    })|>bindCache("basemap")
    outputOptions(output, "mapabase", suspendWhenHidden = FALSE)

    metemapa <- reactive({
      indicador <- input$indicador
      ano <- input$ano
      req(indicador)
      req(ano)
      dados <- basemap|>dplyr::left_join(obj2[obj2$variavel == indicador & obj2$ano == ano,])
    })|>bindCache(input$indicador,input$ano)

    pallet <- reactive({

      indicador <- input$indicador
      map_data <- metemapa()
      print(head(map_data$value))
      tpaleta <- mypallet(map_data$value,indicador)
      tpaleta
    })
    observe({
      camada <- metemapa()
      minimos <- min(camada$value)
      maximos <- max(camada$value)
      pal <- leaflet::colorNumeric(palette = "Reds", domain = NULL,na.color = "transparent")
      mapgl::maplibre_proxy("mapabase") |>
        #    mapgl::clear_layer("indicadorhm")|>
        mapgl::add_fill_layer(paste0(input$indicador,"-",input$ano,"-",Sys.time()),
                              source=camada,
                              fill_sort_key=10,
                              #                          before_id = 'polines',
                              fill_opacity = 0.8,
                              fill_color = mapgl::interpolate(
                                column="value",
                                values=c(0,4),
                                stops = c("lightblue","darkblue"),
                                na_color = "lightgray"
                              ),
                              hover_options = list(
                                fill_color = "red",
                                fill_opacity = 1
                              ),
                              tooltip = "name_muni")|>
        mapgl::add_legend(
          input$indicador,
          values = c(0,4),
          colors = c("lightblue", "darkblue")
        )

    }) |>bindEvent(input$indicador,input$ano)

  })
}

## To be copied in the UI
# mod_rede_policêntrica_ui("rede_policêntrica_1")

## To be copied in the server
# mod_rede_policêntrica_server("rede_policêntrica_1")
