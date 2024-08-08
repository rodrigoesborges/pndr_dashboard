#' setup_dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

## GLOBAL
theme <- "simplex"
bar_height <- 41
item_color <- "darkgrey"
bg_color <- "white"
panel_bgcolor <- "rgba(252,252,252,1)"

mod_setup_dashboard_ui <- function(id){
  ns <- NS(id)
  ## UI ##############

  setup_panel <- tagList(
    # # Loading panel...
    conditionalPanel(
      "output.loading!=''",
      ns = ns ,
      absolutePanel(
        top = bar_height,
        left = 0,
        right = 0,
        bottom = 0,
        style = paste0("background-color: ", panel_bgcolor,";",
                       "text-align: center;",
                       "z-index: 100000;"),
        img(
          src = "www/circ.gif",
          style = paste0("position: fixed;",
                         "top: calc(50vh - 5px);",
                         "left: calc(50vw - 8px);")
        ))),

    # setup Button
    actionLink(
      ns("setup_button"),
      label = NULL,
      style = paste0("position: fixed;
      top:", (bar_height-28)/2,"px;
      right: 10px;
      font-size: 20px;
      color:", item_color,";
      z-index: 5000;"),
      icon = icon("cog")
    ),

    conditionalPanel(
      "output.show_setup_panel != 0",
      ns = ns,
      # Setup background
      absolutePanel(
        id = ns("setup_background"),
        top = 0,
        left = 0,
        right = 0,
        bottom = 0,
        style = paste0("background-color: ", bg_color, ";",
                       "opacity: 0.7;",
                       "text-align: center;",
                       "z-index: 5000;")
      ),
      # Setup Panel
      absolutePanel(
        top = 120,
        left = 25,
        # draggable = TRUE,
        style = "z-index: 5001; border-radius:10px;",
        width = 350,
        class="panel panel-default",
        actionLink(
          ns("setup_close_button"),
          label = NULL,
          style = "
          position: absolute;
          top: 5px;
          right: 10px;
          z-index: 5000;
          padding: 0px;
          color:black;
          font-size: 14px;",
          icon = icon("times")
        ),
        div(
          "Configurações",
          class = "panel-heading",
          style = "text-align: left; border-radius:10px 10px 0px 0px"),
        div(
          class = "panel-body",
          style = "text-align: center; padding-bottom: 0px;",
          withTags(
            table(
              width = "100%",
              tr(
                td(
                  width = "30%",
                  height = "25px",
                  style = paste0("padding-bottom: 15px !important;",
                                 "text-align: left;",
                                 "vertical-align: middle;",
                                 "font-weight: bold"),
                  "Idioma"
                ),
                td(
                  width = "70%",
                  style = "padding: 0px; vertical-align: middle;",
                  selectInput(
                    inputId = "l",
                    label = NULL,
                    selected = "português",
                    choices = c("português","español","english"),
                    width = "100%")
                ),
                tr(
                  td(
                    colspan = 2,
                    style = "padding: 0px 0px; height: 5",
                    hr(style="margin: 5px !important")
                  )
                ),
                tr(
                  td(
                    colspan = 2,
                    table(
                      width = "100%",
                      tr(
                        td(
                          width = "80%",
                          style = paste0("text-align:left;",
                                         "font-weight: bold"),
                          "conjuntos de dados"
                        ),
                        td(
                          width = "20%",
                          actionButton(ns("info_bases"), label = "conjuntos"))
                      ),
                      tr(
                        td(
                          colspan = 2,
                          style = paste0("padding: 5px 0px 5px 0px"),
                          selectizeInput(
                            ns("bases"),
                            label = NULL,
                            choices = c("objetivos","eixos"),
                            selected = "objetivos",
                            width = "100%",
                            multiple = TRUE,
                            options = list(
                              valueField = "code",
                              labelField = "code",
                              render =
                                I("{option: function(item, escape) {
 return '<table width = 100% style=\"text-align: left; margin-left:5px;\">'+
          '<tr><td width = 50%>' +
            '<strong>conjunto: </strong>' + escape(item.code) +
          '</td><td width = 50%>' +
            '<strong>source: </strong>' + item.source +
        '</td></tr></table>';
                              }}"),
                              maxItems = 5
                            )
                          )
                        )
                      )
                    )))))))) |>
        # jqui_draggable are required to solve problem with selectize input within
        # a draggable panel
        jqui_draggable(options = list(cancel = ".selectize-control"))),

    # Bases_info_panel
    conditionalPanel(
      "output.show_bases_info_panel !=1",
      ns = ns ,
      absolutePanel(
        top = 120,
        left = 400,
        # width = 400,
        height = "calc(60vh)",
        draggable = TRUE,
        class="panel panel-default",
        style = "z-index: 5000; border-radius:10px 10px 0px 0px;",
        div(
          class = "panel-heading",
          style = "border-radius:10px 10px 0px 0px;",
          "Conjuntos",
          actionLink(
            "bases_info_close_button",
            label = NULL,
            top = 5,
            right = 5,
            style = paste0("position: absolute;",
                           "top: 5px;",
                           "right: 10px;",
                           "padding: 0px;",
                           "font-size: 14px;",
                           "color: black;"),
            icon = icon("times")
          )
        ),

        div(class = "panel-body",
            uiOutput(ns("bases_info_text")),
            style =  paste0("overflow-y:scroll; height: calc(100% - 50px);")
        )
      ) |> jqui_resizable()
    )
  )
}

#' setup_dashboard Server Functions
#'
#' @noRd
mod_setup_dashboard_server <- function(id){
  moduleServer( id, function(input, output, session,RV=RV){
    ns <- session$ns
    show_setup_panel <- reactiveVal(0)
    output$show_setup_panel <- reactive(show_setup_panel())
    outputOptions(output,"show_setup_panel", suspendWhenHidden = FALSE)
    observeEvent(input$setup_button,show_setup_panel(1))
    observeEvent(input$setup_close_button, {
      show_setup_panel(0)
      show_bases_info_panel(1)
    })

    # # Bases selection
    # RV$bases <- reactive({
    #   input$setup_close_button
    #   input$bases |> isolate()
    # })
    #
    # updateSelectizeInput(
    #   inputId = "bases",
    #   choices = c("objetivos","eixos"),
    #   selected = "objetivos",
    #   server = TRUE
    # )

    # Open/close system for bases_info_panel
    show_bases_info_panel <- reactiveVal(1)
    output$show_bases_info_panel <- renderText(show_bases_info_panel())
    outputOptions(output,"show_bases_info_panel", suspendWhenHidden = FALSE)
    observeEvent(input$bases_info_close_button, show_bases_info_panel(show_bases_info_panel()*-1))
    observeEvent(input$info_bases, show_bases_info_panel(show_bases_info_panel()*-1))

    output$bases_info_text <- renderUI({
      tagList(
          withTags(
            table(
              width = "100%",
              tr(
                td(
                  width = "50%",
                  "OBJ" |> strong(),
                  "obj"
                ),
                td(
                  width = "50%",
                  "obj" |> strong(),
                  "obj"
                )),
              tr(
                td(
                  colspan = 2,
                  "Objetivos" |> strong()
                  #meta_methods$name[z]
                )),
              tr(
                td(
                  colspan = 2,
                  style = "text-align: justifY;",
                  "Descrição" |> strong(),
                  "descrição"
                )),
              tr(
                td(
                  colspan = 2,
                  style = "padding: 0px 0px; height: 5",
                  hr(style="margin: 5px !important")
                )))))})

    # De-active loading panel when output$loading is set to ""
    output$loading <- renderText("")
    outputOptions(output, 'loading', suspendWhenHidden=FALSE)
  })
}

## To be copied in the UI
# mod_setup_dashboard_ui("setup_dashboard_1")

## To be copied in the server
# mod_setup_dashboard_server("setup_dashboard_1")
