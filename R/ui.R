#' UI function for the Survey Solutions Listing Application
#'
#' @description Constructs the `shinydashboard` user interface for the listing
#' application, including the control sidebar, Google Map canvas, second-stage
#' sampling tab, and Survey Solutions assignment tab.
#'
#' @param req Internal Shiny request object.
#'
#' @return A `shiny.tag` object representing the dashboard page.
#' @noRd
#' @keywords internal
main_ui <- function(req) {
  shinydashboard::dashboardPage(
    title = "Listing Application",

    ############################################################################
    ##  1. Header
    ############################################################################
    shinydashboard::dashboardHeader(
      titleWidth = "100%",
      title = shiny::fluidRow(
        shiny::column(
          2,
          shiny::div(
            style = "height:20px; text-align: center;",
            shiny::img(src = "www/logoWBDG.png")
          )
        ),
        shiny::column(
          10,
          shiny::div(
            style = "background-color:#0d47a1; margin-left:5%; margin-top:0px; margin-bottom:0px;
                     height: 60px; padding: 30 0 10 0;",
            shiny::h1(
              "JDC - WB Listing Application",
              align = "center",
              style = "color:#FFFFFF; margin:0 0 0 0;"
            )
          )
        )
      )
    ),

    ############################################################################
    ## 2. SIDEBAR (hidden)
    ############################################################################
    shinydashboard::dashboardSidebar(width = 50, disable = TRUE),

    ############################################################################
    ## 3. BODY
    ############################################################################
    shinydashboard::dashboardBody(
      suso_drawing_js(),
      shiny::tags$head(shiny::tags$style(shiny::HTML("
      .skin-blue .main-header .navbar {
        background-color: #0d47a1;
        height: 65px;
        padding: 30 0 10 0;
      }
      .skin-blue .main-header {
        background-color: #0d47a1;
      }
      .content-wrapper {
        background-color: #FFFFFF;
      }
      .help-block {
        color: #0d47a1;
        text-align: justify;
        font-size: 12px;
        border-style: double;
        padding: 5px;
      }
      .skin-blue .main-header .logo {
        background-image: url(logoWBDG.png);
        background-repeat: no-repeat;
        background-size: 250px 70px !important;
        width: 100%;
        height: 60px !important;
        background-color: #0d47a1;
        color: #FFFFFF;
        text-align: center;
        font-weight: bold;
      }
      .skin-blue .main-header .logo:hover {
        color: #FFFFFF;
        background-color: #0d47a1;
      }
      .action-button {
        color: #FFFFFF !important;
        background-color: #0d47a1 !important;
      }
      .confirm {
        color: #FFFFFF !important;
        background-color: #0d47a1 !important;
      }
      .shiny-input-container {
        color: #0d47a1 !important;
      }
      #shiny-notification-panel {
        position: absolute !important;
        top: 10px !important;
        right: 10px !important;
        height: 50px !important;
        width: 400px !important;
      }
      .shiny-notification-content-text {
        overflow-wrap: break-word;
      }
      "))),

      ##########################################################################
      ## MAIN PAGE
      ##########################################################################
      shiny::fluidRow(shiny::br(), shiny::br()),
      shiny::fluidRow(
        shinyjs::useShinyjs(),
        shiny::column(
          width = 3,
          shiny::tabsetPanel(
            id = "side",
            shiny::tabPanel(
              title = "Controls",
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::fileInput(
                    "new_shape",
                    "Upload Boundary shapefiles from Sampling App",
                    multiple = FALSE,
                    width = "100%",
                    accept = c("application/zip", ".zip")
                  ),
                  shiny::helpText(
                    "Only files from the Survey Solutions Spatial Sampling App are accepted.
                     Files must be zipped directly and without a folder!"
                  )
                )
              ),
              shiny::br(), shiny::br(),
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::selectizeInput(
                    "area_id",
                    "Identification Variable",
                    choices = c(""),
                    width = "100%",
                    options = list(
                      placeholder = "Load Boundary file first",
                      onInitialize = I('function() { this.setValue(""); }')
                    )
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  12,
                  DT::DTOutput("AreaSummary", height = 150)
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  12,
                  shinyjs::hidden(
                    shiny::div(
                      id = "navigation",
                      shiny::fluidRow(
                        shiny::column(
                          6,
                          shiny::actionButton(
                            "loadPrevEA",
                            "Previous",
                            width = "100%",
                            icon = shiny::icon("arrow-left"),
                            style = "color: #fff; background-color: #4286f4; border-color: #008e00"
                          )
                        ),
                        shiny::column(
                          6,
                          shiny::actionButton(
                            "loadNextEA",
                            "Next",
                            width = "100%",
                            icon = shiny::icon("arrow-right"),
                            style = "color: #fff; background-color: #7f0000; border-color: #008e00"
                          )
                        )
                      ),
                      shiny::br(), shiny::br(),
                      shiny::fluidRow(
                        shiny::column(3),
                        shiny::column(
                          6,
                          shiny::actionButton(
                            "clearAllArea",
                            "CLEAR AREA",
                            width = "100%",
                            icon = shiny::icon("arrow-right"),
                            style = "color: #fff; background-color: #FF0000; border-color: #FF0000; font-weight: bold;"
                          )
                        ),
                        shiny::column(3)
                      ),
                      shiny::br(),
                      shiny::fluidRow(
                        shiny::column(1),
                        shiny::column(
                          10,
                          shiny::selectizeInput(
                            "loadFile",
                            "Select existing file",
                            choices = c(""),
                            options = list(
                              placeholder = "Load Boundary file first",
                              onInitialize = I('function() { this.setValue(""); }')
                            )
                          )
                        ),
                        shiny::column(1)
                      )
                    )
                  )
                )
              ),
              shiny::br(), shiny::br(),
              shiny::fluidRow(
                shiny::column(2),
                shiny::column(
                  8,
                  shiny::conditionalPanel(
                    "false",
                    shiny::downloadButton("dwl_pop", "Download Locations")
                  ),
                  shiny::conditionalPanel(
                    "true",
                    shiny::actionButton(
                      "downloadActivate",
                      "Download Locations",
                      width = "100%",
                      icon = shiny::icon("file-download"),
                      style = "color: #fff; background-color: #4286f4; border-color: #008e00"
                    )
                  )
                ),
                shiny::column(2)
              )
            ),
            shiny::tabPanel(
              title = "Sample",
              shiny::br(), shiny::br(),
              modal_adminspsample_ui_tabside("sampleadm")
            ),
            shiny::tabPanel(
              title = "SuSo",
              shiny::br(), shiny::br(),
              mapadminUI2_st2("susoassign")
            )
          )
        ),
        shiny::column(
          width = 9,
          shiny::tabsetPanel(
            id = "mapType",
            shiny::tabPanel(
              title = "Google Map",
              value = "Google",
              googleway::google_mapOutput("detailSampMap", width = "73vw", height = "85vh")
            ),
            shiny::tabPanel(
              title = "2stage_sample",
              value = "samp",
              modal_adminspsample_ui_tabmain("sampleadm")
            ),
            shiny::tabPanel(
              title = "Survey Solutions",
              value = "suso",
              mapadminUI_st2("susoassign")
            )
          )
        )
      )
    )
  )
}
