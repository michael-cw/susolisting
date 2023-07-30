#' Ui function for listing App
#'
#' @noRd
#' @keywords internal
#'


main_ui<-function(req) {
  dashboardPage(
    title = "Listing Application",

    ##################################################################################
    ##  1. Header
    ##################################################################################
    shinydashboard::dashboardHeader(
      titleWidth = "100%",
      title = fluidRow(column(2, div(style="height:20px; text-align: center;",
                                     img(src="www/logoWBDG.png"))),
                       column(10, div(style="background-color:#0d47a1; margin-left:5%; margin-top:0px; margin-bottom:0px;
                                                    height: 60px; padding: 30 0 10 0;",
                                      h1("JDC - WB Listing Application", align="center", style="color:#FFFFFF; margin:0 0 0 0;"))))
    ),


    ##################################################################################
    ## 2. SIDEBAR
    ##    - hidden and not used
    ##################################################################################
    shinydashboard::dashboardSidebar(width = 50, disable = T),

    ##################################################################################
    ## 3. BODY
    ##################################################################################

    shinydashboard::dashboardBody(
      tags$head(tags$style(HTML('
      .skin-blue .main-header .navbar{
        background-color: #0d47a1;
        height: 65px;
        padding: 30 0 10 0;
      }
    '))),
      tags$head(tags$style(HTML('
      .skin-blue .main-header {
        background-color: #0d47a1;
      }
    '))),
      tags$head(tags$style(HTML('
      .content-wrapper{
        background-color: #FFFFFF;
      }
      .help-block {color:#0d47a1; text-align: justify;font-size: 12px;border-style:double;padding:5px;}
    '))),

      tags$head(tags$style(HTML('
      .skin-blue .main-header .logo{
        background-image: url(logoWBDG.png);
        background-repeat: no-repeat;
        background-size: 250px 70px !important;
        background-color: "#FFFFFF;
        width: 100%;
        height: 60px !important;
        background-color:#0d47a1;
        color: #FFFFFF;
        text-align: center;
        font-weight: bold;
      }
    '))),
      tags$head(tags$style(HTML('
      .skin-blue .main-header .logo:hover{
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
    '))),


      ################################################################################################################
      ##      MAIN PAGE
      ##        INCLUDES: Full map, detail map, boxes
      fluidRow(br(), br()),
      fluidRow(
        shinyjs::useShinyjs(),
        ## shiny alert conditional on version
        if (packageVersion("shinyalert")<3) shinyalert::useShinyalert(),
        column(width=2,
               shiny::tabsetPanel(
                 id = "side",
                 shiny::tabPanel(
                   title = "Controls",
                   fluidRow(
                     column(12,
                            fileInput("new_shape", "Upload Boundary shapefiles from Sampling App",
                                      multiple = F, width = "100%",
                                      accept = (c("application/zip", ".zip"))),
                            helpText("Only files from the Survey Solutions Spatial Sampling App are accepted.
                    Files must be zipped directly and without a folder!")
                     )
                   ),br(), br(),
                   fluidRow(
                     column(12,
                            selectizeInput("area_id", "Identification Variable", choices = c(""), width = "100%",
                                           options = list(
                                             placeholder = 'Load Boundary file first',
                                             onInitialize = I('function() { this.setValue(""); }')
                                           )
                            )
                     )
                   ),
                   fluidRow(
                     column(12,
                            DT::dataTableOutput("AreaSummary", height = 150))
                   ),
                   fluidRow(
                     column(12,
                            shinyjs::hidden(
                              div(id="navigation",
                                  fluidRow(column(6,
                                                  actionButton("loadPrevEA", "Previous",
                                                               width = "100%", icon = icon("arrow-left"),
                                                               style="color: #fff; background-color: #4286f4; border-color: #008e00")),
                                           column(6,
                                                  actionButton("loadNextEA", "Next",
                                                               width = "100%", icon = icon("arrow-right"),
                                                               style="color: #fff; background-color: #7f0000; border-color: #008e00")
                                           )
                                  ),br(), br(),
                                  fluidRow(column(3),
                                           column(6,
                                                  actionButton("clearAllArea", "CLEAR AREA",
                                                               width = "100%", icon = icon("arrow-right"),
                                                               style="color: #fff;
                                                       background-color: #FF0000;
                                                       border-color: #FF0000;
                                                       font-weight: bold;")),
                                           column(3)),
                                  br(),
                                  fluidRow(column(1),
                                           column(10,
                                                  selectizeInput("loadFile", "Select existing file", choices = c(""),
                                                                 options = list(
                                                                   placeholder = 'Load Boundary file first',
                                                                   onInitialize = I('function() { this.setValue(""); }')
                                                                 )
                                                  )),
                                           column(1))
                              )
                            )
                     )
                   ), br(), br(),
                   fluidRow(
                     column(2),
                     column(8,
                            conditionalPanel("false",
                                             downloadButton("dwl_pop", "Download Locations")),
                            conditionalPanel("true",
                                             actionButton("downloadActivate", "Download Locations",
                                                          width = "100%", icon = icon("file-download"),
                                                          style="color: #fff; background-color: #4286f4; border-color: #008e00"))),
                     column(2)
                   )
                 ),
                 shiny::tabPanel(
                   title = "Admin"
                 )
               )
        ),
        column(width=10,
               shiny::tabsetPanel(id="mapType",
                                  shiny::tabPanel(title = "Google Map", value = "Google",
                                                  googleway::google_mapOutput("detailSampMap", width = "100%", height = 900)
                                  )
               )
        )
      ),
      fluidRow(
        column(width=2
        ),
        column(width=10)
      )
    )


  )}
