# ------------------------------------------------------------------------------------------- #
# A Shiny module to run a processing script allowing some selections
# ------------------------------------------------------------------------------------------- #

#' A Shiny UI module to run a processing script allowing some selections
#'
#' @param uiId A string that identifies the UI module.
#' 
#' @returns A list of UI elements
#' @export
runProcessUI <- function(uiId) {
  ns <- shiny::NS(uiId)
  shiny::tagList(
    # Quality control: outlier removal
    shiny::checkboxInput(
      inputId = ns("rm_outlier"),
      label = "Quality control: remove outlier samples?",
      value = TRUE,
    ),
    shiny::checkboxInput(
      inputId = ns("log_calibration"),
      label = "Log scale for calibration curve fitting?",
      value = FALSE,
    ),
    shiny::radioButtons(
      inputId = ns("weight"),
      label = "Weighting method",
      selected = "largestR2",
      choiceValues = list("largestR2", "1", "1_div_x", "1_div_x2"),
      choiceNames = list(
        shiny::HTML(paste0("Largest R", shiny::tags$sup("2"), "(iterative)")),
        "1",
        "1/x",
        shiny::HTML(paste0("1/x", shiny::tags$sup("2")))
      ),
    ),
    shiny::radioButtons(
      inputId = ns("llox_method"),
      label = "LOD/LLOQ method",
      selected = "pt_signal_mean_plus_sd",
      choiceValues = list("pt_signal_mean", "pt_signal_mean_plus_sd"),
      choiceNames = list(
        shiny::HTML(paste0("pt. > 3 (or 10) * mean", shiny::tags$sub("Cal0"))),
        shiny::HTML(paste0("pt. > mean", shiny::tags$sub("Cal0"), " + 3 (or 10) * ", 
                           "StdDev", shiny::tags$sub("Cal0")))
      ),
    ),
    shiny::actionButton(ns("run_button"), label = "Process data"),
    shiny::verbatimTextOutput(ns("script_output")),
    shiny::br(),
  )
}

#' A Shiny server module 
#'
#' @param serverId A string that identifies the server module.
#' @export
runProcessServer <- function(serverId) {
  runScriptServer(
    serverId, 
    script_path = "scripts/process.R", 
    what = "Processing data", 
    param_ids = c(
      "rm_outlier", 
      "log_calibration",
      "weight", 
      "llox_method"
    )
  )
}

#' A Shiny app for module testing purposes
runProcessApp <- function() {
  ui <- shiny::fluidPage(
    runProcessUI("proc1"),
  )
  server <- function(input, output, session) {
    runProcessServer("proc1")
  }
  shiny::shinyApp(ui = ui, server = server)
}

# Module test on `code/` where `app.R` is located
# shiny::runApp(runProcessApp())
