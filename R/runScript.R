# ------------------------------------------------------------------------------------------- #
# A Shiny module to run a script and display the output
# ------------------------------------------------------------------------------------------- #

#' A Shiny UI module to run a script and display the output
#'
#' @param uiId A string that identifies the UI module.
#' @param label A string that specifies the label for the action button.
#' 
#' @returns A list of UI elements including an action button and a text output area.
#' @export
runScriptUI <- function(uiId, label) {
  ns <- shiny::NS(uiId)
  shiny::tagList(
    shiny::actionButton(ns("run_button"), label),
    shiny::verbatimTextOutput(ns("script_output")),
    shiny::br(),
  )
}

#' A Shiny server module to run a script and display the output
#'
#' @param serverId A string that identifies the server module.
#' @param script_path A string that specifies the path to the script to be run.
#' @export
runScriptServer <- function(serverId, script_path) {
  shiny::moduleServer(serverId, function(input, output, session) {
    shiny::observeEvent(input[["run_button"]], {
      out <- tryCatch(
        withr::with_dir(
          new = "..",     # Every script is written to be executed at the root of the project
          utils::capture.output(source(script_path, local = TRUE))
        ),
        warning = function(w) w,
        error = function(e) e
      )
      output[["script_output"]] <- shiny::renderText(paste(out, collapse = "\n"))
    })
    NULL
  })
}

#' A Shiny app to run a script and display the output, for testing purposes
runScriptApp <- function() {
  ui <- shiny::fluidPage(
    runScriptUI("script1", "Run script 1"),
  )
  server <- function(input, output, session) {
    runScriptServer("script1", "scripts/read-msdial.R")
  }
  shiny::shinyApp(ui = ui, server = server)
}

# Module test on `code/` where `app.R` is located
# shiny::runApp(runScriptApp())
