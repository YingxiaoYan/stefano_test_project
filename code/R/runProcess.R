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
    
    
    shiny::tags$style(HTML("
  .shiny-options-group label {
    white-space: nowrap;
  }
                           ")),

    
    
    # Option for keeping all calibration points (even though outside range)
    shiny::checkboxInput(
      inputId = ns("optimize_cal_points"),
      label = "Optimize cal curves to exclude points outside sample range",
      value = FALSE,
      width="100%"
    ),
    # JS handler to enable/disable weight options
    shiny::tags$script(HTML("
      Shiny.addCustomMessageHandler('toggleWeightOptions', function(message) {
        let radios = document.querySelectorAll('input[name=\"' + message.inputId + '\"]');
        if (radios.length >= 4) {
          radios[0].disabled = !message.enabled; // largestR2
          radios[2].disabled = !message.enabled; // 1/x
          radios[3].disabled = !message.enabled; // 1/x^2
        }
      });
    ")),
    shiny::tags$hr(style = "border-top: 10px solid #FFFFFF; margin: 10px 0;"),
    # Quality control: outlier removal
    shiny::checkboxInput(
      inputId = ns("rm_outlier"),
      label = "Quality control: remove outlier samples",
      value = TRUE,
      width="100%"
    ),
    shiny::tags$hr(style = "border-top: 10px solid #FFFFFF; margin: 10px 0;"),
    shiny::checkboxInput(
      inputId = "blk_filtering",
      label = "Quality control: blank filtering",
      value = TRUE,
      width="100%"
    ),
    

    numericInput(
      inputId = "blank_filtering_factor",
      label = "Blank filtering factor (> 1):",
      value = 1,
      min = 1,
      width="100%"
    ),
    
    
    shiny::tags$hr(style = "border-top: 10px solid #FFFFFF; margin: 10px 0;"),
    shiny::checkboxInput(
      inputId = ns("log_calibration"),
      label = HTML("Log scale for calibration curve fitting"),
      value = FALSE,
      width="100%"
      
    ),
    shiny::tags$hr(style = "border-top: 10px solid #FFFFFF; margin: 10px 0;"),
    shiny::radioButtons(
      inputId = ns("weight"),
      label = "Weighting method",
      selected = "largestR2",
      inline = TRUE,   # ✅ add this
      choiceValues = list("largestR2", "1", "1_div_x", "1_div_x2"),
      choiceNames = list(
        shiny::HTML(paste0("Largest R", shiny::tags$sup("2"), "(iterative)")),
        "1",
        "1/x",
        shiny::HTML(paste0("1/x", shiny::tags$sup("2")))
      ),
    ),
    shiny::tags$hr(style = "border-top: 10px solid #FFFFFF; margin: 10px 0;"),
    shiny::radioButtons(
      inputId = ns("llox_method"),
      label = "LOD/LLOQ method",
      selected = "pt_signal_mean_plus_sd",
      choiceValues = list("pt_signal_mean", "pt_signal_mean_plus_sd"),
      choiceNames = list(
        shiny::HTML(paste0("pt. > 3 (or 10) * mean", shiny::tags$sub("Cal0"))),
        shiny::HTML(paste0("pt. > mean", shiny::tags$sub("Cal0"), " + 3 (or 10) * StdDev", shiny::tags$sub("Cal0")))
      ),
    ),
    shiny::tags$hr(style = "border-top: 10px solid #FFFFFF; margin: 10px 0;"),
    # RSD% 20% threshold for LLOQ
    shiny::checkboxInput(
      inputId = ns("use_rsd20"),
      label = "Use RSD% 20% threshold for LLOQ",
      value = TRUE,
    ),
    shiny::tags$hr(style = "border-top: 10px solid #FFFFFF; margin: 10px 0;"),
    shiny::actionButton(ns("run_button"), label = "Process data")

  )
}


#' A Shiny UI module to run a processing script allowing some selections
#'
#' @param uiId A string that identifies the UI module.
#' 
#' @returns A list of UI elements
#' @export
runProcessUI_2<- function(uiId) {
  ns <- shiny::NS(uiId)
  shiny::tagList(
    # Option for keeping all calibration points (even though outside range)

  
    shiny::verbatimTextOutput(ns("script_output"),placeholder = TRUE)
    
  )
}

#' A Shiny server module 
#'
#' @param serverId A string that identifies the server module.
#' @export
runProcessServer <- function(serverId, data_info) {
  
  shiny::moduleServer(serverId, function(input, output, session) {
    
    # Disable/enable weight options based on log_calibration
    shiny::observe({
      enabled <- !isTRUE(input$log_calibration)
      session$sendCustomMessage("toggleWeightOptions", list(
        inputId = session$ns("weight"), enabled = enabled
      ))
      
      # Reset selection if user had picked a disabled option
      if (!enabled && input$weight %in% c("largestR2", "1_div_x", "1_div_x2")) {
        updateRadioButtons(session, "weight", selected = "1")
      }
    })
  })  
  runScriptServer(
    serverId, 
    script_path = "scripts/process.R", 
    what = "Processing data", 
    param_ids = c(
      "optimize_cal_points",
      "rm_outlier", 
      "log_calibration",
      "weight", 
      "llox_method",
      "use_rsd20"
    ),
    data_info = data_info
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
