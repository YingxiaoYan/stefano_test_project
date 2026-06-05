# ------------------------------------------------------------------------------------------- #
# A Shiny module to run a script and display the output
# ------------------------------------------------------------------------------------------- #

# "params.yml": The parameter file in YAML format that stores user's MS-DIAL info
# .yml_file <- "../params.yml"

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
    shiny::verbatimTextOutput(ns("script_output"), placeholder = TRUE),
    shiny::br(),   # Add a line break for better spacing
  )
}

#' A Shiny UI module to run a script and display the output
#'
#' @param uiId A string that identifies the UI module.
#' @param label A string that specifies the label for the action button.
#' 
#' @returns A list of UI elements including an action button and a text output area.
#' @export
runScriptUI2 <- function(uiId, label) {
  ns <- shiny::NS(uiId)
  shiny::tagList(
    shiny::actionButton(ns("run_button"), label),
    shiny::verbatimTextOutput(ns("script_output"), placeholder = TRUE),
    shiny::br(),   # Add a line break for better spacing
  )
}


#' A Shiny server module to run a script and display the output
#'
#' @param serverId A string that identifies the server module.
#' @param script_path A string that specifies the path to the script to be run.
#' @param what A string that specifies the context or purpose of the script.
#' @param param_ids A list of parameter IDs to be passed to the script (optional).

#' @export
runScriptServer <- function(serverId, script_path, what, param_ids = NULL, data_info) {
  shiny::moduleServer(serverId, function(input, output, session) {
    output_txt <- shiny::reactiveVal("")
    proc <- shiny::reactiveVal(NULL)
    timer <- shiny::reactiveTimer(1000)
    # data_info <- shiny::reactive(data_info)
    
    shiny::observeEvent(input[["run_button"]], {
      
      # user_inputs <- msdial$get_user_input()
      # FILE <- list(
      #   i = rlang::list2(
      #     # Intermediate status of the data
      #     to_rep = msdial$get_raw_data_file_name(user_inputs, suffix = "to_report"),
      #   )
      # )
      
      # Save .yml file with parameters
      save_user_data_info(.yml_file, .user_params$id, 
                          # data_info)
                          shiny::reactiveValuesToList(data_info))
      
      # Launch the script
      tryCatch({
        # Ensure the script path is valid
        script_path <- normalizePath(script_path, mustWork = TRUE)
        args <- c(script_path)
        

        
        if (!is.null(param_ids)) {
          # If parameter IDs are provided, append parameter values to the args
          for (param_id in param_ids) {
            param_value <- input[[param_id]]
            if (!is.null(param_value)) {
              args <- c(args, paste0("--", param_id, "=", param_value))
            }
          }
        }
        p <- processx::process$new(
          command = "Rscript",
          args = args,
          stdout = "|",   # Pipe output to R
          stderr = "|",   # Pipe errors to R
          poll_connection = TRUE,  # Poll the connection for output
          wd = ".."   # Every script is written to be executed at the root of the project
        )
        proc(p)  # Store the process object
        output_txt(paste(what, "is running...\nTime:", Sys.time(), "\n"))
      }, error = function(e) {
        output_txt(paste(getwd(), "\nError running script:", e$message, sep = "\n"))
        proc(NULL)  # Clear the process object on error
      })
    })
    
    shiny::observe({
      timer()  # Trigger the timer to poll the process output
      p <- proc()
      if (is.null(p)) return()  # If process is not set, do nothing
      # Read output/error lines from the process
      out <- p$read_output_lines()
      err <- p$read_error_lines()
      shiny::isolate({
        output_txt(paste0(output_txt(), "#"))
        all_lines <- c(out, err) |>
          paste(collapse = "\n")  # Combine all lines into a single string
        if (length(out) > 0 || length(err) > 0) {
          # append new output to the existing text
          output_txt(paste(output_txt(), all_lines, "", sep = "\n"))
        }
        if (! p$is_alive()) {    # Process has finished
          output_txt(paste(output_txt(), "Data successfully processed!", sep = "\n"))
          proc(NULL)  # Clear the process object
        }
      })
    })

    output[["script_output"]] <- shiny::renderText({
      # Render the output text
      output_txt()
    })
    NULL
  })
}



#' A Shiny server module to run a script and display the output
#'
#' @param serverId A string that identifies the server module.
#' @param script_path A string that specifies the path to the script to be run.
#' @param what A string that specifies the context or purpose of the script.
#' @param param_ids A list of parameter IDs to be passed to the script (optional).
#' @param input1 normalization method
#' @param input2 blamk substraction or not
#' @export
runScriptServer2 <- function(serverId, script_path, what, param_ids = NULL, data_info,
                            input1=NULL,input2=NULL) {
  shiny::moduleServer(serverId, function(input, output, session) {
    output_txt <- shiny::reactiveVal("")
    proc <- shiny::reactiveVal(NULL)
    timer <- shiny::reactiveTimer(1000)
    # data_info <- shiny::reactive(data_info)
    
    shiny::observeEvent(input[["run_button"]], {
      
      # user_inputs <- msdial$get_user_input()
      # FILE <- list(
      #   i = rlang::list2(
      #     # Intermediate status of the data
      #     to_rep = msdial$get_raw_data_file_name(user_inputs, suffix = "to_report"),
      #   )
      # )
      
      # Save .yml file with parameters
      save_user_data_info(.yml_file, .user_params$id, 
                          # data_info)
                          shiny::reactiveValuesToList(data_info))
      
      # Launch the script
      tryCatch({
        # Ensure the script path is valid
        script_path <- normalizePath(script_path, mustWork = TRUE)
       
        
        norm_method <- input1()   # ✅ call reactive
        blk <- input2()           # ✅ call reactive
        
        
        args <- c(script_path)
        
        if (!is.null(input1)) {
          args <- c(args, paste0("--input1=", norm_method))
        }
        if (!is.null(input2)) {
          args <- c(args, paste0("--input2=", blk))
        }
        
        if (!is.null(param_ids)) {
          # If parameter IDs are provided, append parameter values to the args
          for (param_id in param_ids) {
            param_value <- input[[param_id]]
            if (!is.null(param_value)) {
              args <- c(args, paste0("--", param_id, "=", param_value))
            }
          }
        }
        p <- processx::process$new(
          command = "Rscript",
          args = args,
          stdout = "|",   # Pipe output to R
          stderr = "|",   # Pipe errors to R
          poll_connection = TRUE,  # Poll the connection for output
          wd = ".."   # Every script is written to be executed at the root of the project
        )
        proc(p)  # Store the process object
        output_txt(paste(what, "is running...\nTime:", Sys.time(), "\n"))
      }, error = function(e) {
        output_txt(paste(getwd(), "\nError running script:", e$message, sep = "\n"))
        proc(NULL)  # Clear the process object on error
      })
    })
    
    shiny::observe({
      timer()  # Trigger the timer to poll the process output
      p <- proc()
      if (is.null(p)) return()  # If process is not set, do nothing
      # Read output/error lines from the process
      out <- p$read_output_lines()
      err <- p$read_error_lines()
      shiny::isolate({
        output_txt(paste0(output_txt(), "#"))
        all_lines <- c(out, err) |>
          paste(collapse = "\n")  # Combine all lines into a single string
        if (length(out) > 0 || length(err) > 0) {
          # append new output to the existing text
          output_txt(paste(output_txt(), all_lines, "", sep = "\n"))
        }
        if (! p$is_alive()) {    # Process has finished
          output_txt(paste(output_txt(), "Data successfully processed!", sep = "\n"))
          proc(NULL)  # Clear the process object
        }
      })
    })
    
    output[["script_output"]] <- shiny::renderText({
      # Render the output text
      output_txt()
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
    runScriptServer("script1", "scripts/read-msdial.R", "Read MS-Dial")
  }
  shiny::shinyApp(ui = ui, server = server)
}

# Module test on `code/` where `app.R` is located
# shiny::runApp(runScriptApp())
