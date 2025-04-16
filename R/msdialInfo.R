# ------------------------------------------------------------------------------------------- #
# A Shiny module to collect user's MS-DIAL information
# ------------------------------------------------------------------------------------------- #

options(box.path = "./")   # Where "app.R" is located, "code/"
box::use(R/msdialInfoFuns[...])

# source("R/msdialInfoFuns.R")     # Outsource to simplify this script
roots <- c('wd' = "..")     # This R project home
# "params.yml": The parameter file in YAML format that stores user's MS-DIAL info
.yml_file <- "../params.yml"

#' User project parameters
.user_params <- tibble::tribble(
  ~id,                  ~type,      ~label,
  "title",              "text",     "Title",
  "input_file",         "file",     "Input file",
  "intermediate_dir",   "dir",      "Directory to save intermediate files",
  "table_dir",          "dir",      "Directory to save the exported tables",
  "report_dir",         "dir",      "Directory to save the creating reports",
  "concentration_unit", "text",     "Concentration unit",
  "user",               "text",     "User",
  "university",         "text",     "University",
  "project_title",      "text",     "Project title",
  "free_text",          "textArea", "Free text to the external report",
)

# Default values for the parameters
ids <- .user_params$id
defaults <- lapply(rlang::set_names(ids), \(x) "")
init_data_info <- read_or_create_params(.yml_file, ids, defaults)

#' MS-DIAL User Information UI for Shiny
#' 
#' @param uiId The ID of the UI module. It should be matched with the server ID of 
#'  [msdialInfoServer()].
#' 
#' @returns A Shiny UI module for MS-DIAL user information
#' @md
#' @export
msdialInfoUI <- function(uiId) {
  shiny::tagList(
    purrr::pmap(.user_params, function(id, type, label) {
      inputId <- shiny::NS(uiId, id)
      dplyr::case_match(
        type,
        c("text", "textArea") ~ "txt",
        c("file", "dir") ~ "browse",
      ) |> 
      # [dplyr::case_match()] runs every output and select matched ones, 
      # while [switch()] works like multiple if-else statements
      switch(
        # Ordinary text outputs
        "txt" = textInputOfOneParam(inputId, type, label, value = init_data_info[[id]]),
        "browse" = local({
          outputId <- shiny::NS(uiId, matched_output_id(id))
          rlang::list2(
            # Title of the "Button"
            shiny::strong(label),
            shiny::fluidRow(      # Horizontal layout
              # File/Directory name
              shiny::column(
                width = 8,
                shiny::verbatimTextOutput(outputId, placeholder = TRUE),
              ),
              # "Browse" button
              shiny::column(
                width = 2,
                shinyButtonOfOneParam(inputId, type),
              )
            ),
          )
        })
      )
    }),
    shiny::actionButton(inputId = shiny::NS(uiId, "save_param_button"), 
                        label = "Save the paramters above"),
    # Display the "saved" message
    shiny::verbatimTextOutput(shiny::NS(uiId, "saved")),
  )
}


#' MS-DIAL User Information Server for Shiny
#' 
#' @param serverId The ID of the server module. It should be matched with the UI ID of
#'   [msdialInfoUI()].
#' 
#' @returns A list of reactive values containing the user data information
#' @md
#' @export
msdialInfoServer <- function(serverId) {
  # The inputs obtained by [shinyFiles::shinyDirButton()]
  user_dirs <- .user_params$id[.user_params$type == "dir"]
  
  shiny::moduleServer(serverId, function(input, output, session) {
    # Reactive values to store user data information
    data_info <- do.call(shiny::reactiveValues, init_data_info)
    
    observeFileAndUpdate <- function(id) {
      # File choose ("Browse") button
      shinyFiles::shinyFileChoose(input, id, roots = roots, defaultPath = data_info[[id]])
      # Observe the "Browse" button click event for file choosing
      shiny::observeEvent(input[[id]], {
        # Update the file name when the user selects a file
        data_info[[id]] <- getShinyFileName(button = input[[id]], 
                                            default = data_info[[id]], 
                                            roots = roots)
      })
      # Display the selected file name
      output[[matched_output_id(id)]] <- shiny::renderText(data_info[[id]])
      NULL
    }
    observeFileAndUpdate("input_file")

    observeDirAndUpdate <- function(id) {
      # Directory choose ("Browse") button
      shinyFiles::shinyDirChoose(input, id, roots = roots, defaultPath = "")
      # Observe the "Browse" button click event for directory choosing
      shiny::observeEvent(input[[id]], {
        data_info[[id]] <- getShinyDirName(button = input[[id]], 
                                           default = data_info[[id]], 
                                           roots = roots)
      })
      # Update the directory name when the user selects a directory
      output[[matched_output_id(id)]] <- shiny::renderText(data_info[[id]])
      NULL
    }
    for (id in user_dirs) {
      observeDirAndUpdate(id)
    }
    
    # Observe the "save" button click event
    shiny::observeEvent(input$save_param_button, {
      # Save user inputs to the YAML file
      save_user_data_info(.yml_file, .user_params$id, 
                          shiny::reactiveValuesToList(data_info))
      output$saved <- renderText("User data information saved successfully.")
    })
    # Return of this module, to be used by other modules
    data_info
  })
}

#' MS-DIAL User Information App for modular testing
msdialInfoApp <- function() {
  ui <- shiny::fluidPage(
    msdialInfoUI("data_info"),
  )
  server <- function(input, output, session) {
    msdialInfoServer("data_info")
  }
  shiny::shinyApp(ui, server)
}
