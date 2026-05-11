# ------------------------------------------------------------------------------------------- #
# A Shiny module to collect user's MS-DIAL information
# ------------------------------------------------------------------------------------------- #

# "params.yml": The parameter file in YAML format that stores user's MS-DIAL info
.yml_file <- "../params.yml"

# User project parameters
.user_params <- tibble::tribble(
  ~id,                  ~type,      ~label,
  "project_title",      "text",     "Project title",
  "user",               "text",     "User",
  "university",         "text",     "University",
  "free_text",          "textArea", "Free text to the external report",
  "input_file",         "file",     "Input file (relative path from `home/`)",
  "concentration_unit", "con",     "Concentration unit",
  "intermediate_dir",   "dir",      "Directory to save intermediate files",
  "table_dir",          "dir",      "Directory to save the exported tables",
  "report_dir",         "dir",      "Directory to save the creating reports",



)

# Default values for the parameters
# Wraps everything inside a local() environment.
# Variables created inside do not leak into the global environment.
.init_data_info <- local({
  box::use(R/msdialInfoFuns[read_or_create_params])
  ids <- .user_params$id
  defaults <- lapply(rlang::set_names(ids), \(x) "")
  read_or_create_params(.yml_file, ids, defaults)
})

#' MS-DIAL User Information UI for Shiny
#' 
#' @param uiId The ID of the UI module. It should be matched with the server ID of 
#'  [msdialInfoServer()].
#' 
#' @returns A Shiny UI module for MS-DIAL user information
#' @export
msdialInfoUI <- function(uiId) {
  shiny::tagList(
    
    purrr::pmap(.user_params, function(id, type, label) {
      inputId <- shiny::NS(uiId, id)
      switch(
        type, 
        "text" = shiny::textInput(inputId, 
                                  label, 
                                  value = .init_data_info[[id]], width = "100%"),
        "textArea" = shiny::textAreaInput(inputId, label, 
                                          value = .init_data_info[[id]], width = "100%")

        
      )
      
    }),
    shiny::tags$hr(style = "border-top: 10px solid #FFFFFF; margin: 10px 0;"),
    purrr::pmap(.user_params, function(id, type, label) {
      inputId <- shiny::NS(uiId, id)
      switch(
        type, 
        "con" = shiny::textInput(inputId, 
                                  label, 
                                  value = .init_data_info[[id]], width = "100%"),

        "file" = local({
          outputId <- shiny::NS(uiId, matched_output_id(id))
          rlang::list2(
            # Title of the "Button"
            shiny::strong(label),
            # File name
            shiny::verbatimTextOutput(outputId, placeholder = TRUE),
            shinyFiles::shinyFilesButton(inputId, "Browse",
                                         title = "Select a file",
                                         icon = shiny::icon("file"),
                                         multiple = FALSE),
            shiny::br(),    # Avoids overlap
          )
        })
        
      )
    }),
    shiny::tags$hr(style = "border-top: 10px solid #FFFFFF; margin: 10px 0;"),
      purrr::pmap(.user_params, function(id, type, label) {
        inputId <- shiny::NS(uiId, id)
      switch(
        type,
        "dir" = local({
          outputId <- shiny::NS(uiId, matched_output_id(id))
          rlang::list2(
            # Title of the "Button"
            shiny::strong(label),
            shiny::fluidRow(      # Horizontal layout
              # Directory name
              shiny::column(
                width = 8,
                shiny::verbatimTextOutput(outputId, placeholder = TRUE),
              ),
              # "Browse" button
              shiny::column(
                width = 2,
                shinyFiles::shinyDirButton(inputId, "Browse",
                                           title = "Select a directory",
                                           icon = shiny::icon("folder-open"),
                                           multiple = FALSE),
              )
            )
          )
        })
      )
      
    }),
    shiny::tags$hr(style = "border-top: 10px solid #FFFFFF; margin: 10px 0;"),
    shiny::actionButton(inputId = shiny::NS(uiId, "auto_fill"),
                        label = "Auto-fill intermediate dir to rest",
                        disabled = TRUE),
    # shiny::br(),
    # shiny::br(),
  
    shiny::actionButton(inputId = shiny::NS(uiId, "save_param_button"), 
                        label = "Save the parameters above"),
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
#' @include msdialInfoFuns.R
#' @export
msdialInfoServer <- function(serverId) {
  # The inputs obtained by [shinyFiles::shinyDirButton()]
  user_dirs <- .user_params$id[.user_params$type == "dir"]
  txt_ids <- .user_params$id[.user_params$type %in% c("text", "textArea")]
  .roots <- c('home' = "..")     # The root directory for the file/directory chooser
  .from_roots <- c('home' = ".")    # Returned file/directory path
  
  shiny::moduleServer(serverId, function(input, output, session) {
    # Reactive values to store user data information
    data_info <- do.call(shiny::reactiveValues, .init_data_info)
    # Text inputs are updated as the inputs change
    for(id in txt_ids) {
      shiny::observe(data_info[[id]] <- input[[id]], env = list2env(list(id = id)))
    }
    
    observeFileAndUpdate <- function(id) {
      # File choose ("Browse") button
      shinyFiles::shinyFileChoose(input, id, roots = .roots, 
                                  defaultPath = data_info[[id]])
      # Observe the "Browse" button click event for file choosing
      shiny::observeEvent(input[[id]], {
        # Update the file name when the user selects a file
        data_info[[id]] <- .getShinyFileName(button = input[[id]], 
                                             default = data_info[[id]], 
                                             roots = .from_roots)
      })
      # Display the selected file name
      output[[matched_output_id(id)]] <- shiny::renderText(data_info[[id]])
      NULL
    }
    observeFileAndUpdate("input_file")
    
    observeDirAndUpdate <- function(id) {
      # Directory choose ("Browse") button
      shinyFiles::shinyDirChoose(input, id, roots = .roots, defaultPath = "")
      # Observe the "Browse" button click event for directory choosing
      shiny::observeEvent(input[[id]], {
        data_info[[id]] <- .getShinyDirName(button = input[[id]], 
                                            default = data_info[[id]], 
                                            roots = .from_roots)
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
    
    # Observe user choosing an intermediate dir and enable auto_fill button
    observe({
      req(data_info[[user_dirs[[1]]]])
      updateActionButton(inputId = "auto_fill",
                         disabled=FALSE)
    })

    
    #  Observe auto_fill button and fill up other two fields
    shiny::observeEvent(input$auto_fill, {
      for(id in user_dirs[2:3]){
        data_info[[id]] <- data_info[[user_dirs[[1]]]]
      }
    })
    # Return of this module, to be used by other modules
    return(data_info)
  })
}

#' MS-DIAL User Information App for modular testing
#' @include msdialInfoFuns.R
msdialInfoApp <- function() {
  ui <- shiny::fluidPage(
    msdialInfoUI("data_info"),
  )
  server <- function(input, output, session) {
    msdialInfoServer("data_info")
  }
  shiny::shinyApp(ui, server)
}

# Module test on `code/` where `app.R` is located
# source("R/msdialInfoFuns.R")
# source("R/msdialInfo.R")
# shiny::runApp(msdialInfoApp())
