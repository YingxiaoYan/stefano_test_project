# ------------------------------------------------------------------------------------------- #
# A Shiny module to generate a report
# ------------------------------------------------------------------------------------------- #

#' A Shiny UI module to generate a report
#'
#' @param uiId A string that identifies the UI module.
#'
#' @returns A list of UI elements for generating a report.
#' @export
genReportUI <- function(uiId) {
  ns <- shiny::NS(uiId)
  shiny::tagList(
    shiny::radioButtons(
      inputId = ns("norm_method"),
      label = "Normalization method",
      choices = c("LOESS" = "loess_norm", 
                  "Closest RT" = "closest_norm"),
      selected = "loess_norm",
    ),
    shiny::br(),
    
    shiny::actionButton(
      inputId = ns("gen_internal"),
      label = "Generate internal report",
    ),
    shiny::textOutput(ns("gen_internal_out")),
    shiny::br(),
    
    shiny::actionButton(
      inputId = ns("gen_external"),
      label = "Generate external report",
    ),
    shiny::textOutput(ns("gen_external_out")),
  )
}

#' A Shiny server module to generate a report
#'
#' @param serverId A string that identifies the server module. It should match the UI module's
#'   ID.
#' @param data_info A list of reactive values containing information about the data and output
#'   directory.
#'
#' @returns NULL
#' @export
genReportServer <- function(serverId, data_info) {
  shiny::moduleServer(serverId, function(input, output, session) {
    txt <- shiny::reactiveValues(gen_internal = "", gen_external = "")
    
    # Extract file base name without extension
    fn <- shiny::reactive({
      out <- basename(data_info[["input_file"]]) |>
        tools::file_path_sans_ext(compression = TRUE)
      paste0(out, "-", input$norm_method)
    })
    # Output directory
    out_dir <- shiny::reactive(data_info[["report_dir"]])

    observeButtonAndRun <- function(type, out_fname) {
      id <- paste0("gen_", type)
      qmd <- paste0("report-", type, ".qmd")
      shiny::observeEvent(input[[id]], {
        txt[[id]] <- withr::with_dir(
          new = "reports",            # Otherwise, "Could not fetch resource..."
          tryCatch(
            {
              odir <- file.path("../..", out_dir())      # Matched with the with_dir() above
              quarto::quarto_render(
                qmd,
                output_format = tools::file_ext(out_fname()),  # "html" or "pdf"
                output_file = out_fname(),
                execute_params = rlang::list2(
                  norm_method = input[["norm_method"]],
                  project_title = data_info[["project_title"]],
                ),
                quarto_args = c("--output-dir", odir)
              )
              paste0(stringr::str_to_title(type),
                     " reports `", out_fname(), "` generated on `", out_dir(), "`")
            },
            warning = function(w) w,
            error = function(e) e
          )
        )
      })
      output[[paste0(id, "_out")]] <- shiny::renderText(txt[[id]])
    }
    
    # Generate internal report
    observeButtonAndRun("internal", shiny::reactive(paste0(fn(), "-internal.html")))
    # Generate external report
    observeButtonAndRun("external", shiny::reactive(paste0(fn(), ".pdf")))
    NULL
  })
}

#' A Shiny app to generate a report, for testing purposes
genReportApp <- function() {
  ui <- shiny::fluidPage(
    genReportUI("gen_report"),
  )
  server <- function(input, output) {
    # Placeholder for data_info
    data_info <- shiny::reactiveValues(
      input_file = "path/to/input_file_example.msdial",
      report_dir = "../../reports",
    )
    genReportServer("gen_report", data_info)
  }
  shiny::shinyApp(ui = ui, server = server)
}

# Module test on `code/` where `app.R` is located
# shiny::runApp(genReportApp())

