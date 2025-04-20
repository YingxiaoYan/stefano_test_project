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
  shiny::tagList(
    shiny::actionButton(
      inputId = shiny::NS(uiId, "gen_intnl"),
      label = "Generate internal report",
    ),
    shiny::textOutput(shiny::NS(uiId, "gen_intnl_out")),
    shiny::br(),
    
    shiny::actionButton(
      inputId = shiny::NS(uiId, "gen_extnl"),
      label = "Generate external report",
    ),
    shiny::textOutput(shiny::NS(uiId, "gen_extnl_out")),
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
    txt <- shiny::reactiveValues(i = "", e = "")
    
    # Extract file base name without extension
    fn <- shiny::reactive({
      basename(data_info[["input_file"]]) |> 
        tools::file_path_sans_ext(compression = TRUE)
    })
    # Output directory
    out_dir <- shiny::reactive(data_info[["report_dir"]])
    
    shiny::observeEvent(input[["gen_intnl"]], {
      txt$i <- withr::with_dir(
        new = "code/reports",            # Otherwise, "Could not fetch resource..."
        tryCatch(
          {
            out_fname <- paste0(fn(), "-internal.html")
            odir <- file.path("../../", out_dir())      # Matched with the with_dir() above
            quarto::quarto_render(
              "report-internal.qmd",
              output_format = "html",
              output_file = out_fname,
              quarto_args = c("--output-dir", odir)
            )
            paste0("Internal reports `", out_fname, "` generated on `", out_dir(), "`")
          },
          warning = function(w) w,
          error = function(e) e
        )
      )
    })
    output[["gen_intnl_out"]] <- shiny::renderText(txt$i)
    
    shiny::observeEvent(input[["gen_extnl"]], {
      txt$e <- withr::with_dir(
        new = "code/reports",            # Otherwise, "Could not fetch resource..."
        tryCatch(
          {
            out_fname <- paste0(fn(), ".pdf")
            odir <- file.path("../../", out_dir())      # Matched with the with_dir() above
            quarto::quarto_render(
              "report-external.qmd",
              output_format = "pdf",
              output_file = out_fname,
              quarto_args = c("--output-dir", odir)
            )
            paste0("External reports `", out_fname, "` generated on `", out_dir(), "`")
          },
          warning = function(w) w,
          error = function(e) e
        )
      )
    })
    output[["gen_extnl_out"]] <- shiny::renderText(txt$e)
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
      report_dir = "reports",
    )
    genReportServer("gen_report", data_info)
  }
  shiny::shinyApp(ui = ui, server = server)
}

# Run this module
# shiny::runApp(genReportApp())

