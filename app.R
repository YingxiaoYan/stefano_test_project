# ------------------------------------------------------------------------------------------- #
# This is a Shiny web application.
# ------------------------------------------------------------------------------------------- #

# Load packages and Shiny modules
# options(box.path = "code/")   # No need because app.R is executed where it is located, `code`
box::use(
  shiny[...],
  info = R/msdialInfo,
)
# options(readr.show_progress = FALSE)
# options(shiny.maxRequestSize = 100 * 2^20)

# All scripts and Quarto files are written to be run from the project root directory.
# wd <- normalizePath("../")
# setwd(wd)

# Define UI for application
ui <- fluidPage(
  fluidRow(
    column(
      width = 6,
      info$msdialInfoUI("data_info"),
    ),
    column(
      width = 6,
      actionButton(
        inputId = "read_msdial",
        label = "Read MS-Dial output files"
      ),
      verbatimTextOutput("read_msdial_output"),
      actionButton(
        inputId = "preprocess",
        label = "Preprocess data",
      ),
      verbatimTextOutput("preprocess_output"),
      actionButton(
        inputId = "export_data",
        label = "Export data into tables",
      ),
      verbatimTextOutput("export_data_output"),
      actionButton(
        inputId = "generate_report",
        label = "Generate reports",
      ),
      textOutput("generate_report_output")
    ),
  )
)

# Define server logic 
server <- function(input, output) {
  # setwd(wd)
  info$msdialInfoServer("data_info")
  
  observeEvent(input$read_msdial, {
    out <- tryCatch(
      capture.output(source("code/scripts/read-msdial.R")), 
      warning = function(w) w,
      error = function(e) e
    )
    output$read_msdial_output <- renderPrint(out)
  })
  observeEvent(input$preprocess, {
    out <- tryCatch(
      capture.output(source("code/scripts/preprocess.R")), 
      warning = function(w) w,
      error = function(e) e
    )
    output$preprocess_output <- renderPrint(out)
  })
  observeEvent(input$export_data, {
    out <- tryCatch(
      capture.output(source("code/scripts/export_data.R")), 
      warning = function(w) w,
      error = function(e) e
    )
    output$export_data_output <- renderPrint(out)
  })
  observeEvent(input$generate_report, {
    od <- setwd("code/reports")
    # Extract file base name without extension
    fn <- basename(input$input_file) |> 
      tools::file_path_sans_ext(compression = TRUE)
    fn_lst <- list(i = paste0(fn, "-internal.html"), e = paste0(fn, ".pdf"))
    out <- tryCatch(
      {
        system(paste(
          "quarto render report-internal.qmd --to html --output", fn_lst$i, 
          "--output-dir", file.path("../..", input$report_dir)
        ))       # quarto::quarto_render() doesn't accept --output-dir
        system(paste(
          "quarto render report-external.qmd --to pdf --output", fn_lst$e, 
          "--output-dir", file.path("../..", input$report_dir)
        ))       # quarto::quarto_render() doesn't accept --output-dir
        paste0("Reports generated on reports/: ", fn_lst$i, " and ", fn_lst$e, 
               " on ", input$report_dir)
      },
      warning = function(w) w,
      error = function(e) e
    )
    setwd(od)
    output$generate_report_output <- renderPrint(out)
  })
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
