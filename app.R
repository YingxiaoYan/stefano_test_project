# ------------------------------------------------------------------------------------------- #
# This is a Shiny web application.
# ------------------------------------------------------------------------------------------- #

# Load packages and Shiny modules
# options(box.path = "code/")   # No need because app.R is executed where it is located, `code`
box::use(
  shiny[...],
  info = R/msdialInfo,
  rep  = R/genReport,
)
# options(readr.show_progress = FALSE)

# All scripts and Quarto files are written to be run from the project root directory.
wd <- normalizePath("../")

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
      shiny::br(),
      rep$genReportUI("report"),
    ),
  )
)

# Define server logic 
server <- function(input, output, session) {
  data_info <- info$msdialInfoServer("data_info")
  
  setwd(wd)
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
  rep$genReportServer("report", data_info = data_info)
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
