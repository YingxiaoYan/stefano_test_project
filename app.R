# ------------------------------------------------------------------------------------------- #
# This is a Shiny web application.
# ------------------------------------------------------------------------------------------- #

# options(readr.show_progress = FALSE)

# Define UI for application
ui <- shiny::fluidPage(
  shiny::fluidRow(
    shiny::column(
      width = 6,
      msdialInfoUI("data_info"),
    ),
    shiny::column(
      width = 6,
      runScriptUI("read_msdial", label = "Read MS-Dial output files"),
      runScriptUI("preprocess",  label = "Preprocess data"),
      runScriptUI("export_data", label = "Export data into tables"),
      shiny::hr(),
      genReportUI("report"),
    ),
  )
)

# Define server logic 
server <- function(input, output, session) {
  
  data_info <- msdialInfoServer("data_info")
  
  # Run scripts and display output
  runScriptServer("read_msdial", "scripts/read-msdial.R")
  runScriptServer("preprocess",  "scripts/preprocess.R")
  runScriptServer("export_data", "scripts/export_data.R")
  
  genReportServer("report", data_info = data_info)
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
