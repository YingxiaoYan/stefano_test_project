# ------------------------------------------------------------------------------------------- #
# This is a Shiny web application.
# ------------------------------------------------------------------------------------------- #

options(readr.show_progress = FALSE)      # Avoids progress stored in the "capture.output"

# Settings for Docker container
options(shiny.host = "0.0.0.0")
options(shiny.port = 7579)

# Define UI for application
ui <- shiny::fluidPage(
  shiny::fluidRow(
    shiny::column(
      width = 6,
      msdialInfoUI("data_info"),
    ),
    shiny::column(
      width = 6,
      runReadMsdialUI("read_msdial"),
      runProcessUI("proc"),
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
  runReadMsdialServer("read_msdial", data_info = data_info)
  runProcessServer("proc", data_info = data_info)
  runScriptServer(
    "export_data",
    script_path = "scripts/export_data.R", 
    what = "Export data into tables", 
    data_info = data_info
  )
  
  genReportServer("report", data_info = data_info)
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
