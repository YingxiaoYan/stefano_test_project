# ------------------------------------------------------------------------------------------- #
# This is a Shiny web application.
# ------------------------------------------------------------------------------------------- #

# Load packages and Shiny modules
# options(box.path = "code/")   # No need because app.R is executed where it is located, `code`
box::use(
  shiny[...],
  info = R/msdialInfo,
  rep  = R/genReport,
  scr  = R/runScript,
)
# options(readr.show_progress = FALSE)

# Define UI for application
ui <- fluidPage(
  fluidRow(
    column(
      width = 7,
      info$msdialInfoUI("data_info"),
    ),
    column(
      width = 5,
      scr$runScriptUI("read_msdial", label = "Read MS-Dial output files"),
      scr$runScriptUI("preprocess", label = "Preprocess data"),
      scr$runScriptUI("export_data", label = "Export data into tables"),
      shiny::hr(),
      rep$genReportUI("report"),
    ),
  )
)

# Define server logic 
server <- function(input, output, session) {
  data_info <- info$msdialInfoServer("data_info")
  
  # Run scripts and display output
  scr$runScriptServer("read_msdial", "code/scripts/read-msdial.R")
  scr$runScriptServer("preprocess", "code/scripts/preprocess.R")
  scr$runScriptServer("export_data", "code/scripts/export_data.R")
  
  rep$genReportServer("report", data_info = data_info)
}

# Run the application 
withr::with_dir(
  # All scripts and Quarto files are written to be executed on the project root.
  # This `app.R` is located in `code/` folder.
  new = "..",   
  shiny::shinyApp(ui = ui, server = server)
)
