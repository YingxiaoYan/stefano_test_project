#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

box::use(
  shiny[...],
)
options(shiny.maxRequestSize = 100 * 2^20)


# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Read MS-Dial output files"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "file1",
        label = "Select a file",
        accept = c(
          "text/tsv",
          "text/tab-separated-values,text/plain",
          ".tsv",
          ".txt"
        )
      ), 
      # # Select a file
      # shinyFiles::shinyFilesButton(
      #   id = "file", 
      #   label = "Select a file", 
      #   title = "Please select a file:",
      #   multiple = FALSE
      # )
    ),
    mainPanel(
      textOutput("report")
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  roots <- c(data = "../data/")
  
  # input_file <- reactive({
  #   # Select a file
  #   shinyFiles::shinyFileChoose(
  #     input,
  #     "file",
  #     session = session,
  #     filetypes = c("", "tsv", "txt"),
  #     roots = roots
  #   )
  # })
  
  # if (!is.null(input_file)) {
  # file_selected <- shinyFiles::parseFilePaths(roots, input_file)
  output$report <- renderText({
    out_file <- input$file1 |> 
      tools::file_path_sans_ext() |>
      paste0(".html")
    # p <- quarto::quarto_render(
    #   input = "reports/report-read-msdial.Rmd",
    #   output_format = "html",
    #   output_file = basename(out_file),
    #   execute_params = rlang::list2(
    #     input_file = file_selected$name,
    #   )
    # )
    out_file
    #   "Done"
  })
  # }
#  # Render the report
#  output$report <- shiny::downloadHandler(
#    filename = function() {
#      f <- input$file
#      if (is.null(f)) return("report.html")
#      f$datapath |> 
#        tools::file_path_sans_ext() |> 
#        paste0(".html")
#    },
#    content = function(file) {
#      # temp_report <- tempfile(fileext = ".Rmd")
#      # file.copy("reports/report-read-msdial.Rmd", temp_report, overwrite = TRUE)
#      # knit with params
#      p <- quarto::quarto_render(
#        input = "reports/report-read-msdial.Rmd", 
#        output_format = "html",
#        output_file = basename(file),
#        execute_params = rlang::list2(
#          input_file = input$file$datapath,
#        )
#      )
#      print(p)
#      p
#    }
#  )
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
