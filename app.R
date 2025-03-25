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
options(readr.show_progress = FALSE)
# options(shiny.maxRequestSize = 100 * 2^20)

wd <- normalizePath("../")
setwd(wd)
FILE <- file.path("params.yml")
set_user_input <- function(...) {
  yaml::write_yaml(list(...), file = FILE)
}
if (file.exists(FILE)) {
  user_inputs <- yaml::read_yaml(FILE)
} else {
  user_inputs <- list(
    title = "",
    input_file = "",
    intermediate_dir = "",
    table_dir = "",
    report_dir = "",
    concentration_unit = "",
    user = "",
    university = "",
    project_title = "",
    free_text = ""
  )
  set_user_input(user_inputs)
}

user_in_format <- tibble::tribble(
  ~name, ~type, ~description,
  "title", "textInput", "Title",
  "input_file", "textInput", "Input file",
  "intermediate_dir", "textInput", "Directory to save intermediate files",
  "table_dir", "textInput", "Directory to save the exported tables",
  "report_dir", "textInput", "Directory to save the creating reports",
  "concentration_unit", "textInput", "Concentration unit",
  "user", "textInput", "User",
  "university", "textInput", "University",
  "project_title", "textInput", "Project title",
  "free_text", "textAreaInput", "Free text to the external report"
)

# Define UI for application
ui <- fluidPage(
  fluidRow(
    local({
      # Create list of text inputs, e.g. 
      ## textInput(
      ##   inputId = "title",
      ##   label = "Title",
      ##   value = user_inputs$title
      ## ),
      txtinput_lst <- lapply(1:nrow(user_in_format), function(x) {
        do.call(user_in_format$type[x], list(
          inputId = user_in_format$name[x],
          label = user_in_format$description[x],
          value = user_inputs[[user_in_format$name[x]]],
          width = "100%"
        ))
      })
      first_col_lst <- c(
        txtinput_lst,
        rlang::list2(
          actionButton(
            inputId = "save_user_inputs",
            label = "Save inputs above"
          ),
          verbatimTextOutput("saved"),
          width = 6
        )
      )
      do.call("column", first_col_lst)
    }),
    
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
  setwd(wd)
  observeEvent(input$save_user_inputs, {
    set_user_input(
      title = input$title,
      input_file = input$input_file,
      intermediate_dir = input$intermediate_dir,
      table_dir = input$table_dir,
      report_dir = input$report_dir,
      concentration_unit = input$concentration_unit,
      user = input$user,
      university = input$university,
      project_title = input$project_title,
      free_text = input$free_text
    )
    output$saved <- renderText("Saved")
  })
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
    setwd("code/reports")
    # Extract file base name without extension
    fn <- basename(input$input_file) |> 
      tools::file_path_sans_ext(compression = TRUE)
    fn_lst <- list(i = paste0(fn, "-internal.html"), e = paste0(fn, ".pdf"))
    out <- tryCatch({
      system(paste(
        "quarto render report-internal.qmd --to html --output", fn_lst$i, 
        "--output-dir", file.path("../..", input$report_dir)
      ))       # quarto::quarto_render() doesn't accept --output-dir
      system(paste(
        "quarto render report-external.qmd --to pdf --output", fn_lst$e, 
        "--output-dir", file.path("../..", input$report_dir)
      ))       # quarto::quarto_render() doesn't accept --output-dir
      paste0("Reports generated on reports/: ", fn_lst$i, " and ", fn_lst$e, 
             " on", input$report_dir)
    },
    warning = function(w) w,
    error = function(e) e)
    output$generate_report_output <- renderPrint(out)
  })
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
