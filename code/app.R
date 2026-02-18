## ------------------------------------------------------------------------------------------- #
# This is a Shiny web application.
# ------------------------------------------------------------------------------------------- #

options(readr.show_progress = FALSE)      # Avoids progress stored in the "capture.output"

# Settings for Docker container
options(shiny.host = "0.0.0.0")
options(shiny.port = 7579)
options(shiny.maxRequestSize = 100000000 * 1024^2)  # 500 MB


library(shiny)
library(shinyjs)
library(shinyFiles)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

library(bslib)

library(lubridate)
library(dplyr)
library(echarts4r)
library(DT)


getwd()

# 1) Define a dark, restrained palette

library(bslib)

custom_theme <- bs_theme(
  version = 5,
  bootswatch = NULL,
  
  # Typography
  base_font    = font_google("Inter"),
  heading_font = font_google("Oswald"),
  code_font    = font_google("JetBrains Mono"),
  
  # --- DARK MODE FOUNDATIONS ---
  bg = "#0B0F17",        # main background (deep navy-black)
  fg = "#F2F5FA",        # main text (bright, slightly cool white)
  
  # --- ACCENT COLORS ---
  primary   = "#FF4DA6",   # bright pink for main buttons
  secondary = "#6BCBFF",   # bright cyan for secondary UI
  info      = "#9ADCFF",
  success   = "#3CD070",
  warning   = "#FFD66B",
  danger    = "#FF6B6B",
  
  # Navbar + card colors
  "navbar-bg" = "#0D121C",
  "navbar-fg" = "#FFFFFF",
  "card-bg"   = "#111823",
  "card-border-color" = "#1C2533",
  
  # Links
  "link-color" = "#FF4DA6",
  "link-hover-color" = "#FF74BB"
)

# Add global CSS rules for bright buttons & contrast
custom_theme <- bs_add_rules(
  custom_theme,
  "
  /* --- BUTTONS: BRIGHT TEXT ON DARK THEME --- */
  .btn {
    color: #FFFFFF !important;               /* Bright text */
    border-color: #FF4DA6 !important;        /* Pink border */
    border-width: 2px;
    background-color: #FF4DA6 !important;    /* Bright pink button */
  }

  .btn:hover, .btn:focus {
    background-color: #FF6FBA !important;    /* Lighter hover */
    border-color: #FF6FBA !important;
    box-shadow: 0 0 10px rgba(255, 109, 186, 0.6) !important;
    color: #FFFFFF !important;
  }

  .btn:active {
    background-color: #E63A8F !important;     /* Pressed state */
    border-color: #E63A8F !important;
    color: #FFFFFF !important;
  }

  /* Outline buttons: make text & border bright */
  .btn-outline-primary,
  .btn-outline-secondary,
  .btn-outline-info,
  .btn-outline-success,
  .btn-outline-warning,
  .btn-outline-danger,
  .btn-outline-dark,
  .btn-outline-light {
    color: #FFFFFF !important;
    border-color: #FF4DA6 !important;
  }

  .btn-outline-primary:hover {
    background-color: #FF4DA6 !important;
    color: #FFFFFF !important;
  }

  /* Inputs: dark background, bright text */
  .form-control, .selectize-input {
    background-color: #0F1624 !important;
    color: #FFFFFF !important;
    border-color: #1E2A3A !important;
  }

  .form-control:focus, .selectize-input.focus {
    border-color: #FF4DA6 !important;
    box-shadow: 0 0 6px rgba(255, 77, 166, 0.4) !important;
    color: #FFFFFF !important;
  }
  "
)

# set font for echarts
e_common(
  font_family = "PT Sans",
  theme = NULL
)



ui <-
  
  page_navbar(
    theme = custom_theme,
    title = "Data quality evaluation",
    
    
    nav_panel(
      title = HTML("1.MSDial info input <strong> >> </strong>"),
      layout_columns(
        col_widths = c(3, 9),
        card(
          min_height = "600px",
          msdialInfoUI("data_info"),
          runReadMsdialUI("read_msdial")
        ),
        
        card(
          card_header("Report for MSDial infor input"),
          #htmlOutput("qmd_html")
        )
        
      )
      ## UI
    ),
    nav_panel(
      title = HTML("2. proc <strong> >> </strong>"),
      layout_columns(col_widths = c(3, 9), card(runProcessUI("proc")), card(
        card_header("Report for proc"),
        #htmlOutput("qmd_html")
      ))
    ),
    nav_panel(
      title = HTML("3. Export data <strong> >> </strong>"),
      layout_columns(col_widths = c(3, 9), 
                     card(
                       runScriptUI("export_data", label = "Export data into tables")
                       ), 
                     card(
                       card_header("Report for export data"),
                       #htmlOutput("qmd_html")
                       ))
    ),
    nav_panel(
      title = HTML("4. Report <strong> >> </strong>"),
      
      layout_columns(col_widths = c(3, 9), 
                     card(genReportUI("report")), 
                     card(
        card_header("Report for report"),
        
        
        ################################################################################
        ####### The name of the file needs to be changed
        tags$iframe(
          src = "qreports/Dummy_Test-loess_norm-internal.html",
          #src = "C:/Users/XingxiaoYan/Desktop/SMS-7579-23-exposome/code/tests/testoutput/Dummy_Test-loess_norm-internal.html",
          style = "width:100%; height:90vh; border:none;"
        ),
       
      div(
        class = "d-flex justify-content-center my-2",
        downloadButton(
          outputId = "download_report",
          label    = "Download HTML Report",
          class    = "btn btn-sm btn-primary"   # <- smaller button
        )
      )

        #uiOutput("qmd_html")
        #htmlOutput("qmd_html")
      ))
    )
    
  )



# Define UI for application
# ui <- shiny::fluidPage(
#   shiny::fluidRow(
#     shiny::column(
#       width = 6,
#       msdialInfoUI("data_info"),
#     ),
#     shiny::column(
#       width = 6,
#       runReadMsdialUI("read_msdial"),
#       runProcessUI("proc"),
#       runScriptUI("export_data", label = "Export data into tables"),
#       shiny::hr(),
#       genReportUI("report"),
#     ),
#   )
# )

# Define server logic 
server <- function(input, output, session) {
  
  

  #######################################################################################################################
  ### This path where the files are saved needs to be changed, maybe a default of user definition?

  
 
  data_info <- msdialInfoServer("data_info")
  
  
  
  
  observeEvent(data_info$intermediate_dir, {
    
    relative <- data_info$intermediate_dir  # "./code/tests/testoutput"
    req(relative, relative != "")
    
    project_root <- normalizePath("..", winslash = "/", mustWork = TRUE)
    
    cleaned <- sub("^\\./", "", relative)   # "code/tests/testoutput"
    
    full <- normalizePath(file.path(project_root, cleaned),
                          winslash = "/", mustWork = FALSE)
    
    if (!dir.exists(full)) {
      warning("Directory does not exist: ", full)
      return()
    }
    
    addResourcePath("qreports", full)
  })
  
  
  # 
  # rp <- normalizePath(
  # 
  # 
  #   "C:/Users/XingxiaoYan/Desktop/SMS-7579-23-exposome/code/tests/testoutput",
  #   winslash = "/",
  #   mustWork = TRUE
  # )
  # addResourcePath("qreports", rp)
  
  
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
  
  
  
  
  
  #######################################################################################################################
  ### This path where the files are saved needs to be changed, maybe a default of user definition?
  ### Read from the path where it si saved and then download
  
  
  get_full_path <- function(relative_path) {
    project_root <- normalizePath("..", winslash = "/", mustWork = TRUE)
    cleaned <- sub("^\\./", "", relative_path)
    full <- normalizePath(file.path(project_root, cleaned),
                          winslash = "/", mustWork = FALSE)
    return(full)
  }
  
  
  output$download_report <- downloadHandler(
    
    filename = function() {
      "Report.html"
    },
    
    content = function(file) {
      
      # 1. Get user-selected report_dir
      report_dir <- data_info$report_dir
      req(report_dir, report_dir != "")
      
      # 2. Convert relative path like "./code/tests/testoutput" -> full absolute path
      full_report_dir <- get_full_path(report_dir)
      
      ################################################################################
      ####### The name of the file needs to be changed
      # 3. Build the full report file path
      report_path <- file.path(full_report_dir, 
                               "Dummy_Test-loess_norm-internal.html")
      report_path <- normalizePath(report_path, winslash = "/", mustWork = TRUE)
      
      # 4. Copy the report to Shiny's download location
      file.copy(from = report_path, to = file, overwrite = TRUE)
    }
  )
  
  

  
  
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
