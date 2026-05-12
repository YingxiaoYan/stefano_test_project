## ------------------------------------------------------------------------------------------- #
# This is a Shiny web application.
# ------------------------------------------------------------------------------------------- #

options(readr.show_progress = FALSE)

options(shiny.host = "0.0.0.0")
options(shiny.port = 7579)
options(shiny.maxRequestSize = 100000000 * 1024^2)


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


# 1) Define theme
custom_theme <- bs_theme(
  version = 5,
  bootswatch = NULL,
  
  base_font    = font_google("Inter"),
  heading_font = font_google("Oswald"),
  code_font    = font_google("JetBrains Mono"),
  
  
  bg = "#FFFFFF",   # white background
  fg = "#000000",   # black text
  
  
  primary   = "#F4A259",
  secondary = "#6BCBFF",
  info      = "#9ADCFF",
  success   = "#3CD070",
  warning   = "#FFD66B",
  danger    = "#FF6B6B",
  
  ### >>>>> CARD COLOR UPDATED (BLUE) <<<<<
  "card-bg"            = "#6FAAF7",   # QuANTA deep blue
  "card-border-color"  = "#6FAAF7",   # darker border to match
  "navbar-bg"          = "#0D121C",
  "navbar-fg"          = "#FFFFFF",
  
  "link-color"         = "#F4A259",
  "link-hover-color"   = "#8F8DF7"
)

# Add custom CSS rules
custom_theme <- bs_add_rules(
  custom_theme,
  "
  /* Buttons */
  

.btn {
  background-color: #F4A259 !important;   /* ORANGE (normal) */
  border-color: #F4A259 !important;
  color: #000000 !important;              /* BLACK TEXT */
  border-width: 2px;
}

.btn:hover, .btn:focus {
  background-color: #FFB733 !important;   /* LIGHT ORANGE on hover */
  border-color: #FFB733 !important;
  color: #000000 !important;              /* BLACK TEXT */
  box-shadow: 0 0 10px rgba(255, 165, 0, 0.5) !important;
}

.btn:active {
  background-color: #E69500 !important;   /* DARKER ORANGE when pressed */
  border-color: #E69500 !important;
  color: #000000 !important;
}

/* Outline button behavior */
.btn-outline-primary,
.btn-outline-secondary,
.btn-outline-info,
.btn-outline-success,
.btn-outline-warning,
.btn-outline-danger,
.btn-outline-dark,
.btn-outline-light {
  color: #000000 !important;               /* BLACK TEXT */
  border-color: #F4A259 !important;
}

.btn-outline-primary:hover {
  background-color: #F4A259 !important;    /* ORANGE FILL */
  color: #000000 !important;               /* BLACK TEXT */
}

pre.shiny-text-output, 
pre.shiny-text-output.monospace {
  background-color: #0A0F18 !important;   /* Dark inner background */
  color: #FFFFFF !important;              /* White text */
  border: 2px solid #F4A259 !important;   /* Orange border */
  padding: 8px 12px !important;
  border-radius: 6px !important;
  font-family: JetBrains Mono, monospace !important;
}

/* Optional: highlight when hovered or focused */
pre.shiny-text-output:hover {
  border-color: #FFB733 !important;       /* Lighter orange hover */
  box-shadow: 0 0 6px rgba(255,165,0,0.4) !important;
}




  /* Inputs */
  .form-control, .selectize-input {
    background-color: #0F1624 !important;
    color: #FFFFFF !important;
    border-color: #1E2A3A !important;
  }

  .form-control:focus, .selectize-input.focus {
    border-color: #F4A259 !important;
    box-shadow: 0 0 6px rgba(255, 77, 166, 0.4) !important;
  }

  /* >>>>> CARD COLORS UPDATED (BLUE) <<<<< */


.card {
  color: #FFFFFF !important;
}

/* Keep card headers white */
.card-header,
.card-header * {
  color: #FFFFFF !important;
}


  /* ---------- NAV BAR: white background + black text ---------- */
.navbar, .navbar-nav, .navbar-brand {
  background-color: #FFFFFF !important;
  color: #000000 !important;
}

.navbar .nav-link, 
.navbar .navbar-brand, 
.navbar .navbar-nav .nav-link {
  color: #000000 !important;
}

/* Hover effect */
.navbar .nav-link:hover {
  color: #F4A259 !important;  /* optional orange hover */
}

/* ---------- NAV TABS (the page_navbar panels) ---------- */
.nav-tabs {
  background-color: #FFFFFF !important;
  border-bottom: 1px solid #CCCCCC !important;
}

.nav-tabs .nav-link {
  color: #000000 !important;
  background-color: #FFFFFF !important;
  border: 1px solid #CCCCCC !important;
}

/* Hover effect */
.nav-tabs .nav-link:hover {
  color: #F4A259 !important;
  background-color: #F7F7F7 !important;
}

/* Active tab styling */
.nav-tabs .nav-link.active {
  color: #000000 !important;
  background-color: #FFFFFF !important;
  border: 2px solid #F4A259 !important;   /* orange outline */
  border-bottom: 2px solid #FFFFFF !important; /* seamless effect */
  font-weight: bold;
}


/* Base table */
table.dataTable {
  background-color: #FFFFFF !important;
  color: #000000 !important;
  border-collapse: collapse !important;
}

/* Header */
table.dataTable thead th {
  background-color: #EAF2FF !important;   /* light blue header */
  color: #003366 !important;
  font-weight: 600;
  border-bottom: 2px solid #D0D7E5 !important;
}

/* Body rows */
table.dataTable tbody tr {
  background-color: #FFFFFF !important;
}

/* Zebra striping — SOFT, NO ORANGE */
table.dataTable tbody tr:nth-child(even) {
  background-color: #F8FAFF !important;
}

/* Hover effect */
table.dataTable tbody tr:hover {
  background-color: #EFF4FF !important;
}

/* Cells */
table.dataTable td {
  color: #000000 !important;
  vertical-align: middle;
}

/* Remove Bootstrap primary color bleed */
table.dataTable.no-footer {
  border-bottom: 1px solid #D0D7E5 !important;
}



/* =========================================================
   FINAL FIX — FORCE ALL KABLE TABLE TEXT TO BLACK
   ========================================================= */


[data-bs-theme] .card table,
[data-bs-theme] .card table th,
[data-bs-theme] .card table td,
[data-bs-theme] .card table.table,
[data-bs-theme] .card table.table th,
[data-bs-theme] .card table.table td {
  color: #000000 !important;
}



  "
)

# Echarts font
e_common(
  font_family = "PT Sans",
  theme = NULL
)


################################################################################################
######################################## UI ####################################################
################################################################################################

ui <-
  page_navbar(
    theme = custom_theme,
    title =  tags$img(src = "QuaNTA.jpg", height = "40px", class = "navbar-logo"),
    
    tags$head(
      tags$link(rel = "icon", type = "image/jpg", href = "QuaNTA_square.jpg")
    ),
    
    tags$head(
      
    tags$style(HTML("
  .navbar-brand img,
  .navbar-logo {
    height: 60px !important;
    width: auto !important;
    border-radius: 0 !important;   /* prevents forced rounding */
    object-fit: contain !important;
  }"))
      
    ),
    
    tags$head(
      tags$style(HTML("
    /* Reduce navbar vertical padding */
    .navbar, .navbar-nav .nav-link {
      padding-top: 4px !important;
      padding-bottom: 4px !important;
    }

    /* Optionally reduce brand/logo spacing */
    .navbar-brand {
      padding-top: 0 !important;
      padding-bottom: 0 !important;
    }
  "))
    ),
    
    tags$head(
      tags$style(HTML("
    .navbar-nav .nav-link {
      font-size: 1.2rem !important;   /* Increase text size */
      font-weight: 500;               /* Optional: make text slightly thicker */
    }
  "))
    ),
    
    
    #favicon("www/QuaNTA.jpg"),
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
          card_header("Report for MSDial info input"),
          runReadMsdialUI_2("read_msdial")
        )
      )
    ),
    
    nav_panel(
      title = HTML("2. Processing <strong> >> </strong>"),
      layout_columns(
        col_widths = c(3, 9),
        card(runProcessUI("proc")),
        card(card_header("Report for proc"),
             runProcessUI_2("proc")
             )
      )
    ),
    
    nav_panel(
      title = HTML("3. Quality control <strong> >> </strong>"),
      layout_columns(
        col_widths = c(6, 6), 
        card(card_header("Tables"),
             
             div(
               style = "padding: 0;", 
               
             h3(
               "Table 2.1.1",
               style = "margin: 0;"
               #style = "margin-bottom: 10px; font-weight: 600;"
             ),
             DT::DTOutput("tb_2_1_1")
             ),
             
             div(
               style = "padding: 0;", 
               
               h3(
                 "Table 2.1.2.2",
                 style = "margin: 0;"
                 #style = "margin-bottom: 10px; font-weight: 600;"
               ),
               DT::DTOutput("tb_2_1_2_2")
             ),
             
             div(
               style = "padding: 0;", 
               
               h3(
                 "Table 2.1.3",
                 style = "margin: 0;"
                 #style = "margin-bottom: 10px; font-weight: 600;"
               ),
               
               htmlOutput("tb_2_1_3")
               #DT::DTOutput("tb_2_1_3")
             ),
             
             div(
               style = "padding: 0;", 
               
               h3(
                 "Table 2.2",
                 style = "margin: 0;"
                 #style = "margin-bottom: 10px; font-weight: 600;"
               ),
               #DT::DTOutput("tb_2_2")
               htmlOutput("tb_2_2")
               
             )
             ), 
        card(card_header("Figures"),
             div(
               style = "padding: 0;", 
             h3(
               "Figure 2.2",
               style = "margin: 0;"
               #style = "margin-bottom: 10px; font-weight: 600;"
             ),
             
             plotOutput("ggplot1_2_2"#, 
                        #height = "450px"
                        )
             ),
             div(
               style = "padding: 0;", 
               h3(
                 "Figure 2.2",
                 style = "margin: 0;"
                 #style = "margin-bottom: 10px; font-weight: 600;"
               ),
               
               plotOutput("ggplot2_2_2"#, 
                          #height = "450px"
               )
             )
             )
      )
    ),
    
    nav_panel(
      title = HTML("4. Quantification <strong> >> </strong>"),
      layout_columns(
        col_widths = c(3, 9), 
        card(), 
        card(card_header("Calibration"),
             
             )
      )
    ),
    
    nav_panel(
      title = HTML("5. Report export <strong> >> </strong>"),
      layout_columns(
        col_widths = c(3, 9), 
        card(genReportUI("report"),
             runScriptUI("export_data", label = "Export data into tables")
             
             ), 
        card(
          card_header("Report for report"),
          
          tags$iframe(
            src = "qreports/Dummy_Test-loess_norm-internal.html",
            style = "width:100%; height:90vh; border:none;"
          ),
          
          div(
            class = "d-flex justify-content-center my-2",
            downloadButton(
              outputId = "download_report",
              label    = "Download HTML Report",
              class    = "btn btn-sm btn-primary"
            )
          )
        )
      )
    )
  )



################################################################################################
###################################### SERVER ###################################################
################################################################################################

server <- function(input, output, session) {
  
  #########################
  source("R/_internal-shared_setup_chunky_toR.R")
  
  r <- reactiveValues()
  ###########################################
  
  
  #cat(class(raw_se))
  
  #################################################################################################################################
  ### This is table 2.1.1

  raw_se_r <- reactiveVal(NULL)
  # observeEvent(raw_se, {
  #   req(raw_se)
  #   
  #   # however raw_se is created originally
  #   raw_se_r(raw_se)
  # })
  
  
  raw_se_r <- reactiveVal(NULL)
  
  observeEvent(data_info$intermediate_dir, {
    req(data_info$intermediate_dir)
    
    # raw_se must already exist here (created by your module)
    raw_se_r(raw_se)
  })
  
  
  output$tb_2_1_1 <- DT::renderDataTable({
    # req(raw_se)
    # r$raw_se <- raw_se
    # 
    req(raw_se_r())
    catg <- util$ctrl_smpl_cat( raw_se_r())
    catg[catg == ""] <- "Sample" 
    
    catg_cl <- tibble::tibble(
      Category = catg,
      Class = SumExp::col_df( raw_se_r())$Class,
    ) |>
      dplyr::mutate(Class = ifelse(Category %in% c("CalCurve", "QC"), "", Class)) |>
      tidyr::unite("catg_cl", c(Category, Class), sep = ":", remove = FALSE)
    tb <- janitor::tabyl(catg_cl, catg_cl) |>
      janitor::adorn_pct_formatting() |>
      dplyr::right_join(dplyr::distinct(catg_cl), y = _, by = "catg_cl") |>
      dplyr::arrange(Category, Class)
    stopifnot(anyDuplicated(tb$catg_cl) == 0)   # Ensure no duplicated category-class combinations
    
    tb <-tb|>
      dplyr::select(-catg_cl) 
    # |>
    #   knitr::kable(
    #     row.names = FALSE,          # To avoid excluded row numbers
    #     col.names = c("Category", "Class", "Number of Samples", "Percent"),
    #     align = "llrr"
    #   ) #|>
    #kableExtra::kable_styling(full_width = FALSE) |>
    #kableExtra::collapse_rows(columns = 1, valign = "top")
    
    cat(class(tb))
    
    tb<-as.data.frame(tb)
    colnames(tb)<-c("Category", "Class", "Number of Samples", "Percent")
    
    DT::datatable(
      tb,
      
      # caption = htmltools::tags$caption(
      #   style = "caption-side: top; text-align: left; font-weight: bold;",
      #   "Table 2.1.1"
      # ),
      
      options = list(scrollX = TRUE,
                     lengthChange = FALSE,
                     searching = FALSE,
                     
                     paging = FALSE,            # remove Previous/Next
                     info = FALSE,              # remove "Showing X to Y of Z entries"
                     dom = 't',
                     headerCallback = JS( "function(thead){",
                                          "$(thead).find('th').css({'color': 'green',
                                            'font-weight': 'bold'});", "}" ),
                     columnDefs = list(
                       list(className = 'dt-center', targets = "_all")   # <--- centers all text
                     )
      )
    ) %>%
      DT::formatStyle(
        columns = names(tb),
        color = "#000000",
        backgroundColor = "#FFFFFF",
        target = "cell"
      ) %>%
      DT::formatStyle(
        names(tb),
        backgroundColor = "#F8FAFF",
        target = "row"
      )
    
  })
  
  
  #################################################################################################################################
  ### This is table 2.1.2.2
  #### There is problem of is target mode
  IS_TARGET_MODE <- TRUE
  
  # -------------------------------------------------------------------
  # 1. Calibration sample table (dataframe)
  # -------------------------------------------------------------------
  
  df_show <- if (IS_TARGET_MODE) {
    SumExp::col_df(raw_se) |>
      dplyr::filter(util$ctrl_smpl_cat(raw_se) == "CalCurve") |>
      dplyr::arrange(c_conc) |>
      dplyr::select(sample_name, c_conc)
  } else {
    tibble::tibble(
      sample_name = character(),
      c_conc = numeric()
    )
  }
  
  # Ensure it is a plain data.frame if needed
  df_show <- as.data.frame(df_show)
  
  # -------------------------------------------------------------------
  # 2. Concentration frequency table (dataframe)
  # -------------------------------------------------------------------
  
  df_conc_summary <- df_show |>
    dplyr::count(c_conc, name = "Number_of_Samples") |>
    dplyr::arrange(c_conc)
  
  df_conc_summary <- as.data.frame(df_conc_summary)
  
  colnames(  df_conc_summary)<- c("Calibrant Concentration","Number of Samples")
  output$tb_2_1_2_2 <- DT::renderDataTable({
    DT::datatable(
      df_conc_summary ,
      # caption = htmltools::tags$caption(
      #   style = "caption-side: top; text-align: left; font-weight: bold;",
      #   "Table 2.1.1"
      # ),
      options = list(scrollX = TRUE,
                     lengthChange = FALSE,
                     searching = FALSE,
                     
                     paging = FALSE,            # remove Previous/Next
                     info = FALSE,              # remove "Showing X to Y of Z entries"
                     dom = 't',
                     headerCallback = JS( "function(thead){",
                                          "$(thead).find('th').css({'color': 'green',
                                            'font-weight': 'bold'});", "}" ),
                     columnDefs = list(
                       list(className = 'dt-center', targets = "_all")   # <--- centers all text
                     )
      )
    ) %>%
      DT::formatStyle(
        columns = names(df_conc_summary ),
        color = "#000000",
        backgroundColor = "#FFFFFF",
        target = "cell"
      ) %>%
      DT::formatStyle(
        names(df_conc_summary ),
        backgroundColor = "#F8FAFF",
        target = "row"
      )
  
  })
  
  
  #################################################################################################################################
  ### This is table 2.1.3

  tb_2_1_3<-show$extract_qc_samples_to_list(raw_se) |> 
    sapply(ncol) |> 
    knitr::kable(col.names = c("Class", "Number of Replicates")) |> 
    kableExtra::kable_styling(full_width = FALSE)
 # cat( tb_2_1_3)
  # cat(dim(tb_2_1_3))
  # tb_2_1_3<-as.data.frame(tb_2_1_3)
  # cat(class(tb_2_1_3))
  # cat(dim(tb_2_1_3))
  #colnames(tb_2_1_3)<-c("Class", "Number of Replicates")
  
  
  
  output$tb_2_1_3 <- renderUI({
    
    tb_2_1_3 %>%
      HTML()
    
  })
  
  # output$tb_2_1_3 <- DT::renderDataTable({
  #   DT::datatable(
  #     tb_2_1_3 ,
  #     # caption = htmltools::tags$caption(
  #     #   style = "caption-side: top; text-align: left; font-weight: bold;",
  #     #   "Table 2.1.1"
  #     # ),
  #     options = list(scrollX = TRUE,
  #                    lengthChange = FALSE,
  #                    searching = FALSE,
  #                    
  #                    paging = FALSE,            # remove Previous/Next
  #                    info = FALSE,              # remove "Showing X to Y of Z entries"
  #                    dom = 't',
  #                    headerCallback = JS( "function(thead){",
  #                                         "$(thead).find('th').css({'color': 'green',
  #                                           'font-weight': 'bold'});", "}" ),
  #                    columnDefs = list(
  #                      list(className = 'dt-center', targets = "_all")   # <--- centers all text
  #                    )
  #     )
  #   ) %>%
  #     DT::formatStyle(
  #       columns = names(tb_2_1_3),
  #       color = "#000000",
  #       backgroundColor = "#FFFFFF",
  #       target = "cell"
  #     ) %>%
  #     DT::formatStyle(
  #       names(tb_2_1_3 ),
  #       backgroundColor = "#F8FAFF",
  #       target = "row"
  #     )
  # })
  #################################################################################################################################
  ### This is table 2.1.3
  
  v <- util$std_type(raw_se)
  tb_2_2<-replace(v, v == "", "Not targeted") |>
    show$kable_number_of(what = "Features")
  output$tb_2_2 <- renderUI({
    
    tb_2_2 %>%
      HTML()
    
  })
  
  # output$tb_2_2 <- DT::renderDataTable({
  #   DT::datatable(
  #     tb_2_2 ,
  #     # caption = htmltools::tags$caption(
  #     #   style = "caption-side: top; text-align: left; font-weight: bold;",
  #     #   "Table 2.1.1"
  #     # ),
  #     options = list(scrollX = TRUE,
  #                    lengthChange = FALSE,
  #                    searching = FALSE,
  #                    
  #                    paging = FALSE,            # remove Previous/Next
  #                    info = FALSE,              # remove "Showing X to Y of Z entries"
  #                    dom = 't',
  #                    headerCallback = JS( "function(thead){",
  #                                         "$(thead).find('th').css({'color': 'green',
  #                                           'font-weight': 'bold'});", "}" ),
  #                    columnDefs = list(
  #                      list(className = 'dt-center', targets = "_all")   # <--- centers all text
  #                    )
  #     )
  #   ) %>%
  #     DT::formatStyle(
  #       columns = names(tb_2_2),
  #       color = "#000000",
  #       backgroundColor = "#FFFFFF",
  #       target = "cell"
  #     ) %>%
  #     DT::formatStyle(
  #       names(tb_2_2 ),
  #       backgroundColor = "#F8FAFF",
  #       target = "row"
  #     )
  # })
  
  #################################################################################################################################
  ### This is figure 2.2
  colors <- local({
    x <- unique(util$std_type(raw_se))
    n_non_target <- sum(x == "")
    col <- scales::hue_pal()(length(x) - n_non_target)
    col <- rlang::set_names(col, x[x != ""])
    if (n_non_target > 0) {
      c(col, "grey")     # grey for non-targeted
    } else {
      col
    }
  })
  df1 <- SumExp::row_df(raw_se)
  ggplot1_2_2<-ggplot(df1) +
    geom_point(aes(x = .rt, y = mz, color = .std_type), 
               alpha = ifelse(df1$.std_type == "", 0.1, 1)) +
    labs(
      x = show$label_if_has(df1$.rt), 
      y = show$label_if_has(df1$mz),
      color = show$label_if_has(df1$.std_type),
    ) +
    scale_color_manual(values = colors)
  output$ggplot1_2_2 <- renderPlot({
    ggplot1_2_2
  })
  
  
  
  #################################################################################################################################
  ### This is figure 2.2
  df1 <- SumExp::row_df(raw_se)
  ggplot2_2_2<-ggplot(df1) +
    geom_histogram(aes(x = mz), binwidth = 0.5) +
    labs(
      x = show$label_if_has(df1$mz), 
      y = "Number of features"
    )
  output$ggplot2_2_2 <- renderPlot({
    ggplot2_2_2
  })
  #####################################################
  
  data_info <- msdialInfoServer("data_info")
  
  observeEvent(data_info$intermediate_dir, {
    relative <- data_info$intermediate_dir
    req(relative, relative != "")
    
    project_root <- normalizePath("..", winslash = "/", mustWork = TRUE)
    cleaned <- sub("^\\./", "", relative)
    full <- normalizePath(file.path(project_root, cleaned),
                          winslash = "/", mustWork = FALSE)
    
    if (dir.exists(full)) addResourcePath("qreports", full)
  })
  
  runReadMsdialServer("read_msdial", data_info = data_info)
  runProcessServer("proc", data_info = data_info)
  runScriptServer("export_data",
                  script_path = "scripts/export_data.R",
                  what = "Export data into tables",
                  data_info = data_info)
  genReportServer("report", data_info = data_info)
  
  
  get_full_path <- function(relative_path) {
    project_root <- normalizePath("..", winslash = "/", mustWork = TRUE)
    cleaned <- sub("^\\./", "", relative_path)
    normalizePath(file.path(project_root, cleaned),
                  winslash = "/", mustWork = FALSE)
  }
  
  output$download_report <- downloadHandler(
    filename = function() {
      "Report.html"
    },
    content = function(file) {
      report_dir <- data_info$report_dir
      req(report_dir, report_dir != "")
      
      full_report_dir <- get_full_path(report_dir)
      
      report_path <- file.path(full_report_dir,
                               "Dummy_Test-loess_norm-internal.html")
      report_path <- normalizePath(report_path, winslash = "/", mustWork = TRUE)
      
      file.copy(from = report_path, to = file, overwrite = TRUE)
    }
  )
}


# Launch
shiny::shinyApp(ui = ui, server = server)
