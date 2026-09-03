# =============================================================================
# load_data module - with Image / CSV downloads on the result plots
#
# Downloads added to:
#   Monthly Random Effect .................. key "mre"
#   Disease Incidence Rate (Spatial Plot) .. key "spat"
#   Exposure-lag relationship .............. key "contour"
#   Relative risk plot ..................... key "rrp"
#   Relative risk map ...................... key "rrmap"
#   Attributable number & rate ............. key "attm"   (gg + plotly)
#   Monthly average attributable ........... key "attavm"
#
# Each plot is now built inside a reactive() and merely printed by
# renderPlot()/renderPlotly(), so the downloaded file is the same object that
# is on screen.
#
# Other small fixes (all listed in the chat reply, revert individually if you
# disagree): un-nested shapefile observers, dir.create() before saveRDS(),
# "Knocks" -> "Knots", stray </li> in the climate help block, duplicate VIF
# card title, removed the unused/broken var_names().
#
# Requires: shiny, bs4Dash, dplyr, ggplot2, plotly, htmlwidgets, reactable,
#           readxl, sf.  Optional for static plotly export: kaleido or webshot2.
# =============================================================================


# -----------------------------------------------------------------------------
# Download helpers
# -----------------------------------------------------------------------------

# Row of download controls to drop into a card.ok
# `formats` lets the attribution card offer "html" for its plotly output.
plot_downloads_ui <- function(ns, key, formats = c("png", "pdf", "jpeg")) {
  tags$div(
    style = "margin-top:8px; display:flex; gap:8px; align-items:flex-end; flex-wrap:wrap;",
    tags$div(
      style = "width:95px;",
      selectInput(ns(paste0("dl_fmt_", key)), NULL,
                  choices = formats, selected = formats[1], width = "95px")
    ),
    downloadButton(ns(paste0("dl_img_", key)), "Image",
                   class = "btn btn-sm btn-outline-primary",
                   style = "margin-bottom:15px;"),
    downloadButton(ns(paste0("dl_csv_", key)), "CSV",
                   class = "btn btn-sm btn-outline-secondary",
                   style = "margin-bottom:15px;")
  )
}

# Several plot_* functions in this app return a LIST containing the plot.
# Unwrap to the first plot-like element.
.first_plot <- function(x) {
  plot_classes <- c("gg", "ggplot", "patchwork", "plotly", "htmlwidget", "recordedplot")
  if (inherits(x, plot_classes)) return(x)
  if (is.list(x)) {
    for (el in x) if (inherits(el, plot_classes)) return(el)
  }
  x
}

.gg_layer_data <- function(p) {
  tryCatch(
    dplyr::bind_rows(lapply(ggplot2::ggplot_build(p)$data, as.data.frame)),
    error = function(e) NULL
  )
}

# Best-effort data.frame for CSV export.
# Order: explicit data -> plot$data -> built layer data.
.plot_data <- function(p, fallback = NULL) {
  p <- .first_plot(p)
  d <- NULL
  
  if (!is.null(fallback)) {
    d <- fallback
  } else if (inherits(p, "plotly")) {
    d <- tryCatch(p$x$visdat[[1]](), error = function(e) NULL)
  } else if (inherits(p, "patchwork")) {
    subs <- tryCatch(c(list(p[[1]]), p$patches$plots), error = function(e) list())
    parts <- lapply(subs, function(s) {
      dd <- tryCatch(s$data, error = function(e) NULL)
      if (!is.data.frame(dd) || nrow(dd) == 0) dd <- .gg_layer_data(s)
      dd
    })
    parts <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, parts)
    d <- if (length(parts)) {
      tryCatch(dplyr::bind_rows(parts), error = function(e) parts[[1]])
    } else NULL
  } else if (inherits(p, "ggplot")) {
    d <- tryCatch(p$data, error = function(e) NULL)
    if (!is.data.frame(d) || nrow(d) == 0) d <- .gg_layer_data(p)
  }
  
  if (inherits(d, "sf")) d <- sf::st_drop_geometry(d)
  if (is.data.frame(d)) {
    d <- dplyr::ungroup(d)
    keep <- vapply(d, function(col) !is.list(col), logical(1))
    d <- d[, keep, drop = FALSE]
  }
  d
}

# Writes ggplot / patchwork / plotly / recorded base plot to disk.
.save_plot <- function(p, file, fmt = "png", width = 11, height = 7, dpi = 300) {
  p <- .first_plot(p)
  
  # ---- interactive widgets (district-level attribution chart) --------------
  if (inherits(p, "plotly") || inherits(p, "htmlwidget")) {
    
    if (identical(fmt, "html")) {
      htmlwidgets::saveWidget(p, file, selfcontained = TRUE)
      return(invisible(file))
    }
    
    # 1st choice: kaleido (via plotly::save_image)
    ok <- tryCatch({ plotly::save_image(p, file); TRUE }, error = function(e) FALSE)
    
    # 2nd choice: render the widget and screenshot it
    if (!ok) {
      ok <- tryCatch({
        tmp <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(p, tmp, selfcontained = TRUE)
        if (requireNamespace("webshot2", quietly = TRUE)) {
          webshot2::webshot(tmp, file)
        } else if (requireNamespace("webshot", quietly = TRUE)) {
          webshot::webshot(tmp, file)
        } else stop("no screenshot backend")
        TRUE
      }, error = function(e) FALSE)
    }
    
    if (!ok) {
      stop("Static export of this interactive chart needs the 'kaleido' or 'webshot2' package. Select 'html' in the format box instead.")
    }
    return(invisible(file))
  }
  
  # ---- static plots --------------------------------------------------------
  if (inherits(p, "ggplot") || inherits(p, "patchwork")) {
    ggplot2::ggsave(file, plot = p, device = fmt,
                    width = width, height = height, dpi = dpi, bg = "white")
  } else {
    switch(
      fmt,
      pdf  = grDevices::pdf(file, width = width, height = height),
      jpeg = grDevices::jpeg(file, width = width * dpi, height = height * dpi,
                             res = dpi, quality = 95),
      grDevices::png(file, width = width * dpi, height = height * dpi,
                     res = dpi, bg = "white")
    )
    on.exit(grDevices::dev.off(), add = TRUE)
    if (inherits(p, "recordedplot")) grDevices::replayPlot(p) else print(p)
  }
  invisible(file)
}

# Use this if any plot_* function DRAWS with base graphics instead of
# returning an object:  my_plot <- reactive({ capture_base_plot(fn(...)) })
capture_base_plot <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
  grDevices::recordPlot()
}


# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
load_data_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      bs4Card(
        collapsed = FALSE, collapsible = FALSE,
        title = "Load Health, Climate and Map Data",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        # ---- Health Data ----
        fluidRow(
          column(
            width = 4,
            HTML("
            <div style='font-size: 15px;'>
            <h3>Upload Health Data</h3>
            <details>
            <summary><b>Required columns</b></summary>
            <p></p>
                <ul>
                  <li><code>province_col</code> &ndash; Region/Province name</li>
                  <li><code>district_col</code> &ndash; District - subregion names</li>
                  <li><code>date_col</code> &ndash; Data collection date</li>
                  <li><code>year_col</code> - Year column in the dataset</li>
                  <li><code>month_col</code> - Month column in the dataset</li>
                  <li><code>case_col</code> - Disease case column (Counts)</li>
                  <li><code>case_type</code> - Malaria/Diarrhea</li>
                  <li><code>tot_pop_col</code> - Total Population column</li>
                  <li><code>intervention_col</code> - Intervention column</li>
                  <li><code>irs_col</code> - Indoor Residual Spray</li>
                </ul>
                </details>
                </div>
            "),
            
            tags$div(style = "font-size: 15px;",
                     fileInput(ns("health_file"), "Upload Health XLSX/CSV"),
                     uiOutput(ns("health_warning")),
                     verbatimTextOutput(ns("health_preview")),
                     uiOutput(ns("health_col_selectors"))
            )
          ),
          
          column(
            width = 4,
            HTML("
            <div style='font-size: 15px;'>
            <h3>Upload Climate Data</h3>
            <details>
            <summary><b>Required columns</b></summary>
            <p></p>
                <ul>
                  <li><code>province_col</code> &ndash; Region/Province name</li>
                  <li><code>district_col</code> &ndash; District - subregion names</li>
                  <li><code>tmin_col</code> &ndash; Minimum temperature</li>
                  <li><code>tmean_col</code> &ndash; Average temperature</li>
                  <li><code>tmax_col</code> &ndash; Maximum temperature</li>
                  <li><code>rainfall_col</code> - Cummulative rainfall</li>
                  <li><code>r_humidity_col</code> - Relative humidity</li>
                  <li><code>runoff_col</code> - Surface runoff water</li>
                  <li><code>spi_col</code> (optional) - Standardized Precipitation Index (Drought Index)</li>
                  <li><code>ndvi_col</code> (optional) - Normalized Difference Vegetation Index</li>
                </ul>
                </details>
                </div>
            "),
            tags$div(style = "font-size: 15px;",
                     fileInput(ns("climate_file"), "Upload Climate XLSX/CSV"),
                     uiOutput(ns("climate_warning")),
                     verbatimTextOutput(ns("climate_preview")),
                     uiOutput(ns("climate_col_selectors"))
            )
          ),
          
          column(
            width = 4,
            HTML("
            <div style='font-size: 15px;'>
            <h3>Geographical boundaries (Shapefile)</h3>
            <details>
            <summary><b>Expected columns</b></summary>
            <p></p>
                <ul>
                  <li><code>province_col</code></li>
                  <li><code>district_col</code></li>
                  <li><code>geometry_col</code></li>
                </ul>
                </details>
                </div>
            "),
            tags$div(style = "font-size: 15px;",
                     fileInput(ns("shape_file"), "Upload Shapefile ZIP"),
                     uiOutput(ns("shape_warning")),
                     verbatimTextOutput(ns("shape_preview")),
                     uiOutput(ns("shp_selector")),
                     uiOutput(ns("shp_col_selectors"))
            )
          )
        )
      )
    ),
    
    fluidRow(
      bs4Card(
        collapsed = FALSE, collapsible = FALSE,
        title = "Additional setup",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        tags$div(style = "font-size:15px; line-height:1.5;",
                 
                 tags$p(
                   "Climate-health models require additional parameters to control how climate variables, lags, ",
                   "temporal effects, and epidemiological assumptions enter the modeling framework."
                 ),
                 
                 tags$details(
                   tags$summary(tags$b("Required parameters")),
                   tags$br(),
                   tags$ul(
                     tags$li(
                       tags$b(HTML("<code>Maximum lag (max_lag)</code>: ")),
                       "Health outcomes such as Vector-borne diseases may respond to climate exposures ",
                       "with delays of days/ weeks or months. This parameter defines how far back in time the model ",
                       "should look to capture delayed effects."
                     ),
                     
                     tags$li(
                       tags$b(HTML("<code>Number of knots</code>: ")),
                       "Number of internal knots for the natural spline of each predictor, controlling its flexibility: ",
                       "nk = 1 generates a simple spline with two columns, nk = 2 yields a more flexible ",
                       "spline with three columns, and higher values of nk further increase flexibility but ",
                       "may also raise collinearity among spline terms. Defaults to 1."
                     ),
                     
                     tags$li(
                       tags$b(HTML("<code>Basis matrices (basis_matrices_choices)</code>: ")),
                       "Used to construct flexible temperature, rainfall, or humidity response curves. ",
                       "They allow the model to capture non-linear effects."
                     ),
                     
                     tags$li(
                       tags$b(HTML("<code>INLA parameters (inla_param)</code>: ")),
                       "These are the climate variables used to control for the primary exposure ",
                       "of interest included in the INLA-DLNM model."
                     ),
                     
                     tags$li(
                       tags$b(HTML("<code>Parameter term (param_term)</code>: ")),
                       "Defines the primary exposure of interest (for example, Tmax). ",
                       "This is typically the variable for which results or maps are generated."
                     ),
                     
                     tags$li(
                       tags$b(HTML("<code>Aggregation level (level)</code>: ")),
                       "Specifies whether the modeling is performed at country, province, or district level. ",
                       "This controls how the data is grouped for both climate and health sources."
                     ),
                     
                     tags$li(
                       tags$b(HTML("<code>Threshold parameter (param_threshold)</code>: ")),
                       "Some climate-health relationships only emerge above certain temperature or humidity ",
                       "levels. This parameter lets you define such cutoff points."
                     ),
                     
                     tags$li(
                       tags$b(HTML("<code>Filter year (filter_year)</code>: ")),
                       "Optional parameter allowing the model to restrict analysis to a specific year, ",
                       "useful for scenario modeling or data validation."
                     ),
                     
                     tags$li(
                       tags$b(HTML("<code>Model family (family)</code>: ")),
                       "Specifies the statistical distribution used in the regression model. ",
                       "Poisson is commonly used for count data such as disease cases."
                     ),
                     
                     tags$li(
                       tags$b(HTML("<code>Output options (CSV, Figures, Model, Config)</code>: ")),
                       "Allow saving model outputs, configuration files, and visualization products for ",
                       "later analysis or reproducibility."
                     ),
                     
                     tags$li(
                       tags$b(HTML("<code>Group by year</code>: ")),
                       "Toggles annual aggregation when comparing interannual climate-health patterns."
                     )
                   )
                 ),
                 
                 tags$p(
                   "These parameters provide flexibility to adapt the climate-health model to different datasets, ",
                   "geographic levels, and epidemiological behaviors."
                 )
        ),
        # ------------------ INPUT CONTROLS ---------------------
        uiOutput(ns("health_data_years")),
        
        fluidRow(
          column(4,
                 selectInput(
                   ns("basis_matrices_choices"),
                   "Basis Matrix Variable",
                   choices = c("tmax", "tmin", "rainfall", "r_humidity"),
                   selected = "tmax"
                 )
          ),
          column(4, uiOutput(ns("from_basis_matrix"))),
          column(4,
                 selectInput(
                   ns("level"),
                   "Spatial Level",
                   choices = c("country", "region", "district"),
                   selected = "country"
                 )
          )
        ),
        
        fluidRow(
          column(width = 6, uiOutput(ns("inla_param_ui"))),
          column(6,
                 selectInput(
                   ns("family"),
                   "Model Family",
                   choices = c("poisson", "nbinomial", "nbinomial2"),
                   selected = "poisson"
                 )
          )
        ),
        
        hr(),
        
        fluidRow(
          column(3, checkboxInput(ns("group_by_year"), "Group by Year", value = FALSE)),
          column(3, checkboxInput(ns("config"), "Save Configuration File", value = TRUE)),
          column(3, checkboxInput(ns("save_csv"), "Save CSV Output", value = TRUE)),
          column(3, checkboxInput(ns("save_model"), "Save Model Object", value = FALSE))
        ),
        
        fluidRow(
          column(3, checkboxInput(ns("save_fig"), "Save Figures", value = FALSE))
        )
      )
    ),
    
    fluidRow(
      bs4Card(collapsed = TRUE, collapsible = TRUE,
              title = "Combine Health, Climate and Data",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              actionButton(ns("combine_data"), "Combine Data", class = "btn btn-primary"),
              br(), br(),
              downloadButton(ns("dl_combined_csv"), "Download combined data (CSV)",
                             class = "btn btn-sm btn-outline-secondary"),
              br(), br(),
              reactableOutput(ns("combined_data_table"))
      )
    ),
    
    fluidRow(
      bs4Card(collapsed = TRUE, collapsible = TRUE,
              title = "Plot Health and Climate Time Series",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(width = 4, uiOutput(ns("var_yearlyplot"))),
                column(width = 4, uiOutput(ns("disaggregate"))),
                column(width = 4, uiOutput(ns("select_dissaggregate")))
              ),
              plotlyOutput(ns("hc_ts_plot"))
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        bs4Card(
          collapsible = TRUE, collapsed = FALSE,
          title = "Generate Cross-Basis Matrices (CBM)",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          actionButton(ns("generate_cbm"), "Generate CBM", class = "btn btn-primary"),
          verbatimTextOutput(ns("cbm_summary"))
        ),
        bs4Card(
          collapsible = TRUE, collapsed = FALSE,
          title = "Create indices for INLA models",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          actionButton(ns("create_inlaindex"), "Create", class = "btn btn-primary"),
          verbatimTextOutput(ns("inlaidx_summary"))
        )
      ),
      column(
        width = 6,
        bs4Card(
          collapsible = TRUE, collapsed = FALSE,
          title = "Check multicollinearity (VIF)",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          actionButton(ns("checking_vif"), "Check VIF", class = "btn btn-primary"),
          reactableOutput(ns("vif_summary")),
          uiOutput(ns("vif_comment"))
        )
      )
    ),
    
    actionButton(ns("run_inla_models"), "Run INLA models", class = "btn btn-primary"),
    
    fluidRow(
      verbatimTextOutput(ns("run_inla_summary")),
      verbatimTextOutput(ns("model_results"))
    ),
    
    fluidRow(
      bs4Card(
        collapsible = TRUE, collapsed = FALSE,
        title = "Monthly Random Effect (Seasonality)",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        plotOutput(ns("monthly_random_effects")),
        plot_downloads_ui(ns, "mre")
      ),
      
      bs4Card(
        collapsible = TRUE, collapsed = FALSE,
        title = "Disease Incidence Rate (Spatial Plot)",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        fluidRow(
          column(width = 7, uiOutput(ns("year_selector"))),
          column(width = 5, uiOutput(ns("compare_years")))
        ),
        plotOutput(ns("yearly_spat_random_effects")),
        plot_downloads_ui(ns, "spat")
      )
    ),
    
    fluidRow(
      # contour_plot
      bs4Card(
        collapsible = TRUE, collapsed = FALSE,
        title = "Exposure-lag relationship",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        fluidRow(
          column(width = 6, uiOutput(ns("cp_disaggregate"))),
          column(width = 5, uiOutput(ns("cp_select_dissaggregate"))),
          column(width = 1, actionButton(ns("show_help"), "Help", icon = icon("info-circle")))
        ),
        plotOutput(ns("contour_plot")),
        plot_downloads_ui(ns, "contour")
      ),
      bs4Card(
        collapsible = TRUE, collapsed = FALSE,
        title = "Relative risk plot",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        fluidRow(
          column(width = 6, uiOutput(ns("rrp_disaggregate"))),
          column(width = 5, uiOutput(ns("rrp_select_dissaggregate"))),
          column(width = 1, actionButton(ns("show_help_rrplot"), "Help", icon = icon("info-circle")))
        ),
        plotOutput(ns("plot_relative_risk")),
        plot_downloads_ui(ns, "rrp")
      )
    ),
    
    fluidRow(
      bs4Card(
        collapsible = TRUE, collapsed = FALSE,
        title = "Relative risk map",
        status = "info",
        solidHeader = TRUE,
        width = 6,
        fluidRow(
          column(width = 6, uiOutput(ns("rr_map_disaggregate"))),
          column(width = 5, uiOutput(ns("rrm_select_dissaggregate"))),
          column(width = 1, actionButton(ns("show_help_map"), "Help", icon = icon("info-circle")))
        ),
        plotOutput(ns("plot_rr_map"), height = "400px"),
        plot_downloads_ui(ns, "rrmap")
      ),
      ##attm_disaggregate attm_metric attm_select_year plot_attribution_metric
      bs4Card(
        collapsible  = TRUE,
        collapsed    = FALSE,
        maximizable  = TRUE,        # key: allows full screen expansion
        title        = "Attributable number & rate",
        status       = "info",
        solidHeader  = TRUE,
        width        = 6,
        fluidRow(
          column(width = 4, uiOutput(ns("attm_disaggregate"))),
          column(width = 3, uiOutput(ns("attm_metric"))),
          column(width = 2, uiOutput(ns("attm_select_year"))),
          column(width = 2, uiOutput(ns("specific_districts"))),
          column(width = 1, actionButton(ns("show_help_attr_plot"), "Help", icon = icon("info-circle")))
        ),
        div(
          style = "overflow-x: auto;",
          uiOutput(ns("plot_attribution_metric_ui"))
        ),
        # html is offered because the district view is an interactive widget
        plot_downloads_ui(ns, "attm", formats = c("png", "pdf", "jpeg", "html"))
      )
    ),
    
    fluidRow(
      bs4Card(
        collapsible = TRUE, collapsed = FALSE,
        title = "Monthly average attributable number & rate",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(width = 4, uiOutput(ns("attavm_disaggregate"))),
          column(width = 3, uiOutput(ns("attavm_select_dissaggregate"))),
          column(width = 3, uiOutput(ns("attavm_metric"))),
          column(width = 2, uiOutput(ns("attavm_select_year")))
        ),
        plotOutput(ns("plot_avg_monthly")),
        plot_downloads_ui(ns, "attavm")
      )
    )
  )
}


# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
load_data_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # increasing the upload size
    options(shiny.maxRequestSize = 500 * 1024^2)
    
    # -------------------------------------------------------------------------
    # Download registration (closes over input/output)
    # -------------------------------------------------------------------------
    register_plot_downloads <- function(key, plot_obj, data_obj = NULL, base_name = key) {
      
      fmt <- function() {
        f <- input[[paste0("dl_fmt_", key)]]
        if (is.null(f) || !nzchar(f)) "png" else f
      }
      
      output[[paste0("dl_img_", key)]] <- downloadHandler(
        filename = function() {
          sprintf("%s_%s.%s", base_name, format(Sys.Date(), "%Y%m%d"), fmt())
        },
        content = function(file) {
          p <- tryCatch(plot_obj(), error = function(e) NULL)
          if (is.null(p)) {
            showNotification("Generate the plot first, then download.", type = "warning")
            req(FALSE)
          }
          tryCatch(
            .save_plot(p, file, fmt()),
            error = function(e) {
              showNotification(conditionMessage(e), type = "error", duration = NULL)
              req(FALSE)
            }
          )
        }
      )
      
      output[[paste0("dl_csv_", key)]] <- downloadHandler(
        filename = function() {
          sprintf("%s_%s.csv", base_name, format(Sys.Date(), "%Y%m%d"))
        },
        content = function(file) {
          p  <- tryCatch(plot_obj(), error = function(e) NULL)
          fb <- if (is.null(data_obj)) NULL else tryCatch(data_obj(), error = function(e) NULL)
          d  <- .plot_data(p, fb)
          if (!is.data.frame(d) || nrow(d) == 0) {
            showNotification("No tabular data available for this plot yet.", type = "warning")
            req(FALSE)
          }
          utils::write.csv(d, file, row.names = FALSE)
        }
      )
    }
    
    # -------------------------------------------------------------------------
    # Health data
    # -------------------------------------------------------------------------
    observeEvent(input$health_file, {
      req(input$health_file)
      
      hd_path <- input$health_file$datapath
      ext <- tolower(tools::file_ext(input$health_file$name))
      
      if (ext == "csv") {
        r$health_data <- read.csv(hd_path)
      } else if (ext %in% c("xlsx", "xls")) {
        r$health_data <- readxl::read_excel(hd_path)
      } else {
        showNotification("File must be CSV or Excel", type = "error")
        return()
      }
      
      r$case_types <- c("malaria", "diarrhea")
      r$cols <- c("", names(r$health_data))
    })
    
    output$health_col_selectors <- renderUI({
      req(r$cols)
      
      tagList(
        fluidRow(
          column(
            width = 6,
            selectInput(ns("region_col"), "Region/Province", r$cols, selected = r$cols %>% .[grepl("region|province", ., ignore.case = T)])
          ),
          column(
            width = 6,
            selectInput(ns("district_col"), "District", r$cols, selected = r$cols %>% .[grepl("district", ., ignore.case = T)])
          )
        ),
        fluidRow(
          column(
            width = 4,
            selectInput(ns("date_col"), "Date", r$cols),
          ),
          column(
            width = 4,
            selectInput(ns("year_col"), "Year", r$cols, selected = r$cols %>% .[grepl("year", ., ignore.case = T)])
          ),
          column(
            width = 4,
            selectInput(ns("month_col"), "Month", r$cols, selected = r$cols %>% .[grepl("month", ., ignore.case = T)])
          )
        ),
        fluidRow(
          column(
            width = 6,
            selectInput(ns("case_col"), "Case Count", r$cols),
          ),
          column(
            width = 6,
            selectInput(ns("case_type"), "Case Type", choices = r$case_types, selected = "malaria")
          )
        ),
        fluidRow(
          column(
            width = 4,
            selectInput(ns("tot_pop_col"), "Total population", choices = r$cols, selected = r$cols %>% .[grepl("pop", ., ignore.case = T)])
          ),
          column(
            width = 4,
            selectInput(ns("intervention_col"), "Intervention", r$cols, selected = r$cols %>% .[grepl("intervention", ., ignore.case = T)])
          ),
          column(
            width = 4,
            selectInput(ns("irs_col"), "Indoor Residual Spray", r$cols, selected = r$cols %>% .[grepl("irs", ., ignore.case = T)])
          )
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Climate data
    # -------------------------------------------------------------------------
    observeEvent(input$climate_file, {
      req(input$climate_file)
      
      cd_path <- input$climate_file$datapath
      ext <- tolower(tools::file_ext(input$climate_file$name))
      
      if (ext == "csv") {
        r$climate_data <- read.csv(cd_path)
      } else if (ext %in% c("xlsx", "xls")) {
        r$climate_data <- readxl::read_excel(cd_path)
      } else {
        showNotification("File must be CSV or Excel", type = "error")
        return()
      }
      
      r$climate_cols <- c("", names(r$climate_data))
    })
    
    output$climate_col_selectors <- renderUI({
      req(r$climate_cols)
      
      tagList(
        fluidRow(
          column(width = 6,
                 selectInput(ns("c_district_col"), label = "District",
                             choices = r$climate_cols,
                             selected = r$climate_cols %>% .[grepl("district", ., ignore.case = TRUE)])
          ),
          column(
            width = 6,
            selectInput(ns("tmin_col"), "Min. temp.", r$climate_cols,
                        selected = r$climate_cols %>% .[grepl("min", ., ignore.case = TRUE)])
          )
        ),
        fluidRow(
          column(
            width = 6,
            selectInput(ns("tmean_col"), "Av. temp.", r$climate_cols,
                        selected = r$climate_cols %>% .[grepl("mean|average", ., ignore.case = TRUE)])
          ),
          column(
            width = 6,
            selectInput(ns("tmax_col"), "Max. temp.", r$climate_cols,
                        selected = r$climate_cols %>% .[grepl("max", ., ignore.case = TRUE)])
          )
        ),
        fluidRow(
          column(
            width = 4,
            selectInput(ns("rainfall_col"), "Rainfall", r$climate_cols)
          ),
          column(
            width = 4,
            selectInput(ns("r_humidity_col"), "Rel. humidity", r$climate_cols)
          ),
          column(
            width = 4,
            selectInput(ns("runoff_col"), "Runoff", r$climate_cols,
                        selected = r$climate_cols %>% .[grepl("runoff", ., ignore.case = TRUE)])
          )
        ),
        fluidRow(
          column(
            width = 6,
            selectInput(ns("spi_col"), "SPI", choices = r$climate_cols)
          ),
          column(
            width = 6,
            selectInput(ns("ndvi_col"), "NDVI", choices = r$climate_cols)
          )
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Shapefile  (observers un-nested so they don't stack on re-upload)
    # -------------------------------------------------------------------------
    observeEvent(input$shape_file, {
      req(input$shape_file)
      
      zip_path <- input$shape_file$datapath
      
      r$unzip_dir <- tempfile(pattern = "shp_")
      dir.create(r$unzip_dir)
      
      unzip(zip_path, exdir = r$unzip_dir)
      
      # Detect all .shp files (recursive: ZIPs often contain a folder)
      r$shp_file_list <- list.files(r$unzip_dir, pattern = "\\.shp$",
                                    full.names = TRUE, recursive = TRUE)
      r$selected_shp  <- NULL
      r$shp_cols      <- NULL
    })
    
    output$shp_selector <- renderUI({
      req(r$shp_file_list)
      
      if (length(r$shp_file_list) == 0) {
        return(tags$div(
          style = "color:red; font-weight:bold;",
          "No .shp file found inside the ZIP."
        ))
      }
      
      selectInput(ns("selected_shp"), "Select Shapefile:",
                  choices = basename(r$shp_file_list))
    })
    
    observeEvent(input$selected_shp, {
      req(input$selected_shp, r$shp_file_list)
      
      path <- r$shp_file_list[basename(r$shp_file_list) == input$selected_shp]
      req(length(path) > 0)
      
      r$shp_full_path <- path[1]
      r$selected_shp  <- sf::st_read(r$shp_full_path, quiet = TRUE)
      r$shp_cols      <- c("", names(r$selected_shp))
    })
    
    output$shp_col_selectors <- renderUI({
      req(r$shp_cols)
      
      tagList(
        fluidRow(
          column(
            width = 12,
            selectInput(ns("map_region_col"), "Region/Province", r$shp_cols),
            selectInput(ns("map_district_col"), "District", r$shp_cols),
            selectInput(ns("geometry_col"), "Geometry", r$shp_cols)
          )
        )
      )
    })
    
    output$health_data_years <- renderUI({
      fluidRow(
        column(4,
               numericInput(ns("max_lag"), "Maximum Lag", value = 2, min = 1)
        ),
        column(4,
               numericInput(ns("nk"), "Number of Knots", value = 2, min = 1, max = 2)
        ),
        column(width = 4,
               numericInput(ns("param_threshold"), "Threshold Parameter", value = 1)
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Combine
    # -------------------------------------------------------------------------
    combined_data <- reactiveVal(NULL)
    
    observeEvent(input$combine_data, {
      req(
        input$health_file$datapath, input$climate_file$datapath, r$shp_full_path, input$max_lag
      )
      
      r$data_lists <- combine_health_climate_data(
        health_data_path  = input$health_file$datapath,
        climate_data_path = input$climate_file$datapath,
        map_path          = r$shp_full_path,
        map_region_col    = input$map_region_col,
        map_district_col  = input$map_district_col,
        region_col        = input$region_col,
        district_col      = input$district_col,
        date_col          = if (input$date_col == "") NULL else input$date_col,
        year_col          = input$year_col,
        month_col         = input$month_col,
        case_col          = input$case_col,
        case_type         = input$case_type,
        tot_pop_col       = input$tot_pop_col,
        intervention_col  = if (input$intervention_col == "") NULL else input$intervention_col,
        irs_col           = if (input$irs_col == "") NULL else input$irs_col,
        tmin_col          = input$tmin_col,
        tmean_col         = input$tmean_col,
        tmax_col          = input$tmax_col,
        rainfall_col      = input$rainfall_col,
        r_humidity_col    = input$r_humidity_col,
        geometry_col      = input$geometry_col,
        runoff_col        = input$runoff_col,
        ndvi_col          = if (input$ndvi_col == "") NULL else input$ndvi_col,
        spi_col           = if (input$spi_col == "") NULL else input$spi_col,
        max_lag           = input$max_lag,
        output_dir        = NULL
      )
      
      combined_data(r$data_lists$data)
    })
    
    # display combined data
    output$combined_data_table <- renderReactable({
      req(combined_data())
      
      df <- combined_data() %>%
        dplyr::rename("Region/Province" = region) %>%
        dplyr::mutate(
          year  = as.factor(year),
          month = as.factor(month)
        )
      
      # integer-like: whole numbers only (population, cases, counts)
      is_integer_like <- function(col) {
        is.numeric(col) && all(col == floor(col), na.rm = TRUE)
      }
      
      col_defs <- lapply(df, function(col) {
        if (!is.numeric(col)) {
          colDef()
        } else if (is_integer_like(col)) {
          colDef(format = colFormat(separators = TRUE, digits = 0))
        } else {
          colDef(format = colFormat(digits = 3))
        }
      })
      
      reactable(
        df,
        searchable = TRUE,
        highlight  = TRUE,
        columns    = col_defs,
        theme = reactableTheme(
          backgroundColor = "white",
          rowStyle        = list(backgroundColor = "white"),
          headerStyle     = list(
            background = "white",
            color      = "black",
            fontWeight = "bold"
          )
        )
      )
    })
    
    output$dl_combined_csv <- downloadHandler(
      filename = function() sprintf("combined_data_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        d <- combined_data()
        if (is.null(d)) {
          showNotification("Combine the data first.", type = "warning")
          req(FALSE)
        }
        if (inherits(d, "sf")) d <- sf::st_drop_geometry(d)
        utils::write.csv(d, file, row.names = FALSE)
      }
    )
    
    # -------------------------------------------------------------------------
    # Parameter UI
    # -------------------------------------------------------------------------
    output$inla_param_ui <- renderUI({
      selectInput(ns("inla_param"), "INLA Model Parameters",
                  choices  = c("tmin", "rainfall", "r_humidity", "tmax", "runoff", "spi", "ndvi", "intervention"
                  ) %>% .[. != input$param_term],
                  selected = c("runoff", "rainfall", "r_humidity") %>% .[. != input$param_term],
                  multiple = TRUE
      )
    })
    
    output$from_basis_matrix <- renderUI({
      selectInput(
        ns("param_term"),
        "Parameter Term",
        choices = input$basis_matrices_choices
      )
    })
    
    # -------------------------------------------------------------------------
    # Time series
    # -------------------------------------------------------------------------
    output$var_yearlyplot <- renderUI({
      req(input$param_term, combined_data())
      selectInput(ns("param_yearlyplot"), label = "Select variable to plot",
                  choices = c(input$case_type, "tmax", "tmin", "tmean", "rainfall", "r_humidity"),
                  selected = input$case_type)
    })
    
    output$disaggregate <- renderUI({
      radioButtons(ns("plot_by"), "Plot by", choices = c("country", "Region/Province", "district"), inline = TRUE,
                   selected = "country")
    })
    
    output$select_dissaggregate <- renderUI({
      req(input$plot_by, combined_data())
      if (input$plot_by == "Region/Province") {
        selectInput(ns("selected_item"), "Select provinces",
                    choices = combined_data() %>% dplyr::select(region) %>% unique() %>% dplyr::pull(),
                    multiple = TRUE)
      } else {
        selectInput(ns("selected_item"), "Select districts",
                    choices = combined_data() %>% dplyr::select(district) %>% unique() %>% dplyr::pull(),
                    multiple = TRUE)
      }
    })
    
    output$hc_ts_plot <- renderPlotly({
      req(input$param_term, combined_data(), input$level, input$plot_by)
      
      if (input$plot_by == "Region/Province") {
        req(input$selected_item)
        plot_health_climate_timeseries(combined_data() %>% dplyr::filter(region %in% input$selected_item),
                                       param_term = input$param_yearlyplot,
                                       level = "region",
                                       filter_year = input$filter_year,
                                       case_type = input$case_type,
                                       save_fig = input$save_fig,
                                       output_dir = NULL)
      } else if (input$plot_by == "district") {
        req(input$selected_item)
        plot_health_climate_timeseries(combined_data() %>% dplyr::filter(district %in% input$selected_item),
                                       param_term = input$param_yearlyplot,
                                       level = "district",
                                       filter_year = input$filter_year,
                                       case_type = input$case_type,
                                       save_fig = input$save_fig,
                                       output_dir = NULL)
      } else {
        plot_health_climate_timeseries(combined_data(),
                                       param_term = input$param_yearlyplot,
                                       level = "country",
                                       filter_year = input$filter_year,
                                       case_type = input$case_type,
                                       save_fig = input$save_fig,
                                       output_dir = NULL)
      }
    })
    
    # -------------------------------------------------------------------------
    # CBM / indices / VIF
    # -------------------------------------------------------------------------
    observeEvent(input$generate_cbm, {
      req(combined_data(), input$case_type, input$max_lag)
      r$cbm <- set_cross_basis(combined_data(),
                               nk = input$nk,
                               max_lag = input$max_lag)
    })
    
    output$cbm_summary <- renderPrint({
      cat(ifelse(is.null(r$cbm), "CBM not yet generated", "CBM generated!"))
    })
    
    observeEvent(input$create_inlaindex, {
      req(combined_data(), input$case_type)
      r$inla_indexes <- create_inla_indices(combined_data(), input$case_type)
    })
    
    output$inlaidx_summary <- renderPrint({
      cat(ifelse(is.null(r$inla_indexes), "Not created", "Created!"))
    })
    
    observeEvent(input$checking_vif, {
      req(combined_data())
      
      r$check_vif <- check_diseases_vif(
        data = combined_data(),
        inla_param = input$inla_param,
        param_term = input$param_term
      )
    })
    
    output$vif_summary <- renderReactable({
      req(r$check_vif)
      
      df <- data.frame(r$check_vif, check.names = FALSE) %>%
        rename(
          VIF = vif,
          Interpretation = vif_interpretation,
          Variables = variables
        ) %>%
        mutate(VIF = round(as.numeric(VIF), 3))
      
      reactable(
        df,
        searchable = FALSE,
        highlight = TRUE,
        theme = reactableTheme(
          backgroundColor = "white",
          rowStyle = list(backgroundColor = "white"),
          headerStyle = list(
            background = "white",
            color = "black",
            fontWeight = "bold"
          )
        )
      )
    })
    
    output$vif_comment <- renderUI({
      req(r$check_vif)
      HTML("<p>If VIF value is greater than 5, then the corresponding variable should be removed from the INLA parameters.</p>")
    })
    
    # -------------------------------------------------------------------------
    # Run model
    # -------------------------------------------------------------------------
    observeEvent(input$run_inla_models, {
      req(r$data_lists, input$basis_matrices_choices, input$inla_param, input$case_type, input$family)
      
      r$inla_model <- run_inla_models(r$data_lists,
                                      basis_matrices_choices = input$basis_matrices_choices,
                                      inla_param = input$inla_param,
                                      case_type = input$case_type,
                                      max_lag = input$max_lag,
                                      nk = input$nk,
                                      save_model = input$save_model,
                                      family = input$family,
                                      config = FALSE)
      
      # create the folder first so a fresh checkout does not error here
      temp_dir <- file.path(".", "R", "Functions", "temp")
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(r$inla_model, file.path(temp_dir, "model.rds"))   # saving the model results
      saveRDS(r$data_lists, file.path(temp_dir, "data_list.rds")) # saving combined data
    })
    
    output$run_inla_summary <- renderPrint({
      if (!is.null(r$inla_model)) cat("Model run complete!\n") else cat("Run the model\n")
    })
    
    # -------------------------------------------------------------------------
    # Monthly random effect
    # -------------------------------------------------------------------------
    mre_plot <- reactive({
      req(r$data_lists, r$inla_model$model)
      .first_plot(plot_monthly_random_effects(
        combined_data = r$data_lists, model = r$inla_model$model,
        save_fig = input$save_fig,
        output_dir = NULL
      ))
    })
    
    output$monthly_random_effects <- renderPlot({ mre_plot() })
    
    register_plot_downloads("mre", mre_plot, base_name = "monthly_random_effect")
    
    # -------------------------------------------------------------------------
    # Disease incidence rate (spatial)
    # -------------------------------------------------------------------------
    output$year_selector <- renderUI({
      radioButtons(ns("filter_years"), label = NULL,
                   choices = c("Average", "All Years", "Compare Years"),
                   selected = "All Years", inline = TRUE)
    })
    
    output$compare_years <- renderUI({
      req(r$data_lists$data)
      yrs <- r$data_lists$data$year %>% unique()
      selectInput(ns("selected_years"), label = NULL,
                  choices = yrs, multiple = TRUE,
                  selected = yrs %>% .[[length(.)]])
    })
    
    r$selected_years <- reactive({
      req(input$filter_years)
      if (input$filter_years == "Compare Years") {
        req(input$selected_years)
        input$selected_years
      } else {
        input$filter_years
      }
    })
    
    output$checking_something <- renderPrint({
      r$selected_years()
    })
    
    spat_plot <- reactive({
      req(r$data_lists, r$inla_model$model, r$selected_years())
      .first_plot(plot_yearly_spatial_random_effect(
        combined_data = r$data_lists, model = r$inla_model$model,
        case_type = input$case_type,
        filter_year = r$selected_years(),
        save_fig = input$save_fig,
        output_dir = NULL
      ))
    })
    
    output$yearly_spat_random_effects <- renderPlot({ spat_plot() })
    
    register_plot_downloads("spat", spat_plot, base_name = "disease_incidence_rate_map")
    
    # -------------------------------------------------------------------------
    # Exposure-lag relationship (contour)
    # -------------------------------------------------------------------------
    output$check_vals <- renderPrint({
      cat(input$level_for_cp)
    })
    
    output$cp_disaggregate <- renderUI({
      radioButtons(ns("cp_plot_by"), "Plot by", choices = c("country", "Region/Province", "district"), inline = TRUE,
                   selected = "country")
    })
    
    output$cp_select_dissaggregate <- renderUI({
      req(input$cp_plot_by)
      
      if (input$cp_plot_by == "Region/Province") {
        req(combined_data())
        selectInput(ns("cp_selected_item"), "Select provinces",
                    choices = combined_data() %>% dplyr::select(region) %>% unique() %>% dplyr::pull())
      } else {
        req(combined_data())
        selectInput(ns("cp_selected_item"), "Select districts",
                    choices = combined_data() %>% dplyr::select(district) %>% unique() %>% dplyr::pull())
      }
    })
    
    contour_plot_obj <- reactive({
      req(r$inla_model$model, input$cp_plot_by, combined_data())
      
      p <- if (!input$cp_plot_by %in% c("Region/Province", "district")) {
        contour_plot(data = combined_data(),
                     model = r$inla_model$model,
                     max_lag = input$max_lag,
                     nk = input$nk,
                     level = "country",
                     case_type = input$case_type,
                     param_term = input$param_term,
                     save_fig = input$save_fig,
                     output_dir = NULL
        )
      } else if (input$cp_plot_by == "Region/Province") {
        req(input$cp_selected_item)
        contour_plot(data = combined_data() %>% dplyr::filter(region == input$cp_selected_item),
                     model = r$inla_model$model,
                     max_lag = input$max_lag,
                     nk = input$nk,
                     level = "region",
                     case_type = input$case_type,
                     param_term = input$param_term,
                     save_fig = input$save_fig,
                     output_dir = NULL
        )
      } else {
        req(input$cp_selected_item)
        contour_plot(data = combined_data() %>% dplyr::filter(district == input$cp_selected_item),
                     model = r$inla_model$model,
                     max_lag = input$max_lag,
                     nk = input$nk,
                     level = "district",
                     case_type = input$case_type,
                     param_term = input$param_term,
                     save_fig = input$save_fig,
                     output_dir = NULL
        )
      }
      
      .first_plot(p)
    })
    
    output$contour_plot <- renderPlot({ contour_plot_obj() })
    
    register_plot_downloads("contour", contour_plot_obj, base_name = "exposure_lag_contour")
    
    # -------------------------------------------------------------------------
    # Relative risk map
    # -------------------------------------------------------------------------
    output$rr_map_disaggregate <- renderUI({
      radioButtons(ns("rrm_plot_by"), "Plot by", choices = c("cummulative", "filtered"), inline = TRUE,
                   selected = "cummulative")
    })
    
    output$rrm_select_dissaggregate <- renderUI({
      req(input$rrm_plot_by)
      
      if (input$rrm_plot_by == "filtered") {
        req(combined_data())
        selectInput(ns("rrm_selected_item"), "Select year",
                    choices = combined_data() %>% dplyr::select(year) %>% unique() %>% dplyr::pull(),
                    multiple = TRUE)
      }
    })
    
    rr_map_obj <- reactive({
      req(r$data_lists, r$inla_model$model, input$rrm_plot_by)
      
      p <- if (input$rrm_plot_by == "cummulative") {
        plot_rr_map(combined_data = r$data_lists,
                    model = r$inla_model$model,
                    max_lag = input$max_lag,
                    nk = input$nk,
                    level = "district",
                    case_type = input$case_type,
                    param_term = input$param_term,
                    save_fig = input$save_fig,
                    output_dir = NULL,
                    cumulative = TRUE
        )
      } else {
        req(input$rrm_selected_item)
        plot_rr_map(combined_data = r$data_lists,
                    model = r$inla_model$model,
                    max_lag = input$max_lag,
                    nk = input$nk,
                    level = "district",
                    case_type = input$case_type,
                    param_term = input$param_term,
                    filter_year = input$rrm_selected_item,
                    save_fig = input$save_fig,
                    output_dir = NULL
        )
      }
      
      .first_plot(p)
    })
    
    output$plot_rr_map <- renderPlot({ rr_map_obj() })
    
    register_plot_downloads("rrmap", rr_map_obj, base_name = "relative_risk_map")
    
    # -------------------------------------------------------------------------
    # Relative risk plot
    # -------------------------------------------------------------------------
    output$rrp_disaggregate <- renderUI({
      radioButtons(ns("rrp_plot_by"), "Plot by", choices = c("country", "Region/Province", "district"), inline = TRUE,
                   selected = "country")
    })
    
    output$rrp_select_dissaggregate <- renderUI({
      req(input$rrp_plot_by)
      
      if (input$rrp_plot_by == "Region/Province") {
        req(combined_data())
        selectInput(ns("rrp_selected_item"), "Select provinces",
                    choices = combined_data() %>% dplyr::select(region) %>% unique() %>% dplyr::pull())
      } else {
        req(combined_data())
        selectInput(ns("rrp_selected_item"), "Select districts",
                    choices = combined_data() %>% dplyr::select(district) %>% unique() %>% dplyr::pull())
      }
    })
    
    rr_plot_obj <- reactive({
      req(combined_data(), r$inla_model$model, input$rrp_plot_by)
      
      p <- if (!input$rrp_plot_by %in% c("Region/Province", "district")) {
        plot_relative_risk(data = combined_data(),
                           model = r$inla_model$model,
                           max_lag = input$max_lag,
                           nk = input$nk,
                           level = "country",
                           case_type = input$case_type,
                           param_term = input$param_term,
                           save_fig = input$save_fig,
                           filter_year = NULL,
                           output_dir = NULL
        )
      } else if (input$rrp_plot_by == "Region/Province") {
        req(input$rrp_selected_item)
        plot_relative_risk(data = combined_data() %>% dplyr::filter(region == input$rrp_selected_item),
                           model = r$inla_model$model,
                           max_lag = input$max_lag,
                           nk = input$nk,
                           level = "region",
                           case_type = input$case_type,
                           param_term = input$param_term,
                           save_fig = input$save_fig,
                           filter_year = NULL,
                           output_dir = NULL
        )
      } else {
        req(input$rrp_selected_item)
        plot_relative_risk(data = combined_data() %>% dplyr::filter(district == input$rrp_selected_item),
                           model = r$inla_model$model,
                           max_lag = input$max_lag,
                           nk = input$nk,
                           level = "district",
                           case_type = input$case_type,
                           param_term = input$param_term,
                           save_fig = input$save_fig,
                           filter_year = NULL,
                           output_dir = NULL
        )
      }
      
      .first_plot(p)
    })
    
    output$plot_relative_risk <- renderPlot({ rr_plot_obj() })
    
    register_plot_downloads("rrp", rr_plot_obj, base_name = "relative_risk_curve")
    
    # -------------------------------------------------------------------------
    # Attributable number & rate - controls
    # -------------------------------------------------------------------------
    output$attm_disaggregate <- renderUI({
      radioButtons(ns("attm_plot_by"), "Plot by",
                   choices = c("country", "Region/Province", "district"),
                   inline = TRUE,
                   selected = "country"
      )
    })
    
    output$attm_metric <- renderUI({
      selectInput(ns("selected_attm_metric"), "Select the metric",
                  choices = c("AR_Number", "AR_per_100k")
      )
    })
    
    output$attm_select_year <- renderUI({
      req(input$attm_plot_by)
      if (input$attm_plot_by %in% c("Region/Province", "district")) {
        req(combined_data())
        selectInput(ns("attm_selected_year"), "Year(s)",
                    choices = c("All", sort(unique(combined_data()$year))),
                    selected = "All",
                    multiple = TRUE
        )
      }
    })
    
    output$specific_districts <- renderUI({
      req(input$attm_plot_by)
      if (input$attm_plot_by == "district") {
        req(combined_data())
        
        districts <- sort(unique(combined_data()$district))
        districts <- districts[districts != "All"]
        
        selectInput(ns("select_specific_dist"), "Select districts",
                    choices = c("All", districts),
                    selected = "All",
                    multiple = TRUE
        )
      }
    })
    
    # -------------------------------------------------------------------------
    # For "Attributable number & rate" panel
    # Base calculation - always at district level, runs once
    # -------------------------------------------------------------------------
    r$attr_data_base <- reactive({
      req(combined_data(), input$param_term, r$inla_model$model, input$param_threshold)
      attribution_calculation(
        data            = combined_data(),
        param_term      = input$param_term,
        model           = r$inla_model$model,
        level           = "district",
        param_threshold = input$param_threshold,
        max_lag         = input$max_lag,
        nk              = input$nk,
        case_type       = input$case_type
      )
    })
    
    # Country - aggregated from district base
    r$attr_data_country <- reactive({
      req(r$attr_data_base())
      r$attr_data_base() %>%
        dplyr::group_by(year) %>%
        dplyr::summarise(
          dplyr::across(dplyr::matches("^AR_Number(_LCI|_UCI)?$"),
                        ~ sum(.x, na.rm = TRUE)),
          dplyr::across(dplyr::matches("^AR_(Fraction|per_100k)(_LCI|_UCI)?$"),
                        ~ mean(.x, na.rm = TRUE)),
          .groups = "drop"
        )
    })
    
    # Region - aggregated from district base
    r$attr_data_region <- reactive({
      req(r$attr_data_base())
      r$attr_data_base() %>%
        dplyr::group_by(region, year) %>%
        dplyr::summarise(
          dplyr::across(dplyr::matches("^AR_Number(_LCI|_UCI)?$"),
                        ~ sum(.x, na.rm = TRUE)),
          dplyr::across(dplyr::matches("^AR_(Fraction|per_100k)(_LCI|_UCI)?$"),
                        ~ mean(.x, na.rm = TRUE)),
          .groups = "drop"
        )
    })
    
    # District - filtered from base, collapsed to year level
    r$attr_data_district <- reactive({
      req(r$attr_data_base(), input$select_specific_dist)
      r$attr_data_base() %>%
        dplyr::group_by(region, district, year) %>%
        dplyr::summarise(
          dplyr::across(dplyr::matches("^AR_Number(_LCI|_UCI)?$"),
                        ~ sum(.x, na.rm = TRUE)),
          dplyr::across(dplyr::matches("^AR_(Fraction|per_100k)(_LCI|_UCI)?$"),
                        ~ mean(.x, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::filter(
          if ("All" %in% input$select_specific_dist) TRUE
          else district %in% input$select_specific_dist
        )
    })
    
    # -------------------------------------------------------------------------
    # For "Monthly average attributable number & rate" panel - fully independent
    # -------------------------------------------------------------------------
    r$attr_data_monthly_base <- reactive({
      req(combined_data(), input$param_term, r$inla_model$model, input$param_threshold)
      attribution_calculation(
        data            = combined_data(),
        param_term      = input$param_term,
        model           = r$inla_model$model,
        level           = "district",
        param_threshold = input$param_threshold,
        max_lag         = input$max_lag,
        nk              = input$nk,
        case_type       = input$case_type
      )
    })
    
    r$attr_data_monthly_country <- reactive({
      req(r$attr_data_monthly_base())
      r$attr_data_monthly_base() %>%
        dplyr::group_by(year, month) %>%
        dplyr::summarise(
          dplyr::across(dplyr::matches("^AR_Number(_LCI|_UCI)?$"),
                        ~ sum(.x, na.rm = TRUE)),
          dplyr::across(dplyr::matches("^AR_(Fraction|per_100k)(_LCI|_UCI)?$"),
                        ~ mean(.x, na.rm = TRUE)),
          .groups = "drop"
        )
    })
    
    r$attr_data_monthly_region <- reactive({
      req(r$attr_data_monthly_base(), input$attavm_selected_item)
      r$attr_data_monthly_base() %>%
        dplyr::filter(region %in% input$attavm_selected_item) %>%
        dplyr::group_by(region, year, month) %>%
        dplyr::summarise(
          dplyr::across(dplyr::matches("^AR_Number(_LCI|_UCI)?$"),
                        ~ sum(.x, na.rm = TRUE)),
          dplyr::across(dplyr::matches("^AR_(Fraction|per_100k)(_LCI|_UCI)?$"),
                        ~ mean(.x, na.rm = TRUE)),
          .groups = "drop"
        )
    })
    
    r$attr_data_monthly_district <- reactive({
      req(r$attr_data_monthly_base(), input$attavm_selected_item)
      r$attr_data_monthly_base() %>%
        dplyr::filter(district %in% input$attavm_selected_item) %>%
        dplyr::group_by(district, year, month) %>%
        dplyr::summarise(
          dplyr::across(dplyr::matches("^AR_Number(_LCI|_UCI)?$"),
                        ~ sum(.x, na.rm = TRUE)),
          dplyr::across(dplyr::matches("^AR_(Fraction|per_100k)(_LCI|_UCI)?$"),
                        ~ mean(.x, na.rm = TRUE)),
          .groups = "drop"
        )
    })
    
    # -------------------------------------------------------------------------
    # Attributable number & rate - plots
    # -------------------------------------------------------------------------
    
    # UI output - switches between renderPlot and renderPlotly based on level
    output$plot_attribution_metric_ui <- renderUI({
      req(input$attm_plot_by)
      if (input$attm_plot_by %in% c("country", "Region/Province")) {
        plotOutput(ns("plot_attribution_metric_gg"), height = "500px")
      } else {
        plotlyOutput(ns("plot_attribution_metric_plotly"), height = "500px")
      }
    })
    
    # country + region -> pure ggplot2, no ggplotly conversion
    attm_gg <- reactive({
      req(input$attm_plot_by, input$selected_attm_metric)
      
      p <- if (input$attm_plot_by == "country") {
        req(r$attr_data_country())
        plot_attribution_metric(
          attr_data   = r$attr_data_country(),
          level       = "country",
          metrics     = input$selected_attm_metric,
          filter_year = NULL,
          param_term  = input$param_term,
          case_type   = input$case_type
        )
      } else {
        # Region/Province
        req(r$attr_data_region())
        plot_attribution_metric(
          attr_data   = r$attr_data_region(),
          level       = "region",
          metrics     = input$selected_attm_metric,
          filter_year = if ("All" %in% input$attm_selected_year) NULL
          else input$attm_selected_year,
          param_term  = input$param_term,
          case_type   = input$case_type
        )
      }
      
      # p may be a list - return the first plot object
      .first_plot(p)
    })
    
    output$plot_attribution_metric_gg <- renderPlot({ attm_gg() })
    
    # district only -> Plotly with scroll/zoom
    attm_plotly <- reactive({
      req(input$attm_plot_by, input$selected_attm_metric, r$attr_data_district())
      
      p <- plot_attribution_metric(
        attr_data   = r$attr_data_district(),
        level       = "district",
        metrics     = input$selected_attm_metric,
        filter_year = if ("All" %in% input$attm_selected_year) NULL
        else input$attm_selected_year,
        param_term  = input$param_term,
        case_type   = input$case_type
      )
      
      gg <- .first_plot(p)
      
      # dynamic width based on number of districts
      n_items    <- nrow(dplyr::distinct(r$attr_data_district(), district))
      plot_width <- max(600, n_items * 30)
      
      gg %>%
        plotly::layout(
          width  = plot_width,
          height = 500,
          margin = list(b = 150),
          xaxis  = list(
            tickangle  = -90,
            tickfont   = list(size = 8),
            automargin = TRUE
          ),
          legend = list(orientation = "h", x = 0, y = 1.1)
        ) %>%
        plotly::config(
          scrollZoom          = TRUE,
          displayModeBar      = TRUE,
          modeBarButtonsToAdd = list("scrollZoom")
        )
    })
    
    output$plot_attribution_metric_plotly <- renderPlotly({ attm_plotly() })
    
    # single entry point for the download buttons
    attm_plot_obj <- reactive({
      req(input$attm_plot_by)
      if (input$attm_plot_by == "district") attm_plotly() else attm_gg()
    })
    
    attm_data <- reactive({
      req(input$attm_plot_by)
      
      d <- switch(input$attm_plot_by,
                  "country"         = r$attr_data_country(),
                  "Region/Province" = r$attr_data_region(),
                  r$attr_data_district())
      req(is.data.frame(d))
      
      if (input$attm_plot_by %in% c("Region/Province", "district") &&
          !is.null(input$attm_selected_year) &&
          !("All" %in% input$attm_selected_year)) {
        d <- d %>% dplyr::filter(year %in% input$attm_selected_year)
      }
      d
    })
    
    register_plot_downloads("attm", attm_plot_obj, attm_data,
                            base_name = "attributable_number_rate")
    
    # -------------------------------------------------------------------------
    # Monthly average attributable number & rate
    # -------------------------------------------------------------------------
    output$attavm_disaggregate <- renderUI({
      radioButtons(ns("attavm_plot_by"), "Plot by", choices = c("country", "Region/Province", "district"), inline = TRUE,
                   selected = "country")
    })
    
    output$attavm_select_dissaggregate <- renderUI({
      req(input$attavm_plot_by)
      
      if (input$attavm_plot_by == "Region/Province") {
        req(combined_data())
        selectInput(ns("attavm_selected_item"), "Select provinces",
                    choices = combined_data() %>% dplyr::select(region) %>% unique() %>% dplyr::pull())
      } else if (input$attavm_plot_by == "district") {
        req(combined_data())
        selectInput(ns("attavm_selected_item"), "Select districts",
                    choices = combined_data() %>% dplyr::select(district) %>% unique() %>% dplyr::pull())
      }
    })
    
    output$attavm_metric <- renderUI({
      selectInput(ns("selected_attavm_metric"), "Select the metric", choices = c("AR_Number", "AR_per_100k"))
    })
    
    output$attavm_select_year <- renderUI({
      req(input$attavm_plot_by, combined_data())
      
      if (input$attavm_plot_by %in% c("Region/Province", "district")) {
        req(combined_data())
        selectInput(ns("attavm_selected_year"), "Year (s)",
                    choices = c('All', combined_data() %>% dplyr::select(year) %>% unique() %>% dplyr::pull()),
                    selected = 'All',
                    multiple = FALSE)
      }
    })
    
    attavm_plot_obj <- reactive({
      req(combined_data(), input$selected_attavm_metric, input$attavm_plot_by)
      
      p <- if (!input$attavm_plot_by %in% c("Region/Province", "district")) {
        req(r$attr_data_monthly_country())
        plot_avg_monthly(
          attr_data   = r$attr_data_monthly_country(),
          data        = combined_data(),
          level       = "country",
          metrics     = input$selected_attavm_metric,
          case_type   = input$case_type,
          filter_year = NULL,
          param_term  = input$param_term
        )
      } else if (input$attavm_plot_by == "Region/Province") {
        req(r$attr_data_monthly_region(), input$attavm_selected_item, input$attavm_selected_year)
        plot_avg_monthly(
          attr_data   = r$attr_data_monthly_region(),
          data        = combined_data() %>% dplyr::filter(region %in% input$attavm_selected_item),
          level       = "region",
          metrics     = input$selected_attavm_metric,
          case_type   = input$case_type,
          filter_year = if (input$attavm_selected_year == "All") NULL else input$attavm_selected_year,
          param_term  = input$param_term
        )
      } else {
        req(r$attr_data_monthly_district(), input$attavm_selected_item, input$attavm_selected_year)
        plot_avg_monthly(
          attr_data   = r$attr_data_monthly_district(),
          data        = combined_data() %>% dplyr::filter(district %in% input$attavm_selected_item),
          level       = "district",
          metrics     = input$selected_attavm_metric,
          case_type   = input$case_type,
          filter_year = if (input$attavm_selected_year == "All") NULL else input$attavm_selected_year,
          param_term  = input$param_term
        )
      }
      
      .first_plot(p)
    })
    
    output$plot_avg_monthly <- renderPlot({ attavm_plot_obj() })
    
    attavm_data <- reactive({
      req(input$attavm_plot_by)
      
      d <- if (!input$attavm_plot_by %in% c("Region/Province", "district")) {
        r$attr_data_monthly_country()
      } else if (input$attavm_plot_by == "Region/Province") {
        r$attr_data_monthly_region()
      } else {
        r$attr_data_monthly_district()
      }
      req(is.data.frame(d))
      
      if (input$attavm_plot_by %in% c("Region/Province", "district") &&
          !is.null(input$attavm_selected_year) &&
          input$attavm_selected_year != "All") {
        d <- d %>% dplyr::filter(year %in% input$attavm_selected_year)
      }
      d
    })
    
    register_plot_downloads("attavm", attavm_plot_obj, attavm_data,
                            base_name = "monthly_avg_attributable")
    
    # -------------------------------------------------------------------------
    # Help modals
    # -------------------------------------------------------------------------
    observeEvent(input$show_help, {
      
      showModal(
        modalDialog(
          title = "Exposure-Lag Response Plot",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          
          tagList(
            
            tags$h4("How to interpret this figure"),
            
            tags$ul(
              tags$li("The figure presents the relative risk (RR) of disease incidence as a function of temperature (\u00B0C) or rainfall (mm) and lag (months)."),
              tags$li(tags$b("Lag (x-axis): "), "Delay between temperature/rainfall exposure and its effect on disease incidence."),
              tags$li(tags$b("Temperature/rainfall (y-axis): "), "Range of temperature/rainfall included in the analysis."),
              tags$li(tags$b("Color scale: "),
                      tags$ul(
                        tags$li(tags$span("RR > 1", style = "color:purple; font-weight:bold;"), " \u2192 Increased risk"),
                        tags$li("RR \u2248 1 \u2192 No change"),
                        tags$li(tags$span("RR < 1", style = "color:green; font-weight:bold;"), " \u2192 Reduced risk")
                      )
              ),
              tags$li("Contours represent similar relative risk levels.")
            ),
            
            tags$h4("How to Read the Plot"),
            
            tags$ul(
              tags$li("Fix a temperature/rainfall \u2192 move horizontally across lag."),
              tags$li("Fix a lag \u2192 move vertically across temperature/rainfall."),
              tags$li("Look for strong color deviations."),
              tags$li("Interpret as a combined temperature/rainfall-lag response.")
            ),
            
            tags$h4("Headline result templates"),
            
            tags$div(
              style = "background:#f8f9fa; padding:10px; border-left:4px solid #007bff; border-radius:6px;",
              
              tags$p("\u201CTemperature/rainfall between xx-xx \u00B0C/mm were associated with higher disease incidence risk, with harmful risk observed between xx-xx \u00B0C/mm and protective risk below xx \u00B0C/mm and above xx \u00B0C/mm, with x months lag during yy-yy.\u201D"),
              
              tags$p("\u201CRelative risk varied across the temperature/rainfall range, with peak risk at approximately xx \u00B0C/mm, delayed by x months, and reduced risk around xx \u00B0C/mm during yy-yy.\u201D")
            )
          )
        )
      )
    })
    
    observeEvent(input$show_help_map, {
      showModal(
        modalDialog(
          title = "Map Interpretation Guide",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          
          div(
            style = "max-height:70vh; overflow-y:auto;",
            tagList(
              
              tags$h4("How to interpret this figure"),
              
              tags$p(
                "The map shows district-level ",
                tags$b("relative risk (RR)"),
                " of malaria associated with ",
                tags$b("temperature (\u00B0C)/rainfall (mm).")
              ),
              
              tags$p("Each district is colored based on its RR value:"),
              
              tags$ul(
                tags$li(tags$span("RR > 1", style = "color:#e67e22; font-weight:bold;"),
                        " \u2192 Higher risk (orange shades)"),
                tags$li(tags$span("RR \u2248 1", style = "color:#999999; font-weight:bold;"),
                        " \u2192 No substantial change (neutral shades)"),
                tags$li(tags$span("RR < 1", style = "color:#2e86c1; font-weight:bold;"),
                        " \u2192 Lower risk (blue shades)")
              ),
              
              tags$h4("How to read the map"),
              
              tags$ul(
                tags$li("Match each district's color to the legend to determine its RR."),
                tags$li("Compare colors across districts to assess spatial variation."),
                tags$li("Look for clusters of similar colors indicating geographic patterns."),
                tags$li("Focus on relative differences across districts rather than absolute values.")
              ),
              
              tags$h4("Headline result templates"),
              
              tags$div(
                style = "background:#f8f9fa; padding:12px; border-left:4px solid #007bff; border-radius:6px;",
                
                tags$p(
                  "\u201CDistrict-level relative risk of disease associated with temperature/rainfall ranged from ",
                  tags$i("xx to xx"),
                  ", with higher/lower risks observed in ",
                  tags$i("[region/districts]"),
                  " compared to the reference level during ",
                  tags$i("yy-yy"),
                  ".\u201D"
                ),
                
                tags$p(
                  "\u201CThe highest temperature/rainfall-related disease risk was observed in ",
                  tags$i("[region/district]"),
                  ", where the relative risk was approximately ",
                  tags$i("xx"),
                  ", compared to ",
                  tags$i("xx"),
                  " in ",
                  tags$i("[reference region/district]"),
                  ".\u201D"
                )
              )
            )
          )
        )
      )
    })
    
    observeEvent(input$show_help_rrplot, {
      showModal(
        modalDialog(
          title = "How to interpret relative risk plot",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          
          div(
            style = "max-height:70vh; overflow-y:auto;",
            tagList(
              
              tags$h4("How to interpret this figure"),
              
              tags$p(
                "The top panel shows the ",
                tags$b("relative risk (RR)"),
                " of diarrhea across ",
                tags$b("temperature (tmax)/rainfall (mm).")
              ),
              
              tags$ul(
                tags$li(tags$span("Red line", style = "color:red; font-weight:bold;"),
                        " \u2192 Estimated relative risk (RR)"),
                tags$li("Shaded area \u2192 Uncertainty around the estimate (e.g., confidence interval)"),
                tags$li("Horizontal dashed line (RR = 1) \u2192 Reference level (no effect)")
              ),
              
              tags$p(
                "The bottom panel shows the ",
                tags$b("distribution of temperature values"),
                " (histogram), indicating how frequently each temperature/rainfall occurs."
              ),
              
              tags$h4("How to Read the Graph"),
              
              tags$ul(
                tags$li(
                  "Compare the red curve to the reference line (RR = 1):",
                  tags$ul(
                    tags$li(tags$span("RR > 1", style = "color:#c0392b; font-weight:bold;"), " \u2192 Higher risk"),
                    tags$li(tags$span("RR < 1", style = "color:#2e86c1; font-weight:bold;"), " \u2192 Lower risk")
                  )
                ),
                tags$li("Identify temperature/rainfall ranges where the curve deviates most from 1."),
                tags$li("Use the shaded band to assess uncertainty around the estimate."),
                tags$li("Refer to the histogram to identify commonly observed temperatures/rainfall.")
              ),
              
              tags$h4("What to focus on"),
              
              tags$ul(
                tags$li("How relative risk changes across the temperature/rainfall range"),
                tags$li("Where the strongest associations occur"),
                tags$li("Whether these occur in frequently observed temperature/rainfall ranges (from the histogram)")
              ),
              
              tags$h4("Headline result templates"),
              
              tags$div(
                style = "background:#f8f9fa; padding:12px; border-left:4px solid #007bff; border-radius:6px;",
                
                tags$p(
                  "\u201CRelative risk of diarrhea varied across temperatures/rainfall values, with peak risk observed at approximately ",
                  tags$i("xx"),
                  ", and lower risk observed at temperatures/rainfall below ",
                  tags$i("xx"),
                  " and above ",
                  tags$i("xx"),
                  ", during ",
                  tags$i("yy-yy"),
                  ".\u201D"
                ),
                
                tags$p(
                  "\u201CThe highest temperature/rainfall-related disease risk occurred at around ",
                  tags$i("xx"),
                  ", where relative risk was approximately ",
                  tags$i("xx"),
                  ", compared to lower risks of about ",
                  tags$i("xx"),
                  " at higher/lower temperature/rainfall ranges.\u201D"
                )
              )
            )
          )
        )
      )
    })
    
    observeEvent(input$show_help_attr_plot, {
      showModal(
        modalDialog(
          title = "How to interpret attributable number & rate plot",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          
          div(
            style = "max-height:70vh; overflow-y:auto;",
            tagList(
              
              tags$h4("How to interpret this figure"),
              
              tags$p(
                "The chart shows the ",
                tags$b("attributable rate/number"),
                " of malaria cases due to extreme temperature/rainfall, grouped by ",
                tags$b("region"),
                " and ",
                tags$b("year"),
                "."
              ),
              
              tags$ul(
                tags$li(tags$b("x-axis: "), "Regions"),
                tags$li(tags$b("y-axis: "), "Attributable rate"),
                tags$li("Each region has two bars representing different years")
              ),
              
              tags$h4("How to read the chart"),
              
              tags$ul(
                tags$li("Compare bars within the same region to assess changes over time."),
                tags$li("Compare bars across regions to assess geographic variation.")
              ),
              
              tags$h4("How to interpret"),
              
              tags$ul(
                tags$li("Look at the height of each bar to determine the magnitude of the attributable rate."),
                tags$li("Identify which regions have higher or lower values relative to others."),
                tags$li("Examine differences between years to understand temporal changes.")
              ),
              
              tags$h4("What to focus on"),
              
              tags$ul(
                tags$li("Relative differences across regions"),
                tags$li("Changes between years within regions"),
                tags$li("Overall distribution of attributable rates across all regions.")
              )
            )
          )
        )
      )
    })
    
  })
}