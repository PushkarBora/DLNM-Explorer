# ============================================================
#  DLNM Explorer
#  Distributed Lag Non-Linear Modelling of Exogenous Shocks
#  ICAR-Indian Agricultural Statistics Research Institute, New Delhi
# ------------------------------------------------------------
#  Run locally :  shiny::runApp("path/to/DLNM_App")
#  Packages    :  see install_packages.R
#  Engine      :  R/analysis.R   |   Plots : R/plots.R
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(plotly)
  library(shinycssloaders)
  library(dlnm)
  library(mgcv)
})
# R/analysis.R and R/plots.R are sourced automatically by Shiny.

options(shiny.maxRequestSize = 50 * 1024^2, spinner.color = "#0b5d7a", spinner.type = 6)

APP_TITLE   <- "DLNM Explorer"
APP_TAGLINE <- "Distributed Lag Non-Linear Modelling of Exogenous Shocks on Time Series"
IASRI_URL   <- "https://www.iasri.res.in/"
COUNTER_FILE <- "visitor_count.txt"

# ------------------------------------------------------------
# Visitor counter (file based; one count per new browser session)
# ------------------------------------------------------------
increment_counter <- function() {
  n <- suppressWarnings(tryCatch(as.numeric(readLines(COUNTER_FILE, warn = FALSE)[1]), error = function(e) 0))
  if (length(n) == 0 || is.na(n)) n <- 0
  n <- n + 1
  tryCatch(writeLines(as.character(n), COUNTER_FILE), error = function(e) NULL)
  n
}

# DataTables styling: Bootstrap 5 where the installed DT supports it, otherwise Bootstrap 4
DT_STYLE <- local({
  st <- tryCatch(eval(formals(DT::datatable)$style), error = function(e) character())
  if ("bootstrap5" %in% st) "bootstrap5" else "bootstrap4"
})

spin <- function(x) shinycssloaders::withSpinner(x, type = 6, color = "#0b5d7a")

stat_box <- function(label, value, icon_name, cls = "") {
  div(class = paste("stat", cls),
      div(class = "si", icon(icon_name)),
      div(div(class = "sv", value), div(class = "sl", label)))
}

help_txt <- function(...) div(class = "help", ...)

# ============================================================
#  UI
# ============================================================

banner <- div(
  class = "top-banner",
  tags$a(href = IASRI_URL, target = "_blank", title = "Visit ICAR-IASRI website",
         tags$img(src = "img/iasri_logo.png", alt = "ICAR-Indian Agricultural Statistics Research Institute"))
)

footer <- tags$footer(
  class = "site-footer",
  div(class = "fi",
      div(
        div(class = "copy", HTML("&copy; 2026 "),
            tags$a(href = IASRI_URL, target = "_blank", "ICAR-Indian Agricultural Statistics Research Institute"),
            ", New Delhi. All rights reserved."),
        div(class = "addr", "Library Avenue, PUSA, New Delhi - 110012, India")
      ),
      div(class = "visitor", icon("users"),
          div(div(class = "vl", "Visitors"), div(class = "vn", textOutput("visitor_count", inline = TRUE))))
  )
)

# ---------------- HOME ----------------
home_tab <- tabPanel(
  "Home", value = "home", icon = icon("house"),
  div(class = "hero",
      div(class = "hero-inner",
          div(
            span(class = "pill", icon("seedling"), " ICAR-IASRI | Statistical Web Tool"),
            h1(APP_TITLE, tags$br(), span(class = "accent", style = "font-size:1.55rem;", APP_TAGLINE)),
            p(class = "lead",
              "A user-friendly platform to quantify how an external shock - such as geopolitical risk, ",
              "weather or climate anomalies, or policy and price shocks - affects a time series of interest ",
              "(trade, prices, production, arrivals) through ", tags$b("non-linear"), " and ",
              tags$b("delayed (lagged)"), " pathways."),
            div(class = "hero-actions",
                actionButton("go_method", label = tagList(icon("play"), " Start Analysis"), class = "btn-gold"),
                actionButton("go_instr", label = tagList(icon("book-open"), " How to use"), class = "btn-ghost"))
          ),
          div(class = "hero-card",
              h5(icon("diagram-project"), " What the tool does"),
              div(class = "mini-step", div(class = "num", "1"), div("Upload a Y file (response series) and an X file (shock/exposure series)")),
              div(class = "mini-step", div(class = "num", "2"), div("Explore descriptive statistics, distributions and correlations")),
              div(class = "mini-step", div(class = "num", "3"), div("Test for non-linearity using the BDS test on ARIMAX residuals")),
              div(class = "mini-step", div(class = "num", "4"), div("Fit penalised DLNMs and estimate cumulative and lag-specific effects")),
              div(class = "mini-step", div(class = "num", "5"), div("Visualise the effects and download all tables and figures"))
          )
      )
  ),
  div(class = "page-wrap",
      div(class = "eyebrow", "About the method"),
      h2(class = "section-title", "Why Distributed Lag Non-Linear Models?"),
      p(class = "section-sub",
        "Shocks rarely act instantly or linearly. A sudden rise in geopolitical risk or a climate anomaly may influence ",
        "trade or prices only after several months, and a large shock may matter far more than a small one. ",
        "The DLNM framework captures both features at once."),
      div(class = "feature-grid",
          div(class = "feature", div(class = "ico", icon("chart-column")),
              h4("Descriptive statistics"),
              p("Mean, median, dispersion, skewness, kurtosis, Jarque-Bera and Shapiro-Wilk normality tests and ADF stationarity test for every series.")),
          div(class = "feature g", div(class = "ico", icon("wave-square")),
              h4("BDS non-linearity test"),
              p("An ARIMAX model is fitted for each response-exposure pair and the BDS test checks the residuals for remaining non-linear dependence.")),
          div(class = "feature o", div(class = "ico", icon("cubes")),
              h4("Penalised DLNM"),
              p("A cross-basis of exposure and lag is estimated within a Gaussian GAM (mgcv) with penalised splines and REML smoothing selection.")),
          div(class = "feature", div(class = "ico", icon("layer-group")),
              h4("Cumulative effects"),
              p("Overall effect summed across all lags at chosen exposure levels (percentiles or user values), relative to a reference level.")),
          div(class = "feature g", div(class = "ico", icon("clock-rotate-left")),
              h4("Lag-specific effects"),
              p("Effects at selected lags with confidence intervals, Z statistics, p-values and significance stars, plus 3D and contour surfaces.")),
          div(class = "feature o", div(class = "ico", icon("chart-area")),
              h4("Interactive plots and downloads"),
              p("3D exposure-lag-response surface, contour map, lag-response and exposure-response curves, with Excel tables and publication-quality figures."))
      ),
      br(),
      div(class = "panel",
          withMathJax(),
          h3(icon("square-root-variable"), " Model in brief"),
          p("For a response series \\(Y_t\\) and an exposure series \\(x_t\\), the DLNM is"),
          div(class = "equation",
              "$$Y_t = \\alpha + \\sum_{j=1}^{v_x}\\sum_{k=1}^{v_\\ell} \\eta_{jk}\\, w_{t,jk} + f(t) + s(\\text{season}_t) + \\varepsilon_t,\\qquad
                 w_{t,jk} = \\sum_{\\ell=0}^{L} b_j(x_{t-\\ell})\\, c_k(\\ell)$$"),
          p("where \\(b_j(\\cdot)\\) are basis functions for the exposure dimension, \\(c_k(\\cdot)\\) basis functions for the lag dimension, ",
            "\\(L\\) is the maximum lag, \\(f(t)\\) an optional trend and \\(s(\\cdot)\\) an optional seasonal term. ",
            "Penalties on \\(\\eta\\) are selected by REML. The effect of exposure level \\(x\\) at lag \\(\\ell\\) relative to a reference \\(x_0\\) is ",
            "\\(\\beta_{x,\\ell}\\), and the cumulative effect is \\(\\beta_x = \\sum_{\\ell=0}^{L} \\beta_{x,\\ell}\\)."),
          h3(style = "margin-top:22px;", icon("list-check"), " Outputs you get"),
          tags$ul(
            tags$li("Descriptive statistics table and interactive time-series, distribution and correlation plots"),
            tags$li("BDS test table (embedding dimensions 2 to m, four epsilon values)"),
            tags$li("Cumulative effect table and selected lag-effect table with CI, Z, p-value and significance"),
            tags$li("Interactive 3D surface, contour, lag-response, exposure-response and cumulative effect plots"),
            tags$li("One-click Excel workbook of all tables and a ZIP of publication-quality (300/600 dpi) figures")
          )
      ),
      div(class = "panel",
          h3(icon("book"), " Key references"),
          tags$ol(class = "ref-list",
                  tags$li("Gasparrini, A., Armstrong, B. and Kenward, M.G. (2010). Distributed lag non-linear models. Statistics in Medicine, 29(21), 2224-2234."),
                  tags$li("Gasparrini, A., Scheipl, F., Armstrong, B. and Kenward, M.G. (2017). A penalized framework for distributed lag non-linear models. Biometrics, 73(3), 938-948."),
                  tags$li("Brock, W.A., Dechert, W.D., Scheinkman, J.A. and LeBaron, B. (1996). A test for independence based on the correlation dimension. Econometric Reviews, 15(3), 197-235."),
                  tags$li("Wood, S.N. (2017). Generalized Additive Models: An Introduction with R (2nd ed.). Chapman and Hall/CRC.")
          )
      )
  )
)

# ---------------- METHOD ----------------
upload_card <- function(id, role, cls, ttl, sub, icon_name) {
  card(class = cls,
       card_header(icon(icon_name), ttl, span(class = "sub", sub)),
       card_body(
         div(class = "upload-zone",
             fileInput(paste0(id, "_file"), NULL, accept = c(".csv", ".xlsx", ".xls", ".txt", ".tsv"),
                       buttonLabel = tagList(icon("upload"), " Browse"),
                       placeholder = "CSV, XLSX or XLS file", width = "100%")),
         div(class = "sample-row", icon("file-arrow-down"), "Sample ", role, " file:",
             downloadButton(paste0("dl_sample_", id, "_csv"), "CSV", class = paste("btn-soft", if (id == "x") "btn-soft-g")),
             downloadButton(paste0("dl_sample_", id, "_xlsx"), "Excel", class = paste("btn-soft", if (id == "x") "btn-soft-g"))),
         uiOutput(paste0(id, "_cols_ui")),
         uiOutput(paste0(id, "_info")),
         DTOutput(paste0(id, "_preview"))
       ))
}

settings_panel <- div(
  accordion(
    id = "settings_acc", open = c("Variables and data", "DLNM structure"), multiple = TRUE,
    accordion_panel(
      "Variables and data", icon = icon("table-columns"),
      fluidRow(
        column(6, selectizeInput("y_vars", "Response (Y) variables to analyse", choices = NULL, multiple = TRUE,
                                 options = list(plugins = list("remove_button"), placeholder = "Upload data first")),
               help_txt("Each selected Y is analysed against each selected X.")),
        column(6, selectizeInput("x_vars", "Exposure (X) variables to analyse", choices = NULL, multiple = TRUE,
                                 options = list(plugins = list("remove_button"), placeholder = "Upload data first")))
      ),
      fluidRow(
        column(4, selectInput("y_transform", "Transformation of Y", transform_choices, "none")),
        column(4, selectInput("x_transform", "Transformation of X", transform_choices, "none")),
        column(4, selectInput("freq", "Seasonal period (observations per cycle)",
                              c("Auto-detect" = "auto", "None (1)" = "1", "2 (half-yearly data)" = "2",
                                "4 (quarterly data)" = "4", "7 (daily data, weekly cycle)" = "7",
                                "12 (monthly data)" = "12", "24 (hourly data / fortnightly data)" = "24",
                                "52 (weekly data)" = "52", "60 (minute data, hourly cycle)" = "60",
                                "365 (daily data, yearly cycle)" = "365"), "auto"),
               uiOutput("freq_help"))
      ),
      fluidRow(
        column(6, textInput("effect_label", "Effect axis label (optional)", "", placeholder = "Default: Effect on <Y variable>")),
        column(6, textInput("lag_label", "Lag axis label (optional)", "", placeholder = "Default: Lag (<time unit>)"))
      )
    ),
    accordion_panel(
      "DLNM structure", icon = icon("cubes"),
      fluidRow(
        column(3, numericInput("max_lag", "Maximum lag (L)", 12, min = 1, max = 120, step = 1),
               help_txt("Number of past periods over which the shock can act.")),
        column(3, selectInput("exposure_basis", "Exposure basis", c("P-spline (ps)" = "ps", "Cubic regression spline (cr)" = "cr",
                                                                    "Natural spline (ns)" = "ns", "B-spline (bs)" = "bs",
                                                                    "Polynomial (poly)" = "poly", "Linear (lin)" = "lin"), "ps")),
        column(3, numericInput("exposure_df", "Exposure df", 6, min = 2, max = 20)),
        column(3, numericInput("spline_degree", "Spline / polynomial degree", 3, min = 1, max = 5))
      ),
      fluidRow(
        column(3, selectInput("lag_basis", "Lag basis", c("P-spline (ps)" = "ps", "Cubic regression spline (cr)" = "cr",
                                                          "Natural spline (ns)" = "ns", "B-spline (bs)" = "bs",
                                                          "Polynomial (poly)" = "poly", "Linear (lin)" = "lin"), "ps")),
        column(3, numericInput("lag_df", "Lag df", 6, min = 2, max = 20)),
        column(3, selectInput("trend", "Time trend", c("None" = "none", "Linear" = "linear", "Quadratic" = "quadratic",
                                                       "Smooth (thin-plate spline)" = "smooth"), "linear")),
        column(3, conditionalPanel("input.trend == 'smooth'",
                                   numericInput("trend_df_per_year", "Trend df per seasonal cycle", 4, min = 1, max = 12)))
      ),
      fluidRow(
        column(4, selectInput("seasonality", "Seasonality control", c("None" = "none", "Season dummies (factor)" = "factor",
                                                                      "Cyclic smooth spline" = "cyclic"), "none"),
               help_txt("Season = month, quarter, week, weekday or hour depending on the data; for plain time labels it is the position within the cycle set above.")),
        column(4, selectInput("gam_method", "Smoothing-parameter estimation", c("REML" = "REML", "ML" = "ML", "GCV" = "GCV.Cp"), "REML")),
        column(4, div(class = "note", style = "margin-top:0;", icon("circle-info"),
                      " P-spline and cr bases are penalised (penalised DLNM). ns, bs, poly and lin are fitted unpenalised."))
      )
    ),
    accordion_panel(
      "Prediction and effects", icon = icon("bullseye"),
      fluidRow(
        column(4, selectInput("reference", "Reference (centering) exposure", c("Median" = "median", "Mean" = "mean", "Zero" = "zero",
                                                                               "Quantile" = "quantile", "Custom value" = "custom"), "median"),
               conditionalPanel("input.reference == 'quantile'", sliderInput("reference_quantile", "Reference quantile", 0, 1, 0.5, 0.01)),
               conditionalPanel("input.reference == 'custom'", numericInput("reference_custom", "Reference value", 0))),
        column(4, selectInput("prediction_method", "Exposure levels for effects", c("Percentiles" = "percentiles",
                                                                                    "Evenly spaced grid" = "sequence",
                                                                                    "Custom values" = "custom"), "percentiles"),
               conditionalPanel("input.prediction_method == 'percentiles'",
                                textInput("percentiles", "Percentiles (comma separated)", "5, 25, 50, 75, 95")),
               conditionalPanel("input.prediction_method == 'sequence'",
                                numericInput("grid_points", "Number of grid points", 20, min = 3, max = 200)),
               conditionalPanel("input.prediction_method == 'custom'",
                                textInput("custom_values", "Custom exposure values (comma separated)", "", placeholder = "e.g. 80, 100, 120"))),
        column(4, textInput("selected_lags", "Selected lags for lag-specific effects", "1, 3, 6, 9, 12"),
               help_txt("Comma separated; values above the maximum lag are ignored."),
               sliderInput("confidence_level", "Confidence level", 0.80, 0.99, 0.95, 0.01))
      )
    ),
    accordion_panel(
      "BDS non-linearity test", icon = icon("wave-square"),
      fluidRow(
        column(4, numericInput("bds_m", "Maximum embedding dimension (m)", 5, min = 2, max = 10),
               help_txt("The BDS test is reported for embedding dimensions 2 to m on ARIMAX residuals.")),
        column(8, div(class = "note", style = "margin-top:0;", icon("circle-info"),
                      " For each response-exposure pair an ARIMAX model is fitted automatically and its residuals are tested for remaining non-linear dependence at epsilon = 0.5, 1, 1.5 and 2 times the residual standard deviation."))
      )
    )
  ),
  div(class = "run-bar",
      div(h4(style = "margin:0;", icon("gears"), " Ready to estimate"), uiOutput("run_summary")),
      div(actionButton("reset_defaults", tagList(icon("rotate-left"), " Reset defaults"), class = "btn-soft", style = "margin-right:10px;"),
          actionButton("run", tagList(icon("play"), " Run Analysis"), class = "btn-run"))
  )
)

results_panel <- div(
  uiOutput("results_header"),
  conditionalPanel(
    "output.has_results",
    navset_card_tab(
      id = "res_tabs",
      nav_panel("BDS test", icon = icon("wave-square"),
                div(class = "note", icon("circle-info"),
                    " BDS test on ARIMAX residuals. A small p-value rejects the i.i.d. hypothesis, indicating remaining non-linear structure that supports the DLNM approach. ",
                    "Epsilon is expressed as a multiple of the residual standard deviation. Stars: *** p<0.01, ** p<0.05, * p<0.10."),
                spin(DTOutput("tbl_bds"))),
      nav_panel("Cumulative effects", icon = icon("layer-group"),
                div(class = "note", icon("circle-info"),
                    " Overall effect of the exposure level summed over lags 0 to L, relative to the reference level. ",
                    "Stars: *** p<0.01, ** p<0.05, * p<0.10. Rows are shaded green when significant."),
                spin(DTOutput("tbl_cum"))),
      nav_panel("Lag-specific effects", icon = icon("clock-rotate-left"),
                div(class = "note", icon("circle-info"), " Effect at each selected lag for each exposure level, relative to the reference level."),
                spin(DTOutput("tbl_lag"))),
      nav_panel("Plots", icon = icon("chart-area"),
                fluidRow(
                  column(4, selectInput("plot_y", "Response (Y)", NULL, selectize = FALSE)),
                  column(4, selectInput("plot_x", "Exposure (X)", NULL, selectize = FALSE)),
                  column(4, div(style = "margin-top:30px;", class = "muted", icon("hand-pointer"),
                                " Hover, zoom, rotate; camera icon saves PNG."))
                ),
                uiOutput("interpretation"),
                navset_tab(
                  nav_panel("3D surface", spin(plotlyOutput("pl_surface", height = "620px"))),
                  nav_panel("Contour", spin(plotlyOutput("pl_contour", height = "560px"))),
                  nav_panel("Lag-response", spin(plotlyOutput("pl_lagslice", height = "520px"))),
                  nav_panel("Exposure-response by lag", spin(plotlyOutput("pl_expresp", height = "520px"))),
                  nav_panel("Cumulative effect", spin(plotlyOutput("pl_cum", height = "520px")))
                )),
      nav_panel("Error log", icon = icon("triangle-exclamation"), DTOutput("tbl_err"))
    )
  )
)

downloads_panel <- div(
  conditionalPanel("!output.has_results",
                   div(class = "note warn", icon("hourglass-half"), " Run the analysis first; downloads will appear here.")),
  conditionalPanel(
    "output.has_results",
    fluidRow(
      column(6, card(card_header(icon("file-excel"), "Excel workbook", span(class = "sub", "All result tables, data used, descriptive statistics and settings")),
                     card_body(p("Sheets: Data_Used, Descriptive_Stats, BDS_Test, Cumulative_Effects, Selected_Lag_Effects, Error_Log, Settings."),
                               downloadButton("dl_excel", "Download Excel workbook", class = "btn-dl")))),
      column(6, card(card_header(icon("images"), "Publication figures", span(class = "sub", "PNG figures for every response-exposure pair, in a ZIP archive")),
                     card_body(p("3D surface, contour, lag-response, exposure-response by lag and cumulative effect."),
                               radioButtons("fig_dpi", "Resolution", c("300 dpi" = 300, "600 dpi (slower)" = 600), 300, inline = TRUE),
                               downloadButton("dl_figs", "Download figures (ZIP)", class = "btn-dl"))))
    )
  )
)

method_tab <- tabPanel(
  "Method", value = "method", icon = icon("flask"),
  div(class = "page-wrap",
      div(class = "eyebrow", "Analysis workspace"),
      h2(class = "section-title", "Run the DLNM analysis"),
      p(class = "section-sub", "Follow the steps from left to right. Every step uses the column names of your own files - nothing is fixed in advance."),
      div(class = "method-tabs",
          tabsetPanel(
            id = "method_tabs", type = "pills",
            tabPanel("1. Upload data", value = "upload", icon = icon("upload"),
                     div(class = "note ok", style = "display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:10px;",
                         span(icon("lightbulb"), " New here? Load the built-in sample (monthly trade values of five commodities and two geopolitical-risk indices) to try the tool."),
                         actionButton("load_sample", tagList(icon("database"), " Load sample data"), class = "btn-primary-x")),
                     fluidRow(
                       column(6, upload_card("y", "Y", "card-y", "Upload Y file",
                                             "Time column (any format) + one or more dependent (response) variable columns", "bullseye")),
                       column(6, upload_card("x", "X", "card-x", "Upload X file",
                                             "Time column (any format) + N explanatory (exposure / shock) variable columns", "bolt"))
                     ),
                     card(card_header(icon("sliders"), "Data options"),
                          card_body(fluidRow(
                            column(4, selectInput("date_format", "Time format", time_format_choices, "auto"),
                                   help_txt("Auto-detect reads dates, months, quarters, weeks, years, financial years, date-times and any labels (t1, Period 1, ...).")),
                            column(4, selectInput("missing", "Missing values", c("Remove incomplete rows" = "omit",
                                                                                 "Linear interpolation" = "interpolate"), "omit")),
                            column(4, div(style = "margin-top:28px;", actionButton("to_desc", tagList("Next: Descriptive statistics ", icon("arrow-right")), class = "btn-primary-x")))
                          ))),
                     uiOutput("merge_status"),
                     conditionalPanel("output.has_data",
                                      card(card_header(icon("table"), "Merged analysis dataset", span(class = "sub", "Y and X matched on the time column")),
                                           card_body(DTOutput("merged_preview"))))
            ),
            tabPanel("2. Descriptive statistics", value = "desc", icon = icon("chart-column"),
                     conditionalPanel("!output.has_data", div(class = "note warn", icon("hourglass-half"), " Upload both files (or load the sample) to see descriptive statistics.")),
                     conditionalPanel("output.has_data",
                                      card(card_header(icon("table-list"), "Descriptive statistics",
                                                       span(class = "sub", "JB = Jarque-Bera, SW = Shapiro-Wilk (normality); ADF = Augmented Dickey-Fuller (H0: unit root)")),
                                           card_body(
                                             radioButtons("desc_data", NULL, c("Original data" = "orig", "Transformed data (as set in Model settings)" = "trans"),
                                                          "orig", inline = TRUE),
                                             spin(DTOutput("desc_table")))),
                                      card(card_header(icon("chart-line"), "Visual exploration"),
                                           card_body(
                                             navset_tab(
                                               nav_panel("Time series",
                                                         fluidRow(column(8, selectizeInput("ts_vars", NULL, NULL, multiple = TRUE, width = "100%",
                                                                                           options = list(plugins = list("remove_button")))),
                                                                  column(4, checkboxInput("ts_std", "Standardise (z-scores) to compare scales", FALSE))),
                                                         spin(plotlyOutput("pl_ts", height = "460px"))),
                                               nav_panel("Box plots", spin(plotlyOutput("pl_box", height = "460px"))),
                                               nav_panel("Histogram", selectInput("hist_var", NULL, NULL),
                                                         spin(plotlyOutput("pl_hist", height = "420px"))),
                                               nav_panel("Correlation", spin(plotlyOutput("pl_corr", height = "520px")))
                                             ))),
                                      div(style = "text-align:right;", actionButton("to_settings", tagList("Next: Model settings ", icon("arrow-right")), class = "btn-primary-x")))
            ),
            tabPanel("3. Model settings", value = "settings", icon = icon("sliders"),
                     conditionalPanel("!output.has_data", div(class = "note warn", icon("hourglass-half"), " Upload data first.")),
                     settings_panel),
            tabPanel("4. Results", value = "results", icon = icon("square-poll-vertical"), results_panel),
            tabPanel("5. Downloads", value = "downloads", icon = icon("download"), downloads_panel)
          ))
  )
)

# ---------------- INSTRUCTIONS ----------------
ins_step <- function(n, title, ...) div(class = "ins-step", div(class = "n", n), div(h4(title), ...))

instructions_tab <- tabPanel(
  "Instructions", value = "instructions", icon = icon("book-open"),
  div(class = "page-wrap",
      div(class = "eyebrow", "User guide"),
      h2(class = "section-title", "How to use the tool"),
      p(class = "section-sub", "Prepare two files, upload them, explore, set the model and run. The tool adapts to any number of variables and any regular time frequency."),

      ins_step("1", "Prepare your Y file (response / dependent variables)",
               tags$ul(
                 tags$li("One time column in any format and any position (usually the first column). It is read automatically and shown back exactly as you wrote it."),
                 tags$li("One or more numeric columns with the dependent variables, e.g. trade value, price, production, arrivals."),
                 tags$li("Column headers in the first row. Accepted formats: .csv, .xlsx, .xls (choose the sheet after upload).")),
               tags$table(class = "mini-table",
                          tags$tr(tags$th("Time"), tags$th("Y1"), tags$th("Y2"), tags$th("...")),
                          tags$tr(tags$td("2015-01-01"), tags$td("768.20"), tags$td("1266.65"), tags$td("...")),
                          tags$tr(tags$td("2015-02-01"), tags$td("928.81"), tags$td("1178.63"), tags$td("...")))),
      ins_step("2", "Prepare your X file (explanatory / exposure variables)",
               tags$ul(
                 tags$li("One time column written in the same way as in the Y file (same frequency)."),
                 tags$li("N numeric columns of explanatory variables (shocks), e.g. geopolitical risk index, rainfall or temperature anomaly, exchange rate."),
                 tags$li("Rows are matched with the Y file by time, so only common time points are used. Row order does not matter for calendar times.")),
               tags$table(class = "mini-table",
                          tags$tr(tags$th("Time"), tags$th("X1"), tags$th("X2"), tags$th("...")),
                          tags$tr(tags$td("2015-01-01"), tags$td("106.22"), tags$td("85.59"), tags$td("...")),
                          tags$tr(tags$td("2015-02-01"), tags$td("84.38"), tags$td("88.39"), tags$td("..."))),
               p(class = "muted", icon("download"), " Sample Y and X files (CSV and Excel) can be downloaded on the Method > Upload data page.")),
      ins_step("3", "Upload and check the data",
               tags$ul(
                 tags$li("Go to ", tags$b("Method > 1. Upload data"), ". Upload the Y file in the left box and the X file in the right box, or click ", tags$b("Load sample data"), "."),
                 tags$li("For Excel files, choose the worksheet. Confirm the ", tags$b("time column"), " for each file (detected automatically)."),
                 tags$li("The time column can be written in any way. Automatically recognised: dates (2015-01-31, 31/01/2015, 31.01.2015), months (Jan-2015, January 2015, 01/2015, 2015-01, 201501), ",
                         "quarters (2015Q1, Q1-2015, Quarter 1 2015), half-years (2015H1), weeks (2015-W05, Week 5 2015), years (2015), financial / agricultural years (2015-16, FY2015-16), ",
                         "date-times (2015-01-31 14:00, 31/01/2015 2:00 PM), clock times, Excel date serials, and plain time indices or labels (1, 2, 3 / t1, t2 / Period 1 / Kharif-2015)."),
                 tags$li("The page shows how the time column was read (e.g. 'Q1-2015' = 2015-01-01). If it is misread, choose the ", tags$b("Time format"), " manually, or 'Plain time index / labels' to keep the order of the file."),
                 tags$li("Choose how missing values are handled (remove rows or linear interpolation). Check the merged dataset summary: observations, period, frequency and variables."))),
      ins_step("4", "Explore descriptive statistics",
               tags$ul(
                 tags$li("The table reports N, mean, median, SD, min, quartiles, max, CV (%), skewness, kurtosis, Jarque-Bera and Shapiro-Wilk normality tests and the ADF unit-root test."),
                 tags$li("Use the plot tabs to view time series (optionally standardised), box plots, histograms and the correlation matrix. Tables can be copied or exported with the buttons above them."))),
      ins_step("5", "Set the model",
               p("All settings have sensible defaults. Main options:"),
               tags$table(class = "param-table",
                          tags$tr(tags$th("Setting"), tags$th("Meaning and guidance")),
                          tags$tr(tags$td("Y / X variables"), tags$td("Choose which responses and exposures to analyse. Every Y is paired with every X (one DLNM per pair).")),
                          tags$tr(tags$td("Transformation"), tags$td("Levels, log, log return [ln(Z_t) - ln(Z_t-1)], first difference, % change or standardisation. Differencing removes the first observation.")),
                          tags$tr(tags$td("Seasonal period"), tags$td("Auto-detected from the time column (12 for monthly, 4 for quarterly, 7 for daily, 24 for hourly ...); used for the seasonal term and trend flexibility. Set it manually for plain time labels.")),
                          tags$tr(tags$td("Maximum lag (L)"), tags$td("How many past periods the shock may influence Y (e.g. 12 months). Must be well below half the number of observations.")),
                          tags$tr(tags$td("Exposure / lag basis and df"), tags$td("Shape flexibility in the exposure and lag dimensions. P-splines (ps) with df = 6 give a penalised DLNM; lag df is automatically limited to L + 1.")),
                          tags$tr(tags$td("Trend and seasonality"), tags$td("Control for long-run trend (linear, quadratic or smooth) and seasonal pattern (dummies or cyclic spline).")),
                          tags$tr(tags$td("Reference exposure"), tags$td("Effects are expressed relative to this level (median by default); its effect is zero by construction.")),
                          tags$tr(tags$td("Exposure levels"), tags$td("Levels at which effects are reported: percentiles (5, 25, 50, 75, 95 by default), an even grid or your own values.")),
                          tags$tr(tags$td("Selected lags"), tags$td("Lags reported in the lag-specific table and exposure-response plot.")),
                          tags$tr(tags$td("BDS test"), tags$td("Maximum embedding dimension (m) for the BDS non-linearity test on ARIMAX residuals.")))),
      ins_step("6", "Run and interpret",
               tags$ul(
                 tags$li("Click ", tags$b("Run Analysis"), ". A progress bar shows each response-exposure pair. If a pair fails (e.g. lag too long), the reason is listed in the ", tags$b("Error log"), " and the others still run."),
                 tags$li(tags$b("Cumulative effects:"), " the total change in Y over lags 0 to L when X is at a given level instead of the reference. A positive value means an increase in Y."),
                 tags$li(tags$b("Lag-specific effects:"), " when the effect occurs. Look for lags where the confidence interval excludes zero."),
                 tags$li(tags$b("BDS test:"), " p < 0.05 indicates non-linear dependence left by a linear ARIMAX model, which supports the use of DLNM."),
                 tags$li(tags$b("Plots:"), " choose a pair to see the 3D exposure-lag-response surface, contour map, lag-response curves, exposure-response curves at selected lags and the cumulative curve."))),
      ins_step("7", "Download",
               tags$ul(
                 tags$li("Method > 5. Downloads: one Excel workbook with every table (plus data used and settings) and a ZIP of publication-quality PNG figures at 300 or 600 dpi."),
                 tags$li("Every table also has Copy / CSV / Excel buttons, and every interactive plot has a camera icon to save a PNG."))),
      div(class = "panel",
          h3(icon("circle-question"), " Troubleshooting"),
          tags$ul(
            tags$li(tags$b("'No common time points'"), " - the Y and X files cover different periods or their time columns are written differently. Write both the same way, or set the Time format manually."),
            tags$li(tags$b("'Maximum lag is too large'"), " - reduce the maximum lag or use a longer series."),
            tags$li(tags$b("Log transformation error"), " - log and log-return need strictly positive values; use first difference or none instead."),
            tags$li(tags$b("Analysis is slow"), " - reduce the number of response-exposure pairs or the maximum lag, or use fewer exposure levels."),
            tags$li(tags$b("Numbers with commas"), " (e.g. 1,234.5) are cleaned automatically; non-numeric columns are ignored with a message.")
          ))
  )
)

# ---------------- DEVELOPERS ----------------
dev_card <- function(img, name, role, email, scholar) {
  div(class = "dev",
      div(class = "band"),
      tags$img(src = img, alt = name),
      div(class = "body",
          h4(name),
          div(class = "role", role),
          div(class = "org", "ICAR-Indian Agricultural Statistics Research Institute, New Delhi"),
          div(class = "links",
              tags$a(class = "mail", href = paste0("mailto:", email), icon("envelope"), email),
              tags$a(class = "gs", href = scholar, target = "_blank", icon("graduation-cap"), "Google Scholar profile"))))
}

developers_tab <- tabPanel(
  "Developers", value = "developers", icon = icon("users"),
  div(class = "page-wrap",
      div(class = "eyebrow", "Developer team"),
      h2(class = "section-title", "Meet the team"),
      p(class = "section-sub", "This web application was developed at ICAR-Indian Agricultural Statistics Research Institute, New Delhi."),
      div(class = "dev-grid",
          dev_card("img/ranjit_kumar_paul.jpg", "Dr. Ranjit Kumar Paul", "ICAR National Fellow",
                   "ranjitstat@gmail.com", "https://scholar.google.com/citations?user=wBWuZJgAAAAJ&hl=en"),
          dev_card("img/md_yeasin.jpg", "Dr. Md Yeasin", "Scientist",
                   "yeasin.iasri@gmail.com", "https://scholar.google.com/citations?user=xejMKD0AAAAJ&hl=en"),
          dev_card("img/pushkar_bora.jpg", "Pushkar Bora", "Junior Research Fellow",
                   "borapushkar1999@gmail.com", "https://scholar.google.com/citations?user=xVIXGlwAAAAJ&hl=en")
      )
  )
)

ui <- tagList(
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&family=Merriweather:wght@700;900&display=swap"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(name = "description", content = paste(APP_TITLE, "-", APP_TAGLINE, "| ICAR-IASRI")),
    tags$link(rel = "icon", type = "image/png", href = "img/iasri_logo.png")
  ),
  banner,
  navbarPage(
    title = span(span(class = "brand-badge", "DLNM"), "Explorer"),
    windowTitle = paste(APP_TITLE, "| ICAR-IASRI"),
    id = "nav", collapsible = TRUE,
    theme = bs_theme(version = 5, primary = "#0b5d7a", secondary = "#5f7383", success = "#2e7d32",
                     warning = "#f2a900", base_font = "Inter, Segoe UI, sans-serif",
                     heading_font = "Merriweather, Georgia, serif", "border-radius" = "0.6rem"),
    home_tab, method_tab, instructions_tab, developers_tab
  ),
  footer
)

# ============================================================
#  SERVER
# ============================================================

dt_out <- function(df, digits = 4, page = 10, p_cols = NULL, filter = "top") {
  if (is.null(df) || !nrow(df)) return(DT::datatable(data.frame(Message = "No results available"), rownames = FALSE, style = DT_STYLE, options = list(dom = "t")))
  int_cols <- c("N", "Lag", "Embedding_Dimension")
  num_cols <- names(df)[vapply(df, is.numeric, logical(1)) & !names(df) %in% int_cols]
  pc <- intersect(names(df), c(p_cols, "P_value", "P_Value", "JB_p", "SW_p", "ADF_p", "LB_p", "BP_p"))
  dt <- DT::datatable(
    df, rownames = FALSE, filter = filter, extensions = "Buttons", style = DT_STYLE,
    class = "table-sm table-striped table-hover nowrap",
    options = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel"), pageLength = page,
                   scrollX = TRUE, lengthMenu = c(10, 25, 50, 100)))
  rn <- setdiff(num_cols, pc)
  if (length(rn)) dt <- DT::formatRound(dt, rn, digits)
  if (length(pc)) dt <- DT::formatSignif(dt, pc, 4)
  if ("Signif" %in% names(df))
    dt <- DT::formatStyle(dt, "Signif", target = "row",
                          backgroundColor = DT::styleEqual(c("***", "**", "*"), c("#d7efd8", "#e6f4e6", "#f3faf3")))
  if ("Significant" %in% names(df))
    dt <- DT::formatStyle(dt, "Significant", target = "row", backgroundColor = DT::styleEqual("Yes", "#e6f4e6"))
  dt
}

default_settings <- list(
  y_transform = "none", x_transform = "none", freq = "auto", effect_label = "", lag_label = "",
  max_lag = 12, exposure_basis = "ps", exposure_df = 6, spline_degree = 3, lag_basis = "ps", lag_df = 6,
  trend = "linear", trend_df_per_year = 4, seasonality = "none", gam_method = "REML",
  reference = "median", reference_quantile = 0.5, reference_custom = 0, prediction_method = "percentiles",
  percentiles = "5, 25, 50, 75, 95", grid_points = 20, custom_values = "", selected_lags = "1, 3, 6, 9, 12",
  confidence_level = 0.95, bds_m = 5
)

freq_default_lag <- function(unit) switch(as.character(unit), "months" = 12, "quarters" = 8, "weeks" = 12, "days" = 14,
                                          "hours" = 24, "minutes" = 30, "fortnights" = 12, "half-years" = 4, "years" = 3, 6)

server <- function(input, output, session) {

  # ---------------- visitor counter ----------------
  visits <- increment_counter()
  output$visitor_count <- renderText(formatC(visits, width = 6, flag = "0", format = "d"))

  # ---------------- navigation ----------------
  observeEvent(input$go_method, updateNavbarPage(session, "nav", "method"))
  observeEvent(input$go_instr, updateNavbarPage(session, "nav", "instructions"))
  observeEvent(input$to_desc, updateTabsetPanel(session, "method_tabs", "desc"))
  observeEvent(input$to_settings, updateTabsetPanel(session, "method_tabs", "settings"))

  # ---------------- file state ----------------
  rv <- reactiveValues(y_path = NULL, y_name = NULL, x_path = NULL, x_name = NULL, results = NULL)

  observeEvent(input$y_file, { rv$y_path <- input$y_file$datapath; rv$y_name <- input$y_file$name })
  observeEvent(input$x_file, { rv$x_path <- input$x_file$datapath; rv$x_name <- input$x_file$name })
  observeEvent(input$load_sample, {
    rv$y_path <- "sample_data/Sample_Y_data.csv"; rv$y_name <- "Sample_Y_data.csv"
    rv$x_path <- "sample_data/Sample_X_data.csv"; rv$x_name <- "Sample_X_data.csv"
    showNotification("Sample data loaded: 5 commodities (Y) and 2 GPR indices (X), monthly 2015-2024.", type = "message", duration = 5)
  })

  # sample downloads
  for (id in c("y", "x")) local({
    r <- toupper(id)
    output[[paste0("dl_sample_", id, "_csv")]] <- downloadHandler(
      filename = function() paste0("Sample_", r, "_data.csv"),
      content = function(file) file.copy(file.path("sample_data", paste0("Sample_", r, "_data.csv")), file))
    output[[paste0("dl_sample_", id, "_xlsx")]] <- downloadHandler(
      filename = function() paste0("Sample_", r, "_data.xlsx"),
      content = function(file) file.copy(file.path("sample_data", paste0("Sample_", r, "_data.xlsx")), file))
  })

  make_file_ui <- function(id) {
    sheets <- reactive({ req(rv[[paste0(id, "_path")]]); file_sheets(rv[[paste0(id, "_path")]], rv[[paste0(id, "_name")]]) })
    output[[paste0(id, "_sheet_ui")]] <- renderUI({
      sh <- sheets()
      if (is.null(sh)) return(NULL)
      selectInput(paste0(id, "_sheet"), "Worksheet", sh, sh[1])
    })
    output[[paste0(id, "_cols_ui")]] <- renderUI({
      req(rv[[paste0(id, "_path")]])
      if (is.null(sheets())) uiOutput(paste0(id, "_date_ui"))
      else fluidRow(column(6, uiOutput(paste0(id, "_sheet_ui"))), column(6, uiOutput(paste0(id, "_date_ui"))))
    })
    raw <- reactive({
      req(rv[[paste0(id, "_path")]])
      sh <- sheets()
      sel <- if (!is.null(sh)) { s <- input[[paste0(id, "_sheet")]]; if (is.null(s) || !s %in% sh) sh[1] else s } else NULL
      tryCatch(read_any(rv[[paste0(id, "_path")]], rv[[paste0(id, "_name")]], sel),
               error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    })
    output[[paste0(id, "_date_ui")]] <- renderUI({
      d <- raw(); req(d)
      selectInput(paste0(id, "_date"), "Time column", names(d), guess_date_col(d))
    })
    output[[paste0(id, "_info")]] <- renderUI({
      d <- raw(); req(d)
      div(class = "muted", style = "margin-bottom:8px;", icon("file-lines"), " ", rv[[paste0(id, "_name")]], ": ",
          tags$b(nrow(d)), " rows, ", tags$b(ncol(d) - 1), " variable column(s)")
    })
    output[[paste0(id, "_preview")]] <- renderDT({
      d <- raw(); req(d)
      DT::datatable(utils::head(d, 100), rownames = FALSE, style = DT_STYLE, class = "table-sm table-striped nowrap",
                    options = list(pageLength = 5, scrollX = TRUE, dom = "tip"))
    })
    raw
  }
  y_raw <- make_file_ui("y")
  x_raw <- make_file_ui("x")

  # ---------------- merged dataset ----------------
  merged <- reactive({
    yd <- y_raw(); xd <- x_raw()
    req(yd, xd, input$y_date, input$x_date, input$y_date %in% names(yd), input$x_date %in% names(xd))
    tryCatch(build_dataset(yd, xd, input$y_date, input$x_date, input$date_format, input$missing),
             error = function(e) list(error = conditionMessage(e)))
  })
  data_ok <- reactive({ m <- merged(); is.null(m$error) })

  output$has_data <- reactive(isTruthy(tryCatch(data_ok(), error = function(e) FALSE)))
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)

  freq_used <- reactive({ req(data_ok()); if (input$freq == "auto") merged()$freq else as.numeric(input$freq) })

  output$merge_status <- renderUI({
    m <- tryCatch(merged(), error = function(e) NULL)
    if (is.null(m)) return(div(class = "note", icon("circle-info"), " Upload both the Y file and the X file to build the analysis dataset."))
    if (!is.null(m$error)) return(div(class = "note err", icon("circle-xmark"), " ", m$error))
    d <- m$data
    tagList(
      div(class = "stat-grid",
          stat_box("Observations", nrow(d), "list-ol"),
          stat_box("Period", paste(d$.label[1], "to", d$.label[nrow(d)]), "calendar", "o"),
          stat_box("Time unit", paste0(tools::toTitleCase(m$unit), if (m$freq > 1) paste0(" (cycle ", m$freq, ")") else ""), "clock", "g"),
          stat_box("Y variables", length(m$y_names), "bullseye"),
          stat_box("X variables", length(m$x_names), "bolt", "g")),
      local({
        ex <- if (m$kind != "index") format(d$.x[1], if (m$kind == "datetime") "%Y-%m-%d %H:%M" else "%Y-%m-%d") else NA
        div(class = "note", icon("clock"),
            HTML(paste0(
              " Time column <b>", htmltools::htmlEscape(m$time_name), "</b> read as <b>", htmltools::htmlEscape(m$time_desc), "</b>",
              if (m$kind == "index") " (kept in the order given)"
              else if (!identical(ex, d$.label[1])) paste0(" (e.g. '", d$.label[1], "' = ", ex, ")") else "",
              ". Y and X were ", m$merge_how, ". The time column is shown everywhere exactly as you wrote it.")))
      }),
      if (length(m$messages)) div(class = "note warn", icon("triangle-exclamation"), " ", HTML(paste(m$messages, collapse = "<br>")))
      else div(class = "note ok", icon("circle-check"), " Data merged successfully. Proceed to descriptive statistics or model settings.")
    )
  })

  output$merged_preview <- renderDT({
    req(data_ok())
    dt_out(display_data(merged()), digits = 3, filter = "none")
  })

  # update variable selectors and frequency-dependent defaults when data change
  observeEvent(merged(), {
    req(data_ok())
    m <- merged()
    updateSelectizeInput(session, "y_vars", choices = m$y_names, selected = m$y_names)
    updateSelectizeInput(session, "x_vars", choices = m$x_names, selected = m$x_names)
    allv <- c(m$y_names, m$x_names)
    updateSelectizeInput(session, "ts_vars", choices = allv, selected = allv[seq_len(min(4, length(allv)))])
    updateSelectInput(session, "hist_var", choices = allv)
    L <- min(freq_default_lag(m$unit), max(1, floor(nrow(m$data) / 4)))
    updateNumericInput(session, "max_lag", value = L)
    updateTextInput(session, "selected_lags", value = paste(unique(pmax(1, round(c(1, L / 4, L / 2, 3 * L / 4, L)))), collapse = ", "))
    rv$results <- NULL
  })

  output$freq_help <- renderUI({
    req(data_ok())
    help_txt(paste0("Detected: ", merged()$unit, " data, seasonal period ", merged()$freq, if (merged()$freq <= 1) " (no seasonality)" else ""))
  })

  # transformed data used in the model
  model_data <- reactive({
    req(data_ok())
    m <- merged(); d <- m$data
    out <- tryCatch({
      for (v in m$y_names) d[[v]] <- apply_transform(d[[v]], input$y_transform)
      for (v in m$x_names) d[[v]] <- apply_transform(d[[v]], input$x_transform)
      d <- d[stats::complete.cases(d), , drop = FALSE]; rownames(d) <- NULL; d
    }, error = function(e) list(error = conditionMessage(e)))
    out
  })

  # ---------------- descriptive statistics ----------------
  desc_source <- reactive({
    req(data_ok())
    if (input$desc_data == "trans") { d <- model_data(); validate(need(is.data.frame(d), d$error)); d } else merged()$data
  })
  desc_tbl <- reactive({
    m <- merged(); d <- desc_source()
    vv <- c(m$y_names, m$x_names)
    descriptive_stats(d, vv, c(rep("Y (response)", length(m$y_names)), rep("X (exposure)", length(m$x_names))))
  })
  output$desc_table <- renderDT(dt_out(desc_tbl(), filter = "none"))
  output$pl_ts <- renderPlotly({ req(input$ts_vars); plot_timeseries(desc_source(), input$ts_vars, input$ts_std, merged()$time_name) })
  output$pl_box <- renderPlotly({ m <- merged(); plot_box(desc_source(), c(m$y_names, m$x_names)) })
  output$pl_hist <- renderPlotly({ req(input$hist_var, input$hist_var %in% names(desc_source())); plot_hist(desc_source(), input$hist_var) })
  output$pl_corr <- renderPlotly({ m <- merged(); plot_corr(desc_source(), c(m$y_names, m$x_names)) })

  # ---------------- settings ----------------
  observeEvent(input$reset_defaults, {
    s <- default_settings
    for (nm in c("y_transform", "x_transform", "freq", "exposure_basis", "lag_basis", "trend", "seasonality",
                 "gam_method", "reference", "prediction_method"))
      updateSelectInput(session, nm, selected = s[[nm]])
    for (nm in c("exposure_df", "spline_degree", "lag_df", "trend_df_per_year", "reference_custom", "grid_points",
                 "bds_m"))
      updateNumericInput(session, nm, value = s[[nm]])
    for (nm in c("effect_label", "lag_label", "percentiles", "custom_values"))
      updateTextInput(session, nm, value = s[[nm]])
    updateSliderInput(session, "confidence_level", value = s$confidence_level)
    updateSliderInput(session, "reference_quantile", value = s$reference_quantile)
    if (isTRUE(data_ok())) {
      L <- min(freq_default_lag(merged()$unit), max(1, floor(nrow(merged()$data) / 4)))
      updateNumericInput(session, "max_lag", value = L)
      updateTextInput(session, "selected_lags", value = paste(unique(pmax(1, round(c(1, L / 4, L / 2, 3 * L / 4, L)))), collapse = ", "))
    }
    showNotification("Settings reset to defaults.", type = "message")
  })

  output$run_summary <- renderUI({
    if (!isTRUE(tryCatch(data_ok(), error = function(e) FALSE))) return(span(class = "muted", "Upload data to enable the analysis."))
    np <- length(input$y_vars) * length(input$x_vars)
    md <- model_data()
    nobs <- if (is.data.frame(md)) nrow(md) else NA
    span(class = "muted", tags$b(np), " response-exposure pair(s) | ", tags$b(nobs), " observations after transformation | max lag ",
         tags$b(input$max_lag))
  })

  collect_settings <- function() {
    list(
      freq = freq_used(), unit = merged()$unit, max_lag = as.integer(input$max_lag),
      exposure_basis = input$exposure_basis, lag_basis = input$lag_basis,
      exposure_df = as.integer(input$exposure_df), lag_df = as.integer(input$lag_df),
      spline_degree = as.integer(input$spline_degree),
      trend = input$trend, trend_df_per_year = input$trend_df_per_year %||% 4,
      seasonality = input$seasonality, gam_method = input$gam_method,
      reference = input$reference, reference_quantile = input$reference_quantile, reference_custom = input$reference_custom,
      prediction_method = input$prediction_method, percentiles = parse_num_list(input$percentiles),
      grid_points = input$grid_points, custom_values = parse_num_list(input$custom_values),
      selected_lags = parse_num_list(input$selected_lags), confidence_level = input$confidence_level,
      lb_lag = as.integer(input$max_lag), y_transform = input$y_transform, x_transform = input$x_transform,
      bds_m = as.integer(input$bds_m)
    )
  }

  # ---------------- run analysis ----------------
  observeEvent(input$run, {
    if (!isTRUE(tryCatch(data_ok(), error = function(e) FALSE))) {
      showNotification("Please upload the Y and X files first.", type = "error"); return()
    }
    d <- model_data()
    if (!is.data.frame(d)) { showNotification(d$error, type = "error"); return() }
    yv <- input$y_vars; xv <- input$x_vars
    if (!length(yv) || !length(xv)) { showNotification("Select at least one Y and one X variable.", type = "error"); return() }
    s <- collect_settings()
    if (any(is.na(c(s$max_lag, s$exposure_df, s$lag_df)))) { showNotification("Please fill in all numeric settings.", type = "error"); return() }
    if (s$max_lag >= nrow(d) / 2) {
      showNotification(sprintf("Maximum lag (%d) must be less than half the number of observations (%d).", s$max_lag, nrow(d)), type = "error", duration = 8)
      return()
    }

    pairs <- expand.grid(y = yv, x = xv, stringsAsFactors = FALSE)
    np <- nrow(pairs)
    fits <- list(); bds <- list(); errs <- list()
    log_err <- function(y, x, stage, e) errs[[length(errs) + 1]] <<- data.frame(Response = y, Exposure = x, Stage = stage,
                                                                                 Error_Message = conditionMessage(e), stringsAsFactors = FALSE)
    t0 <- Sys.time()
    withProgress(message = "Running analysis", value = 0, {
      for (i in seq_len(np)) {
        y <- pairs$y[i]; x <- pairs$x[i]; key <- paste(y, x, sep = " || ")
        lbl <- sprintf("Pair %d of %d: %s ~ %s", i, np, y, x)
        incProgress(0, detail = paste(lbl, "- BDS test"))
        tryCatch(bds[[key]] <- run_bds(d[[y]], d[[x]], y, x, s$freq, s$bds_m),
                 error = function(e) log_err(y, x, "BDS test (ARIMAX)", e))
        incProgress(0.3 / np, detail = paste(lbl, "- DLNM"))
        tryCatch(fits[[key]] <- fit_dlnm_pair(d, y, x, s),
                 error = function(e) log_err(y, x, "DLNM", e))
        incProgress(0.4 / np)
      }
    })
    bind <- function(lst, el = NULL) {
      if (!length(lst)) return(data.frame())
      if (!is.null(el)) lst <- lapply(lst, `[[`, el)
      out <- do.call(rbind, lst); rownames(out) <- NULL; out
    }
    rv$results <- list(
      settings = s, data = d, pairs = pairs, fits = fits,
      cumulative = bind(fits, "cumulative"), lag_effects = bind(fits, "lag_effects"),
      bds = bind(bds), errors = bind(errs),
      elapsed = as.numeric(difftime(Sys.time(), t0, units = "secs")),
      desc = tryCatch(desc_tbl(), error = function(e) NULL)
    )
    ok_keys <- names(fits)
    if (length(ok_keys)) {
      ys <- unique(sub(" \\|\\| .*$", "", ok_keys))
      xs <- sub("^.* \\|\\| ", "", ok_keys[startsWith(ok_keys, paste0(ys[1], " || "))])
      updateSelectInput(session, "plot_y", choices = ys, selected = ys[1])
      updateSelectInput(session, "plot_x", choices = xs, selected = xs[1])
    }
    updateTabsetPanel(session, "method_tabs", "results")
    showNotification(sprintf("Analysis finished: %d of %d pairs fitted in %.1f s.", length(fits), np, rv$results$elapsed),
                     type = if (length(errs)) "warning" else "message", duration = 8)
  })

  output$has_results <- reactive(!is.null(rv$results))
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  observe({
    req(rv$results, input$plot_y)
    keys <- names(rv$results$fits)
    xs <- sub("^.* \\|\\| ", "", keys[startsWith(keys, paste0(input$plot_y, " || "))])
    cur <- isolate(input$plot_x)
    updateSelectInput(session, "plot_x", choices = xs, selected = if (!is.null(cur) && cur %in% xs) cur else xs[1])
  })

  output$results_header <- renderUI({
    r <- rv$results
    if (is.null(r)) return(div(class = "note warn", icon("hourglass-half"), " No results yet. Set the model in step 3 and click ", tags$b("Run Analysis"), "."))
    nerr <- if (nrow(r$errors)) nrow(r$errors) else 0
    nsig <- if (nrow(r$cumulative)) sum(r$cumulative$P_value < 0.05, na.rm = TRUE) else 0
    div(class = "stat-grid",
        stat_box("Pairs analysed", nrow(r$pairs), "diagram-project"),
        stat_box("DLNMs fitted", length(r$fits), "circle-check", "g"),
        stat_box("Significant cumulative effects (5%)", nsig, "star", "o"),
        stat_box("Errors", nerr, "triangle-exclamation", if (nerr) "r" else "g"),
        stat_box("Run time", sprintf("%.1f s", r$elapsed), "stopwatch"))
  })

  output$tbl_cum <- renderDT({ req(rv$results); dt_out(rv$results$cumulative) })
  output$tbl_lag <- renderDT({ req(rv$results); dt_out(rv$results$lag_effects) })
  output$tbl_bds <- renderDT({ req(rv$results); dt_out(rv$results$bds) })
  output$tbl_err <- renderDT({
    req(rv$results)
    if (!nrow(rv$results$errors)) return(DT::datatable(data.frame(Status = "No errors - all steps completed successfully."), rownames = FALSE, style = DT_STYLE, options = list(dom = "t")))
    dt_out(rv$results$errors, filter = "none")
  })

  # ---------------- plots for selected pair ----------------
  sel_fit <- reactive({
    req(rv$results, input$plot_y, input$plot_x)
    f <- rv$results$fits[[paste(input$plot_y, input$plot_x, sep = " || ")]]
    req(f); f
  })
  labs <- reactive({
    s <- rv$results$settings
    list(x = input$plot_x,
         lag = if (nzchar(input$lag_label)) input$lag_label else paste0("Lag (", s$unit, ")"),
         eff = if (nzchar(input$effect_label)) input$effect_label else paste("Effect on", input$plot_y),
         unit = s$unit)
  })
  output$pl_surface <- renderPlotly(plot_surface(sel_fit(), labs()$x, labs()$lag, labs()$eff))
  output$pl_contour <- renderPlotly(plot_contour(sel_fit(), labs()$x, labs()$lag, labs()$eff))
  output$pl_lagslice <- renderPlotly(plot_lag_slices(sel_fit(), labs()$lag, labs()$eff))
  output$pl_expresp <- renderPlotly(plot_exposure_response(sel_fit(), labs()$x, labs()$eff))
  output$pl_cum <- renderPlotly(plot_cumulative(sel_fit(), labs()$x, labs()$eff))

  output$interpretation <- renderUI({
    f <- sel_fit(); ce <- f$cumulative; s <- rv$results$settings
    ce <- ce[!is.na(ce$P_value), , drop = FALSE]
    items <- lapply(seq_len(nrow(ce)), function(i) {
      sig <- ce$P_value[i] < 0.05
      tags$li(span(class = if (sig) "sig" else "nsig", if (sig) icon("circle-check") else icon("circle-minus")), " ",
              sprintf("At the %s of %s (%.3f) vs reference %.3f: cumulative effect over lags 0-%d = %.4f (%g%% CI %.4f to %.4f; p = %.4f)%s.",
                      ce$Point[i], input$plot_x, ce$Exposure_Value[i], ce$Reference[i], s$max_lag,
                      ce$Cumulative_Effect[i], 100 * s$confidence_level, ce$CI_Lower[i], ce$CI_Upper[i], ce$P_value[i],
                      if (sig) ", statistically significant" else ", not significant"))
    })
    div(class = "note", style = "margin-top:0;", tags$b(icon("comment-dots"), " Quick interpretation: ", input$plot_y, " ~ ", input$plot_x),
        tags$ul(class = "interp", style = "margin:8px 0 0;", items))
  })

  # ---------------- downloads ----------------
  output$dl_excel <- downloadHandler(
    filename = function() paste0("DLNM_Results_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx"),
    content = function(file) {
      r <- rv$results; req(r)
      d <- display_data(merged(), r$data)
      s <- r$settings
      sets <- data.frame(Setting = names(s), Value = vapply(s, function(v) paste(v, collapse = ", "), character(1)))
      sheets <- list(Data_Used = d, Descriptive_Stats = r$desc, BDS_Test = r$bds,
                     Cumulative_Effects = r$cumulative, Selected_Lag_Effects = r$lag_effects,
                     Error_Log = if (nrow(r$errors)) r$errors else data.frame(Status = "No errors"), Settings = sets)
      wb <- openxlsx::createWorkbook(creator = "DLNM Explorer, ICAR-IASRI")
      hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#0B3A53", fontColour = "#FFFFFF", halign = "center", border = "Bottom")
      for (nm in names(sheets)) {
        df <- sheets[[nm]]; if (is.null(df) || !nrow(df)) df <- data.frame(Message = "No results available")
        openxlsx::addWorksheet(wb, nm)
        openxlsx::writeData(wb, nm, df, headerStyle = hs)
        openxlsx::freezePane(wb, nm, firstRow = TRUE)
        openxlsx::setColWidths(wb, nm, seq_len(ncol(df)), "auto")
        openxlsx::addFilter(wb, nm, row = 1, cols = seq_len(ncol(df)))
      }
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    })

  output$dl_figs <- downloadHandler(
    filename = function() paste0("DLNM_Figures_", format(Sys.time(), "%Y%m%d_%H%M"), ".zip"),
    content = function(file) {
      r <- rv$results; req(r)
      tmp <- file.path(tempdir(), paste0("figs_", as.integer(Sys.time())))
      dir.create(tmp, showWarnings = FALSE)
      s <- r$settings
      files <- character()
      withProgress(message = "Creating figures", value = 0, {
        for (k in names(r$fits)) {
          yx <- strsplit(k, " || ", fixed = TRUE)[[1]]
          incProgress(1 / length(r$fits), detail = paste(yx, collapse = " ~ "))
          pre <- gsub("[^A-Za-z0-9]+", "_", paste0(yx[1], "__", yx[2]))
          files <- c(files, tryCatch(save_publication_figures(
            r$fits[[k]], tmp, pre, xlab = yx[2],
            lag_lab = if (nzchar(input$lag_label)) input$lag_label else paste0("Lag (", s$unit, ")"),
            eff_lab = if (nzchar(input$effect_label)) input$effect_label else paste("Effect on", yx[1]),
            dpi = as.numeric(input$fig_dpi), unit = s$unit), error = function(e) character()))
        }
      })
      zip::zip(file, files = basename(files), root = tmp)
    },
    contentType = "application/zip")
}

shinyApp(ui, server)
