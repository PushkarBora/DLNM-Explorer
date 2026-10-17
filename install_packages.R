# ============================================================
# DLNM Explorer - one-time package installation
# Run once:  source("install_packages.R")
# ============================================================
options(repos = c(CRAN = "https://cloud.r-project.org"),
        timeout = 1200)                  # allow slow connections (seconds per download)

pkgs <- c(
  # web application
  "shiny", "bslib", "DT", "plotly", "shinycssloaders",
  # data input / output
  "readxl", "openxlsx", "lubridate", "zip",
  # statistics
  "dlnm", "mgcv", "vars", "tseries", "moments", "forecast", "lmtest",
  # graphics
  "ggplot2", "fields"
)

# Only required dependencies are installed (not every optional "Suggests" package)
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing)) install.packages(missing, type = getOption("pkgType"))

# Second attempt for anything that failed (e.g. a dropped connection)
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing)) install.packages(missing, type = getOption("pkgType"))

missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing)) {
  message("\nStill missing: ", paste(missing, collapse = ", "),
          "\nRun source(\"install_packages.R\") again, or try another mirror:",
          "\n  options(repos = c(CRAN = \"https://cran.rstudio.com\"))")
} else {
  message("\nAll packages installed. Start the app with:  shiny::runApp()")
}
