# DLNM Explorer — ICAR-IASRI

**Distributed Lag Non-Linear Modelling of Exogenous Shocks on Time Series** — an R Shiny web application.

## Run locally

```r
source("install_packages.R")      # first time only
shiny::runApp("DLNM_App")         # or open app.R in RStudio and click "Run App"
```

R 4.1 or later is required.

## Folder structure

| Path | Purpose |
|---|---|
| `app.R` | User interface (Home, Method, Instructions, Developers) and server logic |
| `R/analysis.R` | Analysis engine: file reading, date parsing, merging, descriptive statistics, BDS test, penalised DLNM, diagnostics, SVAR IRF |
| `R/plots.R` | Interactive (plotly) plots and publication PNG figures |
| `www/styles.css` | Design and colours |
| `www/img/` | ICAR-IASRI logo and developer photographs |
| `sample_data/` | Sample Y and X files (CSV and Excel) offered for download and "Load sample data" |
| `visitor_count.txt` | Created automatically; stores the visitor counter |

## Input files

* **Y file**: one time column + one or more response columns.
* **X file**: one time column + N explanatory (exposure) columns.
* The time column can be written in any way: dates, months (Jan-2015, 2015-01, 201501), quarters (2015Q1, Q1-2015),
  half-years, weeks (2015-W05), years, financial / agricultural years (2015-16), date-times (2015-01-31 14:00),
  Excel serials, or plain labels / indices (1, 2, 3 / t1, t2 / Kharif-2015). It is detected automatically and shown
  back exactly as written; the time unit and seasonal period adapt to it.
* CSV, XLSX or XLS. Rows are matched on the time column. Column names are used everywhere — nothing is hard-coded.

## Easy customisation

* Title and tagline: `APP_TITLE`, `APP_TAGLINE` at the top of `app.R`.
* Colours: the `:root` variables at the top of `www/styles.css`.
* Default model settings: `default_settings` in `app.R`.

## Deployment

* **shinyapps.io**: `rsconnect::deployApp("DLNM_App")`.
* **Shiny Server / Posit Connect**: copy the folder to the server's app directory (e.g. `/srv/shiny-server/dlnm`).
  Make sure the app folder is writable by the `shiny` user so `visitor_count.txt` can be updated.
* On shinyapps.io the file system is reset on each redeploy, so the visitor count restarts from zero. For a permanent
  count, keep the app on an institute server (Shiny Server).

## Notes on the method implementation

* The SVAR uses a just-identified recursive A-model (lower-triangular A with a free diagonal; one-standard-deviation shocks).
  If SVAR estimation fails, the equivalent Cholesky-orthogonalised VAR impulse response is used and the method column says so.
* The BDS test is reported for embedding dimensions 2..m and epsilon = 0.5, 1, 1.5 and 2 times the residual SD.
* For the 3D surface, contour and exposure-response plots, effects are predicted on a fine 60-point grid of the exposure;
  tables use the chosen exposure levels (percentiles by default).
