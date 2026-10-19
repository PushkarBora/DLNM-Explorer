# ============================================================
# DLNM-Shock Analyzer : analysis engine
# ICAR-Indian Agricultural Statistics Research Institute
# ------------------------------------------------------------
# All functions are data-driven: no column names, number of
# variables, lags or frequencies are hard-coded.
# ============================================================

# ------------------------------------------------------------
# 0. UTILITIES
# ------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------------------------------------------
# 1. FILE READING
# ------------------------------------------------------------

file_sheets <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) readxl::excel_sheets(path) else NULL
}

read_any <- function(path, name, sheet = NULL) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(path, sheet = if (is.null(sheet) || sheet == "") 1 else sheet)
  } else if (ext %in% c("csv", "txt", "tsv")) {
    first <- readLines(path, n = 1, warn = FALSE)
    sep <- if (ext == "tsv" || grepl("\t", first)) "\t" else if (grepl(";", first) && !grepl(",", first)) ";" else ","
    df <- utils::read.csv(path, sep = sep, check.names = FALSE,
                          stringsAsFactors = FALSE, strip.white = TRUE)
  } else {
    stop("Unsupported file type '.", ext, "'. Please upload a .csv, .xlsx or .xls file.")
  }
  df <- as.data.frame(df, check.names = FALSE)
  # drop fully empty rows / columns
  blank <- vapply(df, function(v) is.na(v) | (is.character(v) & trimws(as.character(v)) == ""),
                  logical(nrow(df)))
  blank <- matrix(blank, nrow = nrow(df))
  df <- df[rowSums(!blank) > 0, colSums(!blank) > 0, drop = FALSE]
  names(df) <- trimws(names(df))
  df
}

guess_date_col <- function(df) {
  nm <- names(df)
  hit <- grep("date|time|month|year|period|week|day|quarter|qtr|hour|season|^fy|^t$|index|obs", nm, ignore.case = TRUE)
  if (length(hit)) nm[hit[1]] else nm[1]
}

# ------------------------------------------------------------
# 2. TIME COLUMN PARSING (fully automatic, any format)
# ------------------------------------------------------------
# The time column is kept exactly as the user wrote it (".label") for display,
# and converted to an internal ordering value (".x") for sorting, merging and plotting:
#   kind = "date"     -> .x is a Date            (days, weeks, months, quarters, years ...)
#   kind = "datetime" -> .x is a POSIXct (UTC)   (hours, minutes ...)
#   kind = "index"    -> .x is a number          (t1, t2 ... / Period 5 / 1, 2, 3 / any labels)

time_format_choices <- c(
  "Auto-detect (recommended)" = "auto",
  "Plain time index / labels (no calendar)" = "index",
  "YYYY-MM-DD (2015-01-31)" = "%Y-%m-%d",
  "DD-MM-YYYY (31-01-2015)" = "%d-%m-%Y",
  "DD/MM/YYYY (31/01/2015)" = "%d/%m/%Y",
  "MM/DD/YYYY (01/31/2015)" = "%m/%d/%Y",
  "YYYY/MM/DD (2015/01/31)" = "%Y/%m/%d",
  "MM/YYYY (01/2015)" = "%m/%Y",
  "MM-YYYY (01-2015)" = "%m-%Y",
  "YYYY-MM (2015-01)" = "%Y-%m",
  "Mon-YYYY (Jan-2015)" = "%b-%Y",
  "Mon-YY (Jan-15)" = "%b-%y",
  "Mon YYYY (Jan 2015)" = "%b %Y",
  "Month YYYY (January 2015)" = "%B %Y",
  "YYYY (2015)" = "%Y",
  "YYYY-MM-DD HH:MM (2015-01-31 14:00)" = "%Y-%m-%d %H:%M",
  "DD-MM-YYYY HH:MM (31-01-2015 14:00)" = "%d-%m-%Y %H:%M",
  "DD/MM/YYYY HH:MM (31/01/2015 14:00)" = "%d/%m/%Y %H:%M",
  "MM/DD/YYYY HH:MM (01/31/2015 14:00)" = "%m/%d/%Y %H:%M"
)
date_format_choices <- time_format_choices   # backward compatibility

# time values exactly as written by the user
as_label <- function(v) {
  if (inherits(v, "POSIXt")) {
    midnight <- all(is.na(v) | format(v, "%H:%M:%S") == "00:00:00")
    return(format(v, if (midnight) "%Y-%m-%d" else "%Y-%m-%d %H:%M"))
  }
  if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
  if (is.numeric(v)) return(ifelse(is.na(v), NA, format(v, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)))
  out <- trimws(as.character(v))
  out[out %in% c("", "NA", "N/A", "na")] <- NA
  out
}

# fixed-format parser (used when the user picks a format manually)
parse_dates <- function(x, fmt) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x <- trimws(as.character(x))
  if (!grepl("%d", fmt, fixed = TRUE)) {
    if (fmt == "%Y") return(as.Date(paste0(x, "-01-01")))
    return(as.Date(paste0("01-", x), format = paste0("%d-", fmt)))
  }
  as.Date(x, format = fmt)
}

tm <- function(x, kind, desc) list(x = x, kind = kind, desc = desc)

# ordered index for labels that are not calendar times (t1, Period 5, Kharif-2015, ...)
index_time <- function(lab, desc = "Time labels (ordered as in the file)") {
  num <- suppressWarnings(as.numeric(sub("^.*?(-?\\d+(?:\\.\\d+)?)\\D*$", "\\1", lab, perl = TRUE)))
  ok <- !is.na(lab)
  if (all(!is.na(num[ok])) && !anyDuplicated(num[ok]) && all(diff(num[ok]) > 0))
    return(tm(num, "index", paste0(desc, " - numbered")))
  x <- rep(NA_real_, length(lab)); x[ok] <- seq_len(sum(ok))
  tm(x, "index", desc)
}

parse_time <- function(v, fmt = "auto") {
  lab <- as_label(v)
  ok <- !is.na(lab)
  if (fmt == "index") return(index_time(lab, "Time index (as given)"))
  if (inherits(v, "POSIXt")) {
    v <- as.POSIXct(format(v, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
    if (all(is.na(v) | format(v, "%H:%M:%S") == "00:00:00")) return(tm(as.Date(v), "date", "Calendar dates"))
    return(tm(v, "datetime", "Date-time stamps"))
  }
  if (inherits(v, "Date")) return(tm(v, "date", "Calendar dates"))
  if (fmt != "auto") {
    if (grepl("%H", fmt, fixed = TRUE))
      return(tm(as.POSIXct(lab, format = fmt, tz = "UTC"), "datetime", "Date-time stamps (format chosen by user)"))
    return(tm(parse_dates(lab, fmt), "date", "Calendar dates (format chosen by user)"))
  }
  s <- lab
  if (!any(ok)) return(tm(rep(NA_real_, length(s)), "index", "Empty"))
  all_match <- function(re) all(grepl(re, s[ok], perl = TRUE))
  grab <- function(re, i) suppressWarnings(as.integer(sub(re, paste0("\\", i), s, perl = TRUE)))

  # ---- purely numeric values
  num <- suppressWarnings(as.numeric(s))
  if (all(!is.na(num[ok]))) {
    nn <- num[ok]; int <- all(nn == round(nn))
    if (int && all(nn >= 1000 & nn <= 3000)) return(tm(as.Date(paste0(num, "-01-01")), "date", "Years"))
    if (int && all(nn >= 100001 & nn <= 300012) && all(nn %% 100 >= 1 & nn %% 100 <= 12))
      return(tm(as.Date(sprintf("%d-%02d-01", num %/% 100, num %% 100)), "date", "Year-month numbers (YYYYMM)"))
    if (int && all(nn >= 10000101 & nn <= 30001231)) {
      d <- as.Date(as.character(num), format = "%Y%m%d")
      if (all(!is.na(d[ok]))) return(tm(d, "date", "Date numbers (YYYYMMDD)"))
    }
    if (all(nn > 15000 & nn < 80000) && length(unique(nn)) > 1) {        # Excel serial numbers
      if (int) return(tm(as.Date(num, origin = "1899-12-30"), "date", "Excel date serial numbers"))
      return(tm(as.POSIXct(round((num - 25569) * 86400), origin = "1970-01-01", tz = "UTC"), "datetime", "Excel date-time serial numbers"))
    }
    return(tm(num, "index", "Numeric time index"))
  }

  # ---- quarters: 2015Q1, 2015-Q1, 2015 Qtr 1, Q1 2015, Q1-2015, Quarter 1 2015
  r1 <- "^(\\d{4})\\s*[-/:._ ]?\\s*[Qq](?:tr|uarter)?\\s*[-._ ]?([1-4])$"
  r2 <- "^[Qq](?:tr|uarter)?\\s*[-._ ]?([1-4])\\s*[-/:._, ]?\\s*(\\d{4})$"
  if (all_match(r1)) return(tm(as.Date(sprintf("%d-%02d-01", grab(r1, 1), (grab(r1, 2) - 1) * 3 + 1)), "date", "Quarters"))
  if (all_match(r2)) return(tm(as.Date(sprintf("%d-%02d-01", grab(r2, 2), (grab(r2, 1) - 1) * 3 + 1)), "date", "Quarters"))
  # ---- half-years: 2015H1, H2-2015
  r3 <- "^(\\d{4})\\s*[-/:._ ]?\\s*[Hh]\\s*[-._ ]?([12])$"
  r4 <- "^[Hh]\\s*[-._ ]?([12])\\s*[-/:._, ]?\\s*(\\d{4})$"
  if (all_match(r3)) return(tm(as.Date(sprintf("%d-%02d-01", grab(r3, 1), (grab(r3, 2) - 1) * 6 + 1)), "date", "Half-years"))
  if (all_match(r4)) return(tm(as.Date(sprintf("%d-%02d-01", grab(r4, 2), (grab(r4, 1) - 1) * 6 + 1)), "date", "Half-years"))
  # ---- weeks: 2015W05, 2015-W5, 2015 Week 5, W5 2015
  r5 <- "^(\\d{4})\\s*[-/:._ ]?\\s*[Ww](?:k|eek)?\\s*[-._ ]?(\\d{1,2})$"
  r6 <- "^[Ww](?:k|eek)?\\s*[-._ ]?(\\d{1,2})\\s*[-/:._, ]?\\s*(\\d{4})$"
  wk <- function(y, w) as.Date(paste0(y, "-01-01")) + (w - 1) * 7
  if (all_match(r5)) return(tm(wk(grab(r5, 1), grab(r5, 2)), "date", "Weeks"))
  if (all_match(r6)) return(tm(wk(grab(r6, 2), grab(r6, 1)), "date", "Weeks"))
  # ---- financial / agricultural years: 2015-16, 2015/16, 2015-2016, FY2015-16
  r7 <- "^(?:[A-Za-z]+\\s*)?(\\d{4})\\s*[-/]\\s*(\\d{2}|\\d{4})$"
  if (all_match(r7)) {
    y1 <- grab(r7, 1); y2 <- grab(r7, 2)
    if (all(((y2 %% 100) == ((y1 + 1) %% 100))[ok])) return(tm(as.Date(paste0(y1, "-01-01")), "date", "Financial / agricultural years"))
  }
  # ---- date-times with a clock time
  if (all_match("\\d{1,2}:\\d{2}")) {
    if (all_match("^\\d{1,2}:\\d{2}(:\\d{2})?\\s*([AaPp][Mm])?$")) {     # time of day only
      d <- suppressWarnings(lubridate::parse_date_time(paste("2000-01-01", s), c("Ymd HMS", "Ymd HM", "Ymd IMSp", "Ymd IMp"), tz = "UTC", quiet = TRUE))
      if (mean(!is.na(d[ok])) >= 0.9) return(tm(d, "datetime", "Clock times"))
    }
    d <- suppressWarnings(lubridate::parse_date_time(
      s, c("Ymd HMS", "Ymd HM", "dmY HMS", "dmY HM", "mdY HMS", "mdY HM", "Ymd IMSp", "Ymd IMp",
           "dmY IMSp", "dmY IMp", "mdY IMSp", "mdY IMp", "dbY HMS", "dbY HM", "bdY HMS", "bdY HM"),
      tz = "UTC", quiet = TRUE))
    if (mean(!is.na(d[ok])) >= 0.9) {
      if (all(is.na(d) | format(d, "%H:%M:%S") == "00:00:00")) return(tm(as.Date(d), "date", "Calendar dates"))
      return(tm(d, "datetime", "Date-time stamps"))
    }
  }
  # ---- general calendar formats (any separator, month names, 2- or 4-digit years)
  # count date parts (number groups + month name) so that "Jan-2015" is never read as day 20, year 15
  mon_re <- "(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*"
  parts <- lengths(regmatches(s, gregexpr("\\d+", s))) + grepl(mon_re, tolower(s))
  try_orders <- function(ord, desc) {
    d <- suppressWarnings(lubridate::parse_date_time(s, ord, tz = "UTC", quiet = TRUE))
    if (mean(!is.na(d[ok])) >= 0.9 && length(unique(d[ok & !is.na(d)])) > 1) tm(as.Date(d), "date", desc) else NULL
  }
  res <- NULL
  if (all(parts[ok] >= 3))
    res <- try_orders(c("Ymd", "dmY", "mdY", "dbY", "bdY", "Ybd", "dmy", "mdy", "ymd", "dby", "bdy"), "Calendar dates")
  if (is.null(res) && all(parts[ok] == 2))
    res <- try_orders(c("bY", "BY", "Yb", "mY", "Ym", "by", "yb", "my", "ym"), "Months")
  if (!is.null(res)) return(res)

  # ---- anything else: ordered time labels
  index_time(s)
}

to_numeric <- function(v) {
  if (is.numeric(v)) return(as.numeric(v))
  v <- gsub("[,[:space:]]", "", as.character(v))
  v[v %in% c("", "NA", "na", "-", ".", "NaN")] <- NA
  suppressWarnings(as.numeric(v))
}

# ------------------------------------------------------------
# 3. TIME UNIT, SEASONAL PERIOD AND SEASON INDEX
# ------------------------------------------------------------

detect_time_structure <- function(x, kind) {
  if (kind == "index") return(list(freq = 1, unit = "periods"))
  u <- sort(unique(x))
  if (length(u) < 3) return(list(freq = 1, unit = "periods"))
  if (kind == "datetime") {
    md <- stats::median(diff(as.numeric(u)))                 # seconds
    if (md <= 90) return(list(freq = 60, unit = "minutes"))
    if (md <= 5400) return(list(freq = 24, unit = "hours"))
    u <- unique(as.Date(u))
  }
  md <- stats::median(as.numeric(diff(u)))                    # days
  if (md <= 1.5) return(list(freq = 7, unit = "days"))
  if (md <= 8) return(list(freq = 52, unit = "weeks"))
  if (md <= 17) return(list(freq = 24, unit = "fortnights"))
  if (md <= 35) return(list(freq = 12, unit = "months"))
  if (md <= 100) return(list(freq = 4, unit = "quarters"))
  if (md <= 200) return(list(freq = 2, unit = "half-years"))
  list(freq = 1, unit = "years")
}

# legacy helper (kept for compatibility)
frequency_unit <- function(freq) {
  switch(as.character(freq), "365" = "days", "52" = "weeks", "24" = "fortnights", "12" = "months",
         "4" = "quarters", "2" = "half-years", "1" = "years", "periods")
}

season_index <- function(dat, freq, unit = "periods") {
  n <- nrow(dat)
  if (is.null(freq) || freq <= 1) return(rep(1L, n))
  x <- dat$.x
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    m <- as.integer(format(x, "%m"))
    if (unit == "months" && freq == 12) return(m)
    if (unit == "quarters" && freq == 4) return((m - 1L) %/% 3L + 1L)
    if (unit == "half-years" && freq == 2) return(ifelse(m <= 6, 1L, 2L))
    if (unit == "weeks" && freq == 52) return(pmin(52L, as.integer(format(x, "%V"))))
    if (unit == "days" && freq == 7) return(as.integer(format(x, "%u")))
    if (unit == "days" && freq == 365) return(pmin(365L, as.integer(format(x, "%j"))))
    if (unit == "hours" && freq == 24) return(as.integer(format(x, "%H")) + 1L)
    if (unit == "minutes" && freq == 60) return(as.integer(format(x, "%M")) + 1L)
  }
  as.integer(((seq_len(n) - 1) %% freq) + 1)                  # position in the cycle
}

# ------------------------------------------------------------
# 4. TRANSFORMATIONS
# ------------------------------------------------------------

transform_choices <- c(
  "None (levels)" = "none",
  "Natural log: ln(Z)" = "log",
  "Log return: ln(Z_t) - ln(Z_t-1)" = "logret",
  "First difference: Z_t - Z_t-1" = "diff",
  "Percentage change: 100*(Z_t - Z_t-1)/Z_t-1" = "pct",
  "Standardise: (Z - mean)/sd" = "z"
)

apply_transform <- function(v, type) {
  switch(type,
         none = v,
         log = { if (any(v <= 0, na.rm = TRUE)) stop("Log transformation needs strictly positive values."); log(v) },
         logret = { if (any(v <= 0, na.rm = TRUE)) stop("Log return needs strictly positive values."); c(NA, diff(log(v))) },
         diff = c(NA, diff(v)),
         pct = c(NA, 100 * diff(v) / utils::head(v, -1)),
         z = as.numeric(scale(v)),
         v)
}

transform_label <- function(type) names(transform_choices)[transform_choices == type]

# ------------------------------------------------------------
# 5. BUILD ANALYSIS DATA (merge Y and X on the time column)
# ------------------------------------------------------------

TIME_COLS <- c(".x", ".label")

build_dataset <- function(ydf, xdf, y_time, x_time, time_fmt = "auto", missing = "omit") {
  msgs <- character()
  prep <- function(df, tcol, role) {
    if (!tcol %in% names(df)) stop("Time column '", tcol, "' not found in the ", role, " file.")
    tp <- parse_time(df[[tcol]], time_fmt)
    lab <- as_label(df[[tcol]])
    bad <- sum(is.na(tp$x))
    if (bad == nrow(df)) stop("Could not read the time column of the ", role, " file. Choose 'Plain time index / labels' in 'Time format'.")
    if (bad > 0) msgs <<- c(msgs, sprintf("%d row(s) in the %s file had an empty or unreadable time value and were dropped.", bad, role))
    vals <- df[, setdiff(names(df), tcol), drop = FALSE]
    vals[] <- lapply(vals, to_numeric)
    keep <- vapply(vals, function(v) sum(!is.na(v)) > 0, logical(1))
    if (any(!keep)) msgs <<- c(msgs, sprintf("Non-numeric column(s) ignored in %s file: %s", role, paste(names(vals)[!keep], collapse = ", ")))
    vals <- vals[, keep, drop = FALSE]
    if (ncol(vals) == 0) stop("No numeric variable columns found in the ", role, " file.")
    out <- data.frame(.x = tp$x, .label = lab, vals, check.names = FALSE, stringsAsFactors = FALSE)
    out <- out[!is.na(out$.x), , drop = FALSE]
    key <- as.numeric(out$.x)
    if (anyDuplicated(key)) {
      msgs <<- c(msgs, sprintf("Repeated time values in the %s file were averaged.", role))
      first <- !duplicated(key)
      agg <- stats::aggregate(out[, names(vals), drop = FALSE], by = list(.k = key), FUN = mean, na.rm = TRUE)
      base <- out[first, TIME_COLS, drop = FALSE]
      base <- base[match(agg$.k, key[first]), , drop = FALSE]
      out <- data.frame(base, agg[, -1, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
    }
    list(data = out, kind = tp$kind, desc = tp$desc, vars = names(vals))
  }
  Y <- prep(ydf, y_time, "Y")
  X <- prep(xdf, x_time, "X")
  clash <- intersect(Y$vars, X$vars)
  if (length(clash)) {
    names(X$data)[names(X$data) %in% clash] <- paste0(names(X$data)[names(X$data) %in% clash], "_X")
    X$vars[X$vars %in% clash] <- paste0(X$vars[X$vars %in% clash], "_X")
    msgs <- c(msgs, "Columns with the same name in Y and X files were renamed with suffix '_X'.")
  }

  kind <- Y$kind
  if (Y$kind != "index" && X$kind != "index") {
    if (Y$kind != X$kind) {                       # one file has dates, the other date-times
      Y$data$.x <- as.Date(Y$data$.x); X$data$.x <- as.Date(X$data$.x); kind <- "date"
      msgs <- c(msgs, "One file has date-times and the other dates; both were matched by calendar date.")
    }
    Y$data$.k <- as.numeric(Y$data$.x); X$data$.k <- as.numeric(X$data$.x)
    dat <- merge(Y$data, X$data[, c(".k", X$vars), drop = FALSE], by = ".k")
    how <- "matched by time"
  } else {
    ly <- Y$data$.label; lx <- X$data$.label
    common <- intersect(ly, lx)
    if (!anyDuplicated(ly) && !anyDuplicated(lx) && length(common) >= 3) {
      dat <- merge(Y$data, X$data[, c(".label", X$vars), drop = FALSE], by = ".label")
      how <- "matched by identical time labels"
    } else {
      n <- min(nrow(Y$data), nrow(X$data))
      dat <- cbind(Y$data[seq_len(n), , drop = FALSE], X$data[seq_len(n), X$vars, drop = FALSE])
      how <- "aligned by row order"
      if (nrow(Y$data) != nrow(X$data))
        msgs <- c(msgs, sprintf("Y and X files have different numbers of rows (%d vs %d); the first %d rows were aligned.", nrow(Y$data), nrow(X$data), n))
    }
    if (Y$kind != "index") { dat$.x <- as.numeric(rank(as.numeric(dat$.x), ties.method = "first")); kind <- "index" }
  }
  if (nrow(dat) == 0) stop("No common time points between Y and X files. Check that both files cover the same period, or set 'Time format' manually.")
  dat <- dat[order(as.numeric(dat$.x)), c(TIME_COLS, Y$vars, X$vars), drop = FALSE]
  n_before <- nrow(dat)
  vars <- c(Y$vars, X$vars)
  if (missing == "interpolate") {
    for (v in vars) {
      z <- dat[[v]]
      if (anyNA(z) && sum(!is.na(z)) >= 2)
        dat[[v]] <- stats::approx(seq_along(z), z, xout = seq_along(z), rule = 2)$y
    }
  }
  dat <- dat[stats::complete.cases(dat[, vars, drop = FALSE]), , drop = FALSE]
  if (nrow(dat) < n_before)
    msgs <- c(msgs, sprintf("%d row(s) with missing values were removed.", n_before - nrow(dat)))
  rownames(dat) <- NULL
  ts_info <- detect_time_structure(dat$.x, kind)
  list(data = dat, y_names = Y$vars, x_names = X$vars,
       freq = ts_info$freq, unit = ts_info$unit, kind = kind,
       time_name = y_time, time_desc = if (kind == Y$kind) Y$desc else "Time labels (ordered as in the file)",
       merge_how = how, messages = msgs)
}

# data as shown to the user: time column exactly as written + parsed value for transparency
display_data <- function(m, dat = m$data) {
  vars <- setdiff(names(dat), TIME_COLS)
  out <- data.frame(dat$.label, dat[, vars, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[1] <- m$time_name
  if (m$kind != "index") {
    parsed <- format(dat$.x, if (m$kind == "datetime") "%Y-%m-%d %H:%M" else "%Y-%m-%d")
    if (!identical(parsed, dat$.label)) out <- data.frame(out[1], `Read as` = parsed, out[-1], check.names = FALSE)
  }
  out
}

# ------------------------------------------------------------
# 6. DESCRIPTIVE STATISTICS
# ------------------------------------------------------------

stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", ""))))
}

descriptive_stats <- function(dat, vars, roles) {
  out <- lapply(seq_along(vars), function(i) {
    v <- dat[[vars[i]]]
    v <- v[is.finite(v)]
    jb  <- tryCatch(moments::jarque.test(v), error = function(e) NULL)
    adf <- tryCatch(suppressWarnings(tseries::adf.test(v)), error = function(e) NULL)
    sw  <- tryCatch(if (length(v) >= 3 && length(v) <= 5000) stats::shapiro.test(v) else NULL, error = function(e) NULL)
    data.frame(
      Variable = vars[i], Role = roles[i], N = length(v),
      Mean = mean(v), Median = stats::median(v), SD = stats::sd(v),
      Min = min(v), Q1 = unname(stats::quantile(v, .25)),
      Q3 = unname(stats::quantile(v, .75)), Max = max(v),
      CV_percent = 100 * stats::sd(v) / abs(mean(v)),
      Skewness = moments::skewness(v),
      Kurtosis = moments::kurtosis(v),
      Jarque_Bera = if (is.null(jb)) NA else unname(jb$statistic),
      JB_p = if (is.null(jb)) NA else jb$p.value,
      Shapiro_W = if (is.null(sw)) NA else unname(sw$statistic),
      SW_p = if (is.null(sw)) NA else sw$p.value,
      ADF = if (is.null(adf)) NA else unname(adf$statistic),
      ADF_p = if (is.null(adf)) NA else adf$p.value,
      check.names = FALSE, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# ------------------------------------------------------------
# 7. BDS NON-LINEARITY TEST ON ARIMAX RESIDUALS
# ------------------------------------------------------------

run_bds <- function(y, x, y_name, x_name, freq, max_m = 5) {
  fit <- tryCatch(
    forecast::auto.arima(stats::ts(y, frequency = freq), xreg = matrix(x, ncol = 1, dimnames = list(NULL, "x")),
                         seasonal = FALSE, stepwise = FALSE, approximation = FALSE),
    error = function(e) NULL)
  if (is.null(fit)) stop("ARIMAX model could not be fitted for BDS test.")
  res <- as.numeric(stats::residuals(fit))
  res <- res[is.finite(res)]
  arima_txt <- paste0("ARIMA(", paste(forecast::arimaorder(fit)[1:3], collapse = ","), ") with xreg")
  sdr <- stats::sd(res)
  mult <- c(0.5, 1, 1.5, 2)
  tst <- tseries::bds.test(res, m = max_m, eps = mult * sdr)
  stat <- as.matrix(tst$statistic); pv <- as.matrix(tst$p.value)
  ms <- as.integer(rownames(stat))
  out <- expand.grid(i = seq_along(ms), j = seq_along(mult))
  out <- data.frame(
    Response = y_name, Exposure = x_name, ARIMAX = arima_txt,
    Embedding_Dimension = ms[out$i],
    Epsilon_x_SD = mult[out$j], Epsilon = mult[out$j] * sdr,
    BDS_Statistic = stat[cbind(out$i, out$j)],
    P_Value = pv[cbind(out$i, out$j)], stringsAsFactors = FALSE)
  out <- out[order(out$Embedding_Dimension, out$Epsilon_x_SD), ]
  out$Signif <- stars(out$P_Value)
  out$Decision <- ifelse(out$P_Value < 0.05, "Reject i.i.d. (non-linear dependence)", "Fail to reject i.i.d.")
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 8. DLNM HELPERS
# ------------------------------------------------------------

basis_args <- function(fun, df, degree) {
  switch(fun,
         ps = list(fun = "ps", df = df, degree = degree),
         cr = list(fun = "cr", df = df),
         ns = list(fun = "ns", df = df),
         bs = list(fun = "bs", df = df, degree = degree),
         poly = list(fun = "poly", degree = degree),
         lin = list(fun = "lin"),
         stop("Unsupported basis: ", fun))
}

parse_num_list <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(numeric())
  v <- suppressWarnings(as.numeric(strsplit(gsub(";", ",", txt), ",")[[1]]))
  v[!is.na(v)]
}

prediction_points <- function(x, s) {
  if (s$prediction_method == "percentiles") {
    pc <- sort(unique(pmin(100, pmax(0, s$percentiles))))
    if (!length(pc)) pc <- c(5, 25, 50, 75, 95)
    vals <- as.numeric(stats::quantile(x, pc / 100))
    labs <- paste0(format(pc, trim = TRUE), "th percentile")
  } else if (s$prediction_method == "sequence") {
    vals <- seq(min(x), max(x), length.out = max(3, s$grid_points))
    labs <- paste0("Value = ", signif(vals, 5))
  } else {
    vals <- sort(unique(s$custom_values))
    if (!length(vals)) stop("Please supply at least one custom exposure value.")
    vals <- vals[vals >= min(x) & vals <= max(x)]
    if (!length(vals)) stop(sprintf("All custom exposure values lie outside the observed range (%.4g to %.4g).", min(x), max(x)))
    labs <- paste0("Value = ", signif(vals, 5))
  }
  keep <- !duplicated(round(vals, 10))
  list(values = vals[keep], labels = labs[keep])
}

reference_value <- function(x, s) {
  switch(s$reference,
         zero = 0, median = stats::median(x), mean = mean(x),
         quantile = as.numeric(stats::quantile(x, s$reference_quantile)),
         custom = s$reference_custom, stats::median(x))
}

orient <- function(m, n_rows) {
  m <- as.matrix(m)
  if (nrow(m) != n_rows) m <- t(m)
  m
}

collapse_value <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.numeric(x)) return(paste(round(x, 6), collapse = "; "))
  paste(x, collapse = "; ")
}

# ------------------------------------------------------------
# 9. FIT ONE DLNM (one response x one exposure)
# ------------------------------------------------------------

fit_dlnm_pair <- function(dat, y_name, x_name, s) {
  n <- nrow(dat)
  y <- dat[[y_name]]
  x <- dat[[x_name]]
  if (s$max_lag >= n / 2) stop("Maximum lag (", s$max_lag, ") is too large for ", n, " observations.")

  argvar <- basis_args(s$exposure_basis, s$exposure_df, s$spline_degree)
  lag_df <- if (s$lag_basis %in% c("ps", "cr", "ns", "bs")) min(s$lag_df, s$max_lag + 1) else s$lag_df
  arglag <- basis_args(s$lag_basis, lag_df, s$spline_degree)
  cb <- dlnm::crossbasis(x, lag = s$max_lag, argvar = argvar, arglag = arglag)
  pen <- tryCatch(dlnm::cbPen(cb), error = function(e) NULL)

  md <- data.frame(y = y, time_index = seq_len(n))
  md$season <- season_index(dat, s$freq, s$unit %||% "periods")
  md$season_f <- factor(md$season)
  md$cb <- cb

  rhs <- "cb"
  if (s$trend == "linear") rhs <- c(rhs, "time_index")
  else if (s$trend == "quadratic") rhs <- c(rhs, "time_index", "I(time_index^2)")
  else if (s$trend == "smooth") {
    k <- max(3, min(floor(n / 5), ceiling(s$trend_df_per_year * n / s$freq) + 1))
    rhs <- c(rhs, paste0("s(time_index, k=", k, ", bs='tp')"))
  }
  knots <- NULL
  n_season <- length(unique(md$season))
  if (s$seasonality != "none" && n_season < 2) {
    s$seasonality <- "none"
  }
  if (s$seasonality == "factor") rhs <- c(rhs, "season_f")
  else if (s$seasonality == "cyclic") {
    kc <- min(n_season, 12)
    rhs <- c(rhs, paste0("s(season, bs='cc', k=", kc, ")"))
    knots <- list(season = c(0.5, max(md$season) + 0.5))
  }
  form <- stats::as.formula(paste("y ~", paste(rhs, collapse = " + ")))

  fit <- mgcv::gam(form, data = md, family = stats::gaussian(link = "identity"),
                   method = s$gam_method, knots = knots,
                   paraPen = if (!is.null(pen)) list(cb = pen) else NULL,
                   na.action = stats::na.omit)

  cen <- reference_value(x, s)
  pts <- prediction_points(x, s)
  pred <- dlnm::crosspred(cb, model = fit, cen = cen, at = pts$values,
                          cumul = TRUE, ci.level = s$confidence_level)
  # fine grid for surfaces and smooth exposure-response curves
  grid <- seq(min(x), max(x), length.out = 60)
  pred_grid <- dlnm::crosspred(cb, model = fit, cen = cen, at = grid,
                               cumul = FALSE, ci.level = s$confidence_level)

  zq <- stats::qnorm(1 - (1 - s$confidence_level) / 2)
  lags <- seq(pred$lag[1], pred$lag[2])
  pv <- as.numeric(pred$predvar)
  lab <- pts$labels[match(round(pv, 10), round(pts$values, 10))]

  # --- cumulative effects
  ce <- data.frame(
    Response = y_name, Exposure = x_name, Point = lab, Exposure_Value = pv,
    Reference = cen,
    Cumulative_Effect = as.numeric(pred$allfit),
    CI_Lower = as.numeric(pred$alllow), CI_Upper = as.numeric(pred$allhigh),
    stringsAsFactors = FALSE)
  se <- (ce$CI_Upper - ce$CI_Lower) / (2 * zq)
  se[se <= 1e-12] <- NA                       # reference point: effect fixed at 0
  ce$SE <- se
  ce$Z <- ce$Cumulative_Effect / se
  ce$P_value <- 2 * stats::pnorm(abs(ce$Z), lower.tail = FALSE)
  ce$Signif <- stars(ce$P_value)

  # --- selected lag-specific effects
  fitm <- orient(pred$matfit, length(pv))
  lowm <- orient(pred$matlow, length(pv))
  higm <- orient(pred$mathigh, length(pv))
  sel <- sort(unique(s$selected_lags[s$selected_lags >= 0 & s$selected_lags <= s$max_lag]))
  if (!length(sel)) sel <- unique(round(seq(0, s$max_lag, length.out = 5)))
  li <- match(sel, lags)
  le <- do.call(rbind, lapply(seq_along(pv), function(i) {
    data.frame(Response = y_name, Exposure = x_name, Point = lab[i], Exposure_Value = pv[i],
               Lag = sel, Effect = fitm[i, li], CI_Lower = lowm[i, li], CI_Upper = higm[i, li],
               stringsAsFactors = FALSE)
  }))
  le$SE <- (le$CI_Upper - le$CI_Lower) / (2 * zq)
  le$SE[le$SE <= 1e-12] <- NA
  rownames(le) <- NULL
  le$Z <- le$Effect / le$SE
  le$P_value <- 2 * stats::pnorm(abs(le$Z), lower.tail = FALSE)
  le$Signif <- stars(le$P_value)

  # --- diagnostics
  res <- as.numeric(stats::residuals(fit))
  lb_lag <- max(1, min(s$lb_lag, floor(length(res) / 4)))
  lb <- stats::Box.test(res, lag = lb_lag, type = "Ljung-Box")
  jb <- tryCatch(moments::jarque.test(res), error = function(e) NULL)
  adf <- tryCatch(suppressWarnings(tseries::adf.test(res, alternative = "stationary")), error = function(e) NULL)
  bp <- tryCatch({
    bpd <- data.frame(residual_sq = res^2, fitted = as.numeric(stats::fitted(fit)))
    lmtest::bptest(stats::lm(residual_sq ~ fitted, data = bpd))
  }, error = function(e) NULL)
  sm <- summary(fit)
  diag <- data.frame(
    Response = y_name, Exposure = x_name, N_used = length(res),
    AIC = stats::AIC(fit), BIC = stats::BIC(fit),
    Smoothing_Score = if (is.null(fit$gcv.ubre)) NA else as.numeric(fit$gcv.ubre),
    Deviance_Explained = sm$dev.expl, Adjusted_R2 = sm$r.sq,
    Effective_DF = sum(fit$edf, na.rm = TRUE),
    Ljung_Box = unname(lb$statistic), LB_lag = lb_lag, LB_p = lb$p.value,
    Jarque_Bera = if (is.null(jb)) NA else unname(jb$statistic), JB_p = if (is.null(jb)) NA else jb$p.value,
    ADF = if (is.null(adf)) NA else unname(adf$statistic), ADF_p = if (is.null(adf)) NA else adf$p.value,
    Breusch_Pagan = if (is.null(bp)) NA else unname(bp$statistic), BP_p = if (is.null(bp)) NA else bp$p.value,
    stringsAsFactors = FALSE)

  # --- specification
  av <- attr(cb, "argvar"); al <- attr(cb, "arglag")
  spec <- data.frame(
    Response = y_name, Exposure = x_name,
    Specification = c(
      "Outcome transformation", "Exposure transformation", "Distribution", "Link function",
      "Model framework", "Estimation method", "Model formula",
      "Exposure basis", "Exposure df", "Exposure degree", "Exposure knots",
      "Lag basis", "Lag df", "Lag degree", "Lag knots", "Exposure range", "Lag range",
      "Cross-basis dimensions", "Penalised cross-basis", "Number of smoothing parameters",
      "Smoothing parameters", "Smoothing criterion score", "Effective degrees of freedom",
      "Deviance explained", "Adjusted R2", "AIC", "BIC", "Reference (centering) value",
      "Prediction method", "Prediction points", "Cumulative prediction", "Confidence level",
      "Selected lags", "Time trend", "Seasonality", "Time structure", "Observations"),
    Value = c(
      transform_label(s$y_transform), transform_label(s$x_transform),
      fit$family$family, fit$family$link,
      if (!is.null(pen)) "Gaussian GAM + penalised DLNM" else "Gaussian GAM + DLNM (unpenalised cross-basis)",
      s$gam_method, paste(deparse(form, width.cutoff = 500), collapse = " "),
      av$fun, collapse_value(av$df), collapse_value(av$degree), collapse_value(av$knots),
      al$fun, collapse_value(al$df), collapse_value(al$degree), collapse_value(al$knots),
      collapse_value(range(x)), paste(attr(cb, "lag"), collapse = " - "),
      paste(dim(cb), collapse = " x "), if (!is.null(pen)) "Yes" else "No",
      length(fit$sp), collapse_value(fit$sp),
      collapse_value(fit$gcv.ubre), round(sum(fit$edf), 4), round(sm$dev.expl, 4), round(sm$r.sq, 4),
      round(stats::AIC(fit), 4), round(stats::BIC(fit), 4),
      paste0(round(cen, 4), " (", s$reference, ")"),
      s$prediction_method, collapse_value(pv), "Yes",
      paste0(100 * s$confidence_level, "%"), paste(sel, collapse = ", "),
      s$trend, s$seasonality, paste0("time unit: ", s$unit %||% "periods", "; seasonal period: ", s$freq), n),
    stringsAsFactors = FALSE)

  list(pred = pred, pred_grid = pred_grid, labels = lab, cen = cen,
       cumulative = ce, lag_effects = le, diagnostics = diag, spec = spec,
       fit = fit, selected_lags = sel)
}
