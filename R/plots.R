# ============================================================
# DLNM-Shock Analyzer : plotting functions
#   - interactive (plotly) versions for the web page
#   - publication (PNG) versions for download
# ============================================================

pal_surface <- c("#002B7F", "#0057B8", "#008ED6", "#00C6D7", "#00BFA5", "#18C97E",
                 "#66D52F", "#C8E629", "#FFE600", "#FFC400", "#FF8C00", "#FF3D00",
                 "#D50000", "#9B0000")
pal_series <- c("#0B5D7A", "#E07A1F", "#2E8B57", "#C0392B", "#7D3C98", "#1F77B4",
                "#B8860B", "#D35400", "#16A085", "#8E44AD", "#2C3E50", "#E84393")
pal_div <- c("#0000FF", "#4D4DE0", "#8080CC", "#B3B3F2", "#E6E6FF", "#FFFFFF",
             "#FFE6E6", "#FFB3B3", "#FF8080", "#FF4D4D", "#FF0000")

series_cols <- function(n) {
  if (n <= length(pal_series)) pal_series[seq_len(n)] else grDevices::colorRampPalette(pal_series)(n)
}

hex_rgba <- function(hex, a) {
  v <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], a)
}

plotly_theme <- function(p, xlab = "", ylab = "", legend = TRUE) {
  p |>
    plotly::layout(
      font = list(family = "Inter, Segoe UI, Arial", size = 13, color = "#1f2d3d"),
      xaxis = list(title = xlab, gridcolor = "#e8eef3", zeroline = FALSE),
      yaxis = list(title = ylab, gridcolor = "#e8eef3", zeroline = FALSE),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      hovermode = "x unified", showlegend = legend,
      legend = list(orientation = "h", y = -0.2),
      margin = list(t = 30, r = 20, b = 60, l = 70)
    ) |>
    plotly::config(displaylogo = FALSE,
                   toImageButtonOptions = list(format = "png", scale = 3))
}

# ---------------- descriptive plots ----------------

plot_timeseries <- function(dat, vars, standardise = FALSE, xlab = "Time") {
  p <- plotly::plot_ly()
  cols <- series_cols(length(vars))
  x <- if (is.null(dat$.x)) seq_len(nrow(dat)) else dat$.x
  lab <- if (is.null(dat$.label)) as.character(x) else dat$.label
  for (i in seq_along(vars)) {
    v <- dat[[vars[i]]]
    if (standardise) v <- as.numeric(scale(v))
    p <- plotly::add_lines(p, x = x, y = v, name = vars[i], text = lab,
                           hovertemplate = paste0("%{text}<br>", vars[i], ": %{y:.4g}<extra></extra>"),
                           line = list(color = cols[i], width = 2))
  }
  plotly_theme(p, xlab, if (standardise) "Standardised value (z-score)" else "Value") |>
    plotly::layout(hovermode = "closest")
}

plot_corr <- function(dat, vars) {
  cm <- stats::cor(dat[, vars, drop = FALSE], use = "pairwise.complete.obs")
  plotly::plot_ly(x = vars, y = vars, z = cm, type = "heatmap",
                  zmin = -1, zmax = 1,
                  colorscale = list(c(0, "#B2182B"), c(0.5, "#F7F7F7"), c(1, "#2166AC")),
                  text = round(cm, 3), texttemplate = "%{text}",
                  hovertemplate = "%{y} vs %{x}<br>r = %{z:.3f}<extra></extra>") |>
    plotly_theme(legend = FALSE) |>
    plotly::layout(hovermode = "closest", yaxis = list(autorange = "reversed"),
                   margin = list(l = 140, b = 120))
}

plot_box <- function(dat, vars) {
  p <- plotly::plot_ly()
  cols <- series_cols(length(vars))
  for (i in seq_along(vars)) {
    p <- plotly::add_boxplot(p, y = as.numeric(scale(dat[[vars[i]]])), name = vars[i],
                             marker = list(color = cols[i]), line = list(color = cols[i]),
                             boxmean = TRUE)
  }
  plotly_theme(p, "", "Standardised value (z-score)", legend = FALSE) |>
    plotly::layout(hovermode = "closest")
}

plot_hist <- function(dat, v) {
  plotly::plot_ly(x = dat[[v]], type = "histogram", nbinsx = 25,
                  marker = list(color = "rgba(11,93,122,0.75)", line = list(color = "#fff", width = 1))) |>
    plotly_theme(v, "Frequency", legend = FALSE) |>
    plotly::layout(hovermode = "closest", bargap = 0.05)
}

# ---------------- DLNM interactive plots ----------------

lag_seq <- function(pred) seq(pred$lag[1], pred$lag[2])

plot_surface <- function(res, xlab, lag_lab, eff_lab) {
  pg <- res$pred_grid
  z <- orient(pg$matfit, length(pg$predvar))
  plotly::plot_ly(x = lag_seq(pg), y = as.numeric(pg$predvar), z = z, type = "surface",
                  colorscale = lapply(seq_along(pal_surface), function(i)
                    list((i - 1) / (length(pal_surface) - 1), pal_surface[i])),
                  colorbar = list(title = list(text = eff_lab, side = "right")),
                  hovertemplate = paste0(lag_lab, ": %{x}<br>", xlab, ": %{y:.3f}<br>Effect: %{z:.4f}<extra></extra>"),
                  contours = list(z = list(show = TRUE, usecolormap = TRUE, project = list(z = TRUE)))) |>
    plotly::layout(scene = list(
      xaxis = list(title = lag_lab), yaxis = list(title = xlab), zaxis = list(title = eff_lab),
      camera = list(eye = list(x = -1.6, y = -1.6, z = 0.9))),
      font = list(family = "Inter, Segoe UI, Arial", size = 12),
      paper_bgcolor = "rgba(0,0,0,0)", margin = list(l = 0, r = 0, t = 10, b = 0)) |>
    plotly::config(displaylogo = FALSE, toImageButtonOptions = list(format = "png", scale = 3))
}

plot_contour <- function(res, xlab, lag_lab, eff_lab) {
  pg <- res$pred_grid
  z <- orient(pg$matfit, length(pg$predvar))
  lim <- max(abs(z), na.rm = TRUE)
  plotly::plot_ly(x = as.numeric(pg$predvar), y = lag_seq(pg), z = t(z), type = "contour",
                  zmin = -lim, zmax = lim,
                  colorscale = lapply(seq_along(pal_div), function(i)
                    list((i - 1) / (length(pal_div) - 1), pal_div[i])),
                  contours = list(coloring = "heatmap", showlabels = TRUE,
                                  labelfont = list(size = 10, color = "#333")),
                  line = list(width = 0.6, color = "rgba(60,60,60,0.35)"),
                  colorbar = list(title = list(text = eff_lab, side = "right")),
                  hovertemplate = paste0(xlab, ": %{x:.3f}<br>", lag_lab, ": %{y}<br>Effect: %{z:.4f}<extra></extra>")) |>
    plotly_theme(xlab, lag_lab, legend = FALSE) |>
    plotly::layout(hovermode = "closest")
}

# lag-response curves at each prediction point
plot_lag_slices <- function(res, lag_lab, eff_lab, max_curves = 8) {
  pr <- res$pred
  pv <- as.numeric(pr$predvar)
  idx <- if (length(pv) > max_curves) unique(round(seq(1, length(pv), length.out = max_curves))) else seq_along(pv)
  fit <- orient(pr$matfit, length(pv)); lo <- orient(pr$matlow, length(pv)); hi <- orient(pr$mathigh, length(pv))
  lg <- lag_seq(pr)
  cols <- series_cols(length(idx))
  p <- plotly::plot_ly()
  for (k in seq_along(idx)) {
    i <- idx[k]
    nm <- paste0(res$labels[i], " (", signif(pv[i], 4), ")")
    p <- p |>
      plotly::add_ribbons(x = lg, ymin = lo[i, ], ymax = hi[i, ], name = nm, legendgroup = nm,
                          showlegend = FALSE, fillcolor = hex_rgba(cols[k], 0.13),
                          line = list(width = 0), hoverinfo = "skip") |>
      plotly::add_lines(x = lg, y = fit[i, ], name = nm, legendgroup = nm,
                        line = list(color = cols[k], width = 3))
  }
  plotly_theme(p, lag_lab, eff_lab) |>
    plotly::layout(shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                                      line = list(dash = "dash", color = "grey"))))
}

# exposure-response curves at selected lags
plot_exposure_response <- function(res, xlab, eff_lab) {
  pg <- res$pred_grid
  pv <- as.numeric(pg$predvar)
  fit <- orient(pg$matfit, length(pv)); lo <- orient(pg$matlow, length(pv)); hi <- orient(pg$mathigh, length(pv))
  lg <- lag_seq(pg)
  sel <- res$selected_lags
  cols <- series_cols(length(sel))
  p <- plotly::plot_ly()
  for (k in seq_along(sel)) {
    j <- match(sel[k], lg)
    nm <- paste("Lag", sel[k])
    p <- p |>
      plotly::add_ribbons(x = pv, ymin = lo[, j], ymax = hi[, j], name = nm, legendgroup = nm,
                          showlegend = FALSE, fillcolor = hex_rgba(cols[k], 0.13),
                          line = list(width = 0), hoverinfo = "skip") |>
      plotly::add_lines(x = pv, y = fit[, j], name = nm, legendgroup = nm,
                        line = list(color = cols[k], width = 3))
  }
  plotly_theme(p, xlab, eff_lab) |>
    plotly::layout(shapes = list(
      list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0, line = list(dash = "dash", color = "grey")),
      list(type = "line", yref = "paper", y0 = 0, y1 = 1, x0 = res$cen, x1 = res$cen, line = list(dash = "dot", color = "#999"))))
}

# overall cumulative exposure-response (sum over all lags)
plot_cumulative <- function(res, xlab, eff_lab) {
  pg <- res$pred_grid
  pv <- as.numeric(pg$predvar)
  ce <- res$cumulative
  plotly::plot_ly() |>
    plotly::add_ribbons(x = pv, ymin = as.numeric(pg$alllow), ymax = as.numeric(pg$allhigh),
                        name = "Confidence band", fillcolor = "rgba(11,93,122,0.18)",
                        line = list(width = 0), hoverinfo = "skip") |>
    plotly::add_lines(x = pv, y = as.numeric(pg$allfit), name = "Cumulative effect",
                      line = list(color = "#0B5D7A", width = 3)) |>
    plotly::add_markers(x = ce$Exposure_Value, y = ce$Cumulative_Effect, name = "Prediction points",
                        text = ce$Point, marker = list(color = "#E07A1F", size = 10, line = list(color = "#fff", width = 1.5)),
                        hovertemplate = "%{text}<br>x = %{x:.3f}<br>Effect = %{y:.4f}<extra></extra>") |>
    plotly_theme(xlab, paste("Cumulative", tolower(eff_lab))) |>
    plotly::layout(shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                                      line = list(dash = "dash", color = "grey"))))
}

# ============================================================
# PUBLICATION FIGURES (PNG) - based on the original R script
# ============================================================

save_publication_figures <- function(res, out_dir, prefix, xlab, lag_lab, eff_lab, dpi = 300, unit = "periods") {
  files <- character()
  pr <- res$pred; pg <- res$pred_grid
  pv <- as.numeric(pr$predvar); gv <- as.numeric(pg$predvar)
  lg <- lag_seq(pr)
  zg <- orient(pg$matfit, length(gv))
  zlim <- range(zg, na.rm = TRUE)
  if (diff(zlim) == 0) zlim <- zlim + c(-1, 1) * 1e-6
  lag_axis <- pretty(lg)

  # 1. 3D surface with continuous legend
  f <- file.path(out_dir, paste0(prefix, "_3D_Surface.png"))
  grDevices::png(f, width = 15, height = 8, units = "in", res = dpi, type = "cairo")
  graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(5, 1))
  graphics::par(mar = c(2, 2, 1, 1))
  zf <- (zg[-1, -1] + zg[-1, -ncol(zg)] + zg[-nrow(zg), -1] + zg[-nrow(zg), -ncol(zg)]) / 4
  pal100 <- grDevices::colorRampPalette(pal_surface)(100)
  ci <- pmax(1, pmin(100, as.integer(1 + (zf - zlim[1]) / diff(zlim) * 99)))
  graphics::persp(x = gv, y = lg, z = zg, col = pal100[ci], border = NA, theta = 225, phi = 35,
                  r = 3, d = 4, expand = 0.9, shade = 0.4, ticktype = "detailed",
                  xlab = xlab, ylab = lag_lab, zlab = eff_lab, zlim = zlim, box = TRUE)
  graphics::par(mar = c(2, 2, 1, 1))
  fields::image.plot(legend.only = TRUE, zlim = zlim, col = grDevices::colorRampPalette(pal_surface)(100),
                     legend.lab = eff_lab, legend.line = 2.5,
                     axis.args = list(at = pretty(zlim, 6), labels = format(pretty(zlim, 6), digits = 3), las = 1))
  grDevices::dev.off(); files <- c(files, f)

  # 2. contour
  f <- file.path(out_dir, paste0(prefix, "_Contour.png"))
  grDevices::png(f, width = 10, height = 10, units = "in", res = dpi, type = "cairo")
  lim <- max(abs(zlim))
  graphics::filled.contour(
    x = gv, y = lg, z = zg, levels = seq(-lim, lim, length.out = 22),
    color.palette = function(n) grDevices::colorRampPalette(pal_div)(n),
    plot.title = graphics::title(xlab = xlab, ylab = lag_lab),
    plot.axes = { graphics::axis(1); graphics::axis(2, at = lag_axis); graphics::box() },
    key.title = graphics::title(main = eff_lab, cex.main = 0.8))
  grDevices::dev.off(); files <- c(files, f)

  pub_par <- function() graphics::par(mar = c(5.5, 5.5, 2, 2), mgp = c(3, 1, 0), tcl = -0.3,
                                      cex.axis = 1, cex.lab = 1.15, family = "sans")

  # 3. lag-response at prediction points
  f <- file.path(out_dir, paste0(prefix, "_Lag_Response.png"))
  grDevices::png(f, width = 10, height = 8, units = "in", res = dpi, type = "cairo")
  pub_par()
  fit <- orient(pr$matfit, length(pv)); lo <- orient(pr$matlow, length(pv)); hi <- orient(pr$mathigh, length(pv))
  idx <- if (length(pv) > 8) unique(round(seq(1, length(pv), length.out = 8))) else seq_along(pv)
  cols <- series_cols(length(idx))
  yl <- range(c(lo[idx, ], hi[idx, ]), na.rm = TRUE); yl <- yl + c(-1, 1) * 0.1 * diff(yl)
  graphics::plot(lg, fit[idx[1], ], type = "n", ylim = yl, xlab = lag_lab, ylab = eff_lab, xaxt = "n")
  graphics::axis(1, at = lag_axis)
  graphics::abline(h = 0, col = "grey40", lty = 2, lwd = 1.5)
  for (k in seq_along(idx)) graphics::polygon(c(lg, rev(lg)), c(lo[idx[k], ], rev(hi[idx[k], ])),
                                              col = grDevices::adjustcolor(cols[k], 0.15), border = NA)
  for (k in seq_along(idx)) graphics::lines(lg, fit[idx[k], ], col = cols[k], lwd = 3)
  graphics::legend("topright", legend = paste0(res$labels[idx], " (", round(pv[idx], 3), ")"),
                   col = cols, lwd = 3, bty = "n", cex = 0.85)
  graphics::box(); grDevices::dev.off(); files <- c(files, f)

  # 4. exposure-response at selected lags
  f <- file.path(out_dir, paste0(prefix, "_Exposure_Response_by_Lag.png"))
  grDevices::png(f, width = 10, height = 8, units = "in", res = dpi, type = "cairo")
  pub_par()
  fg <- orient(pg$matfit, length(gv)); lgw <- orient(pg$matlow, length(gv)); hgw <- orient(pg$mathigh, length(gv))
  sel <- res$selected_lags; li <- match(sel, lg); cols <- series_cols(length(sel))
  ym <- max(abs(c(lgw[, li], hgw[, li])), na.rm = TRUE)
  graphics::plot(gv, fg[, li[1]], type = "n", xlab = xlab, ylab = eff_lab, ylim = c(-ym, ym))
  graphics::abline(h = 0, col = "grey40", lty = 2, lwd = 1.5)
  for (k in seq_along(li)) graphics::polygon(c(gv, rev(gv)), c(lgw[, li[k]], rev(hgw[, li[k]])),
                                             col = grDevices::adjustcolor(cols[k], 0.15), border = NA)
  for (k in seq_along(li)) graphics::lines(gv, fg[, li[k]], col = cols[k], lwd = 3)
  graphics::legend("topright", legend = paste("Lag", sel), col = cols, lwd = 3, bty = "n", cex = 0.9)
  graphics::box(); grDevices::dev.off(); files <- c(files, f)

  # 5. overall cumulative
  f <- file.path(out_dir, paste0(prefix, "_Cumulative_Effect.png"))
  grDevices::png(f, width = 10, height = 8, units = "in", res = dpi, type = "cairo")
  pub_par()
  yl <- range(c(pg$alllow, pg$allhigh), na.rm = TRUE)
  graphics::plot(gv, pg$allfit, type = "n", ylim = yl, xlab = xlab, ylab = paste("Cumulative", tolower(eff_lab)))
  graphics::polygon(c(gv, rev(gv)), c(pg$alllow, rev(pg$allhigh)), col = grDevices::adjustcolor("#0B5D7A", 0.18), border = NA)
  graphics::abline(h = 0, col = "grey40", lty = 2, lwd = 1.5)
  graphics::lines(gv, pg$allfit, col = "#0B5D7A", lwd = 3)
  graphics::points(res$cumulative$Exposure_Value, res$cumulative$Cumulative_Effect, pch = 21, bg = "#E07A1F", cex = 1.6)
  graphics::box(); grDevices::dev.off(); files <- c(files, f)

  files
}
