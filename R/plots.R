# ------------------------------------------------------------
# Plots drawn by the procedures (once, when the result is computed).
# All go through .with_plot() so options(statcram.plot = FALSE) turns
# them off and a plotting failure never hides the numbers.
# ------------------------------------------------------------

.density_fun <- function(dist, df = NULL, mean = 0, sd = 1) {
  switch(dist,
    z = function(x) dnorm(x, mean, sd),
    t = function(x) dt((x - mean) / sd, df) / sd,
    chisq = function(x) dchisq(x, df),
    f = function(x) stats::df(x, df[1], df[2]))
}

.shade <- function(f, lo, hi, col) {
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) return(invisible())
  xs <- seq(lo, hi, length.out = 200)
  polygon(c(lo, xs, hi), c(0, f(xs), 0), col = col, border = NA)
}

# Sampling distribution of the test statistic with the rejection region
# shaded and the observed statistic marked.
.plot_test <- function(stat, alt, alpha, dist = "z", df = NULL, main = "") {
  .with_plot(function() {
    f <- .density_fun(dist, df)
    if (dist == "chisq") {
      crit <- qchisq(1 - alpha, df)
      hi <- max(qchisq(0.999, df), stat * 1.1, crit * 1.2)
      xr <- c(0, hi)
    } else {
      crit <- .crit(alt, alpha, dist, df)
      lim <- max(4, abs(stat) * 1.15)
      xr <- c(-lim, lim)
    }
    xs <- seq(xr[1], xr[2], length.out = 400)
    plot(xs, f(xs), type = "l", lwd = 2, xlab = if (dist == "z") "Z" else if (dist == "t") "t" else "chi-square",
         ylab = "density", main = main, las = 1)
    red <- grDevices::adjustcolor("firebrick", 0.45)
    if (dist == "chisq" || alt == "greater") .shade(f, max(crit), xr[2], red)
    if (dist != "chisq" && alt == "less") .shade(f, xr[1], crit, red)
    if (dist != "chisq" && alt == "two.sided") {
      .shade(f, xr[1], crit[1], red); .shade(f, crit[2], xr[2], red)
    }
    abline(v = stat, col = "navy", lwd = 2, lty = 2)
    legend("topright", bty = "n", cex = 0.8,
           legend = c(sprintf("rejection region (alpha = %s)", .f(alpha)), sprintf("observed = %s", .f(stat))),
           fill = c(red, NA), border = c(NA, NA), lty = c(NA, 2), col = c(NA, "navy"), lwd = c(NA, 2))
  })
}

# Probability as a shaded area under a density curve.
.plot_area <- function(dist, lo, hi, mean = 0, sd = 1, df = NULL, main = "", xlab = "x") {
  .with_plot(function() {
    f <- .density_fun(dist, df, mean, sd)
    xr <- switch(dist,
      z = mean + c(-4, 4) * sd,
      t = mean + c(-1, 1) * max(4, qt(0.995, df)) * sd,
      chisq = c(0, qchisq(0.999, df)))
    xr <- range(c(xr, lo[is.finite(lo)], hi[is.finite(hi)]))
    xs <- seq(xr[1], xr[2], length.out = 400)
    plot(xs, f(xs), type = "l", lwd = 2, xlab = xlab, ylab = "density", main = main, las = 1)
    col <- grDevices::adjustcolor("steelblue", 0.5)
    for (i in seq_along(lo)) .shade(f, max(lo[i], xr[1]), min(hi[i], xr[2]), col)
  })
}
