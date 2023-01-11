library(mcmcplots)

# From package mcmcplots, modified
mcmcplot_new <- function (mcmcout, parms = NULL, regex = NULL, random = NULL, 
          leaf.marker = "[\\[_]", dir = tempdir(), filename = "MCMCoutput", 
          extension = "html", title = NULL, heading = title, 
          col = NULL, lty = 1, xlim = NULL, ylim = NULL, style = c("gray", 
                                                                   "plain"), greek = FALSE) {
  if (is.null(title)) 
    title <- paste("MCMC Plots: ", deparse(substitute(mcmcout)), 
                   sep = "")
  if (is.null(heading)) 
    heading <- title
  style <- match.arg(style)
  current.devices <- dev.list()
  on.exit(sapply(dev.list(), function(dev) if (!(dev %in% current.devices)) dev.off(dev)))
  mcmcout <- convert.mcmc.list(mcmcout)
  nchains <- length(mcmcout)
  if (is.null(col)) {
    col <- mcmcplotsPalette(nchains)
  }
  css.file <- system.file("MCMCoutput.css", package = "mcmcplots")
  css.file <- paste("file:///", css.file, sep = "")
  htmlfile <- mcmcplots:::.html.begin(dir, filename, extension, title = title, cssfile = css.file)
  if (is.null(varnames(mcmcout))) {
    warning("Argument 'mcmcout' did not have valid variable names, so names have been created for you.")
    varnames(mcmcout) <- varnames(mcmcout, allow.null = FALSE)
  }
  parnames <- parms2plot(varnames(mcmcout), parms, regex, random, 
                         leaf.marker, do.unlist = FALSE)
  if (length(parnames) == 0) 
    stop("No parameters matched arguments 'parms' or 'regex'.")
  np <- length(unlist(parnames))
  cat("\n<div id=\"outer\">\n", file = htmlfile, append = TRUE)
  cat("<h1>", heading, "</h1>", sep = "", 
      file = htmlfile, append = TRUE)
  # cat("<div id=\"toc\">\n", file = htmlfile, append = TRUE)
  # #cat("\n<h2>Table of Contents</h2>", file = htmlfile, 
  # #    append = TRUE)
  # cat("<ul id=\"toc_items\">\n", file = htmlfile, append = TRUE)
  # for (group.name in names(parnames)) {
  #   cat(sprintf("<li class=\"toc_item\"><a href=\"#%s\">%s</a></li>\n", 
  #               group.name, group.name), file = htmlfile, append = TRUE)
  # }
  # cat("</ul></div>\n", file = htmlfile, append = TRUE)
  cat("<style> img {padding: 0 !important; display: block !important; margin: 0 auto !important;max-width: 100% !important;} </style>\n", file = htmlfile, append = TRUE)
  cat("<div class=\"main\">\n", file = htmlfile, append = TRUE)
  htmlwidth <- 1600
  htmlheight <- 1000
  for (group.name in names(parnames)) {
    # cat(sprintf("<h2><a name=\"%s\">Plots for %s</a></h2>\n", 
    #             group.name, group.name), file = htmlfile, append = TRUE)
    for (p in parnames[[group.name]]) {
      pctdone <- round(100 * match(p, unlist(parnames))/np)
      cat("\r", rep(" ", getOption("width")), 
          sep = "")
      cat("\rPreparing plots for ", group.name, ".  ", 
          pctdone, "% complete.", sep = "")
      gname <- paste(p, ".png", sep = "")
      png(file.path(dir, gname), width = htmlwidth, height = htmlheight, type = "cairo")
      plot_err <- tryCatch({
        mcmcplot1_new(mcmcout[, p, drop = FALSE], col = col, 
                  lty = lty, xlim = xlim, ylim = ylim, style = style, 
                  greek = greek, cex = 6)
      }, error = function(e) {
        e
      })
      dev.off()
      if (inherits(plot_err, "error")) {
        cat(sprintf("<p class=\"plot_err\">%s. %s</p>", 
                    p, plot_err), file = htmlfile, append = TRUE)
      }
      else {
        mcmcplots:::.html.img(file = htmlfile, class = "mcmcplot", 
                  src = substring(file.path(dir, gname), 5))#, height = htmlheight) # , width = htmlwidth
      }
      if (p != parnames[[length(parnames)]])
        cat("<hr>\n", file = htmlfile, append = TRUE)
    }
  }
  cat("\r", rep(" ", getOption("width")), 
      "\r", sep = "")
  cat("\n</div>\n</div>\n", file = htmlfile, append = TRUE)
  mcmcplots:::.html.end(htmlfile)
  #full.name.path <- paste("file://", htmlfile, sep = "")
  #browseURL(full.name.path)
  invisible(htmlfile)
}

mcmcplot1_new <- function (x, col = mcmcplotsPalette(n), lty = 1, xlim = NULL, 
                           ylim = NULL, style = c("gray", "plain"), greek = FALSE, cex = NULL) {
  x <- convert.mcmc.list(x)
  style <- match.arg(style)
  n <- length(x)
  parname <- varnames(x)
  label <- parname
  if (greek) {
    label <- .to.greek(label)
  }
  opar <- par(mar = c(5, 4.5, 2, 1) + 0.2, oma = c(3, 0, 4, 0) + 
                0.1, cex.axis=1.5, cex.lab=2.5, cex.main=1.2, cex.sub=1)
  on.exit(par(opar))
  layout(matrix(c(1, 2, 1, 3, 4, 4), 3, 2, byrow = TRUE))
  denoverplot1_new(x, col = "steelblue", lty = lty, lwd = 4, xlim = xlim, ylim = ylim, 
               style = style, xlab = label, ylab = "Density", cex = cex)
  autplot1_new(x, style = style, col = "steelblue", cex = cex, lwd = 5)
  rmeanplot1_new(x, style = style, col = "steelblue", cex = cex, lwd = 4)
  traplot1_new(x, col = "steelblue", lty = lty, style = style, ylab = label, 
           xlab = "Iteration", cex = cex, lwd = 4)
  if (greek) {
    title(parse(text = label), cex.main = 1.2, outer = TRUE)
  }
  else {
     title(paste("Diagnostics for ", parname, sep = ""), 
           cex.main = 4, outer = TRUE)
  }
}

denoverplot1_new <- function (..., ci = NULL, col = NULL, lty = 1, lwd = 1, xlim = NULL, ylim = NULL, 
                              xlab = "", ylab = "Density", main = NULL, style = c("gray", 
                                                                                  "plain"), cex = cex, gpar = NULL) {
  dat <- list(...)
  style <- match.arg(style)
  if (length(dat) == 1 && is.list(dat[[1]])) 
    dat <- dat[[1]]
  n <- length(dat)
  if (n == 1 && is.list(dat[[1]])) {
    dat <- dat[[1]]
    n <- length(dat)
  }
  if (is.null(col)) {
    col <- mcmcplotsPalette(n)
  }
  denout <- lapply(dat, density, bw = "SJ")
  xx <- sapply(denout, function(den) den$x)
  yy <- sapply(denout, function(den) den$y)
  if (style == "plain") 
    do.call("matplot", c(list(x = xx, y = yy, col = col, 
                              lty = lty, lwd = lwd, xlim = xlim, ylim = ylim, xlab = xlab, 
                              ylab = ylab, main = main, type = "l", cex = cex), gpar))
  if (style == "gray") {
    do.call("matplot", c(list(x = xx, y = yy, type = "n", 
                              bty = "n", xaxt = "n", yaxt = "n", 
                              xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, 
                              main = main, cex = cex)))
    .graypr_new(cex = cex)
    do.call("matlines", c(list(x = xx, y = yy, col = col, 
                               lty = lty, lwd = lwd, cex = cex), gpar))
  }
  if (!is.null(ci)) {
    lb <- sapply(dat, quantile, (1 - ci)/2)
    ub <- sapply(dat, quantile, ci + (1 - ci)/2)
    do.call("abline", c(list(v = lb, col = col, lty = lty), 
                        gpar))
    do.call("abline", c(list(v = ub, col = col, lty = lty), 
                        gpar))
  }
}

autplot1_new <- function (x, chain = 1, lag.max = NULL, partial = FALSE, col = mcmcplotsPalette(1), 
                          style = c("gray", "plain"), ylim = NULL, cex = NULL, lwd = NULL, ...) {
  style <- match.arg(style)
  if (partial) {
    ylab <- "Partial Autocorrelation"
    xacf <- pacf(as.ts(x[[chain]]), lag.max = lag.max, plot = FALSE)
  }
  else {
    ylab <- "Autocorrelation"
    xacf <- acf(as.ts(x[[chain]]), lag.max = lag.max, plot = FALSE)
  }
  clim <- c(-1, 1) * qnorm(0.975)/sqrt(xacf$n.used)
  for (j in 1:nvar(x)) {
    if (is.null(ylim)) {
      ylim <- range(c(clim, xacf$acf[, j, j]))
    }
    if (style == "gray") {
      plot(xacf$lag[, j, j], xacf$acf[, j, j], type = "n", 
           ylab = ylab, xlab = "Lag", ylim = ylim, 
           bty = "n", xaxt = "n", yaxt = "n", cex = cex, 
           ...)
      .graypr_new(cex = cex)
      rect(par("usr")[1], clim[1], par("usr")[2], 
           clim[2], col = rgb(0.5, 0.5, 0.5, 0.35), border = NA)
      lines(xacf$lag[, j, j], xacf$acf[, j, j], type = "h", 
            lwd = lwd, col = col)
    }
    if (style == "plain") {
      plot(xacf$lag[, j, j], xacf$acf[, j, j], type = "h", 
           ylab = ylab, xlab = "Lag", ylim = ylim, 
           lwd = 2, ...)
      abline(h = c(0, clim), col = "gray")
    }
  }
}

rmeanplot1_new <- function (x, col = NULL, lty = 1, style = c("gray", "plain"), cex = NULL, lwd = NULL, 
                            ...) {
  x <- convert.mcmc.list(lapply(x, function(mco) apply(mco, 
                                                       2, function(y) cumsum(y)/seq_along(y))))
  traplot1_new(x = x, col = col, lty = lty, style = style, xlab = "Iteration", 
           ylab = "Running mean", cex = cex, lwd = lwd, ...)
}

traplot1_new <- function (x, col = NULL, lty = 1, style = c("gray", "plain"), cex = NULL, lwd = NULL, 
                          ...) {
  style <- match.arg(style)
  nchains <- nchain(x)
  if (is.null(col)) {
    col <- mcmcplotsPalette(nchains)
  }
  xx <- as.vector(time(x))
  yy <- do.call("cbind", as.list(x))
  if (style == "plain") {
    matplot(xx, yy, type = "l", col = col, lty = lty, 
            ...)
  }
  if (style == "gray") {
    matplot(xx, yy, type = "n", xaxt = "n", yaxt = "n", cex = cex, 
            bty = "n", ...)
    .graypr_new(cex = cex)
    matlines(xx, yy, col = col, lty = lty, lwd = lwd)
  }
}

.graypr_new <- function (x.axis = TRUE, y.axis = TRUE, x.major = TRUE, y.major = TRUE, 
                     x.minor = TRUE, y.minor = TRUE, x.malty = 1, y.malty = 1, 
                     x.milty = 1, y.milty = 1, cex = NULL) {
  if (x.axis) 
    axis(1, lwd = 0, lwd.ticks = 2, cex= cex)
  if (y.axis) 
    axis(2, lwd = 0, lwd.ticks = 2, cex = cex)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], 
       par("usr")[4], border = NA, col = "whitesmoke")
  x.ticks <- axTicks(1)
  y.ticks <- axTicks(2)
  if (x.major) {
    abline(v = x.ticks, col = gray(0.95), lty = x.malty, lwd = 2)
  }
  if (y.major) {
    abline(h = y.ticks, col = gray(0.95), lty = y.malty, lwd = 2)
  }
  if (x.minor) {
    x.sep <- diff(x.ticks)[1]/2
    x.minorgrid <- c(min(x.ticks) - x.sep, x.ticks + x.sep)
    abline(v = x.minorgrid, col = gray(0.95), lty = x.milty)
  }
  if (y.minor) {
    y.sep <- diff(y.ticks)[1]/2
    y.minorgrid <- c(min(y.ticks) - y.sep, y.ticks + y.sep)
    abline(h = y.minorgrid, col = gray(0.95), lty = y.milty)
  }
}