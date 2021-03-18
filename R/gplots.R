### gg-style base plots
# gplot, gplot.default, gplot.data.frame, gplot.density, gplot.formula, 
# gplot.lm, gplot.table, gqqnorm, gqqnorm.default, gqqplot, gcurve,
# gplot.function, gplot.factor, gplot.histogram, ghist, ghist.default, 
# ghist.Date, ghist.POSIXt, gbarplot, gbarplot.default, gboxplot, 
# gboxplot.default, gboxplot.matrix gboxplot.formula, gbxp, gstripchart,
# gstripchart.default, gstripchart.formula, gspineplot, gspineplot.default,
# gspineplot.formula, gsunflowerplot
###


#' Generic X-Y plotting
#' 
#' Generic plotting for \code{R} objects with default grid background. For 
#' simple scatter plots, \code{\link{plot.default}} will be used. However, 
#' there are \code{plot} methods for many \code{R} objects including
#' \code{\link{function}s}, \code{\link{data.frame}s}, 
#' \code{\link{density}} objects, etc. Use \code{methods(gplot)} and the 
#' documentation for these.
#' 
#' @inheritParams graphics::plot.default
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @examples
#' gplot(1:10)
#' gplot(mtcars)
#' gplot(mpg ~ wt, data = mtcars)
#' 
#' @export

gplot <- function(x, y, ...) {
  UseMethod('gplot')
}

#' @rdname gplot
#' @export
gplot.default <- function(x, y = NULL, type = "p", xlim = NULL, ylim = NULL, 
                          log = "", main = NULL, sub = NULL, xlab = NULL, 
                          ylab = NULL, ann = par("ann"), axes = TRUE, 
                          frame.plot = axes, panel.first = NULL, 
                          panel.last = NULL, asp = NA, ..., grid = TRUE, 
                          bty = 'n', col.grid = 'grey90', col.acc = 'white',
                          col.axis = 'grey50', col.ticks = col.axis) {
  localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
  localWindow <- function(..., col, bg, pch, cex, lty, lwd) plot.window(...)
  localTitle <- function(..., col, bg, pch, cex, lty, lwd) title(...)
  
  xlabel <- if (!missing(x)) 
    deparse(substitute(x))
  ylabel <- if (!missing(y)) 
    deparse(substitute(y))
  xy <- xy.coords(x, y, xlabel, ylabel, log)
  xlab <- if (is.null(xlab)) 
    xy$xlab
  else xlab
  ylab <- if (is.null(ylab)) 
    xy$ylab
  else ylab
  xlim <- if (is.null(xlim)) 
    range(xy$x[is.finite(xy$x)])
  else xlim
  ylim <- if (is.null(ylim)) 
    range(xy$y[is.finite(xy$y)])
  else ylim
  dev.hold()
  on.exit(dev.flush())
  plot.new()
  localWindow(xlim, ylim, log, asp, ...)
  panel.first
  p <- par('usr')
  if (log != '')
    p <- switch(log, x = c(10 ^ p[1:2], p[3:4]), 
                y = c(p[1:2], 10 ^ p[3:4]), xy = 10 ^ p, yx = 10 ^ p,
                warning('\'log\' should be \'\', \'x\', \'y\', or \'xy\''))
  ax <- axis(side = 1, labels = FALSE, lwd = 0)
  ay <- axis(side = 2, labels = FALSE, lwd = 0)
  if (axes) {
    axis(1, at = ax, lwd = 0, lwd.ticks = 1, tcl = -.2, las = 1,
         col.axis = 'grey50', col.ticks = 'grey50', cex.axis = .8)
    axis(2, at = ay, lwd = 0, lwd.ticks = 1, tcl = -.2, las = 1,
         col.axis = 'grey50', col.ticks = 'grey50', cex.axis = .8)
  }
  if (grid) {
    rect(p[1], p[3], p[2], p[4], col = col.grid)
    abline(h = ay, lwd = 1.5, col = col.acc)
    if (!grepl('y', log))
      abline(h = ay + diff(ay[1:2]) / 2 - diff(ay[1:2]), lwd = .5, col = col.acc)
    abline(v = ax, lwd = 1.5, col = col.acc)
    if (!grepl('x', log))
      abline(v = ax + diff(ax[1:2]) / 2 - diff(ax[1:2]), lwd = .5, col = col.acc)
    box('plot', col = col.grid)
  }
  plot.xy(xy, type, ...)
  panel.last
  if (ann) 
    localTitle(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  invisible()
}

#' @rdname gplot
#' @export
gplot.data.frame <- function(x, grid = TRUE, col.grid = 'grey90', col.acc = 'white', ...) {
  gplot2 <- function(x, xlab = names(x)[1L], ylab = names(x)[2L], ...,
                     grid = grid, col.grid = col.grid, col.acc = col.acc)
    gplot(x[[1L]], x[[2L]], xlab = xlab, ylab = ylab, ..., grid = grid, 
          col.grid = col.grid, col.acc = col.acc)
  if (!is.data.frame(x)) 
    stop("'gplot.data.frame' applied to non data frame")
  if (ncol(x) == 1) {
    x1 <- x[[1L]]
    cl <- class(x1)
    if (cl %in% c("integer", "numeric")) 
      gstripchart(x1, ..., grid = grid, col.grid = col.grid, col.acc = col.acc)
    else gplot(x1, ..., grid = grid, col.grid = col.grid, col.acc = col.acc)
  } else if (ncol(x) == 2) {
    gplot2(x, ..., grid = grid, col.grid = col.grid, col.acc = col.acc)
  } else {
    gpairs(data.matrix(x), ..., grid = grid, col.grid = col.grid, col.acc = col.acc)
  }
}

#' gplot method for kernel density estimation
#' 
#' The \code{gplot} method for density objects
#' 
#' @inheritParams stats::plot.density
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gplot.density <- function(x, main = NULL, xlab = NULL, ylab = "Density",
                          type = "l", zero.line = TRUE, grid = TRUE, col.grid = 'grey90', col.acc = 'white', ...) {
  if (is.null(xlab)) 
    xlab <- paste("N =", x$n, "  Bandwidth =", formatC(x$bw))
  if (is.null(main)) 
    main <- deparse(x$call)
  gplot.default(x, main = main, xlab = xlab, ylab = ylab, type = type, ...)
  if (zero.line) 
    abline(h = 0, lwd = 0.1, col = "gray")
  invisible(NULL)
}

#' Formula notation for gplot scatterplots
#' 
#' Specify a \code{\link{gplot}} via a formula
#' 
#' @inheritParams graphics::plot.formula
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gplot.formula <- function(formula, data = parent.frame(), ..., subset, 
                          ylab = varnames[response], ask = dev.interactive(),
                          grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
  m <- match.call(expand.dots = FALSE)
  eframe <- parent.frame()
  md <- eval(m$data, eframe)
  if (is.matrix(md)) 
    m$data <- md <- as.data.frame(data)
  dots <- lapply(m$..., eval, md, eframe)
  nmdots <- names(dots)
  if ("main" %in% nmdots) 
    dots[["main"]] <- enquote(dots[["main"]])
  if ("sub" %in% nmdots) 
    dots[["sub"]] <- enquote(dots[["sub"]])
  if ("xlab" %in% nmdots) 
    dots[["xlab"]] <- enquote(dots[["xlab"]])
  m$ylab <- m$... <- m$ask <- m$grid <- m$col.grid <- m$col.acc <- NULL
  subset.expr <- m$subset
  m$subset <- NULL
  m <- as.list(m)
  m[[1L]] <- stats::model.frame.default
  m <- as.call(c(m, list(na.action = NULL)))
  mf <- eval(m, eframe)
  if (!missing(subset)) {
    s <- eval(subset.expr, data, eframe)
    l <- nrow(mf)
    dosub <- function(x) 
      if (length(x) == l) x[s]
    else x
    dots <- lapply(dots, dosub)
    mf <- mf[s, ]
  }
  horizontal <- FALSE
  if ("horizontal" %in% names(dots)) 
    horizontal <- dots[["horizontal"]]
  response <- attr(attr(mf, "terms"), "response")
  if (response) {
    varnames <- names(mf)
    y <- mf[[response]]
    funname <- NULL
    xn <- varnames[-response]
    if (is.object(y)) {
      found <- FALSE
      for (j in class(y)) {
        funname <- paste0("gplot.", j)
        if (exists(funname)) {
          found <- TRUE
          break
        }
      }
      if (!found) 
        funname <- NULL
    }
    if (is.null(funname)) 
      funname <- "gplot"
    if (length(varnames) > 2L) {
      oask <- devAskNewPage(ask)
      on.exit(devAskNewPage(oask))
    }
    if (length(xn)) {
      if (!is.null(xlab <- dots[["xlab"]])) 
        dots <- dots[-match("xlab", names(dots))]
      for (i in xn) {
        xl <- if (is.null(xlab)) 
          i
        else xlab
        yl <- ylab
        if (horizontal && is.factor(mf[[i]])) {
          yl <- xl
          xl <- ylab
        }
        do.call(funname, c(list(mf[[i]], y, ylab = yl, xlab = xl), dots))
      }
    }
    else do.call(funname, c(list(y, ylab = ylab), dots))
  }
  else do.call("gplot.data.frame", c(list(mf), grid = grid, col.grid = col.grid,
                                     col.acc = col.acc, dots))
  invisible()
}

#' gplot diagnostics for an lm object
#' 
#' Six plots (selectable by \code{which}) are currently available: a plot of
#' residuals against fitted values, a Scale-Location plot of 
#' \emph{sqrt(| residuals |)} against fitted values, a Normal Q-Q plot, a 
#' plot of Cook's distances versus row labels, a plot of residuals against 
#' leverages, and a plot of Cook's distances against leverage/(1-leverage). 
#' By default, the first three and 5 are provided.
#' 
#' @inheritParams stats::plot.lm
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gplot.lm <- function(x, which = c(1L:3L, 5L), grid = TRUE, col.grid = 'grey90', col.acc = 'white', 
                     caption = list("Residuals vs Fitted","Normal Q-Q",
                                    "Scale-Location", "Cook's distance",
                                    "Residuals vs Leverage",
                                    expression("Cook's dist vs Leverage  " * h[ii]/(1 - h[ii]))),
                     panel = if (add.smooth) panel.smooth else points, sub.caption = NULL, 
                     main = "", ask = prod(par("mfcol")) < length(which) && dev.interactive(), 
                     ..., id.n = 3, labels.id = names(residuals(x)), cex.id = 0.75, 
                     qqline = TRUE, cook.levels = c(0.5, 1), add.smooth = getOption("add.smooth"), 
                     label.pos = c(4, 2), cex.caption = 1) {
  dropInf <- function(x, h) {
    if (any(isInf <- h >= 1)) {
      warning(gettextf("not plotting observations with leverage one:\n  %s", 
                       paste(which(isInf), collapse = ", ")), call. = FALSE, 
              domain = NA)
      x[isInf] <- NaN
    }
    x
  }
  if (!inherits(x, "lm")) 
    stop("use only with \"lm\" objects")
  if (!is.numeric(which) || any(which < 1) || any(which > 6)) 
    stop("'which' must be in 1:6")
  isGlm <- inherits(x, "glm")
  show <- rep(FALSE, 6)
  show[which] <- TRUE
  r <- residuals(x)
  yh <- predict(x)
  w <- weights(x)
  if (!is.null(w)) {
    wind <- w != 0
    r <- r[wind]
    yh <- yh[wind]
    w <- w[wind]
    labels.id <- labels.id[wind]
  }
  n <- length(r)
  if (any(show[2L:6L])) {
    s <- if (inherits(x, "rlm")) 
      x$s
    else if (isGlm) 
      sqrt(summary(x)$dispersion)
    else sqrt(deviance(x)/df.residual(x))
    hii <- lm.influence(x, do.coef = FALSE)$hat
    if (any(show[4L:6L])) {
      cook <- if (isGlm) 
        cooks.distance(x)
      else cooks.distance(x, sd = s, res = r)
    }
  }
  if (any(show[2L:3L])) {
    ylab23 <- if (isGlm) 
      "Std. deviance resid."
    else "Standardized residuals"
    r.w <- if (is.null(w)) 
      r
    else sqrt(w) * r
    rs <- dropInf(r.w/(s * sqrt(1 - hii)), hii)
  }
  if (any(show[5L:6L])) {
    r.hat <- range(hii, na.rm = TRUE)
    isConst.hat <- all(r.hat == 0) || diff(r.hat) < 1e-10 * 
      mean(hii, na.rm = TRUE)
  }
  if (any(show[c(1L, 3L)])) 
    l.fit <- if (isGlm) 
      "Predicted values"
  else "Fitted values"
  if (is.null(id.n)) 
    id.n <- 0
  else {
    id.n <- as.integer(id.n)
    if (id.n < 0L || id.n > n) 
      stop(gettextf("'id.n' must be in {1,..,%d}", n), domain = NA)
  }
  if (id.n > 0L) {
    if (is.null(labels.id)) 
      labels.id <- paste(1L:n)
    iid <- 1L:id.n
    show.r <- sort.list(abs(r), decreasing = TRUE)[iid]
    if (any(show[2L:3L])) 
      show.rs <- sort.list(abs(rs), decreasing = TRUE)[iid]
    text.id <- function(x, y, ind, adj.x = TRUE) {
      labpos <- if (adj.x) 
        label.pos[1 + as.numeric(x > mean(range(x)))]
      else 3
      text(x, y, labels.id[ind], cex = cex.id, xpd = TRUE, 
           pos = labpos, offset = 0.25)
    }
  }
  getCaption <- function(k) if (length(caption) < k) 
    NA_character_
  else as.graphicsAnnot(caption[[k]])
  if (is.null(sub.caption)) {
    cal <- x$call
    if (!is.na(m.f <- match("formula", names(cal)))) {
      cal <- cal[c(1, m.f)]
      names(cal)[2L] <- ""
    }
    cc <- deparse(cal, 80)
    nc <- nchar(cc[1L], "c")
    abbr <- length(cc) > 1 || nc > 75
    sub.caption <- if (abbr) 
      paste(substr(cc[1L], 1L, min(75L, nc)), "...")
    else cc[1L]
  }
  one.fig <- prod(par("mfcol")) == 1
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  if (show[1L]) {
    ylim <- range(r, na.rm = TRUE)
    if (id.n > 0) 
      ylim <- extendrange(r = ylim, f = 0.08)
    dev.hold()
    gplot(yh, r, xlab = l.fit, ylab = "Residuals", main = main, 
          ylim = ylim, type = "n", ...)
    panel(yh, r, ...)
    if (one.fig) 
      title(sub = sub.caption, ...)
    mtext(getCaption(1), 3, 0.25, cex = cex.caption)
    if (id.n > 0) {
      y.id <- r[show.r]
      y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
      text.id(yh[show.r], y.id, show.r)
    }
    abline(h = 0, lty = 'F4', col = "black")
    dev.flush()
  }
  if (show[2L]) {
    ylim <- range(rs, na.rm = TRUE)
    ylim[2L] <- ylim[2L] + diff(ylim) * 0.075
    dev.hold()
    qq <- gqqnorm(rs, main = main, ylab = ylab23, ylim = ylim, ...)
    if (qqline) 
      qqline(rs, lty = 'F4', col = "red")
    if (one.fig) 
      title(sub = sub.caption, ...)
    mtext(getCaption(2), 3, 0.25, cex = cex.caption)
    if (id.n > 0) 
      text.id(qq$x[show.rs], qq$y[show.rs], show.rs)
    dev.flush()
  }
  if (show[3L]) {
    sqrtabsr <- sqrt(abs(rs))
    ylim <- c(0, max(sqrtabsr, na.rm = TRUE))
    yl <- as.expression(substitute(sqrt(abs(YL)), list(YL = as.name(ylab23))))
    yhn0 <- if (is.null(w)) 
      yh
    else yh[w != 0]
    dev.hold()
    gplot(yhn0, sqrtabsr, xlab = l.fit, ylab = yl, main = main, 
          ylim = ylim, type = "n", ...)
    panel(yhn0, sqrtabsr, ...)
    if (one.fig) 
      title(sub = sub.caption, ...)
    mtext(getCaption(3), 3, 0.25, cex = cex.caption)
    if (id.n > 0) 
      text.id(yhn0[show.rs], sqrtabsr[show.rs], show.rs)
    dev.flush()
  }
  if (show[4L]) {
    if (id.n > 0) {
      show.r <- order(-cook)[iid]
      ymx <- cook[show.r[1L]] * 1.075
    }
    else ymx <- max(cook, na.rm = TRUE)
    dev.hold()
    gplot(cook, type = "h", ylim = c(0, ymx), main = main, 
          xlab = "Obs. number", ylab = "Cook's distance", ...)
    if (one.fig) 
      title(sub = sub.caption, ...)
    mtext(getCaption(4), 3, 0.25, cex = cex.caption)
    if (id.n > 0) 
      text.id(show.r, cook[show.r], show.r, adj.x = FALSE)
    dev.flush()
  }
  if (show[5L]) {
    ylab5 <- if (isGlm) 
      "Std. Pearson resid."
    else "Standardized residuals"
    r.w <- residuals(x, "pearson")
    if (!is.null(w)) 
      r.w <- r.w[wind]
    rsp <- dropInf(r.w/(s * sqrt(1 - hii)), hii)
    ylim <- range(rsp, na.rm = TRUE)
    if (id.n > 0) {
      ylim <- extendrange(r = ylim, f = 0.08)
      show.rsp <- order(-cook)[iid]
    }
    do.plot <- TRUE
    if (isConst.hat) {
      if (missing(caption)) 
        caption[[5L]] <- "Constant Leverage:\n Residuals vs Factor Levels"
      aterms <- attributes(terms(x))
      dcl <- aterms$dataClasses[-aterms$response]
      facvars <- names(dcl)[dcl %in% c("factor", "ordered")]
      mf <- model.frame(x)[facvars]
      if (ncol(mf) > 0) {
        dm <- data.matrix(mf)
        nf <- length(nlev <- unlist(unname(lapply(x$xlevels, length))))
        ff <- if (nf == 1) 
          1
        else rev(cumprod(c(1, nlev[nf:2])))
        facval <- (dm - 1) %*% ff
        xx <- facval
        dev.hold()
        gplot(facval, rsp, xlim = c(-1/2, sum((nlev - 1) * ff) + 1/2), 
              ylim = ylim, xaxt = "n", main = main, 
              xlab = "Factor Level Combinations", ylab = ylab5, type = "n", ...)
        axis(1, at = ff[1L] * (1L:nlev[1L] - 1/2) - 1/2, labels = x$xlevels[[1L]])
        mtext(paste(facvars[1L], ":"), side = 1, line = 0.25, adj = -0.05)
        abline(v = ff[1L] * (0:nlev[1L]) - 1/2, col = "black", lty = "F4")
        panel(facval, rsp, ...)
        abline(h = 0, lty = 'F4', col = "black")
        dev.flush()
      } else {
        message(gettextf("hat values (leverages) are all = %s\n and there are no factor predictors; no plot no. 5", 
                         format(mean(r.hat))), domain = NA)
        frame()
        do.plot <- FALSE
      }
    } else {
      xx <- hii
      xx[xx >= 1] <- NA
      dev.hold()
      gplot(xx, rsp, xlim = c(0, max(xx, na.rm = TRUE)), ylim = ylim,
            main = main, xlab = "Leverage", ylab = ylab5, type = "n", ...)
      panel(xx, rsp, ...)
      abline(h = 0, v = 0, lty = 'F4', col = "black")
      if (one.fig) 
        title(sub = sub.caption, ...)
      if (length(cook.levels)) {
        p <- length(coef(x))
        usr <- par("usr")
        hh <- seq.int(min(r.hat[1L], r.hat[2L]/100), usr[2L], length.out = 101)
        for (crit in cook.levels) {
          cl.h <- sqrt(crit * p * (1 - hh) / hh)
          lines(hh, cl.h, lty = 2, col = 2)
          lines(hh, -cl.h, lty = 2, col = 2)
        }
        legend("bottomleft", legend = "Cook's distance", 
               lty = 2, col = 2, bty = "n")
        xmax <- min(0.99, usr[2L])
        ymult <- sqrt(p * (1 - xmax)/xmax)
        aty <- c(-sqrt(rev(cook.levels)) * ymult, sqrt(cook.levels) * ymult)
        axis(4, at = aty, labels = paste(c(rev(cook.levels),cook.levels)),
             mgp = c(0.25, 0.25, 0), las = 2, lwd = 0,
             tck = 0, cex.axis = cex.id, col.axis = 'grey50')
      }
      dev.flush()
    }
    if (do.plot) {
      mtext(getCaption(5), 3, 0.25, cex = cex.caption)
      if (id.n > 0) {
        y.id <- rsp[show.rsp]
        y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
        text.id(xx[show.rsp], y.id, show.rsp)
      }
    }
  }
  if (show[6L]) {
    g <- dropInf(hii/(1 - hii), hii)
    ymx <- max(cook, na.rm = TRUE) * 1.025
    dev.hold()
    gplot(g, cook, xlim = c(0, max(g, na.rm = TRUE)),ylim = c(0, ymx),
          main = main, ylab = "Cook's distance",
          xlab = expression("Leverage  " * h[ii]), xaxt = "n", type = "n", ...)
    panel(g, cook, ...)
    athat <- pretty(hii)
    axis(1, at = athat/(1 - athat), labels = paste(athat))
    if (one.fig) 
      title(sub = sub.caption, ...)
    p <- length(coef(x))
    bval <- pretty(sqrt(p * cook/g), 5)
    usr <- par("usr")
    xmax <- usr[2L]
    ymax <- usr[4L]
    for (i in seq_along(bval)) {
      bi2 <- bval[i]^2
      if (ymax > bi2 * xmax) {
        xi <- xmax + strwidth(" ") / 3
        yi <- bi2 * xi
        abline(0, bi2, lty = 2)
        text(xi, yi, paste(bval[i]), adj = 0, xpd = TRUE)
      } else {
        yi <- ymax - 1.5 * strheight(" ")
        xi <- yi/bi2
        lines(c(0, xi), c(0, yi), lty = 2)
        text(xi, ymax - 0.8 * strheight(" "), paste(bval[i]), 
             adj = 0.5, xpd = TRUE)
      }
    }
    mtext(getCaption(6), 3, 0.25, cex = cex.caption)
    if (id.n > 0) {
      show.r <- order(-cook)[iid]
      text.id(g[show.r], cook[show.r], show.r)
    }
    dev.flush()
  }
  if (!one.fig && par("oma")[3L] >= 1) 
    mtext(sub.caption, outer = TRUE, cex = 1.25)
  invisible()
}

#' gplot method for \code{table} objects
#' 
#' This is a method of the generic \code{gplot} function for (contingency) 
#' \code{\link{table}} objects. Whereas for two- and more dimensional tables, 
#' a \code{\link{mosaicplot}} is drawn, one-dimensional ones are plotted as
#' bars.
#' 
#' @inheritParams graphics::plot.table
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gplot.table <- function(x, type = "h", ylim = c(0, max(x)), lwd = 2,
                        xlab = NULL, ylab = NULL, frame.plot = is.num, ...,
                        grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
  xnam <- deparse(substitute(x))
  rnk <- length(dim(x))
  if (rnk == 0L) 
    stop("invalid table 'x'")
  if (rnk == 1L) {
    dn <- dimnames(x)
    nx <- dn[[1L]]
    if (is.null(xlab)) 
      xlab <- names(dn)
    if (is.null(xlab)) 
      xlab <- ""
    if (is.null(ylab)) 
      ylab <- xnam
    is.num <- suppressWarnings(!any(is.na(xx <- as.numeric(nx))))
    x0 <- if (is.num) 
      xx
    else seq_along(x)
    gplot(x0, unclass(x), type = type, ylim = ylim, xlab = xlab, 
          ylab = ylab, frame.plot = frame.plot, lwd = lwd, ..., xaxt = "n",
          grid = grid, col.grid = col.grid, col.acc = col.acc)
    localaxis <- function(..., col, bg, pch, cex, lty) axis(...)
    if (!identical(list(...)$axes, FALSE)) 
      localaxis(1, at = x0, labels = nx, ...)
  } else {
    if (length(dots <- list(...)) && !is.null(dots$main)) 
      mosaicplot(x, xlab = xlab, ylab = ylab, ...)
    else mosaicplot(x, xlab = xlab, ylab = ylab, main = xnam, ...)
  }
}

#' Quantile-quantile gplots
#' 
#' @description
#' qqnorm is a generic function the default method of which produces a normal
#' QQ plot of the values in \code{y}. \code{qqline} adds a line to a 
#' “theoretical”, by default normal, quantile-quantile plot which passes 
#' through the \code{probs} quantiles, by default the first and third quartiles.
#' 
#' \code{qqplot} produces a QQ plot of two datasets.
#' 
#' Graphical parameters may be given as arguments to \code{gqqnorm}, 
#' \code{gqqplot} and \code{qqline}.
#' 
#' @inheritParams stats::qqnorm
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gqqnorm <- function(y, ...) {
  UseMethod('gqqnorm')
}

#' @rdname gqqnorm
#' @export
gqqnorm.default <- function(y, ylim, main = "Normal Q-Q Plot", 
                            xlab = "Theoretical Quantiles", 
                            ylab = "Sample Quantiles", plot.it = TRUE, 
                            datax = FALSE, grid = TRUE, col.grid = 'grey90', col.acc = 'white', ...) {
  if (has.na <- any(ina <- is.na(y))) {
    yN <- y
    y <- y[!ina]
  }
  if (0 == (n <- length(y))) 
    stop("y is empty or has only NAs")
  if (plot.it && missing(ylim)) 
    ylim <- range(y)
  x <- qnorm(ppoints(n))[order(order(y))]
  if (has.na) {
    y <- x
    x <- yN
    x[!ina] <- y
    y <- yN
  }
  if (plot.it) 
    if (datax) 
      gplot(y, x, main = main, xlab = ylab, ylab = xlab, xlim = ylim, ...)
  else gplot(x, y, main = main, xlab = xlab, ylab = ylab, ylim = ylim, ...)
  invisible(if (datax)
    list(x = y, y = x) else list(x = x,y = y))
}

#' @rdname gqqnorm
#' @export
gqqplot <- function(x, y, plot.it = TRUE, xlab = deparse(substitute(x)), 
                    ylab = deparse(substitute(y)), ..., 
                    grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
  sx <- sort(x)
  sy <- sort(y)
  lenx <- length(sx)
  leny <- length(sy)
  if (leny < lenx) 
    sx <- approx(1L:lenx, sx, n = leny)$y
  if (leny > lenx) 
    sy <- approx(1L:leny, sy, n = lenx)$y
  if (plot.it) 
    gplot(sx, sy, xlab = xlab, ylab = ylab, ..., 
          grid = grid, col.grid = col.grid, col.acc = col.acc)
  invisible(list(x = sx, y = sy))
}


#' Draw function curves
#' 
#' Draws a curve corresponding to a function over the interval \code{[from, to]}.
#' \code{gcurve} can plot also an expression in the variable \code{xname}, 
#' default \code{x}.
#' 
#' @inheritParams graphics::curve
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gcurve <- function(expr, from = NULL, to = NULL, n = 101, add = FALSE,
                   type = "l", xname = "x", xlab = xname, ylab = NULL,
                   log = NULL, xlim = NULL, ..., 
                   grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
  sexpr <- substitute(expr)
  if (is.name(sexpr)) {
    expr <- call(as.character(sexpr), as.name(xname))
  } else {
    if (!((is.call(sexpr) || is.expression(sexpr)) && xname %in% all.vars(sexpr))) 
      stop(gettextf("'expr' must be a function, or a call or an expression containing '%s'",
                    xname), domain = NA)
    expr <- sexpr
  }
  if (dev.cur() == 1L && !identical(add, FALSE)) {
    warning("'add' will be ignored as there is no existing plot")
    add <- FALSE
  }
  addF <- identical(add, FALSE)
  if (is.null(ylab)) 
    ylab <- deparse(expr)
  if (is.null(from) || is.null(to)) {
    xl <- if (!is.null(xlim)) 
      xlim
    else if (!addF) {
      pu <- par("usr")[1L:2L]
      if (par("xaxs") == "r") 
        pu <- extendrange(pu, f = -1/27)
      if (par("xlog")) 
        10^pu
      else pu
    } else c(0, 1)
    if (is.null(from)) 
      from <- xl[1L]
    if (is.null(to)) 
      to <- xl[2L]
  }
  lg <- if (length(log)) 
    log
  else if (!addF && par("xlog")) 
    "x"
  else ""
  if (length(lg) == 0) 
    lg <- ""
  if (grepl("x", lg, fixed = TRUE)) {
    if (from <= 0 || to <= 0) 
      stop("'from' and 'to' must be > 0 with log=\"x\"")
    x <- exp(seq.int(log(from), log(to), length.out = n))
  } else x <- seq.int(from, to, length.out = n)
  ll <- list(x = x)
  names(ll) <- xname
  y <- eval(expr, envir = ll, enclos = parent.frame())
  if (length(y) != length(x)) 
    stop("'expr' did not evaluate to an object of length 'n'")
  if (isTRUE(add)) 
    lines(x = x, y = y, type = type, ...)
  else gplot(x = x, y = y, type = type, xlab = xlab, ylab = ylab, 
             xlim = xlim, log = lg, grid = grid, col.grid = col.grid,
             col.acc = col.acc, ...)
  invisible(list(x = x, y = y))
}

#' @rdname gcurve
#' @export
gplot.function <- function(x, y = 0, to = 1, from = y, xlim = NULL, 
                           ylab = NULL, grid = TRUE, col.grid = 'grey90', col.acc = 'white', ...) {
  if (!missing(y) && missing(from)) 
    from <- y
  if (is.null(xlim)) {
    if (is.null(from)) 
      from <- 0
  } else {
    if (missing(from)) 
      from <- xlim[1L]
    if (missing(to)) 
      to <- xlim[2L]
  }
  if (is.null(ylab)) {
    sx <- substitute(x)
    ylab <- if (mode(x) != "name") 
      deparse(sx)[1L]
    else {
      xname <- list(...)[["xname"]]
      if (is.null(xname)) 
        xname <- "x"
      paste0(sx, "(", xname, ")")
    }
  }
  gcurve(expr = x, from = from, to = to, xlim = xlim, ylab = ylab, ...)
}

#' Plotting factor variables
#' 
#' @description
#' This function implements a scatterplot for \code{\link{factor}} arguments
#' for the \emph{generic} \code{\link{gplot}} function.
#' 
#' If \code{y} is missing \code{\link{gbarplot}} is produced; for numeric 
#' \code{y} \code{\link{gboxplot}} is used; for a factor \code{y} a 
#' \code{\link{gspineplot}} is shown; and for any other type of \code{y}, the
#' next \code{gplot} method is called, normally \code{\link{gplot.default}}.
#' 
#' @inheritParams graphics::plot.factor
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gplot.factor <- function(x, y, legend.text = NULL, ..., 
                         grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
  if (missing(y) || is.factor(y)) {
    dargs <- list(...)
    axisnames <- if (!is.null(dargs$axes)) 
      dargs$axes
    else if (!is.null(dargs$xaxt)) 
      dargs$xaxt != "n"
    else TRUE
  }
  if (missing(y)) {
    gbarplot(table(x), axisnames = axisnames, ..., grid = grid, 
             col.grid = col.grid, col.acc = col.acc)
  } else if (is.factor(y)) {
    if (is.null(legend.text)) 
      gspineplot(x, y, ..., grid = grid, col.grid = col.grid, col.acc = col.acc)
    else {
      args <- c(list(x = x, y = y, grid = grid, col.grid = col.grid, 
                     col.acc = col.acc), list(...))
      args$yaxlabels <- legend.text
      do.call("gspineplot", args)
    }
  } else if (is.numeric(y)) 
    gboxplot(y ~ x, ..., grid = grid, col.grid = col.grid, col.acc = col.acc)
  else NextMethod("gplot")
}

#' gplot histograms
#' 
#' These are methods for objects of class "\code{histogram}", typically 
#' produced by \code{\link{ghist}}.
#' 
#' @inheritParams graphics::plot.histogram
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gplot.histogram <- function(x, freq = equidist, density = NULL, angle = 45,
                            col = 'white', border = par("fg"), lty = NULL,
                            main = paste("Histogram of", paste(x$xname, collapse = "\n")),
                            sub = NULL, xlab = x$xname, ylab, xlim = range(x$breaks),
                            ylim = NULL, axes = TRUE, labels = FALSE, add = FALSE, 
                            ann = TRUE, grid = TRUE, col.grid = 'grey90', col.acc = 'white', ...) {
  equidist <- if (is.logical(x$equidist)) 
    x$equidist
  else {
    h <- diff(x$breaks)
    diff(range(h)) < 1e-07 * mean(h)
  }
  if (freq && !equidist) 
    warning("the AREAS in the plot are wrong -- rather use 'freq = FALSE'")
  y <- if (freq) 
    x$counts
  else x$density
  nB <- length(x$breaks)
  if (is.null(y) || 0L == nB) 
    stop("'x' is wrongly structured")
  dev.hold()
  on.exit(dev.flush())
  if (!add) {
    if (is.null(ylim)) 
      ylim <- range(y, 0)
    if (missing(ylab)) 
      ylab <- if (!freq) 
        "Density"
    else "Frequency"
    gplot(NULL, xlim = xlim, ylim = ylim, axes = axes, xlab = '', ylab = '',
          grid = grid, col.grid = col.grid, col.acc = col.acc)
    if (ann) 
      title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
  }
  rect(x$breaks[-nB], 0, x$breaks[-1L], y, col = col, border = border, 
       angle = angle, density = density, lty = lty)
  if ((logl <- is.logical(labels) && labels) || is.character(labels)) 
    text(x$mids, y, labels = if (logl) {
      if (freq) 
        x$counts
      else round(x$density, 3)
    } else labels, adj = c(0.5, -0.5))
  invisible()
}

#' ghistograms
#' 
#' The generic function \code{\link{ghist}} computes a histogram of the given 
#' data values. If \code{plot = TRUE}, the resulting object of 
#' \code{\link{class}} "\code{histogram}" is plotted by \code{\link{gplot.histogram}}
#' before it is returned.
#' 
#' @inheritParams graphics::hist
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' @export

ghist <- function(x, ...) {
  UseMethod('ghist')
}

#' @rdname ghist
#' @export
ghist.default <- function(x, breaks = "Sturges", freq = NULL, probability = !freq,
                          include.lowest = TRUE, right = TRUE, density = NULL, angle = 45, 
                          col = 'white', border = NULL, main = paste("Histogram of", xname), 
                          xlim = range(breaks), ylim = NULL, xlab = xname, ylab, axes = TRUE, 
                          plot = TRUE, labels = FALSE, nclass = NULL, warn.unused = TRUE, 
                          grid = TRUE, col.grid = 'grey90', col.acc = 'white', ...) {
  if (!is.numeric(x)) 
    stop("'x' must be numeric")
  xname <- paste(deparse(substitute(x), 500), collapse = "\n")
  n <- length(x <- x[is.finite(x)])
  n <- as.integer(n)
  if (is.na(n)) 
    stop("invalid length(x)")
  use.br <- !missing(breaks)
  if (use.br) {
    if (!missing(nclass)) 
      warning("'nclass' not used when 'breaks' is specified")
  } else if (!is.null(nclass) && length(nclass) == 1L) 
    breaks <- nclass
  use.br <- use.br && (nB <- length(breaks)) > 1L
  if (use.br) 
    breaks <- sort(breaks)
  else {
    if (!include.lowest) {
      include.lowest <- TRUE
      warning("'include.lowest' ignored as 'breaks' is not a vector")
    }
    if (is.character(breaks)) {
      breaks <- match.arg(tolower(breaks), 
                          c("sturges", "fd", "freedman-diaconis", "scott"))
      breaks <- switch(breaks, sturges = nclass.Sturges(x), 
                       `freedman-diaconis` = , fd = nclass.FD(x), scott = nclass.scott(x), 
                       stop("unknown 'breaks' algorithm"))
    } else if (is.function(breaks)) {
      breaks <- breaks(x)
    }
    if (length(breaks) == 1) {
      if (!is.numeric(breaks) || !is.finite(breaks) || breaks < 1L) 
        stop("invalid number of 'breaks'")
      breaks <- pretty(range(x), n = breaks, min.n = 1)
      nB <- length(breaks)
      if (nB <= 1) 
        stop(gettextf("hist.default: pretty() error, breaks=%s", 
                      format(breaks)), domain = NA)
    } else {
      if (!is.numeric(breaks) || length(breaks) <= 1) 
        stop(gettextf("Invalid breakpoints produced by 'breaks(x)': %s", 
                      format(breaks)), domain = NA)
      breaks <- sort(breaks)
      nB <- length(breaks)
      use.br <- TRUE
    }
  }
  nB <- as.integer(nB)
  if (is.na(nB)) 
    stop("invalid length(breaks)")
  h <- diff(breaks)
  equidist <- !use.br || diff(range(h)) < 1e-07 * mean(h)
  if (!use.br && any(h <= 0)) 
    stop("'breaks' are not strictly increasing")
  freq1 <- freq
  if (is.null(freq)) {
    freq1 <- if (!missing(probability)) 
      !as.logical(probability)
    else equidist
  } else if (!missing(probability) && any(probability == freq)) 
    stop("'probability' is an alias for '!freq', however they differ.")
  diddle <- 1e-07 * stats::median(diff(breaks))
  fuzz <- if (right) 
    c(if (include.lowest) -diddle else diddle, rep.int(diddle, length(breaks) - 1))
  else c(rep.int(-diddle, length(breaks) - 1), if (include.lowest) diddle else -diddle)
  fuzzybreaks <- breaks + fuzz
  h <- diff(fuzzybreaks)
  counts <- .Call(graphics:::C_BinCount, x, fuzzybreaks, right, include.lowest)
  if (any(counts < 0L)) 
    stop("negative 'counts'. Internal Error.", domain = NA)
  if (sum(counts) < n) 
    stop("some 'x' not counted; maybe 'breaks' do not span range of 'x'")
  dens <- counts/(n * diff(breaks))
  mids <- 0.5 * (breaks[-1L] + breaks[-nB])
  r <- structure(list(breaks = breaks, counts = counts, density = dens, 
                      mids = mids, xname = xname, equidist = equidist),
                 class = "histogram")
  if (plot) {
    gplot(r, freq = freq1, col = col, border = border, angle = angle, 
          density = density, main = main, xlim = xlim, ylim = ylim, 
          xlab = xlab, ylab = ylab, axes = axes, labels = labels, ...,
          grid = grid, col.grid = col.grid, col.acc = col.acc)
    invisible(r)
  } else {
    if (warn.unused) {
      nf <- names(formals())
      nf <- nf[is.na(match(nf, c("x", "breaks", "nclass",
                                 "plot", "include.lowest", "right")))]
      missE <- lapply(nf, function(n) substitute(missing(.), list(. = as.name(n))))
      not.miss <- !sapply(missE, eval, envir = environment())
      if (any(not.miss)) 
        warning(sprintf(ngettext(sum(not.miss), "argument %s is not made use of",
                                 "arguments %s are not made use of"),
                        paste(sQuote(nf[not.miss]), collapse = ", ")), domain = NA)
    }
    r
  }
}

#' ghistogram of a date or date-time object
#' 
#' Method for \code{\link{ghist}} applied to date or date-time objects.
#' 
#' @inheritParams graphics::hist.Date
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

ghist.Date <- function(x, breaks, ..., xlab = deparse(substitute(x)),
                       plot = TRUE, freq = FALSE, start.on.monday = TRUE, format,
                       grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
  if (!inherits(x, "Date")) 
    stop("wrong method")
  force(xlab)
  incr <- 1
  if (missing(breaks)) 
    stop("Must specify 'breaks' in hist(<Date>)")
  if (inherits(breaks, "Date")) {
    breaks <- as.Date(breaks)
    d <- min(abs(diff(unclass(breaks))))
    if (d > 1) 
      incr <- 1
    if (d > 7) 
      incr <- 7
    if (d > 28) 
      incr <- 28
    if (d > 366) 
      incr <- 366
    num.br <- FALSE
  } else {
    num.br <- is.numeric(breaks) && length(breaks) == 1L
    if (num.br) {
    } else if (is.character(breaks) && length(breaks) == 1L) {
      valid <- pmatch(breaks, c("days", "weeks", "months", "years", "quarters"))
      if (is.na(valid)) 
        stop("invalid specification of 'breaks'")
      start <- as.POSIXlt(min(x, na.rm = TRUE))
      incr <- 1
      if (valid > 1L) {
        start$isdst <- -1L
      }
      if (valid == 2L) {
        start$mday <- start$mday - start$wday
        if (start.on.monday) 
          start$mday <- start$mday + ifelse(start$wday > 0L, 1L, -6L)
        incr <- 7
      }
      if (valid == 3L) {
        start$mday <- 1
        end <- as.POSIXlt(max(x, na.rm = TRUE))
        end <- as.POSIXlt(end + (31 * 86400))
        end$mday <- 1
        end$isdst <- -1
        breaks <- as.Date(seq(start, end, "months")) - 1
      } else if (valid == 4L) {
        start$mon <- 0L
        start$mday <- 1L
        end <- as.POSIXlt(max(x, na.rm = TRUE))
        end <- as.POSIXlt(end + (366 * 86400))
        end$mon <- 0L
        end$mday <- 1L
        end$isdst <- -1
        breaks <- as.Date(seq(start, end, "years")) - 1
      } else if (valid == 5L) {
        qtr <- rep(c(0L, 3L, 6L, 9L), each = 3L)
        start$mon <- qtr[start$mon + 1L]
        start$mday <- 1L
        end <- as.POSIXlt(max(x, na.rm = TRUE))
        end <- as.POSIXlt(end + (93 * 86400))
        end$mon <- qtr[end$mon + 1L]
        end$mday <- 1L
        end$isdst <- -1
        breaks <- as.Date(seq(start, end, "3 months")) - 1
      } else {
        start <- as.Date(start)
        maxx <- max(x, na.rm = TRUE)
        breaks <- seq(start, maxx + incr, breaks)
        breaks <- breaks[seq_len(1L + max(which(breaks < maxx)))]
      }
    } else stop("invalid specification of 'breaks'")
  }
  res <- ghist.default(unclass(x), unclass(breaks), plot = FALSE, 
                       warn.unused = FALSE, ...)
  res$equidist <- TRUE
  res$intensities <- res$intensities * incr
  res$xname <- xlab
  if (plot) {
    myplot <- function(res, xlab, freq, format, breaks, right, include.lowest,
                       labels = FALSE, axes = TRUE, xaxt = par("xaxt"), ...,
                       grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
      gplot(res, xlab = xlab, axes = FALSE, freq = freq, labels = labels, ...,
            grid = grid, col.grid = col.grid, col.acc = col.acc)
      if (axes && xaxt != "n") {
        axis(2, ..., lwd = 0, lwd.ticks = 1, tcl = -.2, las = 1,
             col.axis = 'grey50', col.ticks = 'grey50')
        if (num.br) 
          breaks <- c.Date(res$breaks)
        axis.Date(1, at = breaks, format = format, ..., lwd = 0, 
                  lwd.ticks = 1, tcl = -.2, las = 1,
                  col.axis = 'grey50', col.ticks = 'grey50')
      }
    }
    myplot(res, xlab, freq, format, breaks, ..., 
           grid = grid, col.grid = col.grid, col.acc = col.acc)
  }
  invisible(res)
}

#' @rdname ghist.Date
#' @export
ghist.POSIXt <- function(x, breaks, ..., xlab = deparse(substitute(x)),
                         plot = TRUE, freq = FALSE, start.on.monday = TRUE,
                         format, grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
  if (!inherits(x, "POSIXt")) 
    stop("wrong method")
  xlab
  x <- as.POSIXct(x)
  incr <- 1
  if (missing(breaks)) 
    stop("Must specify 'breaks' in hist(<POSIXt>)")
  if (inherits(breaks, "POSIXt")) {
    breaks <- as.POSIXct(breaks)
    d <- min(abs(diff(unclass(breaks))))
    if (d > 60) 
      incr <- 60
    if (d > 3600) 
      incr <- 3600
    if (d > 86400) 
      incr <- 86400
    if (d > 86400 * 7) 
      incr <- 86400 * 7
    if (d > 86400 * 28) 
      incr <- 86400 * 28
    if (d > 86400 * 366) 
      incr <- 86400 * 366
    num.br <- FALSE
  } else {
    num.br <- is.numeric(breaks) && length(breaks) == 1
    if (num.br) {
    } else if (is.character(breaks) && length(breaks) == 1) {
      valid <- pmatch(breaks, c("secs", "mins", "hours", "days", "weeks",
                                "months", "years", "quarters"))
      if (is.na(valid)) 
        stop("invalid specification of 'breaks'")
      start <- as.POSIXlt(min(x, na.rm = TRUE))
      incr <- 1
      if (valid > 1L) {
        start$sec <- 0
        incr <- 59.99
      }
      if (valid > 2L) {
        start$min <- 0L
        incr <- 3600 - 1
      }
      if (valid > 3L) {
        start$hour <- 0L
        incr <- 86400 - 1
      }
      if (valid > 4L) {
        start$isdst <- -1L
      }
      if (valid == 5L) {
        start$mday <- start$mday - start$wday
        if (start.on.monday) 
          start$mday <- start$mday + ifelse(start$wday > 0, 1, -6)
        incr <- 7 * 86400
      }
      if (valid == 6L) {
        start$mday <- 1L
        end <- as.POSIXlt(max(x, na.rm = TRUE))
        end <- as.POSIXlt(end + (31 * 86400))
        end$mday <- 1L
        end$isdst <- -1L
        breaks <- seq(start, end, "months")
        ind <- seq_along(breaks[-1L])
        breaks[ind] <- breaks[ind] - 86400
      } else if (valid == 7L) {
        start$mon <- 0L
        start$mday <- 1L
        end <- as.POSIXlt(max(x, na.rm = TRUE))
        end <- as.POSIXlt(end + (366 * 86400))
        end$mon <- 0L
        end$mday <- 1L
        end$isdst <- -1L
        breaks <- seq(start, end, "years")
        ind <- seq_along(breaks[-1L])
        breaks[ind] <- breaks[ind] - 86400
      } else if (valid == 8L) {
        qtr <- rep(c(0L, 3L, 6L, 9L), each = 3L)
        start$mon <- qtr[start$mon + 1L]
        start$mday <- 1L
        end <- as.POSIXlt(max(x, na.rm = TRUE))
        end <- as.POSIXlt(end + (93 * 86400))
        end$mon <- qtr[end$mon + 1L]
        end$mday <- 1L
        end$isdst <- -1L
        breaks <- seq(start, end, "3 months")
        ind <- seq_along(breaks[-1L])
        breaks[ind] <- breaks[ind] - 86400
      } else {
        maxx <- max(x, na.rm = TRUE)
        breaks <- seq(start, maxx + incr, breaks)
        breaks <- breaks[seq_len(1L + max(which(breaks < maxx)))]
      }
    } else stop("invalid specification of 'breaks'")
  }
  res <- ghist.default(unclass(x), unclass(breaks), plot = FALSE,
                       warn.unused = FALSE, ...)
  res$equidist <- TRUE
  res$intensities <- res$intensities * incr
  res$xname <- xlab
  if (plot) {
    myplot <- function(res, xlab, freq, format, breaks, right, include.lowest,
                       labels = FALSE, axes = TRUE, xaxt = par("xaxt"), ..., 
                       grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
      gplot(res, xlab = xlab, axes = FALSE, freq = freq, labels = labels, ..., 
            grid = grid, col.grid = col.grid, col.acc = col.acc)
      if (axes) {
        axis(2, ..., lwd = 0, lwd.ticks = 1, tcl = -.2, las = 1,
             col.axis = 'grey50', col.ticks = 'grey50')
        if (xaxt != "n") {
          if (num.br) 
            breaks <- c.POSIXct(res$breaks)
          axis.POSIXct(1, at = breaks, format = format, ..., lwd = 0, 
                       lwd.ticks = 1, tcl = -.2, las = 1,
                       col.axis = 'grey50', col.ticks = 'grey50')
        }
      }
    }
    myplot(res, xlab, freq, format, breaks, ...,
           grid = grid, col.grid = col.grid, col.acc = col.acc)
  }
  invisible(res)
}

#' gbarplots
#' 
#' Creates a gbarplot with vertical or horizontal bars
#' 
#' @inheritParams graphics::barplot
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gbarplot <- function(height, ...) {
  UseMethod('gbarplot')
}

#' @rdname gbarplot
#' @export
gbarplot.default <- function(height, width = 1, space = NULL, names.arg = NULL,
                             legend.text = NULL, beside = FALSE, horiz = FALSE, density = NULL, 
                             angle = 45, col = NULL, border = par("fg"), main = NULL, 
                             sub = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, 
                             xpd = TRUE, log = "", axes = TRUE, axisnames = TRUE, cex.axis = .8, 
                             cex.names = par("cex.axis"), inside = TRUE, plot = TRUE, 
                             axis.lty = 0, offset = 0, add = FALSE, args.legend = NULL, 
                             grid = TRUE, col.grid = 'grey90', col.acc = 'white', ...) {
  if (!missing(inside)) 
    .NotYetUsed("inside", error = FALSE)
  if (is.null(space)) 
    space <- if (is.matrix(height) && beside) 
      c(0, 1)
  else 0.2
  space <- space * mean(width)
  if (plot && axisnames && is.null(names.arg)) 
    names.arg <- if (is.matrix(height)) 
      colnames(height)
  else names(height)
  if (is.vector(height) || (is.array(height) && (length(dim(height)) == 1))) {
    height <- cbind(height)
    beside <- TRUE
    if (is.null(col)) 
      col <- "white"
  } else if (is.matrix(height)) {
    if (is.null(col)) 
      col <- gray.colors(nrow(height))
  } else stop("'height' must be a vector or a matrix")
  if (is.logical(legend.text)) 
    legend.text <- if (legend.text && is.matrix(height)) 
      rownames(height)
  stopifnot(is.character(log))
  logx <- logy <- FALSE
  if (log != "") {
    logx <- length(grep("x", log)) > 0L
    logy <- length(grep("y", log)) > 0L
  }
  if ((logx || logy) && !is.null(density)) 
    stop("Cannot use shading lines in bars when log scale is used")
  NR <- nrow(height)
  NC <- ncol(height)
  if (beside) {
    if (length(space) == 2) 
      space <- rep.int(c(space[2L], rep.int(space[1L], NR - 1)), NC)
    width <- rep_len(width, NR)
  } else {
    width <- rep_len(width, NC)
  }
  offset <- rep_len(as.vector(offset), length(width))
  delta <- width/2
  w.r <- cumsum(space + width)
  w.m <- w.r - delta
  w.l <- w.m - delta
  log.dat <- (logx && horiz) || (logy && !horiz)
  if (log.dat) {
    if (min(height + offset, na.rm = TRUE) <= 0) 
      stop("log scale error: at least one 'height + offset' value <= 0")
    if (logx && !is.null(xlim) && min(xlim) <= 0) 
      stop("log scale error: 'xlim' <= 0")
    if (logy && !is.null(ylim) && min(ylim) <= 0) 
      stop("log scale error: 'ylim' <= 0")
    rectbase <- if (logy && !horiz && !is.null(ylim)) 
      ylim[1L]
    else if (logx && horiz && !is.null(xlim)) 
      xlim[1L]
    else 0.9 * min(height, na.rm = TRUE)
  } else rectbase <- 0
  if (!beside) 
    height <- rbind(rectbase, apply(height, 2L, cumsum))
  rAdj <- offset + (if (log.dat) 
    0.9 * height
    else -0.01 * height)
  delta <- width/2
  w.r <- cumsum(space + width)
  w.m <- w.r - delta
  w.l <- w.m - delta
  if (horiz) {
    if (is.null(xlim)) 
      xlim <- range(rAdj, height + offset, na.rm = TRUE)
    if (is.null(ylim)) 
      ylim <- c(min(w.l), max(w.r))
  } else {
    if (is.null(xlim)) 
      xlim <- c(min(w.l), max(w.r))
    if (is.null(ylim)) 
      ylim <- range(rAdj, height + offset, na.rm = TRUE)
  }
  if (beside) 
    w.m <- matrix(w.m, ncol = NC)
  if (plot) {
    dev.hold()
    opar <- if (horiz) 
      par(xaxs = "i", xpd = xpd)
    else par(yaxs = "i", xpd = xpd)
    on.exit({
      dev.flush()
      par(opar)
    })
    if (!add) {
      ## use 1,1 here to appease warning with log(y)
      gplot(1, 1, xlim = xlim, ylim = ylim, axes = FALSE, xlab = '', ylab = '',
            grid = grid, col.grid = col.grid, col.acc = col.acc, log = log, pch = '')
    }
    xyrect <- function(x1, y1, x2, y2, horizontal = TRUE, ...) {
      if (horizontal) 
        rect(x1, y1, x2, y2, ...)
      else rect(y1, x1, y2, x2, ...)
    }
    if (beside) 
      xyrect(rectbase + offset, w.l, c(height) + offset, 
             w.r, horizontal = horiz, angle = angle, density = density, 
             col = col, border = border)
    else {
      for (i in 1L:NC) {
        xyrect(height[1L:NR, i] + offset[i], w.l[i], 
               height[-1, i] + offset[i], w.r[i], horizontal = horiz, 
               angle = angle, density = density, col = col, 
               border = border)
      }
    }
    if (axisnames && !is.null(names.arg)) {
      at.l <- if (length(names.arg) != length(w.m)) {
        if (length(names.arg) == NC) 
          colMeans(w.m)
        else stop("incorrect number of names")
      } else w.m
      axis(if (horiz) 
        2
        else 1, at = at.l, labels = names.arg, lty = axis.lty, 
        lwd = 0, lwd.ticks = 1, tcl = -.2, las = 1,
        col.axis = 'grey50', col.ticks = 'grey50',
        cex.axis = cex.names, ...)
    }
    if (!is.null(legend.text)) {
      legend.col <- rep_len(col, length(legend.text))
      if ((horiz & beside) || (!horiz & !beside)) {
        legend.text <- rev(legend.text)
        legend.col <- rev(legend.col)
        density <- rev(density)
        angle <- rev(angle)
      }
      xy <- par("usr")
      if (is.null(args.legend)) {
        legend(xy[2L] - xinch(0.1), xy[4L] - yinch(0.1), 
               legend = legend.text, angle = angle, density = density, 
               fill = legend.col, xjust = 1, yjust = 1)
      } else {
        args.legend1 <- list(x = xy[2L] - xinch(0.1), 
                             y = xy[4L] - yinch(0.1), legend = legend.text, 
                             angle = angle, density = density, fill = legend.col, 
                             xjust = 1, yjust = 1)
        args.legend1[names(args.legend)] <- args.legend
        do.call("legend", args.legend1)
      }
    }
    title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
    if (axes) 
      axis(if (horiz) 
        1
        else 2, cex.axis = cex.axis, lwd = 0, lwd.ticks = 1,
        tcl = -.2, las = 1, col.axis = 'grey50', col.ticks = 'grey50', ...)
    invisible(w.m)
  }
  else w.m
}

#' gboxplots
#' 
#' Produce a box-and-whisker plot(s) of the given (grouped) values.
#' 
#' @inheritParams graphics::boxplot
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gboxplot <- function(x, ...) {
  UseMethod('gboxplot')
}

#' @rdname gboxplot
#' @export
gboxplot.default <- function(x, ..., range = 1.5, width = NULL, varwidth = FALSE, 
                             notch = FALSE, outline = TRUE, names, plot = TRUE,border = par("fg"),
                             col = NULL, log = "", pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
                             horizontal = FALSE, add = FALSE, at = NULL, 
                             grid = TRUE, col.grid = "grey90", col.acc = "white") {
  args <- list(x, ...)
  namedargs <- if (!is.null(attributes(args)$names)) 
    attributes(args)$names != ""
  else rep_len(FALSE, length(args))
  groups <- if (is.list(x)) 
    x
  else args[!namedargs]
  if (0L == (n <- length(groups))) 
    stop("invalid first argument")
  if (length(class(groups))) 
    groups <- unclass(groups)
  if (!missing(names)) 
    attr(groups, "names") <- names
  else {
    if (is.null(attr(groups, "names"))) 
      attr(groups, "names") <- 1L:n
    names <- attr(groups, "names")
  }
  cls <- sapply(groups, function(x) class(x)[1L])
  cl <- if (all(cls == cls[1L])) 
    cls[1L]
  else NULL
  for (i in 1L:n) groups[i] <- list(boxplot.stats(unclass(groups[[i]]), range))
  stats <- matrix(0, nrow = 5L, ncol = n)
  conf <- matrix(0, nrow = 2L, ncol = n)
  ng <- out <- group <- numeric(0L)
  ct <- 1
  for (i in groups) {
    stats[, ct] <- i$stats
    conf[, ct] <- i$conf
    ng <- c(ng, i$n)
    if ((lo <- length(i$out))) {
      out <- c(out, i$out)
      group <- c(group, rep.int(ct, lo))
    }
    ct <- ct + 1
  }
  if (length(cl) && cl != "numeric") 
    oldClass(stats) <- cl
  z <- list(stats = stats, n = ng, conf = conf, out = out, 
            group = group, names = names)
  if (plot) {
    if (is.null(pars$boxfill) && is.null(args$boxfill)) 
      pars$boxfill <- col
    do.call("gbxp", c(list(z, notch = notch, width = width, 
                           varwidth = varwidth, log = log, border = border, 
                           pars = pars, outline = outline, horizontal = horizontal, 
                           add = add, at = at, grid = grid, col.grid = col.grid,
                           col.acc = col.acc), args[namedargs]))
    invisible(z)
  }
  else z
}

#' Draw a gboxplot for each column (row) of a matrix
#' 
#' Interpreting the columns (or rows) of a matrix as different groupd, draw a 
#' gboxplot for each.
#' 
#' @inheritParams graphics::boxplot.matrix
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gboxplot.matrix <- function(x, use.cols = TRUE, ...,
                            grid = TRUE, col.grid = "grey90", col.acc = "white") {
  groups <- if (use.cols) 
    split(x, rep.int(1L:ncol(x), rep.int(nrow(x), ncol(x))))
  else split(x, seq(nrow(x)))
  if (length(nam <- dimnames(x)[[1 + use.cols]]))
    names(groups) <- nam
  invisible(gboxplot(groups, ...))
}

#' @rdname gboxplot
#' @export
gboxplot.formula <- function(formula, data = NULL, ..., subset, na.action = NULL,
                             grid = TRUE, col.grid = "grey90", col.acc = "white") {
  if (missing(formula) || (length(formula) != 3L)) 
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m$... <- m$grid <- m$col.grid <- m$col.acc <- NULL
  m$na.action <- na.action
  require(stats, quietly = TRUE)
  m[[1L]] <- quote(stats::model.frame)
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, "terms"), "response")
  gboxplot(split(mf[[response]], mf[-response]), 
           grid = grid, col.grid = col.grid, col.acc = col.acc, ...)
}

#' Draw boxplots from summaries
#' 
#' \code{gbxp} draws box plots based on the given summaries in \code{z}. It is
#' usually called from within \code{\link{gboxplot}} but can be invoked 
#' directly.
#' 
#' @inheritParams graphics::bxp
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gbxp <- function(z, notch = FALSE, width = NULL, varwidth = FALSE, outline = TRUE, 
                 notch.frac = 0.5, log = "", border = par("fg"), pars = NULL, 
                 horizontal = FALSE, add = FALSE, at = NULL, 
                 show.names = NULL, ..., 
                 grid = TRUE, col.grid = "grey90", col.acc = "white") {
  pars <- c(list(...), pars)
  pars <- pars[unique(names(pars))]
  bplt <- function(x, wid, stats, out, conf, notch, xlog, i) {
    ok <- TRUE
    if (!anyNA(stats)) {
      xP <- if (xlog) 
        function(x, w) x * exp(w)
      else function(x, w) x + w
      wid <- wid/2
      if (notch) {
        ok <- stats[2L] <= conf[1L] && conf[2L] <= stats[4L]
        xx <- xP(x, wid * c(-1, 1, 1, notch.frac, 1, 
                            1, -1, -1, -notch.frac, -1))
        yy <- c(stats[c(2, 2)], conf[1L], stats[3L], 
                conf[2L], stats[c(4, 4)], conf[2L], stats[3L], 
                conf[1L])
      } else {
        xx <- xP(x, wid * c(-1, 1, 1, -1))
        yy <- stats[c(2, 2, 4, 4)]
      }
      if (!notch) 
        notch.frac <- 1
      wntch <- notch.frac * wid
      xypolygon(xx, yy, lty = "blank", col = boxfill[i])
      xysegments(xP(x, -wntch), stats[3L], xP(x, +wntch), 
                 stats[3L], lty = medlty[i], lwd = medlwd[i], 
                 col = medcol[i], lend = 1)
      xypoints(x, stats[3L], pch = medpch[i], cex = medcex[i], 
               col = medcol[i], bg = medbg[i])
      xysegments(rep.int(x, 2), stats[c(1, 5)], rep.int(x, 
                                                        2), stats[c(2, 4)], lty = whisklty[i], lwd = whisklwd[i], 
                 col = whiskcol[i])
      xysegments(rep.int(xP(x, -wid * staplewex[i]), 2), 
                 stats[c(1, 5)], rep.int(xP(x, +wid * staplewex[i]), 
                                         2), stats[c(1, 5)], lty = staplelty[i], lwd = staplelwd[i], 
                 col = staplecol[i])
      xypolygon(xx, yy, lty = boxlty[i], lwd = boxlwd[i], 
                border = boxcol[i])
      if ((nout <- length(out))) {
        xysegments(rep(x - wid * outwex, nout), out, 
                   rep(x + wid * outwex, nout), out, lty = outlty[i], 
                   lwd = outlwd[i], col = outcol[i])
        xypoints(rep.int(x, nout), out, pch = outpch[i], 
                 lwd = outlwd[i], cex = outcex[i], col = outcol[i], 
                 bg = outbg[i])
      }
      if (any(inf <- !is.finite(out))) {
        warning(sprintf(ngettext(length(unique(out[inf])), 
                                 "Outlier (%s) in boxplot %d is not drawn", 
                                 "Outliers (%s) in boxplot %d are not drawn"), 
                        paste(unique(out[inf]), collapse = ", "), i), domain = NA)
      }
    }
    return(ok)
  }
  if (!is.list(z) || 0L == (n <- length(z$n))) 
    stop("invalid first argument")
  if (is.null(at)) 
    at <- 1L:n
  else if (length(at) != n) 
    stop(gettextf("'at' must have same length as 'z$n', i.e. %d", n), domain = NA)
  if (is.null(z$out)) 
    z$out <- numeric()
  if (is.null(z$group) || !outline) 
    z$group <- integer()
  if (is.null(pars$ylim)) 
    ylim <- range(z$stats[is.finite(z$stats)], 
                  if (outline) z$out[is.finite(z$out)], 
                  if (notch) z$conf[is.finite(z$conf)])
  else {
    ylim <- pars$ylim
    pars$ylim <- NULL
  }
  if (length(border) == 0L) 
    border <- par("fg")
  dev.hold()
  on.exit(dev.flush())
  if (!add) {
    if (is.null(pars$xlim)) 
      xlim <- range(at, finite = TRUE) + c(-0.5, 0.5)
    else {
      xlim <- pars$xlim
      pars$xlim <- NULL
    }
    if (horizontal)
      ## use 1,1 here to appease warning with log(y)
      gplot(1, 1 , ylim = xlim, xlim = ylim, log = log, xaxs = pars$yaxs,
            axes = FALSE, xlab = '', ylab = '', pch = '')
    else gplot(1, 1, xlim = xlim, ylim = ylim, log = log, yaxs = pars$yaxs,
               axes = FALSE, xlab = '', ylab = '')
  }
  xlog <- (par("ylog") && horizontal) || (par("xlog") && !horizontal)
  pcycle <- function(p, def1, def2 = NULL) rep(if (length(p)) p else if (length(def1)) def1 else def2, 
                                               length.out = n)
  p <- function(sym) pars[[sym, exact = TRUE]]
  boxlty <- pcycle(pars$boxlty, p("lty"), par("lty"))
  boxlwd <- pcycle(pars$boxlwd, p("lwd"), par("lwd"))
  boxcol <- pcycle(pars$boxcol, border)
  boxfill <- pcycle(pars$boxfill, par("bg"))
  boxwex <- pcycle(pars$boxwex, 0.8 * {
    if (n <= 1) 
      1
    else stats::quantile(diff(sort(if (xlog) 
      log(at)
      else at)), 0.1)
  })
  medlty <- pcycle(pars$medlty, p("lty"), par("lty"))
  medlwd <- pcycle(pars$medlwd, 3 * p("lwd"), 3 * par("lwd"))
  medpch <- pcycle(pars$medpch, NA_integer_)
  medcex <- pcycle(pars$medcex, p("cex"), par("cex"))
  medcol <- pcycle(pars$medcol, border)
  medbg <- pcycle(pars$medbg, p("bg"), par("bg"))
  whisklty <- pcycle(pars$whisklty, p("lty"), "dashed")
  whisklwd <- pcycle(pars$whisklwd, p("lwd"), par("lwd"))
  whiskcol <- pcycle(pars$whiskcol, border)
  staplelty <- pcycle(pars$staplelty, p("lty"), par("lty"))
  staplelwd <- pcycle(pars$staplelwd, p("lwd"), par("lwd"))
  staplecol <- pcycle(pars$staplecol, border)
  staplewex <- pcycle(pars$staplewex, 0.5)
  outlty <- pcycle(pars$outlty, "blank")
  outlwd <- pcycle(pars$outlwd, p("lwd"), par("lwd"))
  outpch <- pcycle(pars$outpch, p("pch"), par("pch"))
  outcex <- pcycle(pars$outcex, p("cex"), par("cex"))
  outcol <- pcycle(pars$outcol, border)
  outbg <- pcycle(pars$outbg, p("bg"), par("bg"))
  outwex <- pcycle(pars$outwex, 0.5)
  width <- if (!is.null(width)) {
    if (length(width) != n | anyNA(width) | any(width <= 0)) 
      stop("invalid boxplot widths")
    boxwex * width/max(width)
  } else if (varwidth) 
    boxwex * sqrt(z$n/max(z$n))
  else if (n == 1) 
    0.5 * boxwex
  else rep.int(boxwex, n)
  if (horizontal) {
    xypoints <- function(x, y, ...) points(y, x, ...)
    xypolygon <- function(x, y, ...) polygon(y, x, ...)
    xysegments <- function(x0, y0, x1, y1, ...) segments(y0, x0, y1, x1, ...)
  } else {
    xypoints <- points
    xypolygon <- polygon
    xysegments <- segments
  }
  ok <- TRUE
  for (i in 1L:n) 
    ok <- ok & bplt(at[i], wid = width[i], stats = z$stats[, i], 
                    out = z$out[z$group == i], conf = z$conf[, i], 
                    notch = notch, xlog = xlog, i = i)
  if (!ok) 
    warning("some notches went outside hinges ('box'): maybe set notch=FALSE")
  axes <- is.null(pars$axes)
  if (!axes) {
    axes <- pars$axes
    pars$axes <- NULL
  }
  if (axes) {
    ax.pars <- pars[names(pars) %in% c("xaxt", "yaxt", "xaxp", "yaxp",
                                       "cex.axis", "format")]
    gax.pars <- list(lwd = 0, lwd.ticks = 1, tcl = -.2, las = 1,
                     col.axis = 'grey50', col.ticks = 'grey50')
    if (is.null(show.names)) 
      show.names <- n > 1
    if (show.names) 
      do.call("axis", c(list(side = 1 + horizontal, at = at, labels = z$names),
                        gax.pars, ax.pars))
    do.call("Axis", c(list(x = z$stats, side = 2 - horizontal), gax.pars, ax.pars))
  }
  do.call("title", 
          pars[names(pars) %in% c("main", "cex.main", "col.main","sub","cex.sub",
                                  "col.sub", "xlab", "ylab", "cex.lab", "col.lab")])
  invisible(at)
}

#' 1-D scatter plots
#' 
#' \code{gstripchart} produces one-dimensional scatter plots (or dot plots) of 
#' the given data. These plots are a good alternative to \code{\link{gboxplot}s}
#' when the sample sizes are small.
#' 
#' @inheritParams graphics::stripchart
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gstripchart <- function(x, ...) {
  UseMethod('gstripchart')
}

#' @rdname gstripchart
#' @export
gstripchart.default <- function(x, method = "overplot", jitter = 0.1, offset = 1/3, 
                                vertical = FALSE, group.names, add = FALSE, at = NULL, xlim = NULL, 
                                ylim = NULL, ylab = NULL, xlab = NULL, dlab = "", glab = "", 
                                log = "", pch = 0, col = par("fg"), cex = par("cex"), axes = TRUE, 
                                frame.plot = axes, ..., 
                                grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
  method <- pmatch(method, c("overplot", "jitter", "stack"))[1L]
  if (is.na(method) || method == 0L) 
    stop("invalid plotting method")
  groups <- if (is.list(x)) 
    x
  else if (is.numeric(x)) 
    list(x)
  n <- length(groups)
  if (!n) 
    stop("invalid first argument")
  if (!missing(group.names)) 
    attr(groups, "names") <- group.names
  else if (is.null(attr(groups, "names"))) 
    attr(groups, "names") <- seq_len(n)
  if (is.null(at)) 
    at <- seq_len(n)
  else if (length(at) != n) 
    stop(gettextf("'at' must have length equal to the number %d of groups", 
                  n), domain = NA)
  if (is.null(dlab)) 
    dlab <- deparse(substitute(x))
  dev.hold()
  on.exit(dev.flush())
  if (!add) {
    dlim <- range(unlist(groups, use.names = FALSE), na.rm = TRUE)
    glim <- c(1L, n)
    if (method == 2L) {
      glim <- glim + jitter * if (n == 1) 
        c(-5, 5)
      else c(-2, 2)
    } else if (method == 3) {
      glim <- glim + if (n == 1L) 
        c(-1, 1)
      else c(0, 0.5)
    }
    if (is.null(xlim)) 
      xlim <- if (vertical) 
        glim
    else dlim
    if (is.null(ylim)) 
      ylim <- if (vertical) 
        dlim
    else glim
    ## use 1,1 here to appease warning with log(y)
    gplot(1, 1, xlim = xlim, ylim = ylim, axes = FALSE, xlab = '', ylab = '',
          grid = grid, col.grid = col.grid, col.acc = col.acc, log = log, pch = '')
    gax.pars <- list(lwd = 0, lwd.ticks = 1, tcl = -.2, las = 1, cex.axis = .8,
                     col.axis = 'grey50', col.ticks = 'grey50')
    if (vertical) {
      if (axes) {
        if (n > 1L) 
          do.call("axis", c(list(side = 1, at = at, labels = names(groups)),gax.pars))
        do.call("Axis", c(list(x = x, side = 2), gax.pars))
      }
      if (is.null(ylab)) 
        ylab <- dlab
      if (is.null(xlab)) 
        xlab <- glab
    } else {
      if (axes) {
        do.call("Axis", c(list(x = x, side = 1), gax.pars))
        if (n > 1L) 
          do.call("axis", c(list(side = 2, at = at, labels = names(groups)),gax.pars))
      }
      if (is.null(xlab)) 
        xlab <- dlab
      if (is.null(ylab)) 
        ylab <- glab
    }
    title(xlab = xlab, ylab = ylab, ...)
  }
  csize <- cex * if (vertical) 
    xinch(par("cin")[1L])
  else yinch(par("cin")[2L])
  for (i in seq_len(n)) {
    x <- groups[[i]]
    y <- rep.int(at[i], length(x))
    if (method == 2L) 
      y <- y + stats::runif(length(y), -jitter, jitter)
    else if (method == 3L) {
      xg <- split(x, factor(x))
      xo <- lapply(xg, seq_along)
      x <- unlist(xg, use.names = FALSE)
      y <- rep.int(at[i], length(x)) + (unlist(xo, use.names = FALSE) - 1) * offset * csize
    }
    if (vertical) 
      points(y, x, col = col[(i - 1L)%%length(col) + 1L], 
             pch = pch[(i - 1L)%%length(pch) + 1L], cex = cex, ...)
    else points(x, y, col = col[(i - 1L)%%length(col) + 1L], 
                pch = pch[(i - 1L)%%length(pch) + 1L], cex = cex, ...)
  }
  invisible()
}

#' @rdname gstripchart
#' @export
gstripchart.formula <- function(x, data = NULL, dlab = NULL, ..., 
                                subset, na.action = NULL, 
                                grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
  if (missing(x) || (length(x) != 3L)) 
    stop("formula missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m$... <- m$grid <- m$col.grid <- m$col.acc <- NULL
  m$formula <- m$x
  m$x <- NULL
  m$na.action <- na.action
  require(stats, quietly = TRUE)
  m[[1L]] <- quote(stats::model.frame)
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, "terms"), "response")
  if (is.null(dlab)) 
    dlab <- names(mf)[response]
  gstripchart(split(mf[[response]], mf[-response]), dlab = dlab, ...,
              grid = grid, col.grid = col.grid, col.acc = col.acc)
}

#' Spine plots and spinograms
#' 
#' Spine plots are a special case of mosaic plots and can be seen as a 
#' generalization of stacked (or highlighted) bar plots. Analogously, spinograms
#' are an extension of histograms.
#' 
#' @inheritParams graphics::spineplot
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gspineplot <- function(x, ...) {
  UseMethod('gspineplot')
}

#' @rdname gspineplot
#' @export
gspineplot.default <- function(x, y = NULL, breaks = NULL, tol.ylab = 0.05,
                               off = NULL, ylevels = NULL, col = NULL, main = "", xlab = NULL, 
                               ylab = NULL, xaxlabels = NULL, yaxlabels = NULL, xlim = NULL, 
                               ylim = c(0, 1), axes = TRUE, ..., grid = TRUE, col.grid = 'grey90',
                               col.acc = 'white') {
  if (missing(y)) {
    if (length(dim(x)) != 2L) 
      stop("a 2-way table has to be specified")
    tab <- x
    x.categorical <- TRUE
    if (is.null(xlab)) 
      xlab <- names(dimnames(tab))[1L]
    if (is.null(ylab)) 
      ylab <- names(dimnames(tab))[2L]
    xnam <- dimnames(tab)[[1L]]
    ynam <- dimnames(tab)[[2L]]
    ny <- NCOL(tab)
    nx <- NROW(tab)
  } else {
    if (!is.factor(y)) 
      stop("dependent variable should be a factor")
    if (!is.null(ylevels)) 
      y <- factor(y, levels = if (is.numeric(ylevels)) 
        levels(y)[ylevels]
        else ylevels)
    x.categorical <- is.factor(x)
    if (is.null(xlab)) 
      xlab <- deparse(substitute(x))
    if (is.null(ylab)) 
      ylab <- deparse(substitute(y))
    if (x.categorical) {
      tab <- table(x, y)
      xnam <- levels(x)
      nx <- NROW(tab)
    }
    ynam <- levels(y)
    ny <- length(ynam)
  }
  if (is.null(col)) 
    col <- gray.colors(ny)
  col <- rep_len(col, ny)
  off <- if (!x.categorical) 
    0
  else if (is.null(off)) 
    0.02
  else off/100
  yaxlabels <- if (is.null(yaxlabels)) 
    ynam
  else rep_len(yaxlabels, ny)
  if (x.categorical) {
    xat <- c(0, cumsum(prop.table(margin.table(tab, 1)) + off))
    xaxlabels <- if (is.null(xaxlabels)) 
      xnam
    else rep_len(xaxlabels, nx)
  } else {
    if (!(xnumeric <- is.numeric(x))) {
      xorig <- x
      x <- as.numeric(x)
    }
    if (is.null(breaks)) {
      breaks <- list()
    } else {
      breaks <- as.numeric(breaks)
    }
    if (!is.list(breaks)) 
      breaks <- list(breaks = breaks)
    breaks <- c(list(x = x), breaks)
    breaks$plot <- FALSE
    breaks <- do.call("ghist", breaks)$breaks
    x1 <- cut(x, breaks = breaks, include.lowest = TRUE)
    xat <- c(0, cumsum(prop.table(table(x1))))
    tab <- table(x1, y)
    nx <- NROW(tab)
    xaxlabels <- if (is.null(xaxlabels)) {
      if (xnumeric) 
        breaks
      else c(xorig[1L], xorig[c(diff(as.numeric(x1)) > 0, TRUE)])
    } else {
      rep_len(xaxlabels, nx + 1L)
    }
  }
  yat <- rbind(0, apply(prop.table(tab, 1), 1L, cumsum))
  yat[is.na(yat)] <- 1
  if (is.null(xlim)) 
    xlim <- c(0, 1 + off * (nx - 1L))
  else if (any(xlim < 0) || any(xlim > 1)) {
    warning("x axis is on a cumulative probability scale, 'xlim' must be in [0,1]")
    if (min(xlim) > 1 || max(xlim) < 0) 
      xlim <- c(0, 1)
    else xlim <- c(max(min(xlim), 0), min(max(xlim), 1))
  }
  if (any(ylim < 0) || any(ylim > 1)) {
    warning("y axis is on a cumulative probability scale, 'ylim' must be in [0,1]")
    if (min(ylim) > 1 || max(ylim) < 0) 
      ylim <- c(0, 1)
    else ylim <- c(max(min(ylim), 0), min(max(ylim), 1))
  }
  dev.hold()
  on.exit(dev.flush())
  gplot(0, 0, xlim = xlim, ylim = ylim, type = "n", axes = FALSE, 
        xaxs = "i", yaxs = "i", main = main, xlab = xlab, ylab = ylab,
        grid = grid, col.grid = col.grid, col.acc = col.acc)
  ybottom <- as.vector(yat[-(ny + 1L), ])
  ytop <- as.vector(yat[-1L, ])
  xleft <- rep(xat[1L:nx], rep(ny, nx))
  xright <- rep(xat[2L:(nx + 1L)] - off, rep(ny, nx))
  col <- rep(col, nx)
  rect(xleft, ybottom, xright, ytop, col = col, ...)
  if (axes) {
    if (x.categorical) 
      axis(1, at = (xat[1L:nx] + xat[2L:(nx + 1L)] - off)/2, cex.axis = .8,
           labels = xaxlabels, tick = FALSE, las = 1, lwd = 0, lwd.ticks = 1,
           tcl = -.2, col.axis = 'grey50', col.ticks = 'grey50')
    else axis(1, at = xat, labels = xaxlabels, las = 1, lwd = 0, lwd.ticks = 1,
              tcl = -.2, col.axis = 'grey50', col.ticks = 'grey50', cex.axis = .8)
    yat <- yat[, 1L]
    equidist <- any(diff(yat) < tol.ylab)
    yat <- if (equidist) 
      seq.int(1/(2 * ny), 1 - 1/(2 * ny), by = 1/ny)
    else (yat[-1L] + yat[-length(yat)])/2
    axis(2, at = yat, labels = yaxlabels, tick = FALSE, las = 1, lwd = 0, cex.axis = .8,
         lwd.ticks = 1,tcl = -.2, col.axis = 'grey50', col.ticks = 'grey50')
    axis(4, las = 1, lwd = 0, lwd.ticks = 1, cex.axis = .8,
         tcl = -.2, col.axis = 'grey50', col.ticks = 'grey50')
  }
  if (!x.categorical) 
    box(col = col.grid)
  names(dimnames(tab)) <- c(xlab, ylab)
  invisible(tab)
}

#' @rdname gspineplot
#' @export
gspineplot.formula <- function(formula, data = NULL, breaks = NULL, tol.ylab = 0.05, 
                               off = NULL, ylevels = NULL, col = NULL, main = "", xlab = NULL, 
                               ylab = NULL, xaxlabels = NULL, yaxlabels = NULL, xlim = NULL, 
                               ylim = c(0, 1), axes = TRUE, ..., subset = NULL,
                               grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
  m <- match.call(expand.dots = FALSE)
  m$grid <- m$col.grid <- m$col.acc <- NULL
  m <- m[c(1L, match(c("formula", "data", "subset"), names(m), 0L))]
  # require(stats, quietly = TRUE)
  m[[1L]] <- quote(stats::model.frame)
  mf <- eval.parent(m)
  if (NCOL(mf) != 2L) 
    stop("'formula' should specify exactly two variables")
  y <- mf[, 1L]
  if (!is.factor(y)) 
    stop("dependent variable should be a factor")
  if (!is.null(ylevels)) 
    y <- factor(y, levels = if (is.numeric(ylevels)) 
      levels(y)[ylevels]
      else ylevels)
  x <- mf[, 2L]
  if (is.null(xlab)) 
    xlab <- names(mf)[2L]
  if (is.null(ylab)) 
    ylab <- names(mf)[1L]
  gspineplot(x, y, breaks = breaks, tol.ylab = tol.ylab, off = off, 
             ylevels = NULL, col = col, main = main, xlab = xlab, 
             ylab = ylab, xaxlabels = xaxlabels, yaxlabels = yaxlabels, 
             xlim = xlim, ylim = ylim, axes = axes, ..., grid = grid,
             col.grid = col.grid, col.acc = col.acc)
}

#' Scatterplot matrices
#' 
#' A matrix of scatterplots is produced.
#' 
#' @inheritParams graphics::pairs
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gpairs <- function(x, ...) {
  UseMethod('gpairs')
}

#' @rdname gpairs
#' @export
gpairs.default <- function(x, labels, panel = points, ..., lower.panel = panel, 
                           upper.panel = panel, diag.panel = NULL, text.panel = textPanel, 
                           label.pos = 0.5 + has.diag/3, line.main = 3, cex.labels = NULL, 
                           font.labels = 1, row1attop = TRUE, gap = 1, log = "",
                           grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
  if (doText <- missing(text.panel) || is.function(text.panel)) 
    textPanel <- function(x = 0.5, y = 0.5, txt, cex, font) 
      text(x, y, txt, cex = cex, font = font)
  localAxis <- function(side, x, y, xpd, bg, col = NULL, main, oma, ...) {
    xpd <- NA
    if (side%%2L == 1L && xl[j]) 
      xpd <- FALSE
    if (side%%2L == 0L && yl[i]) 
      xpd <- FALSE
    if (side%%2L == 1L) 
      Axis(x, side = side, xpd = xpd, ...)
    else Axis(y, side = side, xpd = xpd, ...)
  }
  localPlot <- function(..., main, oma, font.main, cex.main) 
    gplot(..., grid = grid, col.grid = col.grid, col.acc = col.acc)
  localLowerPanel <- function(..., main, oma, font.main, cex.main) lower.panel(...)
  localUpperPanel <- function(..., main, oma, font.main, cex.main) upper.panel(...)
  localDiagPanel <- function(..., main, oma, font.main, cex.main) diag.panel(...)
  dots <- list(...)
  nmdots <- names(dots)
  if (!is.matrix(x)) {
    x <- as.data.frame(x)
    for (i in seq_along(names(x))) {
      if (is.factor(x[[i]]) || is.logical(x[[i]])) 
        x[[i]] <- as.numeric(x[[i]])
      if (!is.numeric(unclass(x[[i]]))) 
        stop("non-numeric argument to 'pairs'")
    }
  } else if (!is.numeric(x)) 
    stop("non-numeric argument to 'gpairs'")
  panel <- match.fun(panel)
  if ((has.lower <- !is.null(lower.panel)) && !missing(lower.panel)) 
    lower.panel <- match.fun(lower.panel)
  if ((has.upper <- !is.null(upper.panel)) && !missing(upper.panel)) 
    upper.panel <- match.fun(upper.panel)
  if ((has.diag <- !is.null(diag.panel)) && !missing(diag.panel)) 
    diag.panel <- match.fun(diag.panel)
  if (row1attop) {
    tmp <- lower.panel
    lower.panel <- upper.panel
    upper.panel <- tmp
    tmp <- has.lower
    has.lower <- has.upper
    has.upper <- tmp
  }
  nc <- ncol(x)
  if (nc < 2) 
    stop("only one column in the argument to 'gpairs'")
  if (doText) {
    if (missing(labels)) {
      labels <- colnames(x)
      if (is.null(labels)) 
        labels <- paste("var", 1L:nc)
    } else if (is.null(labels)) 
      doText <- FALSE
  }
  oma <- if ("oma" %in% nmdots) 
    dots$oma
  main <- if ("main" %in% nmdots) 
    dots$main
  if (is.null(oma)) 
    oma <- c(4, 4, if (!is.null(main)) 6 else 4, 4)
  opar <- par(mfrow = c(nc, nc), mar = rep.int(gap/2, 4), oma = oma)
  on.exit(par(opar))
  dev.hold()
  on.exit(dev.flush(), add = TRUE)
  xl <- yl <- logical(nc)
  if (is.numeric(log)) 
    xl[log] <- yl[log] <- TRUE
  else {
    xl[] <- grepl("x", log)
    yl[] <- grepl("y", log)
  }
  for (i in if (row1attop) 
    1L:nc
    else nc:1L) for (j in 1L:nc) {
      l <- paste0(ifelse(xl[j], "x", ""), ifelse(yl[i], "y", ""))
      localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, 
                type = "n", ..., log = l)
      if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
        box(col = col.grid)
        if (i == 1 && (!(j%%2L) || !has.upper || !has.lower)) 
          localAxis(1L + 2L * row1attop, x[, j], x[, i], ...)
        if (i == nc && (j%%2L || !has.upper || !has.lower)) 
          localAxis(3L - 2L * row1attop, x[, j], x[, i], ...)
        if (j == 1 && (!(i%%2L) || !has.upper || !has.lower)) 
          localAxis(2L, x[, j], x[, i], ...)
        if (j == nc && (i%%2L || !has.upper || !has.lower)) 
          localAxis(4L, x[, j], x[, i], ...)
        mfg <- par("mfg")
        if (i == j) {
          if (has.diag) 
            localDiagPanel(as.vector(x[, i]), ...)
          if (doText) {
            par(usr = c(0, 1, 0, 1))
            if (is.null(cex.labels)) {
              l.wid <- strwidth(labels, "user")
              cex.labels <- max(0.8, min(2, 0.9/max(l.wid)))
            }
            xlp <- if (xl[i]) 
              10^0.5
            else 0.5
            ylp <- if (yl[j]) 
              10^label.pos
            else label.pos
            text.panel(xlp, ylp, labels[i], cex = cex.labels, font = font.labels)
          }
        } else if (i < j) 
          localLowerPanel(as.vector(x[, j]), as.vector(x[, i]), ...)
        else localUpperPanel(as.vector(x[, j]), as.vector(x[, i]), ...)
        if (any(par("mfg") != mfg)) 
          stop("the 'panel' function made a new plot")
      } else par(new = FALSE)
    }
  if (!is.null(main)) {
    font.main <- if ("font.main" %in% nmdots) 
      dots$font.main
    else par("font.main")
    cex.main <- if ("cex.main" %in% nmdots) 
      dots$cex.main
    else par("cex.main")
    mtext(main, 3, line.main, outer = TRUE, at = 0.5, cex = cex.main, 
          font = font.main)
  }
  invisible(NULL)
}

#' @rdname gpairs
#' @export
gpairs.formula <- function(formula, data = NULL, ..., subset, 
                           na.action = stats::na.pass, grid = TRUE, 
                           col.grid = 'grey90', col.acc = 'white') {
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m$... <- grid <- col.grid <- col.acc <- NULL
  m$na.action <- na.action
  m[[1L]] <- quote(stats::model.frame)
  # require(stats, quietly = TRUE)
  mf <- eval(m, parent.frame())
  gpairs(mf, ..., grid = grid, col.grid = col.grid, col.acc = col.acc)
}

#' Plot columns of matrices
#' 
#' Plot the columns of one matrix against the columns of another.
#' 
#' @inheritParams graphics::matplot
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gmatplot <- function(x, y, type = "p", lty = 1:5, lwd = 1, lend = par("lend"), 
                     pch = NULL, col = 1:6, cex = NULL, bg = NA, xlab = NULL, 
                     ylab = NULL, xlim = NULL, ylim = NULL, ..., add = FALSE, 
                     verbose = getOption("verbose"), 
                     grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
  paste.ch <- function(chv) paste0("\"", chv, "\"", collapse = " ")
  str2vec <- function(string) {
    if (nchar(string, type = "c")[1L] > 1L) 
      strsplit(string[1L], NULL)[[1L]]
    else string
  }
  xlabel <- if (!missing(x)) 
    deparse(substitute(x))
  ylabel <- if (!missing(y)) 
    deparse(substitute(y))
  if (missing(x)) {
    if (missing(y)) 
      stop("must specify at least one of 'x' and 'y'")
    else x <- seq_len(NROW(y))
  } else if (missing(y)) {
    y <- x
    ylabel <- xlabel
    x <- seq_len(NROW(y))
    xlabel <- ""
  }
  kx <- ncol(x <- as.matrix(x))
  ky <- ncol(y <- as.matrix(y))
  n <- nrow(x)
  if (n != nrow(y)) 
    stop("'x' and 'y' must have same number of rows")
  if (kx > 1L && ky > 1L && kx != ky) 
    stop("'x' and 'y' must have only 1 or the same number of columns")
  if (kx == 1L) 
    x <- matrix(x, nrow = n, ncol = ky)
  if (ky == 1L) 
    y <- matrix(y, nrow = n, ncol = kx)
  k <- max(kx, ky)
  type <- str2vec(type)
  if (is.null(pch)) {
    pch <- c(1L:9L, 0L, letters, LETTERS)
    if (k > length(pch) && any(type %in% c("p", "o", "b"))) 
      warning("default 'pch' is smaller than number of columns and hence recycled")
  } else if (is.character(pch)) 
    pch <- str2vec(pch)
  if (verbose) 
    message("matplot: doing ", k, " plots with ", 
            paste0(" col= (", paste.ch(col), ")"), 
            paste0(" pch= (", paste.ch(pch), 
                   ")"), " ...\n", domain = NA)
  ii <- match("log", names(xargs <- list(...)), nomatch = 0L)
  log <- if (ii != 0) 
    xargs[[ii]]
  xy <- xy.coords(x, y, xlabel, ylabel, log = log)
  xlab <- if (is.null(xlab)) 
    xy$xlab
  else xlab
  ylab <- if (is.null(ylab)) 
    xy$ylab
  else ylab
  xlim <- if (is.null(xlim)) 
    range(xy$x[is.finite(xy$x)])
  else xlim
  ylim <- if (is.null(ylim)) 
    range(xy$y[is.finite(xy$y)])
  else ylim
  if (length(type) < k) 
    type <- rep_len(type, k)
  if (length(lty) < k) 
    lty <- rep_len(lty, k)
  if (length(lend) < k) 
    lend <- rep_len(lend, k)
  if (length(lwd) < k && !is.null(lwd)) 
    lwd <- rep_len(lwd, k)
  if (length(pch) < k) 
    pch <- rep_len(pch, k)
  if (length(col) < k) 
    col <- rep_len(col, k)
  if (length(bg) < k) 
    bg <- rep_len(bg, k)
  if (is.null(cex)) 
    cex <- 1
  if (length(cex) < k) 
    cex <- rep_len(cex, k)
  ii <- seq_len(k)
  dev.hold()
  on.exit(dev.flush())
  if (!add) {
    ii <- ii[-1L]
    gplot(x[, 1L], y[, 1L], type = type[1L], xlab = xlab, 
          ylab = ylab, xlim = xlim, ylim = ylim, lty = lty[1L], 
          lwd = lwd[1L], lend = lend[1L], pch = pch[1L], col = col[1L], 
          cex = cex[1L], bg = bg[1L], ..., grid = grid, col.grid = col.grid,
          col.acc = col.acc)
  }
  for (i in ii) 
    lines(x[, i], y[, i], type = type[i], lty = lty[i], lwd = lwd[i],
          lend = lend[i], pch = pch[i], col = col[i], cex = cex[i], bg = bg[i])
  invisible()
}

#' @rdname gmatplot
#' @export
gmatpoints <- function(x, y, type = "p", lty = 1:5, lwd = 1, pch = NULL,
                       col = 1:6, ..., grid = TRUE, col.grid = 'grey90', 
                       col.acc = 'white') {
  matplot(x = x, y = y, type = type, lty = lty, lwd = lwd, pch = pch, 
          col = col, add = TRUE, ..., grid = grid, col.grid = col.grid, 
          col.acc = col.acc)
}

#' @rdname gmatplot
#' @export
gmatlines <- function(x, y, type = "l", lty = 1:5, lwd = 1, pch = NULL, 
                      col = 1:6, ..., grid = TRUE, col.grid = 'grey90', 
                      col.acc = 'white') {
  gmatplot(x = x, y = y, type = type, lty = lty, lwd = lwd, pch = pch,
           col = col, add = TRUE, ..., grid = grid, col.grid = col.grid,
           col.acc = col.acc)
}

#' Produce a gsunflower scatter plot
#' 
#' Multiple points are plotted as "sunflowers" with multiple leaves ("petals")
#' such that overplotting is visualised instead of accidental and invisible.
#' 
#' @inheritParams graphics::sunflowerplot
#' @param grid logical; if \code{TRUE}, a background grid will be drawn
#' @param col.grid \code{grid} color
#' @param col.acc \code{grid} accent color
#' 
#' @export

gsunflowerplot <- function(x, ...) {
  UseMethod('gsunflowerplot')
}

#' @rdname gsunflowerplot
#' @export
gsunflowerplot.default <- function(x, y = NULL, number, log = "", digits = 6L, xlab = NULL, 
                                   ylab = NULL, xlim = NULL, ylim = NULL, add = FALSE, rotate = FALSE, 
                                   pch = 16, cex = 0.8, cex.fact = 1.5, col = par("col"), bg = NA, 
                                   size = 1/8, seg.col = 2, seg.lwd = 1.5, ..., 
                                   grid = TRUE, col.grid = 'grey90', col.acc = 'white') {
  xlabel <- if (!missing(x)) 
    deparse(substitute(x))
  ylabel <- if (!missing(y)) 
    deparse(substitute(y))
  is.xyn <- (is.list(x) && all(c("x", "y", "number") %in% names(x)))
  xy <- if (is.xyn) {
    number <- x$number
    x
  } else xy.coords(x, y, xlabel, ylabel, log)
  if (!add) {
    xlab <- if (is.null(xlab)) 
      xy$xlab
    else xlab
    ylab <- if (is.null(ylab)) 
      xy$ylab
    else ylab
    xlim <- if (is.null(xlim)) 
      range(xy$x[is.finite(xy$x)])
    else xlim
    ylim <- if (is.null(ylim)) 
      range(xy$y[is.finite(xy$y)])
    else ylim
  }
  n <- length(xy$x)
  if (missing(number)) {
    tt <- xyTable(xy, digits = digits)
    x <- tt$x
    y <- tt$y
    number <- tt$number
  } else {
    if (length(number) != n) 
      stop("'number' must have same length as 'x' and 'y'")
    np <- number > 0
    x <- xy$x[np]
    y <- xy$y[np]
    number <- number[np]
  }
  n <- length(x)
  dev.hold()
  on.exit(dev.flush())
  if (!add) 
    gplot(x, y, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, 
          log = log, type = "n", ..., grid = grid, col.grid = col.grid, 
          col.acc = col.acc)
  n.is1 <- number == 1
  if (any(n.is1)) 
    points(x[n.is1], y[n.is1], pch = pch, col = col, bg = bg, 
           cex = cex)
  if (any(!n.is1)) {
    points(x[!n.is1], y[!n.is1], pch = pch, col = col, bg = bg, 
           cex = cex/cex.fact)
    i.multi <- (1L:n)[number > 1]
    ppin <- par("pin")
    pusr <- par("usr")
    xr <- size * abs(pusr[2L] - pusr[1L])/ppin[1L]
    yr <- size * abs(pusr[4L] - pusr[3L])/ppin[2L]
    i.rep <- rep.int(i.multi, number[number > 1])
    z <- numeric()
    for (i in i.multi) z <- c(z, 1L:number[i] + if (rotate) stats::runif(1) else 0)
    deg <- (2 * pi * z)/number[i.rep]
    segments(x[i.rep], y[i.rep], x[i.rep] + xr * sin(deg), 
             y[i.rep] + yr * cos(deg), col = seg.col, lwd = seg.lwd)
  }
  invisible(list(x = x, y = y, number = number))
}

#' @rdname gsunflowerplot
#' @export
gsunflowerplot.formula <- function(formula, data = NULL, xlab = NULL, ylab = NULL, ..., 
                                   subset, na.action = NULL, grid = TRUE, col.grid = 'grey90',
                                   col.acc = 'white') {
  if (missing(formula) || (length(formula) != 3L)) 
    stop("formula missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m$xlab <- m$ylab <- m$... <- m$grid <- m$col.grid <- m$col.acc <- NULL
  m$na.action <- na.action
  require(stats, quietly = TRUE)
  m[[1L]] <- quote(stats::model.frame)
  mf <- eval(m, parent.frame())
  if (NCOL(mf) != 2L) 
    stop("'formula' should specify exactly two variables")
  if (is.null(xlab)) 
    xlab <- names(mf)[2L]
  if (is.null(ylab)) 
    ylab <- names(mf)[1L]
  gsunflowerplot(mf[[2L]], mf[[1L]], xlab = xlab, ylab = ylab, ...,
                 grid = grid, col.grid = col.grid, col.acc = col.acc)
}
