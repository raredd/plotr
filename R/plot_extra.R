### plotting extras
# color_bar, zoomin, ptlocator, polyShade, bpCI, pstar_, inset, grcols,
# click_text, click_shape, ctext, cmtext, ctitle, ctext_, polygon2, subplot,
# coords, fig, arrows2, carrows, laxis, pretty_sci, oom, parse_sci, caxis,
# subrect
# 
# unexported:
# to_sci_
###


#' Color legend
#' 
#' Continuous color bar legend.
#' 
#' @param colors vector of color names (or hexadecimal) from low to high
#' @param x numeric vector of data
#' @param y optional numeric vector of data; to color by a variable other than
#' \code{x}, \code{y} must be specified for \code{labels} coordinates to work 
#' properly; see examples
#' @param labels optional labels for color bar
#' @param at.x x-coordinate of lower-left corner of bar
#' @param at.y y-coordinate of lower-left corner of bar; note that the bar can 
#' extend ouside the plotting region, so use \code{cex.x} and \code{cex.y} to
#' scale the bar down (or up)
#' @param cex.x,cex.y scaling parameters
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @return An \code{\link{invisible}} vector of colors (hexadecimal format)
#' used for the bar. Note that these colors may be mapped to \code{x} so that 
#' only one \code{color_bar} needs to be called in the \code{col} argument for
#' both the coloring of points and the generation of the legend; see examples.
#' 
#' @examples
#' plot.new()
#' color_bar(c('black','red','white','blue'), at.x = 0, at.y = 0)
#' text(x = 0, y = seq(0, par('usr')[4], length.out = 6), pos = 4, xpd = TRUE,
#'      labels = pretty(seq(0, par('usr')[4]), n = 6), cex = 0.8, offset = 0.75)
#' 
#' 
#' ## calling color_bar once in the col argument will color x accordingly
#' ## and plot the legend simultaneously
#' 
#' ## compare
#' plot(mtcars$mpg, pch = 19, cex = 2, 
#'      col = rawr:::col_scaler(mtcars$mpg, c('yellow', 'red')))
#' plot(mtcars$mpg, pch = 19, cex = 2,
#'      col = color_bar(c('yellow', 'red'), mtcars$mpg, labels = mtcars$mpg))
#' 
#' 
#' ## plot a color_bar legend by a variable other than x
#' ##   use y argument to match the variable in the original plot call
#' ##   and x as the variable to color and label
#' 
#' ## compare
#' with(mtcars, {
#'   plot(mpg, pch = 19, cex = 2,
#'        main = 'color by weight (red = heavier)',
#'        col = rawr:::col_scaler(wt, c('yellow', 'red')))
#' })
#' 
#' with(mtcars, {
#'   plot(mpg, pch = 19, cex = 2,
#'        main = 'color by weight (red = heavier)',
#'        col = color_bar(c('yellow', 'red'), x = wt, y = mpg, labels = wt))
#' })
#'
#' @export

color_bar <- function(colors, x = NULL, y = x, labels = NULL,
                      at.x = par('usr')[2L], at.y = par('usr')[3L],
                      cex.x = 1, cex.y = 1, ...) {
  op <- par(..., no.readonly = TRUE)
  on.exit(par(op))
  
  par(mar = c(5, 4, 4, 4) + .1, xpd = TRUE, new = TRUE)
  bx <- par('usr')
  nc <- 1000
  colors <- colorRampPalette(colors)(nc)
  
  bx.y <- bx[3:4]
  sapply(0:nc, function(ii)
    segments(
      x0 = at.x, y0 = at.y + ii * diff(bx.y) / nc * cex.y,
      x1 = at.x + diff(bx[1:2]) / nc * 20 * cex.x,
      y1 = at.y + ii * diff(bx.y) / nc * cex.y,
      col = colors[ii], lwd = 1, xpd = TRUE
    )
  )
  
  if (!is.null(labels))
    text(at.x, pretty(y), pretty(labels), pos = 4L, offset = 1)
  
  if (!is.null(x))
    invisible(colors[rawr::rescaler(x, c(1, nc))])
}

#' Zoom for points in base \code{R} plot
#' 
#' Provides a summary statistic for sample points in a plot.
#' 
#' @param x,y x- and y-axis variables
#' @param ... additional arguments passed to \code{\link{identify}}
#' 
#' @examples
#' set.seed(1)
#' x <- runif(10)
#' y <- rnorm(10, mean = 5)
#' 
#' par(mfrow = c(1, 2))
#' plot(x, y, xlab = 'mean', ylab = 'sd')
#' 
#' zoomin(x, y)
#' ## ESC to quit
#' 
#' @export

zoomin <- function(x, y, ...) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  ans <- tryCatch(
    identify(x, y, n = 1L, plot = FALSE, ...),
    
    error = function(e) {
      if (grepl('plot.new has not been called yet', e$message, fixed = TRUE)) {
        par(mfrow = 1:2)
        plot(x, y)
        op <- par(no.readonly = TRUE)
      } else return(e)
      
      identify(x, y, n = 1L, plot = FALSE, ...)
    }
  )
  
  zoom <- function (x, y, xlim, ylim, xd, yd) {
    rxlim <- x + c(-1, 1) * (diff(range(xd)) / 20)
    rylim <- y + c(-1, 1) * (diff(range(yd)) / 20)
    
    par(mfrow = c(1, 2))
    plot(xd, yd, xlab = 'mean', ylab = 'sd')
    
    xext <- yext <- rxext <- ryext <- 0
    
    if (par('xaxs') == 'r') {
      xext  <- diff(xlim) * 0.04
      rxext <- diff(rxlim) * 0.04
    }
    if (par('yaxs') == 'r') {
      yext  <- diff(ylim) * 0.04
      ryext <- diff(rylim) * 0.04
    }
    
    rect(rxlim[1L] - rxext, rylim[1L] - ryext,
         rxlim[2L] + rxext, rylim[2L] + ryext)
    xylim <- par('usr')
    xypin <- par('pin')
    
    rxi0 <- xypin[1L] * (xylim[2L] - (rxlim[1L] - rxext)) / diff(xylim[1:2])
    rxi1 <- xypin[1L] * (xylim[2L] - (rxlim[2L] + rxext)) / diff(xylim[1:2])
    y01i <- xypin[2L] * (xylim[4L] - (rylim[2L] + ryext)) / diff(xylim[3:4])
    y02i <- xypin[2L] * ((rylim[1L] - ryext) - xylim[3L]) / diff(xylim[3:4])
    mu <- x
    
    curve(dnorm(x, mean = mu, sd = y), from = -4 * y + mu, to = 4 * y + mu,
          xlab = sprintf('mean: %.2f, sd: %.2f', mu, y), ylab = '')
    
    xypin <- par('pin')
    par(xpd = NA)
    xylim <- par('usr')
    xymai <- par('mai')
    
    x0 <- xylim[1L] - diff(xylim[1:2]) *
      (xymai[2L] + xymai[4L] + rxi0) / xypin[1L]
    x1 <- xylim[1L] - diff(xylim[1:2]) *
      (xymai[2L] + xymai[4L] + rxi1) / xypin[1L]
    y01 <- xylim[4L] - diff(xylim[3:4]) * y01i / xypin[2L]
    y02 <- xylim[3L] + diff(xylim[3:4]) * y02i / xypin[2L]
    
    par(xpd = TRUE)
    xend <- xylim[1L] - diff(xylim[1:2]) * xymai[2L] / (2 * xypin[1L])
    xprop0 <- (xylim[1L] - xend) / (xylim[1L] - x0)
    xprop1 <- (xylim[2L] - xend) / (xylim[2L] - x1)
    
    par(xpd = NA)
    segments(c(x0, x0, x1, x1),
             c(y01, y02, y01, y02),
             c(xend, xend, xend, xend),
             c(xylim[4L] - (xylim[4L] - y01) * xprop0,
               xylim[3L] + (y02 - xylim[3L]) * xprop0,
               xylim[4L] - (xylim[4L] - y01) * xprop1,
               xylim[3L] + (y02 - xylim[3L]) * xprop1))
    par(mfg = c(1, 1))
    
    plot(xd, yd, xlab = 'mean', ylab = 'sd')
  }
  
  if (length(ans)) {
    zoom(x[ans], y[ans], range(x), range(y), x, y)
    points(x[ans], y[ans], pch = 19L)
    zoomin(x, y)
  }
}

#' Point locator
#' 
#' Interactively select points on an existing figure and annotated with new
#' plotting characters.
#' 
#' @param n the maximum number of points to locate
#' @param x,y x- and y-coordinates
#' @param col color for points
#' @param pch plotting character
#' @param ... additional graphical arguments passed to \code{\link{points}}
#' 
#' @return
#' Returns (invisibly) the indices of selected points.
#' 
#' @references
#' \url{http://www.r-bloggers.com/data-point-locator-function/}
#' 
#' @examples
#' \dontrun{
#' set.seed(1)
#' n <- 200
#' x <- sort(runif(n, 0, 10 * pi))
#' y <- sin(x) + rnorm(n, 0, .2)
#' 
#' plot(x, y, cex = 2)
#' p <- ptlocator(10, x, y, cex = 2)
#' cbind(x = x[p], y = y[p])
#' }
#' 
#' @export

ptlocator <- function(n = 512L, x, y, col = adjustcolor('red', alpha.f = 0.5),
                      pch = 16L, ...) {
  xsc <- scale(x)
  ysc <- scale(y)
  pos <- NULL
  
  for(ii in seq.int(n)) {
    pt <- locator(1L)
    if (!is.null(pt)) {
      ptxsc <- scale(pt$x, center = attr(xsc, 'scaled:center'),
                     scale = attr(xsc, 'scaled:scale'))
      ptysc <- scale(pt$y, center = attr(ysc, 'scaled:center'),
                     scale = attr(ysc, 'scaled:scale'))
      pos.i <- which.min(sqrt((c(ptxsc) - c(xsc)) ^ 2 +
                                (c(ptysc) - c(ysc)) ^ 2))
      points(x[pos.i], y[pos.i], col = col, pch = pch, ...)
      pos <- c(pos, pos.i)
    } else return(invisible(pos))
  }
  
  invisible(pos)
}

#' Polygon shading
#' 
#' Color or shade the area under a curve.
#' 
#' @param x,y x- and y-values from the curve
#' @param from a vector of x-coordinates \emph{from} which to color
#' @param to a vector of x-coordinates \emph{to} which to color (same length)
#' as \code{from}
#' @param n tuning parameter for fitting the shading region to the curve; a
#' lower value will result in a worse fit around the curve
#' @param miny by default, shading will extend from the curve to \code{min(y)}
#' @param horiz logical; if \code{TRUE}, the y-axis is assumed to be the
#' horizontal
#' @param ... additional parameters passed to \code{\link{polygon}}; common
#' uses are \code{density} for shading lines, \code{col} for shading color(s),
#' \code{border} for border color, or \code{lty} for line type
#' 
#' @seealso
#' \url{http://www.fromthebottomoftheheap.net/2013/01/11/shading-regions-under-a-curve/}
#' 
#' @examples
#' set.seed(1)
#' x <- density(c(rnorm(75), rnorm(25, 5)))
#' 
#' plot(x)
#' polyShade(x$x, x$y, -1, 2, col = 'red', border = NA)
#' polyShade(x$x, x$y, from = c(-Inf, 6), to = c(-2, Inf),
#'           col = adjustcolor('red', .3), border = NA)
#' polyShade(x$x, x$y, 0, 4, col = 'blue', density = 20, lty = 4,
#'           miny = par('usr')[1], border = NA)
#' 
#' ## horizontal
#' plot(x$y, x$x, type = 'l')
#' polyShade(x$x, x$y, horiz = TRUE, col = 'blue')
#' 
#' @export

polyShade <- function(x, y, from = -Inf, to = Inf, n = 1e3,
                      miny = min(y, na.rm = TRUE), horiz = FALSE, ...) {
  if (!identical(lf <- length(from), lt <- length(to)))
    stop('\'from\' and \'to\' should have the same length')
  
  drawPoly <- function(fun, from, to, n, miny, ...) {
    Sq <- seq(from, to, length.out = n)
    dd <- data.frame(x = c(Sq[1L], Sq, Sq[n]), y = c(miny, fun(Sq), miny))
    if (horiz)
      names(dd) <- c('y', 'x')
    polygon(dd$x, dd$y, ...)
  }
  
  from[from == -Inf] <- min(x, na.rm = TRUE)
  to[to == Inf] <- max(x, na.rm = TRUE)
  
  interp <- approxfun(x = x, y = y)
  mapply(drawPoly, from = from, to = to, ...,
         MoreArgs = list(fun = interp, n = n, miny = miny))
  
  invisible(NULL)
}

#' Barplot confidence intervals
#' 
#' Add confidence intervals (error bars) and group comparisons to barplots.
#' 
#' @param x the return value of \code{\link{barplot}}, i.e., a vector or
#' matrix (when \code{beside = TRUE}) of all bar (or group) midpoints
#' @param horiz logical; if \code{TRUE}, \code{bpCI} assumes horizontal bars
#' @param ci logical; draw error bars (must give \code{ci.u}, \code{ci.l})
#' @param ci.u,ci.l a numeric vector or matrix having the same dimensions as
#' \code{x} giving the upper and lower intervals, respectively
#' @param ci.width width of the ends of the error bars, will depend on 
#' \code{range(x)}
#' @param sig logical; if \code{TRUE}, draws group comparisons (must give
#' \code{pvals} to plot sig stars)
#' @param pvals p-values of group comparisons to be displayed as sig stars
#' @param pch plotting character to be used for significance; default is 
#' \code{*} and uses same significance codes as \code{\link{printCoefmat}}
#' @param show.p logical; if \code{TRUE}, p-values are shown and formatted
#' with \code{\link[rawr]{pvalr}}
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @examples
#' ## generate data and p-values
#' hh <- t(VADeaths)[1:2, 5:1]
#' ci.l <- hh * 0.85
#' ci.u <- hh * 1.15
#' pvals <- pt(apply(hh, 2, diff), 1) / 5:1
#' 
#' bp <- barplot(hh, beside = TRUE, ylim = c(0, 100))
#' bpCI(bp, ci.u = ci.u, ci.l = ci.l, sig = TRUE, pvals = pvals)
#' bpCI(bp, ci.u = ci.u, ci.l = ci.l, sig = TRUE, pvals = pvals,
#'      show.p = TRUE, pch = FALSE)
#' mtext("Signif. codes:  0 '+++' 0.001 '++' 0.01 '+' 0.05 '.' 0.1 ' ' 1",
#'       side = 1, at = par('usr')[2], line = 2, adj = 1, cex = .8, font = 3)
#' 
#' 
#' bp <- barplot(hh <- cbind(x = c(465, 91) / 465 * 100,
#'                           y = c(200, 840) / 840 * 100,
#'                           z = c(37, 17) / 37 * 100),
#'               beside = TRUE, width = c(465, 840, 37),
#'               col = c(1, 2), ylim = c(0,130))
#' 
#' ci.l <- hh * 0.85
#' ci.u <- hh * 1.15
#' pv <- pt(-abs(apply(hh, 2, diff)), 1)
#' 
#' bpCI(bp, ci.u = ci.u, ci.l = ci.l, sig = TRUE, pvals = pv, ci.width = 100,
#'      col = 'red', lty = 'dashed', lwd = 2)
#' mtext("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
#'       side = 1, at = par('usr')[2], line = 2, adj = 1, cex = .8, font = 3)
#' 
#' @export

bpCI <- function(x, horiz = FALSE, ci = TRUE, ci.u, ci.l, ci.width = 0.5,
                 sig = FALSE, pvals, pch = '*', show.p = FALSE, ...) {
  op <- par(..., no.readonly = TRUE)
  on.exit(par(op))
  
  if (ci) {
    ci.width <- ci.width / 2
    if (horiz) {
      ci.l <- t(ci.l)
      ci.u <- t(ci.u)
      segments(ci.l, t(x), ci.u, t(x))
      segments(ci.u, t(x - ci.width), ci.u, t(x + ci.width))
      segments(ci.l, t(x - ci.width), ci.l, t(x + ci.width))
    } else {
      segments(x, ci.l, x, ci.u)
      segments(x - ci.width, ci.u, x + ci.width, ci.u)
      segments(x - ci.width, ci.l, x + ci.width, ci.l)
    }
    if (sig) {
      if (horiz)
        stop('\'sig\' is not supported when \'horiz = TRUE\'')
      if (nrow(x) > 2L)
        stop('\'sig\' is not supported for > 2 bars per group')
      
      yy <- rbind(c(ci.u[1L, ] + 3L), c(apply(ci.u, 2L, max) + 5L),
                  c(apply(ci.u, 2L, max) + 5L), c(ci.u[2L, ] + 3L))
      xx <- apply(x, 2L, function(y) rep(y, each = nrow(x)))
      sapply(seq.int(ncol(x)), function(ii) lines(xx[, ii], yy[, ii]))
      xt <- colMeans(x)
      yt <- apply(ci.u, 2L, max)
      
      if (!(is.null(pch) | identical(pch, FALSE) | is.na(pch)))
        text(xt, yt, pstar_(pvals, pch), pos = 3L)
      if (show.p)
        text(xt, yt, rawr::pvalr(pvals), pos = 3L, offset = 1.5)
    }
  }
}

pstar_ <- function(pv, pch, non_sig = 'NS') {
  symnum(
    pv, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = chartr('*', pch, c('***' , '**' , '*', '.', non_sig))
  )
}

#' Inset plots
#'
#' Inset new plots in existing plot windows.
#' 
#' \code{x} should be of the form \code{x0, x1} and similar for \code{y} 
#' giving the starting and ending coordinates to draw the inset plot and must
#' be in the range defined in the current plotting area.
#' 
#' Alternatively, \code{x} can be a keyword ("bottomright", "bottom",
#' "bottomleft", "left", "topleft", "top", "topright" "right", or "center")
#' giving the approximate location of the inset plot. \code{pct} is used to
#' adjust the size.
#' 
#' @param x a keyword (see details) or a vector of length two giving the 
#' positions on the current plot at which to draw the inset plot along the
#' x-axis
#' @param y ignored if \code{x} is a keyword or a vector of length two giving
#' the positions on the current plot at which to draw the inset plot along the
#' y-axis
#' @param pct inset plot scaling (only used if \code{x} is a keyword)
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @examples
#' op <- par(no.readonly = TRUE)
#' 
#' plot(mpg ~ wt, data = mtcars, col = 'blue')
#' abline(lm(mpg ~ wt, data = mtcars), col = 'red')
#' inset('topright', pct = .4)
#' hist(mtcars$mpg, ann = FALSE, panel.last = box(),
#'      col = 'dodgerblue2', las = 1)
#' par(op)
#' 
#' plot(1:10, type = 'n')
#' op <- par(no.readonly = TRUE)
#' Map(function(x) {
#'  inset(x, las = 1, col = 'red', pct = 1/3)
#'  plot(rnorm(10), ann = FALSE, axes = FALSE, panel.last = box())
#'  par(op)
#'  Sys.sleep(0.5)
#'  }, c('bottomright', 'bottom', 'bottomleft', 'left',
#'       'topleft', 'top', 'topright', 'right', 'center')
#' )
#' 
#' @export

inset <- function(x, y = NULL, pct = 0.25, ...) {
  m <- substitute(...())
  usr <- par('usr')
  plt <- par('plt')
  pctx <- pct * diff(plt[1:2])
  pcty <- pct * diff(plt[3:4])
  
  auto <- if (is.character(x))
    match.arg(x, c("bottomright", "bottom", "bottomleft", "left",
                   "topleft", "top", "topright", "right", "center")) else NA
  
  xx <- switch(
    auto,
    bottomright = c(plt[2L] - pctx, plt[2L]),
    bottom      = mean(plt[1:2]) + c(-1, 1) * pctx / 2,
    bottomleft  = c(plt[1L], plt[1L] + pctx),
    left        = c(plt[1L], plt[1L] + pctx),
    topleft     = c(plt[1L], plt[1L] + pctx),
    top         = mean(plt[1:2]) + c(-1, 1) * pctx / 2,
    topright    = c(plt[2L] - pctx, plt[2L]),
    right       = c(plt[2L] - pctx, plt[2L]),
    center      = mean(plt[1:2]) + c(-1, 1) * pctx / 2
  )
  
  yy <- switch(
    auto,
    bottomright = c(plt[3L], plt[3L] + pcty),
    bottom      = c(plt[3L], plt[3L] + pcty),
    bottomleft  = c(plt[3L], plt[3L] + pcty),
    left        = mean(plt[3:4]) + c(-1, 1) * pcty / 2,
    topleft     = c(plt[4L] - pcty, plt[4L]),
    top         = c(plt[4L] - pcty, plt[4L]),
    topright    = c(plt[4L] - pcty, plt[4L]),
    right       = mean(plt[3:4]) + c(-1, 1) * pcty / 2,
    center      = mean(plt[3:4]) + c(-1, 1) * pcty / 2
  )
  
  # xx <- rescaler(xx, plt[1:2], usr[1:2])
  # yy <- rescaler(yy, plt[3:4], usr[3:4])
  if (is.na(auto)) {
    xx <- grconvertX(x, 'user', 'ndc')
    yy <- grconvertY(y, 'user', 'ndc')
  }
  
  if ('mar' %ni% names(m))
    par(fig = c(xx, yy), new = TRUE, mar = c(0,0,0,0), ...)
  else par(fig = c(xx, yy), new = TRUE, ...)
  
  invisible(c(xx, yy))
}

#' Choose n colors using the golden ratio
#'
#' This chooses \code{n} colour hues using a sequence generated by the Golden
#' Ratio.
#'
#' @param n number of colors
#' @param s,v numeric vectors of values in the range \code{[0, 1]} for 
#' "saturation" and "value," respectively, to be combined to form a vector of
#' colors; values in shorter arguments are recycled
#' @param alpha  numeric vector of values in the range \code{[0, 1]} for alpha 
#' transparency channel (0 is transparent and 1 is opaque)
#' @seealso \code{\link{hsv}}
#' 
#' @examples
#' plot(1:5, 1:5, col = grcols(5), pch = 20, cex = 3)
#' 
#' plot(c(1, 6), c(0, 1), type = 'n', axes = FALSE, 
#'      bty = 'n', xlab = '', ylab = '')
#' rect(1:5, 0, 2:6, 1, col = grcols(5), border = NA)
#' 
#' @export

grcols <- function(n, s = .5, v = 1, alpha = 1) {
  GR <- 2 / (1 + sqrt(5))
  hues <- (seq(0, n - 1) * GR) %% 1
  hsv(hues, s = s, v = v, alpha = alpha)
}

#' Add text interactively in base \code{R} graphics
#' 
#' Add text and expressions anywhere in a plot (including margins) with mouse
#' click(s).
#' 
#' @param expr a character string of text or an \code{\link{expression}}
#' @param ... additional graphical parameters passed to \code{\link{text}} (or 
#' \code{\link{par}}) such as \code{col}, \code{srt}, \code{family}, etc
#'
#' @seealso
#' \code{\link{click_shape}}; \code{\link{plotmath}} for help with plotting
#' mathematical expressions
#' 
#' @return
#' (Invisibly) a vector of length two with the x- and y-coordinates of the text.
#' 
#' @examples
#' \dontrun{
#' plot.new()
#' click_text('hello', col = 'red', cex = .5)
#' click_text('goodbye', family = 'HersheyScript', cex = 3)
#' click_text(expression(sum(x ^ 2) == 5 ^ hat(x)), srt = 45)
#' }
#' 
#' @export

click_text <- function(expr, ...) {
  op <- par(no.readonly = TRUE) 
  on.exit(par(op))
  
  par(mar = c(0,0,0,0), xpd = NA)
  co <- locator(1L)
  text(co[[1L]], co[[2L]], if (missing(expr)) '' else expr, ...)
  
  invisible(c(x = co[[1L]], y = co[[2L]]))
}

#' Add shapes interactively in base \code{R} graphics
#' 
#' Add shapes anywhere in a plot (including margins) with mouse click(s).
#' 
#' @param shape type of shape; choices are \code{'box'}, \code{'arrow'},
#' \code{'line'}, \code{'poly'}, \code{'circle'}, and \code{'cyl'}
#' @param corners number of corners to draw if \code{shape = 'poly'}
#' @param ... additional arguments or graphical parameters passed to the
#' shape functions
#' 
#' @seealso \code{\link{click_text}}, \code{\link{rect}},
#' \code{\link{arrows}}, \code{\link{rect}}, \code{\link{rect}},
#' \code{\link{segments}}, \code{\link{polygon}},
#' \code{\link[plotrix]{draw.circle}}, \code{\link[plotrix]{cylindrect}}
#' 
#' @examples
#' \dontrun{
#' op <- par(xpd = NA)
#' plot.new()
#' plot.window(0:1, 0:1, asp = 1)
#' click_shape('line') # a line segment
#' click_shape('arrow', col = 'blue', code = 2, lwd = 2, length = .15)
#' click_shape('rect', border = 'purple', col = 'pink', lwd = 2)
#' click_shape('rect', col = NULL, border = 'purple', lwd = 2)
#' click_shape('line', col = 'orange', lty = 3, lwd = 3)
#' click_shape('poly', corners = 5, border = 'green', col = 'orange')
#' click_shape('poly', corners = 3, border = 'red', col = 'yellow', lty = 1)
#' click_shape('cyl', col = 'orange')
#' click_shape('circle', col = 'orange', border = 'black', lty = 3, lwd = 3)
#' par(op)
#' }
#' 
#' @export

click_shape <- function(shape = c('circle', 'arrow', 'rect', 'cyl', 'line', 'poly'),
                        corners = 3L, ...) {
  shape  <- match.arg(shape)
  coords <- if (shape %in% 'poly')
    locator(as.integer(corners)) else unlist(locator(2L))
  
  ARROW <- function(...) {
    arrows(coords[1L], coords[3L], coords[2L], coords[4L], ...)
  }
  CIRCLE <- function(...) {
    rad <- sqrt(((coords[2L] - coords[1L]) ^ 2) + ((coords[4L] - coords[3L]) ^ 2))
    plotrix::draw.circle(coords[1L], coords[3L], radius = rad, ...)
  }
  CYL <- function(...) {
    plotrix::cylindrect(coords[1L], coords[3L], coords[2L], coords[4L], ...)
  }
  LINE <- function(...) {
    segments(coords[1L], coords[3L], coords[2L], coords[4L], ...)
  }
  POLY <- function(...) {
    polygon(coords, ...)
  }
  RECT <- function(...) {
    rect(coords[1L], coords[3L], coords[2L], coords[4L], ...)
  }
  
  suppressWarnings({
    switch(
      shape,
      arrow = ARROW(...), circle = CIRCLE(...), cyl = CYL(...),
      line = LINE(...), poly = POLY(...), rect = RECT(...),
      stop('Invalid shape')
    )
  })
  
  invisible(coords)
}

#' Color text
#' 
#' Add color to individual words in text functions. \code{ctext},
#' \code{cmtext}, and \code{ctitle} are analogous to \code{\link{text}},
#' \code{\link{mtext}}, and \code{\link{title}}, respectively. Note that
#' \code{title} accepts some graphical parameters specific to the label type,
#' e.g., \code{col.main}, but this is not implemented in \code{ctitle}--colors
#' will be recycled if more than one label type is given. Similarly, further
#' graphical parameters such as \code{cex} or \code{line} will be passed to
#' all label types; see examples.
#' 
#' @param text vector of text
#' @param cols vector of colors; should be the same lenght as \code{text} or
#' will me recycled with a warning
#' @param space logical; if \code{TRUE}, adds space between \code{text}
#' @param ... additional parameters passed to \code{text} or \code{mtext}
#' @param main,sub,xlab,ylab vector(s) of text for specific labels
#' 
#' @examples
#' plot(1, ann = FALSE)
#' ctext(x = 1, y = 1, text = c('hello','little','point'), cols = 1:3, pos = 1)
#' cmtext(c('a','side','label'), 1:2, space = FALSE, side = 4, cex = 3)
#' cmtext(c('a','a','a'), 4:6, space = FALSE, side = 4, cex = 3, line = -2)
#' 
#' ## note that line, cex, font, etc will be recycled
#' ctitle(main = c('the','main','label'), xlab = c('x','label'),
#'        ylab = c('y','label'), sub = c('sub', 'label'), col = 3:5)
#' ctitle(xlab = c('another','label'), ylab = c('another','label'),
#'        font = 3, col = 1:2, line = 2, cex = 1.5)
#'
#' @export

ctext <- function(text, cols, space = TRUE, ...) {
  if (missing(cols))
    cols <- rep_len(1L, length(text))
  l <- ctext_(text, cols, space)
  for (ii in seq_along(l$text))
    text(labels = l$text[[ii]], col = l$colors[ii], ...)
  
  invisible(NULL)
}

#' @rdname ctext
#' @export
cmtext <- function(text, cols, space = TRUE, ...) {
  if (missing(cols))
    cols <- rep_len(1L, length(text))
  l <- ctext_(text, cols, space)
  for (ii in seq_along(l$text))
    mtext(text = l$text[[ii]], col = l$colors[ii], ...)
  
  invisible(NULL)
}

#' @rdname ctext
#' @export
ctitle <- function(main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                   cols, space = TRUE, ...) {
  m <- match.call(expand.dots = FALSE)
  dots <- m$...
  ml <- dots$line
  dots$line <- NULL
  if (missing(cols))
    cols <- rep_len(1L, length(text))
  wh <- c('main', 'sub', 'xlab', 'ylab')
  l <- setNames(lapply(wh, function(x)
    ## if c() not used to pass text, this could cause problems
    ctext_(as.character(m[[x]])[-1L], cols, space)), wh)
  m <- par('mgp')
  
  if (length(l$main$text)) {
    ll <- l$main
    for (ii in seq_along(ll$text))
      do.call(
        'mtext',
        c(list(text = ll$text[[ii]], col = ll$colors[ii],
               side = 3L, font = 2L, line = ml %||% (m[1L] * .5)), dots)
      )
  }
  
  if (length(l$sub$text)) {
    ll <- l$sub
    for (ii in seq_along(ll$text))
      do.call(
        'mtext',
        c(list(text = ll$text[[ii]], col = ll$colors[ii],
               side = 1L, line = ml %||% (m[1L] + 1)), dots)
      )
  }
  
  if (length(l$xlab$text)) {
    ll <- l$xlab
    for (ii in seq_along(ll$text))
      do.call(
        'mtext',
        c(list(text = ll$text[[ii]], col = ll$colors[ii],
               side = 1L, line = ml %||% m[1L]), dots)
      )
  }
  
  if (length(l$ylab$text)) {
    ll <- l$ylab
    for (ii in seq_along(ll$text))
      do.call(
        'mtext',
        c(list(text = ll$text[[ii]], col = ll$colors[ii],
               side = 2L, line = ml %||% m[1]), dots)
      )
  }
  
  invisible(NULL)
}

ctext_ <- function(text, cols, space) {
  # (ctext_(c('one','two','three'), 3:4, TRUE))
  # (ctext_(c('one','one','one'), 3:4, TRUE)) ## breaks old version
  if (space && (lt <- length(text)) > 1L)
    text[2:lt] <- paste0(' ', text[2:lt])
  
  l <- lapply(seq_along(text), function(x) {
    txt <- shQuote(text)
    txt[-x] <- sprintf('phantom(%s)', txt[-x])
    xx <- sprintf('expression(%s)', paste0(txt, collapse = ' * '))
    eval(parse(text = xx))
  })
  
  if ((lt <- length(text)) > (lc <- length(cols))) {
    # warning('colors will be recycled', domain = NA)
    cols <- rep_len(cols, lt)
  }
  
  list(text = l, colors = cols)
}

#' polygon2
#' 
#' Draw \emph{regular} \code{\link{polygon}}s as plotting characters. This
#' function calculates the \code{{x,y}} coordinates for given centroids and
#' radii and passes each pair of vectors to \code{\link{polygon}} to draw.
#' Note that all arguments except \code{x}, \code{y}, \code{radius},
#' \code{sides}, and \code{srt} are passed directly to \code{\link{polygon}},
#' so see \code{?polygon} for additional details.
#' 
#' @param x,y x- and y-coordinate vectors of polygon centroids
#' @param radius distance from centroid to each vertex
#' @param sides number of sides for polygons; since only regular polygons may
#' be plotted, possible values are \code{(1:360)[360L \%\% 1:360 == 0L]}
#' @param srt rotation in degrees for polygon
#' @param density density of shading lines in lines per inch
#' @param angle slope of shading lines, given as an angle in degrees (counter-
#' clockwise)
#' @param border polygon border color
#' @param col polygon fill color
#' @param lty line type to be used for border and/or shading lines
#' @param fillOddEven logical controlling polygon shading mode
#' @param ... additional graphical parameters such as \code{xpd}, \code{lend},
#' \code{ljoin}, and \code{lmitre} passed to \code{\link{par}}
#' 
#' @examples
#' ## sides can be any of
#' (sides <- (1:360)[360L %% 1:360 == 0L])
#' plot.new()
#' for (ii in sides)
#'   polygon2(0.5, 0.5, sides = ii)  ## okay
#' # polygon2(0.5, 0.5, sides = 7)   ## error
#' 
#' x <- mtcars$mpg
#' y <- mtcars$wt
#' plot(x, y, type = 'n', asp = 1)
#' polygon2(x, y, density = 30, angle = 90, col = mtcars$gear)
#' polygon2(x, y + 1, srt = 30, lty = 'dotted', col = 'transparent')
#' polygon2(x, y + -2, radius = .5, sides = 5)
#' 
#' @export

polygon2 <- function(x, y = NULL, radius, sides = 6, srt = 0,
                     density = NULL, angle = 45, border = NULL, col = NA,
                     lty = par('lty'), fillOddEven = FALSE, ...) {
  ## helper
  ## get {x,y} around circle with r=radius for n sides
  get_coords <- function(x, y, radius, sides, srt) {
    stopifnot(360L %% sides == 0L)
    deg <- seq(90, 450, 360L %/% as.integer(sides)) + srt
    coords <- p2c(radius, deg, TRUE)
    list(x = x + coords$x, y = y + coords$y)
  }
  
  ## expand params to be passed to vectorized polygon
  radius <- if (missing(radius))
    sqrt(prod(par('cin'))) else radius
  rl <- max(length(x), length(y))
  m <- as.list(match.call(expand.dots = TRUE))
  f <- formals(polygon2)
  m[[1]] <- f$`...` <- NULL
  m <- modifyList(f, m)
  m$x <- m$y <- m$radius <- m$sides <- m$srt <- NULL
  
  ## filter out NULL params (y, density, border) if given
  m <- Filter(Negate(is.null), m)
  m <- lapply(m, function(xx)
    if (!is.null(eval(xx))) rep_len(eval(xx), rl))
  
  V <- Vectorize(get_coords, SIMPLIFY = FALSE)
  coords <- V(x, y, radius, sides, srt)
  lapply(seq_along(coords), function(ii)
    do.call('polygon', c(coords[[ii]], lapply(m, '[', ii))))
  
  invisible(coords)
}

#' Subplot
#' 
#' Embed a subplot in an existing plot at specified coordinates.
#' 
#' The coordinates for \code{expr} may be given by a keyword passed to
#' \code{x} (similar to \code{\link{legend}}). \code{x} will be grepped for
#' "right", "left", "top", and/or "bottom", and the coordinates for
#' \code{expr} will be automatically calculated; see examples.
#' 
#' If \code{x} and \code{y} are numeric and length 1, these will be used as
#' the center of the plotting region; if length 2, these will be used as the
#' minimum and maximum coordinates of the plotting region (similarly if
#' \code{x} is missing and the user is prompted by \code{\link{locator}} to
#' give the coordinates interactively).
#' 
#' @param expr an expression defining the plot to be embedded
#' @param x,y coordinates of the new plot given by a keyword (see details)
#' or values accepted by \code{\link{xy.coords}}; if missing, the user
#' will be prompted to select the bottom-left and top-right coordinates for
#' the plotting region
#' @param log character, \code{"x"}, \code{"y"} or both, as for
#' \code{\link{plot}}; sets negative values to \code{NA} and gives a warning
#' @param size the size of the plot in inches if \code{x} and \code{y} are
#' length 1
#' @param vadj vertical adjustment of the plot when \code{y} is a scalar; the
#' default is to center vertically; 0 means place the bottom of the plot at
#' \code{y}, and 1 places the top of the plot at \code{y}
#' @param hadj horizontal adjustment of the plot when \code{x} is a scalar,
#' the default is to center horizontally; 0 means place the left edge of the
#' plot at \code{x}, and 1 means place the right edge of the plot at \code{x}
#' @param inset 1 or 2 numbers representing the proportion of the plot to
#' inset the subplot from edges when \code{x} is a character string; the first
#' element is the horizontal inset, and the second is the vertical inset
#' @param type character string of \code{"plt"} or \code{"fig"}; if "plt"
#' then the plotting region is defined by \code{x}, \code{y}, and \code{size}
#' with axes, etc. outside the box; if "fig" then all annotations are also
#' inside the box
#' @param pars a list of parameters to be passed to \code{\link{par}} before
#' running \code{expr}
#' 
#' @seealso
#' \code{TeachingDemos::subplot}; \url{http://sickel.net/blogg/?p=688};
#' \code{\link{inset}}
#' 
#' @return
#' The list of graphical parameters used when drawing \code{expr}, useful for
#' adding an additional subplot after \code{subplot} has finished.
#' 
#' @examples
#' plot(1)
#' subplot(plot(density(rnorm(100)), ann = FALSE), 1.2, 1.2)
#' subplot(plot(density(rnorm(100)), ann = FALSE, axes = FALSE), 'bottomright')
#' 
#' 
#' \dontrun{
#' ## augment a map
#' library('maptools')
#' data(state.vbm)
#' 
#' plot(state.vbm, fg = NULL)
#' tmp <- cbind(state.vbm$center_x, state.vbm$center_y)
#' 
#' for (i in 1:50) {
#'   tmp2 <- as.matrix(USArrests[i, c(1, 4)])
#'   tmp3 <- max(USArrests[, c(1, 4)])
#'   subplot({
#'     barplot(matrix(tmp2), ylim = c(0, tmp3), yaxt = 'n', col = 1:2,
#'             beside = TRUE, border = 1:2)
#'    }, x = tmp[i, 1], y = tmp[i, 2], size = c(.1, .1))
#' }
#' legend('bottomright', legend = colnames(USArrests)[c(1, 4)], fill = 1:2)
#' }
#' 
#' 
#' set.seed(1)
#' tmp <- rnorm(25)
#' qqnorm(tmp)
#' qqline(tmp)
#' pars <- subplot(hist(tmp, ann = FALSE), 0, -1)
#' 
#' ## wrong way to add a reference line to histogram
#' abline(v = 0, col = 2, lwd = 2)
#' 
#' ## right way to add a reference line to histogram
#' op <- par(no.readonly = TRUE)
#' par(pars)
#' abline(v = 0, col = 3, lwd = 2)
#' par(op)
#' 
#' @export

subplot <- function(expr, x, y = NULL, log = NULL, size = c(1,1),
                    vadj = 0.5, hadj = 0.5, inset = c(0,0),
                    type = c('plt', 'fig'), pars = NULL) {
  type <- match.arg(type)
  size <- rep_len(size, 2L)
  # op <- par(no.readonly = TRUE)
  op <- par(c(type, 'usr', names(pars)))
  on.exit(par(op))
  
  if (missing(x))
    x <- locator(2L)
  else {
    suppressWarnings({
      x <- sort(x)
      y <- sort(y)
    })
  }
  
  if (is.character(x)) {
    if (length(inset) == 1L)
      inset <- rep(inset, 2L)
    x.char <- x
    usr <- par('usr')
    x <- mean(usr[1:2])
    y <- mean(usr[3:4])
    
    if (length(grep('left', x.char, ignore.case = TRUE))) {
      x <- usr[1L] + inset[1L] * (usr[2L] - usr[1L])
      if (missing(hadj))
        hadj <- 0
    }
    if (length(grep('right', x.char, ignore.case = TRUE))) {
      x <- usr[2L] - inset[1L] * (usr[2L] - usr[1L])
      if (missing(hadj))
        hadj <- 1
    }
    if (length(grep('top', x.char, ignore.case=TRUE))) {
      y <- usr[4L] - inset[2L] * (usr[4L] - usr[3L])
      if (missing(vadj))
        vadj <- 1
    }
    if (length(grep('bottom', x.char, ignore.case = TRUE))) {
      y <- usr[3L] + inset[2L] * (usr[4L] - usr[3L])
      if (missing(vadj))
        vadj <- 0
    }
  }
  
  xy <- xy.coords(x, y, log = log)
  
  if (length(xy$x) != 2L) {
    pin <- par('pin')
    xx <- grconvertX(xy$x[1], to = 'npc')
    yy <- grconvertY(xy$y[1], to = 'npc')
    
    x <- c(xx - hadj * size[1L] / pin[1L],
           xx + (1 - hadj) * size[1L] / pin[1L])
    y <- c(yy - vadj * size[2L] / pin[2L],
           yy + (1 - vadj) * size[2L] / pin[2L])
    
    xyx <- grconvertX(x, from = 'npc', to = 'nfc')
    xyy <- grconvertY(y, from = 'npc', to = 'nfc')
  } else {
    xyx <- grconvertX(xy$x, to = 'nfc')
    xyy <- grconvertY(xy$y, to = 'nfc')
  }
  
  par(pars)
  if (type == 'fig') {
    xyx <- grconvertX(xyx, from = 'nfc', to = 'ndc')
    xyy <- grconvertY(xyy, from = 'nfc', to = 'ndc')
    par(fig = c(xyx, xyy), new = TRUE)
  } else {
    par(plt = c(xyx, xyy), new = TRUE)
  }
  
  expr
  
  invisible(par(no.readonly = TRUE))
}

#' Plotting coordinates
#' 
#' Return the user plot, figure, inner, and device \emph{\{x,y\}} coordinates
#' for a vector of normalized (i.e., in \code{[0,1]}) coordinates. Or, if
#' \code{line} and \code{side} are given, the x (or y) user coordinates.
#' 
#' @param x,y normalized x- and y-coordinates in \code{[0,1]}, recycled as
#' needed
#' @param to character string giving the coordinate system to convert to
#' @param line,side the margin line starting at 0 counting outwards and side
#' of the plot (1=below, 2=left, 3=above, 4=right); see \code{\link{mtext}}
#' 
#' @seealso
#' \code{\link[=grconvertX]{convertXY}}; \code{\link{mtext}}
#' 
#' @examples
#' op <- par(oma = 1:4, mar = 1:4, xpd = NA, pch = 16, xpd = NA)
#' plot.new()
#' box('plot', col = 1)
#' box('figure', col = 2)
#' box('outer', col = 3)
#' # box('inner', col = 4)
#' 
#' xx <- c(1,2,1,2)
#' yy <- c(1,1,2,2)
#' 
#' co <- coords()
#' 
#' points(co$plot$x[xx], co$plot$y[yy], cex = 5, col = 1)
#' points(co$figure$x[xx], co$figure$y[yy], cex = 5, col = 2)
#' points(co$device$x[xx], co$device$y[yy], cex = 5, col = 3)
#' 
#' 
#' co <- coords(seq(0, 1, 0.1), 1)
#' 
#' points(co$plot$x, co$plot$y, cex = 2, col = 4)
#' points(co$figure$x, co$figure$y, cex = 2, col = 5)
#' points(co$device$x, co$device$y, cex = 2, col = 6)
#' 
#' 
#' ## use line/side for x or y coordinates depending on side
#' mtext('text', line = 1, side = 3, at = 0.5)
#' text(0.5, coords(line = 1, side = 3), 'text', col = 2)
#' 
#' mtext('text', line = -1:4, side = 4, at = 0.5)
#' text(coords(line = -1:4, side = 4), 0.5, 'text', col = 2, srt = 90)
#' 
#' par(op)
#' 
#' @export

coords <- function(x = 0:1, y = x, to = 'user', line, side) {
  xy <- cbind(x, y)
  x  <- xy[, 1L]
  y  <- xy[, 2L]
  
  if (!missing(line) | !missing(side)) {
    lh <- par('cin')[2L] * par('cex') * par('lheight')
    
    sapply(line, function(li) {
      li <- li + 0.5
      x  <- diff(grconvertX(x, 'in', 'user')) * lh * li
      y  <- diff(grconvertY(y, 'in', 'user')) * lh * li
      
      (par('usr')[c(3, 1, 4, 2)] + c(-y, -x, y, x))[match(side, 1:4)]
    })
  } else
    list(
      plot   = list(x = grconvertX(x, 'npc', to), y = grconvertY(y, 'npc', to)),
      figure = list(x = grconvertX(x, 'nfc', to), y = grconvertY(y, 'nfc', to)),
      inner  = list(x = grconvertX(x, 'nic', to), y = grconvertY(y, 'nic', to)),
      device = list(x = grconvertX(x, 'ndc', to), y = grconvertY(y, 'ndc', to))
    )
}

#' Determine figure location
#' 
#' Convenience function to determine the location of a figure within a matrix.
#' 
#' @param idx the figure index
#' @param dim the dimensions of the figure matrix, usually set with
#' \code{par(mfrow = )}
#' @param byrow logical; use \code{FALSE} if \code{par(mfcol = )} was used
#' since this sets figures to be column-major; otherwise, figures are assumed
#' to be drawn row-major
#' 
#' @examples
#' op <- par(no.readonly = TRUE)
#' ## eg, a 1x1 figure
#' fig(1, c(1, 1))
#' 
#' par(mfrow = c(2, 4))
#' sapply(1:8, function(x)
#'   plot(1, main = paste(x, '-', names(fig(x, par('mfrow'))))))
#' 
#' par(mfcol = c(2, 4))
#' sapply(1:8, function(x)
#'   plot(1, main = paste(x, '-', names(fig(x, par('mfcol'), FALSE)))))
#' 
#' ## par(mfrow = c(4, 4))
#' i <- fig(1:16, c(4, 4))
#' matrix(names(i), 4, 4, byrow = TRUE)
#' 
#' ## for mfcol, set byrow = FALSE
#' ## par(mfcol = c(2, 8))
#' i <- fig(1:16, c(2, 8), FALSE)
#' matrix(names(i), 2, 8, byrow = FALSE)
#' 
#' ## par(mfrow = c(1, 4))
#' i <- fig(1:4, c(1, 4))
#' matrix(names(i), 1, 4, byrow = TRUE)
#' 
#' ## par(mfrow = c(4, 1))
#' i <- fig(1:4, c(4, 1))
#' matrix(names(i), 4, 1, byrow = TRUE)
#' 
#' 
#' ## example usage
#' pars <- list(
#'   ltop    = list(pch = 4, tcl = 0, bty = 'l'),
#'   lcenter = list(pch = 1, las = 1, tcl = 0, bty = 'n'),
#'   lbottom = list(pch = 16, las = 0, tcl = 0.5, bty = '7')
#' )
#' 
#' par(mfrow = c(5, 1), oma = c(5, 4, 4, 2), mar = c(0, 0, 0, 0))
#' ## apply a different set of pars depending on the figure location
#' sapply(1:5, function(ii) {
#'   f <- fig(ii, par('mfrow'))
#'   par(pars[[names(f)]])
#'   plot(1)
#' })
#' 
#' par(op)
#' 
#' @export

fig <- function(idx, dim, byrow = TRUE) {
  fig <- function(idx, mat) {
    sapply(idx, function(ii) {
      ii <- c(which(mat == ii, arr.ind = TRUE))
      res <- c(
        findInterval(ii[1L], c(2L, nrow(mat))),
        findInterval(ii[2L], c(2L, ncol(mat)))
      )
      paste0(res, collapse = '')
    })
  }
  
  loc <- c(
    'full', 'topleft', 'top', 'topright', 'left', 'center', 'right',
    'bottomleft', 'bottom', 'bottomright',
    'ltop', 'lcenter', 'lbottom', 'wleft', 'wcenter', 'wright'
  )
  mat <- if (is.matrix(dim))
    dim
  else matrix(seq.int(prod(dim)), dim[1L], dim[2L], byrow = byrow)
  
  res <- if (identical(dim(mat), c(1L, 1L)))
    rep_len(0L, length(idx))
  else if (ncol(mat) == 1L)
    findInterval(idx, c(2, nrow(mat))) + 10L
  else if (nrow(mat) == 1L)
    findInterval(idx, c(2, ncol(mat))) + 13L
  else sapply(idx, function(ii)
    which(fig(1:9, matrix(1:9, 3L, 3L, byrow = TRUE)) %in% fig(ii, mat)))
  
  setNames(res, loc[res + 1L])
}

#' Add filled arrows to a plot
#' 
#' Draw filled arrows between pairs of points.
#' 
#' @param x0,y0 coordinates of points \strong{from} which to draw
#' @param x1,y1 coordinates of points \strong{to} which to draw
#' @param size,width size parameters for arrows
#' @param curve a numeric value greater than 0 giving the curvature of the
#' arrows; default is \code{1} for staight lines, but values less than or
#' greater than 1 may be given for
#' @param code integer determining \emph{kind} of arrows to be drawn; arrows
#' are drawn at \code{{x0[i], y0[i]}}, \code{{x1[i], y1[i]}}, or both for
#' \code{code = 1:3}, respectively, or no heads drawn if \code{code = 0}
#' @param col,lty,lwd color, line type, and line width of the segment
#' @param fill,border fill and border color of arrow
#' @param sadj optional vector of the form \code{{x0,y0,x1,y1}} for adjusting
#' the \code{\link{segments}} of the arrows
#' @param ... additional graphical parameters passed to \code{\link{segments}}
#' and further to \code{\link{par}}
#' 
#' @seealso
#' \code{arrows}; \code{\link{carrows}}; \url{https://github.com/cran/sfsmisc}
#' 
#' @author
#' Original: Andreas Ruckstuhl, 19 May 1994; Cosmetic: Martin Machler, June
#' 1998; Modifications: Robert Redd
#' 
#' @examples
#' plot.new()
#' plot.window(c(-pi, pi), c(-1,1), asp = 1)
#' curve(sin(x), -pi, pi, add = TRUE)
#' xx <- seq(-pi, pi, length.out = 5)
#' 
#' ## arrows point to locations
#' arrows2(xx, sin(xx), xx + 0, sin(xx + 0))
#' 
#' ## arrows "follow" along curve
#' arrows2(xx, sin(xx), xx + .1, sin(xx + .1), col = 5, border = 2)
#' 
#' arrows2(-3,-1,3,1, code = 3, border = 2, fill = 0)
#' arrows2(-2, -1, -2, 1, code = 1, fill = 4, col = 4)
#' arrows2(-1, -1, -1, 1, code = 2, fill = 2, size = .5, width = .5)
#' arrows2(0, -1, 0, 1, code = 3, curve = 1.5)
#' arrows2(1, -1, 1, 1, code = 3, curve = .5, width = .5, lwd = 10, col = 3)
#' arrows2(2, -1, 2, 1, code = 3, lwd = 0)
#' 
#' @export

arrows2 <- function(x0, y0, x1 = x0, y1 = y0, size = 1, width = 0.1 / cin,
                    curve = 1, code = 2L, col = par('fg'), lty = par('lty'),
                    lwd = par('lwd'), fill = col, border = fill,
                    sadj = c(0,0,0,0), ...) {
  stopifnot(
    length(code) == 1L,
    code %in% 0:3
  )
  
  ## create coordinates of a polygon for a unit arrow head
  cin <- size * par('cin')[2L]
  uin <- 1 / xyinch()
  x <- sqrt(seq(0, cin ^ 2, length.out = 1000L))
  delta <- 0.005 / 2.54
  wx2 <- width * x ^ curve
  
  ## polar, NA to "break" long polygon, see plotr:::c2p
  x <- c(-x, -rev(x))
  y <- c(-wx2 - delta, rev(wx2) + delta)
  
  deg <- c(atan2(y, x), NA)
  rad <- c(sqrt(x ** 2 + y ** 2), NA)
  
  segments(x0 + sadj[1L], y0 + sadj[2L], x1 + sadj[3L], y1 + sadj[4L],
           col = col, lty = lty, lwd = lwd, lend = 1, xpd = NA, ...)
  
  if (code == 0L)
    return(invisible(NULL))
  
  if (code %in% 2:3) {
    theta <- atan2((y1 - y0) * uin[2L], (x1 - x0) * uin[1L])
    lx  <- length(x0)
    Rep <- rep.int(length(deg), lx)
    xx  <- rep.int(x1, Rep)
    yy  <- rep.int(y1, Rep)
    theta <- rep.int(theta, Rep) + rep.int(deg, lx)
    rad <- rep.int(rad, lx)
    polygon(xx + rad * cos(theta) / uin[1L], yy + rad * sin(theta) / uin[2L],
            col = fill, xpd = NA, border = border)
  }
  
  if (code %in% c(1L, 3L)) {
    arrows2(x1, y1, x0, y0, size, width, code = 2L, curve, col = col,
            lty = 0L, lwd = 0, fill = fill, border = border, ...)
  }
  
  invisible(NULL)
}

#' Curved arrows
#' 
#' Draw an arrow along the arc of a circle.
#' 
#' @param p1,p2 vectors of length two giving the \code{{x,y}} coordinates for
#' two points to draw a connecting arc
#' @param arc a vector of length two with the starting and ending positions
#' to draw the arc, in radians or degrees
#' @param degree logical; if \code{TRUE}, \code{arc} should be in degrees;
#' however, if either element of \code{arc} is greater than \code{2 * pi},
#' they are assumed to be in degrees and will be converted to radians; set to
#' \code{TRUE} to avoid conversion of small angles to radians
#' @param pad a vector of length two giving 1) padding between the tips of
#' the arrow/segment and the points and 2) additional padding between the
#' segment endpoints and tip of arrow--useful for thick lines which may
#' protrude from under the arrowhead
#' @param flip logical; if \code{TRUE}, the arrow will be rotated around the
#' circle 180 degrees
#' @param dir optional vector of directions for arrows; by default, arrows
#' will point to the nearest endpoint; \code{dir} should be a vector of
#' \code{1}s and \code{-1}s and will be recycled as necessary; other values
#' will be ignored and result in no arrows for those positions
#' @param col,lwd,lty color, line width, and line type passed to
#' \code{\link{lines}}
#' @param size,width,curve,fill,border additional parameters passed to
#' \code{\link{arrows2}}
#' 
#' @return
#' A list containing the arc endpoints and the center and radius of the
#' corresponding circle.
#' 
#' @seealso
#' \code{\link{arrows2}}; \code{\link{xspline}}
#' 
#' @examples
#' plot.new()
#' plot.window(c(-2,2), c(-2,2))
#' p <- matrix(c(rep(-1:1, 2), rep(-1:1, each = 2)), ncol = 2)
#' points(p)
#' 
#' carrows(p1 <- p[2, ], p2 <- p[1, ], pad = 0.3)
#' carrows(p1 <- p[4, ], p2 <- p[5, ], dir = c(0, 1), col = 3)
#' carrows(p1 <- p[6, ], p2 <- p[3, ], lwd = 10, pad = c(0.05, 0.1))
#' carrows(p1 <- p[1, ], p2 <- p[6, ], flip = TRUE)
#' carrows(p1 <- p[1, ], p2 <- p[5, ], dir = c(1, 0))
#' 
#' @export

carrows <- function(p1, p2, arc, degree = FALSE, pad = 0.01 * 1:2,
                    flip = FALSE, dir = NULL,
                    ## lines
                    col = par('col'), lwd = par('lwd'), lty = par('lty'),
                    ## arrows2
                    size = 1, width = size / 2, curve = 1, fill = col,
                    border = NA) {
  code_ <- function(x) c(2L, 0L, 1L)[match(x, -1:1)]
  pad_  <- function(x, pad) rawr::ht(x, -length(x) * (1 - pad))
  
  ## try to guess code for arrows2
  slope <- (p2[2L] - p1[2L]) / (p2[1L] - p1[1L])
  slope[!is.finite(slope) | slope == 0] <- 1
  code  <- code_(sign(slope))
  
  ## calculate arc based on p1, p2
  radius  <- sqrt(sum((p1 - p2) ^ 2)) / 2
  centers <- (p1 + p2) / 2
  
  arc <- if (!missing(arc)) {
    if (degree | any(arc > 2 * pi))
      arc * (pi / 180) else arc[1:2]
  } else sapply(list(p1, p2), function(x)
    atan2(x[2L] - centers[2L], x[1L] - centers[1L]))
  
  ## convert polar to cart and plot lines/arrows
  theta <- seq(arc[1L], arc[2L], length.out = 500L) + if (flip) pi else 0
  pad <- rep_len(pad, 2L)
  th <- pad_(theta, pad[1L])
  xx <- centers[1L] + radius * cos(th)
  yy <- centers[2L] + radius * sin(th)
  lines(pad_(xx, pad[2L]), pad_(yy, pad[2L]), col = col, lwd = lwd, lty = lty)
  
  xx <- rawr::ht(xx, 4L)
  yy <- rawr::ht(yy, 4L)
  arrows2(xx[1L], yy[1L], xx[2L], yy[2L], size = size, width = width,
          curve = curve, code = dir[1L] %||% code, col = col, lty = 0,
          lwd = 0, fill = fill, border = border)
  arrows2(xx[4L], yy[4L], xx[3L], yy[3L], size = size, width = width,
          curve = curve, code = dir[2L] %||% code, col = col, lty = 0,
          lwd = 0, fill = fill, border = border)
  
  invisible(list(arc = arc, centers = centers, radius = radius))
}

#' Add a logarithmic axis
#' 
#' Draw minor ticks on a log scale axis.
#' 
#' @param side an integer specifying which side of the plot the axis is to be
#' drawn on: 1=below, 2=left, 3=above, 4=right
#' @param nticks number of minor ticks between each pair of major ticks
#' @param labels logical; if \code{TRUE}, major ticks will be labeled using
#' \code{\link{pretty_sci}}
#' @param digits,base,limit,simplify additional arguments passed to
#' \code{\link{pretty_sci}}
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @return
#' A list with elements \code{at.major} and \code{at.minor} giving the points
#' at which tick marks were drawn for the major and minor axes, respectively.
#' 
#' @seealso
#' \code{\link{pretty_sci}}; \code{\link[sfsmisc]{axTexpr}}; \code{\link{axis}}
#' 
#' @examples
#' x <- 1:10
#' y <- function(base) base ^ x
#' op <- par(mar = c(3,5,3,5), las = 1)
#' plot(x, log(y(2), 2), ann = FALSE, axes = FALSE)
#' laxis(2, base = 2, limit = -1)
#' 
#' par(new = TRUE)
#' plot(x, y(10), log = 'y', axes = FALSE, ann = FALSE)
#' laxis(4, nticks = 10, tcl = .5, col.axis = 2)
#' 
#' par(new = TRUE)
#' plot(x, x, log = 'x', axes = FALSE, ann = FALSE, xpd = NA)
#' laxis(1, nticks = 10, tcl = -1, col.axis = 1, lwd = 2)
#' abline(v = x)
#' 
#' par(op)
#' 
#' @export

laxis <- function(side = 1L, nticks = 5L, labels = TRUE, digits = 0L,
                  base = 10, limit = base ^ 3, simplify = TRUE, ...) {
  ap <- par(switch(side, 'xaxp', 'yaxp', 'xaxp', 'yaxp', stop('Invalid axis')))
  yl <- c(-1, 1) + if (base == 10) log10(ap[-3L]) else c(1, ap[2L])
  pp <- seq(yl[1L], yl[2L])
  
  at0 <- at1 <- base ^ pp
  at2 <- c(sapply(pp, function(x)
    seq(1, base, length.out = nticks) * base ^ x))
  if (base != 10) {
    at1 <- log(at1, base)
    at2 <- log(at2, base)
  }
  
  op <- par(..., no.readonly = TRUE)
  on.exit(par(op))
  
  axis(side, at1, lwd = par('lwd'), if (labels)
    pretty_sci(at0, digits, base, limit, simplify) else FALSE)
  axis(side, at2, FALSE, tcl = par('tcl') * 0.5, lwd = 0,
       lwd.ticks = par('lwd'))
  
  invisible(list(at.major = at1, at.minor = at2))
}

#' Print scientific numbers
#' 
#' Functions to parse numeric vectors in scientific notation and return an
#' \code{\link{expression}} for a pretty display.
#' 
#' @param x a numeric vector
#' @param digits integer indicating the number of decimal places to be used
#' @param base a positive or complex number: the base with respect to which
#' logarithms are computed (default is 10)
#' @param limit a numeric value whose order of magnitude will set a limit
#' beyond which values of \code{x} will be displayed in scientific notation;
#' default is to display numbers beyond a magnitude of \code{base^3}; to
#' display scientific notation always, use a negative value
#' @param simplify logical; if \code{TRUE} (default), removes "1 x" from
#' scientific format
#' 
#' @seealso
#' \code{\link[sfsmisc]{pretty10exp}}; \code{\link[rawr]{roundr}};
#' \code{\link{format}}; \code{\link{sprintf}}
#' 
#' @return
#' For \code{oom} an integer vector of magnitudes. For \code{parse_sci} an
#' expression of values in scientific notation. For \code{pretty_sci} an
#' expression of values in standard or scientific notation or combination
#' depending on the value of \code{limit}.
#' 
#' @examples
#' x <- 10 ^ (1:5) / 10
#' oom(x)
#' oom(1 / x)
#' 
#' parse_sci(x)
#' parse_sci(x, simplify = FALSE)
#' parse_sci(x, base = 100)
#' parse_sci(1.1 * 2 ^ (1:5), 1, 2)
#' 
#' par(xpd = NA, mar = c(6,4,4,2) + .1)
#' plot(1:5, type = 'n', axes = FALSE, ann = FALSE)
#' axis(2, at = 1:5, labels = pretty_sci(x, simplify = FALSE), las = 1)
#' 
#' text(1:5, 0, pretty_sci(1 / x ^ 10))
#' text(1:5, 1, pretty_sci(1 / x, digits = 3))
#' text(1:5, 2, pretty_sci(1 / x, digits = 2, limit = 1e2))
#' text(1:5, 3, x)
#' text(1:5, 4, pretty_sci(x, limit = 1e2))
#' text(1:5, 5, pretty_sci(x, digits = 1))
#' text(1:5, 6, pretty_sci(x ^ 10))
#' 
#' text(1:5, -1, pretty_sci(1 / x, limit = -1, simplify = FALSE))
#' 
#' @export

pretty_sci <- function(x, digits = 0L, base = 10,
                       limit = base ^ 3, simplify = TRUE) {
  l <- as.list(x)
  limit <- if (limit < 0)
    -1 else oom(limit, base)
  om <- sapply(l, oom, base)
  
  sapply(seq_along(l), function(y)
    if (abs(om[y]) > limit)
      parse_sci(l[[y]], digits, base, simplify)
    else rawr::roundr(l[[y]], digits))
}

#' @rdname pretty_sci
#' @export
oom <- function(x, base = 10) {
  as.integer(ifelse(x == 0, 0L, floor(log(abs(x), base))))
}

#' @rdname pretty_sci
#' @export
parse_sci <- function(x, digits = 0L, base = 10, simplify = TRUE) {
  stopifnot(is.numeric(x))
  
  x <- to_sci_(x, digits, base)
  x <- strsplit(x, 'e[+]?[0]?')
  
  xbg <- sapply(x, function(y) rawr::roundr(as.numeric(y[[1L]]), digits))
  xsm <- sapply(x, '[[', 2L)
  txt <- do.call('sprintf', list(fmt = '"%s"%%*%%%s^%s', xbg, base, xsm))
  
  parse(text = if (simplify)
    gsub('\\"1\\"%\\*%\\s*', '', txt) else txt)
}

to_sci_ <- function(x, digits, base) {
  ## generalized format(x, scientific = TRUE)'er
  # base <- 2; digits = 1; x <- 1.1 * base ^ (1:5)
  # rawr:::to_sci_(x, 1, base)
  stopifnot(is.numeric(x))
  xbg <- rawr::roundr(x / base ^ oom(x, base), digits)
  xsm <- formatC(oom(x, base), width = 2, flag = 0)
  sprintf('%se+%s', xbg, xsm)
}

#' Add a color axis
#' 
#' Add a color-coded axis.
#' 
#' @param side an integer specifying which side of the plot the axis is to be
#' drawn on: 1=below, 2=left, 3=above, and 4=right
#' @param prop the proportion of each section to be scaled to the axis width
#' @param col a vector of colors equal to the length of \code{prop}
#' @param lwd line width for the axis
#' @param ... additional parameters passed to \code{\link{segments}}
#' 
#' @return
#' A list containing the start and end positions for each \code{prop} scaled
#' to the axis width.
#' 
#' @examples
#' x <- runif(100)
#' y <- runif(100)
#' 
#' plot(x, y)
#' caxis(1, 1:4, col = 2:5)
#' at <- caxis(3, 1:4, col = 2:5)
#' text(at$end, par('usr')[4], paste('group', 1:4),
#'      col = 2:5, xpd = NA, adj = c(1, -1))
#' 
#' @export

caxis <- function(side, prop, col = NULL, lwd = 3, ...) {
  side <- as.integer(side)[1L]
  stopifnot(side %in% 1:4)
  
  u <- par('usr')
  p <- prop / sum(prop)
  
  la <- length(p) + 1L
  at <- rescale(cumsum(c(0, p)), if (side %in% c(1L, 3L)) u[1:2] else u[3:4])
  
  if (is.null(col))
    col <- grey.colors(la - 1L)
  
  switch(
    side,
    segments(at[-la], u[3L], at[-1L], u[3L], col = col, lwd = lwd, ...),
    segments(u[1L], at[-la], u[1L], at[-1L], col = col, lwd = lwd, ...),
    segments(at[-la], u[4L], at[-1L], u[4L], col = col, lwd = lwd, ...),
    segments(u[2L], at[-la], u[2L], at[-1L], col = col, lwd = lwd, ...)
  )
  
  invisible(list(start = at[-la], end = at[-1L]))
}

#' subrect
#' 
#' Draw shapes inside a \code{\link{rect}}
#' 
#' @param xleft,ybottom,xright,ytop coordinates of the main \code{rect}
#' @param type,pos the type (diagonal or square) and position of the shape
#'   inside the \code{rect}
#' @param ... additional arguments passed to \code{\link{rect}}
#' 
#' @examples
#' pos <- eval(formals(subrect)$pos)
#' par(mfrow = n2mfrow(length(pos)))
#' 
#' for (p in pos) {
#'   plot(0, 0, main = p, type = 'n', xlab = '', ylab = '')
#'   rect(-0.5, -0.5, 0.5, 0.5)
#'   subrect(-0.5, -0.5, 0.5, 0.5, type = 'diag', pos = p, col = 'red')
#' }
#' 
#' for (p in pos) {
#'   plot(0, 0, main = p, type = 'n', xlab = '', ylab = '')
#'   rect(-0.5, -0.5, 0.5, 0.5)
#'   subrect(-0.5, -0.5, 0.5, 0.5, type = 'square', pos = p, col = 'red')
#' }
#' 
#' @export

subrect <- function(xleft, ybottom, xright, ytop, type = c('diagonal', 'square'),
                    pos = c('topleft', 'topright', 'bottomleft', 'bottomright'),
                    ...) {
  type <- match.arg(type)
  pos <- match.arg(pos)
  
  if (length(xleft) == 4L) {
    ybottom <- xleft[2L]
    xright <- xleft[3L]
    ytop <- xleft[4L]
    xleft <- xleft[1L]
  }
  
  if (type == 'diagonal') {
    left <- c(xleft, xright, xleft, xleft)
    right <- c(xleft, xright, xright, xleft)
    top <- c(ytop, ytop, ybottom, ytop)
    bottom <- c(ybottom, ybottom, ytop, ybottom)
  } else {
    xmid <- mean(c(xleft, xright))
    ymid <- mean(c(ybottom, ytop))
    
    left <- c(xleft, xmid, xmid, xleft, xleft)
    right <- c(xmid, xright, xright, xmid, xmid)
    top <- c(ymid, ymid, ytop, ytop, ymid)
    bottom <- c(ybottom, ybottom, ymid, ymid, ybottom)
  }
  
  co <- switch(
    match.arg(pos),
    topleft = list(x = left, y = top),
    topright = list(x = right, y = top),
    bottomleft = list(x = left, y = bottom),
    bottomright = list(x = right, y = bottom)
  )
  
  polygon(co, ...)
  
  invisible(co)
}
