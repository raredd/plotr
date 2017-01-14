### plotting extras
# color_bar, zoomin, ptlocator, polyShade, bpCI, pstar_, inset, grcols,
# click_text, click_shape, ctext, cmtext, ctitle, ctext_, polygon2, subplot
###


#' Color legend
#' 
#' Continuous color bar legend
#' 
#' @param cols vector of color names (or hexadecimal) from low to high
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
#' text(x = 0, y = seq(0, par('usr')[4], length = 6), pos = 4, xpd = TRUE,
#'      labels = pretty(seq(0, par('usr')[4]), n = 6), cex = .8, offset = .75)
#' 
#' \dontrun{
#' ## calling color_bar once in the col argument will color x accordingly
#' # and plot the legend simultaneously
#' 
#' ## compare:
#' png('/users/rawr/desktop/tmp.png')
#' par(mfrow = c(2,1))
#' plot(mtcars$mpg, pch = 19, cex = 2, 
#'      col = colorRampPalette(c('yellow','red'))(1000)[rescaler(mtcars$mpg, c(1, 1000))])
#' plot(mtcars$mpg, pch = 19, cex = 2, bty = 'l',
#'      col = color_bar(c('yellow','red'), mtcars$mpg, labels = mtcars$mpg))
#' dev.off()
#'
#' ## plot a color_bar legend by a variable other than x
#' # use y argument to match the variable in the original plot call
#' # and x as the variable to color and label
#' 
#' ## compare:
#' png('/users/rawr/desktop/tmp.png')
#' par(mfrow = c(2,1))
#' with(mtcars,
#'      plot(mpg, pch = 19, cex = 2, main = 'color by weight (red = heavier)',
#'      col = colorRampPalette(c('yellow','red'))(1000)[rescaler(wt, c(1, 1000))]))
#' with(mtcars,
#'      plot(mpg, pch = 19, cex = 2, bty = 'l', main = 'color by weight (red = heavier)',
#'      col = color_bar(c('yellow','red'), x = wt, y = mpg, labels = wt)))
#' dev.off()
#'}
#'
#' @export

color_bar <- function(cols, 
                      x = NULL, 
                      y = x,
                      labels = NULL,
                      at.x = par('usr')[2], 
                      at.y = par('usr')[3], 
                      cex.x = 1, 
                      cex.y = 1,
                      ...) {
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(list(...))
  
  par(mar = c(5, 4, 4, 4) + .1, xpd = TRUE)
  bx <- par('usr')
  nc <- 1000
  cols <- colorRampPalette(cols)(nc)
  
  bx.y <- c(bx[3], bx[4])
  sapply(0:nc, function(x) {
    segments(at.x, 
             at.y + x * diff(bx.y) / nc * cex.y, 
             at.x + diff(bx[1:2]) / nc * 20 * cex.x, 
             at.y + x * diff(bx.y) / nc * cex.y, 
             col = cols[x], lwd = 1, xpd = TRUE)
  })
  if (!is.null(labels))
    text(x = at.x, y = pretty(y), labels = pretty(labels), 
         pos = 4, cex = .8, offset = .75)
  if (!is.null(x))
    invisible(cols[rescaler(x, c(1, nc))])
}

#' Zoom for points in base \code{R} plot
#' 
#' Provides a summary statistic for sample points in a plot (in progress).
#' 
#' @param x x-coordinates
#' @param y y-coordinates
#' @param ... other options passed to \code{\link{identify}}
#' 
#' @examples
#' set.seed(1618)
#' x <- runif(10)
#' y <- rnorm(10, mean = 5)
#' 
#' par(mfrow = c(1, 2))
#' plot(x, y, xlab = 'mean', ylab = 'sd')
#' 
#' zoomin(x, y)
#' ## ESC to quit
#' @export

zoomin <- function(x, y, ...) {
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  ans <- identify(x, y, n = 1, plot = FALSE, ...)
  
  zoom <- function (x, y, xlim, ylim, xd, yd) {
    
    rxlim <- x + c(-1, 1) * (diff(range(xd)) / 20)
    rylim <- y + c(-1, 1) * (diff(range(yd)) / 20)
    
    par(mfrow = c(1, 2))
    plot(xd, yd, xlab = 'mean', ylab = 'sd')
    
    xext <- yext <- rxext <- ryext <- 0
    
    if (par('xaxs') == 'r') {
      xext <- diff(xlim) * 0.04
      rxext <- diff(rxlim) * 0.04
    }
    if (par('yaxs') == 'r') {
      yext <- diff(ylim) * 0.04
      ryext <- diff(rylim) * 0.04
    }
    
    rect(rxlim[1] - rxext, rylim[1] - ryext, rxlim[2] + rxext, rylim[2] + ryext)
    xylim <- par('usr')
    xypin <- par('pin')
    
    rxi0 <- xypin[1] * (xylim[2] - (rxlim[1] - rxext)) / diff(xylim[1:2])
    rxi1 <- xypin[1] * (xylim[2] - (rxlim[2] + rxext)) / diff(xylim[1:2])
    y01i <- xypin[2] * (xylim[4] - (rylim[2] + ryext)) / diff(xylim[3:4])
    y02i <- xypin[2] * ((rylim[1] - ryext) - xylim[3]) / diff(xylim[3:4])
    mu <- x
    
    curve(dnorm(x, mean = mu, sd = y), from = -4 * y + mu, to = 4 * y + mu, 
          xlab = paste('mean:', round(mu, 2), ', sd: ', round(y, 2)), ylab = '')
    
    xypin <- par('pin')
    par(xpd = NA)
    xylim <- par('usr')
    xymai <- par('mai')
    
    x0 <- xylim[1] - diff(xylim[1:2]) * (xymai[2] + xymai[4] + rxi0)/xypin[1]
    x1 <- xylim[1] - diff(xylim[1:2]) * (xymai[2] + xymai[4] + rxi1)/xypin[1]
    y01 <- xylim[4] - diff(xylim[3:4]) * y01i/xypin[2]
    y02 <- xylim[3] + diff(xylim[3:4]) * y02i/xypin[2]
    
    par(xpd = TRUE)
    
    xend <- xylim[1] - diff(xylim[1:2]) * xymai[2] / (2 * xypin[1])
    xprop0 <- (xylim[1] - xend) / (xylim[1] - x0)
    xprop1 <- (xylim[2] - xend) / (xylim[2] - x1)
    par(xpd = NA)
    segments(c(x0, x0, x1, x1), 
             c(y01, y02, y01, y02), 
             c(xend, xend, xend, xend), 
             c(xylim[4] - (xylim[4] - y01) * xprop0, 
               xylim[3] + (y02 - xylim[3]) * xprop0, 
               xylim[4] - (xylim[4] - y01) * xprop1, 
               xylim[3] + (y02 - xylim[3]) * xprop1))
    par(mfg = c(1, 1))
    
    plot(xd, yd, xlab = 'mean', ylab = 'sd')
  }
  
  if (length(ans)) {
    zoom(x[ans], y[ans], range(x), range(y), x, y)
    points(x[ans], y[ans], pch = 19)
    zoomin(x, y)
  }
}

#' Point locator
#' 
#' Interactive point locator
#' 
#' @param n the maximum number of points to locate
#' @param x x-coordinates
#' @param y y-coordinates
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

ptlocator <- function(n = 512, x, y, col = tcol('red', 80), pch = 20, ...) {
  xsc <- scale(x)
  ysc <- scale(y)
  #   pos <- numeric(n)
  pos <- NULL
  for(i in seq(n)) {
    pt <- locator(1)
    if (!is.null(pt)){
      ptxsc <- scale(pt$x, center = attr(xsc, "scaled:center"),
                     scale = attr(xsc, "scaled:scale"))
      ptysc <- scale(pt$y, center = attr(ysc, "scaled:center"),
                     scale = attr(ysc, "scaled:scale"))
      pos.i <- which.min(sqrt((c(ptxsc) - c(xsc)) ^ 2 + (c(ptysc) - c(ysc)) ^ 2))
      points(x[pos.i], y[pos.i], col = col, pch = pch, ...)
      #       pos[i] <- pos.i
      pos <- c(pos, pos.i)
    } else return(invisible(pos))
  }
  return(invisible(pos))
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
#' @param ... additional parameters passed to \code{\link{polygon}}; common
#' uses are \code{density} for shading lines, \code{col} for shading color(s),
#' \code{border} for border color, or \code{lty} for line type
#' 
#' @seealso
#' \url{http://www.fromthebottomoftheheap.net/2013/01/11/shading-regions-under-a-curve/}
#' 
#' @examples
#' set.seed(1)
#' x <- c(rnorm(75), rnorm(25, 5))
#' plot(xx <- density(x), panel.first = {
#'   polyShade(xx$x, xx$y, from = c(min(xx$x), 6), to = c(-2, max(xx$x)),
#'             col = adjustcolor('red', .3), border = NA)
#' })
#' 
#' polyShade(xx$x, xx$y, -1, 2, col = 'red', border = NA)
#' polyShade(xx$x, xx$y, 0, 4, col = 'blue', density = 20, lty = 4, 
#'           miny = par('usr')[1], border = NA)
#' 
#' @export

polyShade <- function(x, y, from, to, n = 50, miny, ...) {
  if (!identical(lf <- length(from), lt <- length(to)))
    stop('\'from\' and \'to\' should have the same length')
  if (missing(miny))
    miny <- min(y)
  
  drawPoly <- function(fun, from, to, n = 50, miny, ...) {
    Sq <- seq(from = from, to = to, length = n)
    polygon(x = c(Sq[1], Sq, Sq[n]), y = c(miny, fun(Sq), miny), ...)
  }
  interp <- approxfun(x = x, y = y)
  mapply(drawPoly, from = from, to = to, ...,
         MoreArgs = list(fun = interp, n = n, miny = miny))
  invisible()
}

#' Barplot confidence intervals
#' 
#' Add confidence intervals (error bars) and group comparisons to barplots.
#' 
#' @param bp the return value of \code{\link{barplot}}, i.e., a vector or
#' matrix (when \code{beside = TRUE}) of all bar (or group) midpoints
#' @param horiz logical; if \code{TRUE}, \code{bpCI} assumes horizontal bars
#' @param ci logical; draw error bars (must give \code{ci.u}, \code{ci.l})
#' @param ci.u,ci.l a numeric vector or matrix having the same dimensions as
#' \code{bp} giving the upper and lower intervals, respectively
#' @param ci.width width of the ends of the error bars, will depend on 
#' \code{range(bp)}
#' @param sig logical; if \code{TRUE}, draws group comparisons (must give
#' \code{pvals} to plot sig stars)
#' @param pvals p-values of group comparisons to be displayed as sig stars
#' @param pch plotting character to be used for significance; default is 
#' \code{*} and uses same significance codes as \code{\link{printCoefmat}}
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @examples
#' ## generate data and p-values
#' hh <- t(VADeaths)[1:2, 5:1]
#' ci.l <- hh * 0.85
#' ci.u <- hh * 1.15
#' pvals <- pt(apply(hh, 2, diff), 1) / 5:1
#' 
#' bp <- barplot(hh, beside = TRUE, ylim = c(0,100))
#' bpCI(bp, ci.u = ci.u, ci.l = ci.l, sig = TRUE, pvals = pvals, pch = "+")
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

bpCI <- function(bp, horiz = FALSE, ci = TRUE, ci.u, ci.l, ci.width = .5,
                 sig = FALSE, pvals, pch, ...) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(...)
  if (ci) {
    ci.width <- ci.width / 2
    if (horiz) {
      ci.l <- t(ci.l)
      ci.u <- t(ci.u)
      segments(ci.l, t(bp), ci.u, t(bp))
      segments(ci.u, t(bp - ci.width), ci.u, t(bp + ci.width))
      segments(ci.l, t(bp - ci.width), ci.l, t(bp + ci.width))
    } else {
      segments(bp, ci.l, bp, ci.u)
      segments(bp - ci.width, ci.u, bp + ci.width, ci.u)
      segments(bp - ci.width, ci.l, bp + ci.width, ci.l)
    }
    if (sig) {
      if (horiz)
        stop('\'sig\' is not supported when \'horiz = TRUE\'')
      if (nrow(bp) > 2)
        stop('\'sig\' is not supported for > 2 bars per group')
      if (missing(pch))
        pch <- '*'
      yy <- rbind(c(ci.u[1, ] + 3), c(apply(ci.u, 2 , max) + 5),
                  c(apply(ci.u, 2, max) + 5), c(ci.u[2, ] + 3))
      xx <- apply(bp, 2, function(x) rep(x, each = nrow(bp)))
      sapply(1:ncol(bp), function(x) lines(xx[, x], yy[, x]))
      xt <- colMeans(bp)
      yt <- apply(ci.u, 2, max) + 7
      text(pstar_(pvals, pch), x = xt, y = yt)
    }
  }
}

pstar_ <- function(pv, pch) {
  symnum(pv, corr = FALSE, na = FALSE, 
         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
         symbols = gsub('\\*', pch, c("***", "**", "*", ".", "NS")))
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
#' plot(mpg ~ wt, data = mtcars, col = 'blue')
#' abline(lm(mpg ~ wt, data = mtcars), col = 'red')
#' inset('topright', pct = .4)
#' hist(mtcars$mpg, ann = FALSE, panel.last = box(),
#'      col = 'dodgerblue2', las = 1)
#' 
#' par(op)
#' plot(1:10)
#' op <- par(no.readonly = TRUE)
#' Map(function(x) {
#'  inset(x, las = 1, col = 'red', pct = 1/3)
#'  plot(rnorm(10), ann = FALSE, axes = FALSE, panel.last = box())
#'  par(op)
#'  Sys.sleep(.5)
#'  }, c("bottomright", "bottom", "bottomleft", "left",
#'       "topleft", "top", "topright", "right", "center"))
#' 
#' @export

inset <- function(x, y = NULL, pct = .25, ...) {
  m <- substitute(...())
  usr <- par('usr')
  plt <- par('plt')
  pctx <- pct * diff(plt[1:2])
  pcty <- pct * diff(plt[3:4])
  
  auto <- if (is.character(x))
    match.arg(x, c("bottomright", "bottom", "bottomleft", "left",
                   "topleft", "top", "topright", "right", "center")) else NA
  
  xx <- switch(auto, bottomright = c(plt[2] - pctx, plt[2]),
               bottom = mean(plt[1:2]) + c(-1, 1) * pctx / 2,
               bottomleft = c(plt[1], plt[1] + pctx),
               left = c(plt[1], plt[1] + pctx),
               topleft = c(plt[1], plt[1] + pctx),
               top = mean(plt[1:2]) + c(-1, 1) * pctx / 2,
               topright = c(plt[2] - pctx, plt[2]),
               right = c(plt[2] - pctx, plt[2]),
               center = mean(plt[1:2]) + c(-1, 1) * pctx / 2)
  
  yy <- switch(auto, bottomright = c(plt[3], plt[3] + pcty),
               bottom = c(plt[3], plt[3] + pcty),
               bottomleft = c(plt[3], plt[3] + pcty),
               left = mean(plt[3:4]) + c(-1, 1) * pcty / 2,
               topleft = c(plt[4] - pcty, plt[4]),
               top = c(plt[4] - pcty, plt[4]),
               topright = c(plt[4] - pcty, plt[4]),
               right = mean(plt[3:4]) + c(-1, 1) * pcty / 2,
               center = mean(plt[3:4]) + c(-1, 1) * pcty / 2)
  
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
#' mathematical expressions; \code{link{tcol}}
#' 
#' @return
#' A vector of length two with the x- and y-coordinates of the text position.
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
  co <- locator(1)
  text(co[[1]], co[[2]], labels = if (missing(expr)) '' else expr, ...)
  c(x = co[[1]], y = co[[2]])
}

#' Add shapes interactively in base \code{R} graphics
#' 
#' Add shapes anywhere in a plot (including margins) with mouse click(s).
#' 
#' @param shape type of shape; choices are \code{'box'}, \code{'arrow'},
#' \code{'line'}, \code{'poly'}, \code{'circle'}, and \code{'cyl'}
#' @param col shape outline color
#' @param border border colour for shape; defaults to value for \code{col}
#' @param corners number of corners to draw for a polygon
#' @param lty,lwd graphical parameters; see \code{\link{par}}
#' @param density the density of shading lines in lines per inch; the default
#' value of \code{NULL} means that no shading lines are drawm; a zero value of
#' \code{density} means no shading or filling, whereas negative values and 
#' \code{NA} suppress shading (and so allow color filling)
#' @param length length of the edges of the arrow head (in inches); see 
#' \code{\link{arrows}}
#' @param code integer code for 'arrows' determining \emph{kind} of arrows to 
#' be drawn
#' @param ... ignored
#' 
#' @seealso \code{\link{click_text}}, \code{\link{tcol}}, \code{\link{rect}},
#' \code{\link{arrows}}, \code{\link{rect}}, \code{\link{rect}},
#' \code{\link{segments}}, \code{\link{polygon}},
#' \code{\link[plotrix]{draw.circle}}, \code{\link[plotrix]{cylindrect}}
#' 
#' @examples
#' \dontrun{
#' plot.new()
#' click_shape() # a line segment
#' click_shape('arrow', col = 'blue', code = 2, lwd = 2, length = .15)
#' click_shape('box', border = 'purple', col = 'pink', lwd = 2)
#' click_shape('box', col = NULL, border = 'purple', lwd = 2)
#' click_shape('line', col = 'orange', lty = 3, lwd = 3)
#' click_shape('poly', corners = 5, border = 'green', col = 'orange')
#' click_shape('poly', corners = 3, border = 'red', col = 'yellow', lty = 1)
#' click_shape('cyl', col = 'orange')
#' click_shape('circle', col = 'orange', border = 'black', lty = 3, lwd = 3)
#' }
#' 
#' @export

click_shape <- function(shape = 'line', col = 'black', border = col,
                        corners = 5, lty = par('lty'), lwd = par('lwd'), 
                        density = NULL, length = 1, code = 2, ...) {
  op <- par(no.readonly = TRUE) 
  on.exit(par(op))
  par(xpd = NA)
  
  RECT <- function(...) {
    co <- c(unlist(locator(1)), unlist(locator(1)))
    rect(co[1], co[2], co[3], co[4], col = col, 
         density = density, border = border, lty = lty, lwd = lwd,
         length = NULL)
  }
  ARRO <- function(...) {
    co <- c(unlist(locator(1)), unlist(locator(1)))
    arrows(co[1], co[2], co[3], co[4], code = code,
           col = col, border = NULL, lty = lty, lwd = lwd, length = length)
  }
  LINE <- function(...){
    co <- c(unlist(locator(1)), unlist(locator(1)))
    segments(co[1], co[2], co[3], co[4], col = col, 
             border = NULL, lty = lty, lwd = lwd, length = NULL)
  }
  POLY <- function(...) {
    locations <- locator(corners)
    polygon(locations, col = col, density = density,
            border = border, lty = lty, lwd = lwd, length = NULL)
  }
  CIRC <- function(...) {
    co <- c(unlist(locator(1)), unlist(locator(1)))
    rad <- sqrt(((co[3] - co[1]) ** 2) + ((co[4] - co[2]) ** 2))
    draw.circle(co[1], co[2], radius = rad, col = col,
                border = border, lty = lty, lwd = lwd)
  }
  CYLI <- function(...) {
    coor <- unlist(locator(2))
    cylindrect(coor[1], coor[3], coor[2], coor[4], col = col, border = border)
  }
  
  suppressWarnings(
    switch(shape,
           box    = RECT(col, border, lty, lwd, density),
           arrow  = ARRO(col, border, lty, lwd, code, length),
           line   = LINE(col, border, lty, lwd),
           poly   = POLY(col, border, lty, lwd, density, corners),
           circle = CIRC(col, border, lty, lwd),
           cyl    = CYLI(col, border),
           stop('Invalid Argumets'))
  )
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
    cols <- rep(1, length(text))
  l <- ctext_(text, cols, space)
  for (ii in seq_along(l$text))
    text(labels = l$text[[ii]], col = l$colors[ii], ...)
  invisible(NULL)
}

#' @rdname ctext
#' @export
cmtext <- function(text, cols, space = TRUE, ...) {
  if (missing(cols))
    cols <- rep(1, length(text))
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
    cols <- rep(1, length(text))
  wh <- c('main','sub','xlab','ylab')
  l <- setNames(lapply(wh, function(x)
    ## if c() not used to pass text, this could cause problems
    ctext_(as.character(m[[x]])[-1], cols, space)), wh)
  m <- par('mgp')
  if (length(l$main$text)) {
    ll <- l$main
    for (ii in seq_along(ll$text))
      do.call('mtext', c(list(text = ll$text[[ii]], col = ll$colors[ii],
                              side = 3, font = 2, line = ml %||% (m[1] * .5)), dots))
  }
  if (length(l$sub$text)) {
    ll <- l$sub
    for (ii in seq_along(ll$text))
      do.call('mtext', c(list(text = ll$text[[ii]], col = ll$colors[ii],
                              side = 1, line = ml %||% (m[1] + 1)), dots))
  }
  if (length(l$xlab$text)) {
    ll <- l$xlab
    for (ii in seq_along(ll$text))
      do.call('mtext', c(list(text = ll$text[[ii]], col = ll$colors[ii],
                              side = 1, line = ml %||% m[1]), dots))
  }
  if (length(l$ylab$text)) {
    ll <- l$ylab
    for (ii in seq_along(ll$text))
      do.call('mtext', c(list(text = ll$text[[ii]], col = ll$colors[ii],
                              side = 2, line = ml %||% m[1]), dots))
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
#' pars <- subplot(hist(tmp, ann = F), 0, -1)
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
      x <- usr[1] + inset[1] * (usr[2] - usr[1])
      if (missing(hadj))
        hadj <- 0
    }
    if (length(grep('right', x.char, ignore.case = TRUE))) {
      x <- usr[2] - inset[1] * (usr[2] - usr[1])
      if (missing(hadj))
        hadj <- 1
    }
    if (length(grep('top', x.char, ignore.case=TRUE))) {
      y <- usr[4] - inset[2] * (usr[4] - usr[3])
      if (missing(vadj))
        vadj <- 1
    }
    if (length(grep('bottom', x.char, ignore.case = TRUE))) {
      y <- usr[3] + inset[2] * (usr[4] - usr[3])
      if (missing(vadj))
        vadj <- 0
    }
  }
  
  xy <- xy.coords(x, y, log = log)
  
  if (length(xy$x) != 2L) {
    pin <- par('pin')
    xx <- grconvertX(xy$x[1], to = 'npc')
    yy <- grconvertY(xy$y[1], to = 'npc')
    
    x <- c(xx - hadj * size[1] / pin[1],
           xx + (1 - hadj) * size[1] / pin[1])
    y <- c(yy - vadj * size[2] / pin[2],
           yy + (1 - vadj) * size[2] / pin[2])
    
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
