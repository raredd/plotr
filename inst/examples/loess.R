#' LOESS demonstration
#' 
#' Function to demonstrate locally-weighted regression.
#' 
#' @param x,y numeric vectors of the same length
#' @param span the smoothing parameter \eqn{\alpha}
#' @param degree degree of polynomials to be used; must be in \eqn{{0,1,2}}
#' @param family if \code{"gaussian"}, fitting is by least-squares, and if
#' \code{"symmetric"}, a re-descending M estimator is used with Tukey's
#' biweight function
#' @param nearest logical; indicates how \eqn{x_0} should be determined,
#' the value at which \eqn{\hat{f}(x_0) f^(x_0)} is computed; if
#' \code{nearest} is \code{TRUE}, the closest \emph{data} value is taken
#' @param nout number of points at which to evaluate, i.e, determining
#' \eqn{u_i}, \eqn{i = 1,2, \dots, \mathtt{nout}}, at which
#' \eqn{\hat{f}(u_i) f^(u_i)} is computed
#' @param xlim x-range; to extend or determine (iff \code{strictlim = TRUE})
#' the \eqn{x}-range for plotting
#' @param ylim y-range; to extend or determine (iff \code{strictlim = TRUE})
#' the \eqn{y}-range for plotting
#' @param strictlim logical; determines if \code{xlim} and \code{ylim} should
#' be strict limits (as e.g., in \code{\link{plot.default}}), or just a
#' suggestion to \emph{extend} the data-dependent ranges
#' @param verbose logical; if \code{TRUE}, prints to console
#' @param inch.sym symbol size in inches of the maximal weight circle symbol
#' @param pch plotting character, see \code{\link{points}}
#' @param shade logical; if \code{TRUE}, \code{\link{polygon}(.., density=..)}
#' will be used to shade off the regions where the weights are zero
#' @param w.symbols logical; indicates if the non-zero weights should be
#' visualized by circles with radius proportional to \code{inch.sym} and
#' \eqn{\sqrt{w}} where \eqn{w} are the weights
#' @param sym.col,w.col,line.col colors for the symbols, weights, and lines,
#' respectively
#' 
#' @author
#' Greg Snow <gls@byu.edu>, S-news, 27 Sep 2001
#' 
#' @examples
#' loess2(mtcars$wt, mtcars$mpg)
#' 
#' data('ethanol', package = 'lattice')
#' loess2(ethanol$E, ethanol$NOx, span = .25)
#' loess2(ethanol$E, ethanol$NOx, span = .25, family = 'symmetric')
#' ## tricube kernel estimate
#' loess2(ethanol$E, ethanol$NOx, degree = 0)
#' 
#' ## artificial example with one outlier
#' set.seed(1)
#' n2 <- 50; x <- 1:(1 + 2 * n2)
#' fx <- (x / 10 - 5) ^ 2
#' y <- fx + 4 * rnorm(x)
#' y[n2 + 1] <- 1e4
#' 
#' ## not robust !!
#' loess2(x, y, span = 1 / 3, ylim = c(0,1000))
#' loess2(x, y, span = 1 / 3, family = 'symm')
#' loess2(x, y, span = 1 / 3, family = 'symm', w.symb = FALSE, ylim = c(0,40))
#' ## but see warnings() -- there's a "fixup"
#' loess2(x, y, span = 1 / 3, family = 'symm', ylim = c(0,40))
#' 
#' @export

loess2 <- function(x, y, span = 1 / 2, degree = 1,
                   family = c('gaussian','symmetric'), nearest = FALSE,
                   nout = 501, xlim = numeric(0), ylim = xlim,
                   strictlim = TRUE, verbose = TRUE, w.symbols = TRUE,
                   inch.sym = 0.25, sym.col = 'blue', pch = 4,
                   shade = TRUE, w.col = 'light blue', line.col = 'steelblue') {
  
  family <- match.arg(family)
  ## drop NA's and sort:
  miss.xy <- is.na(x) | is.na(y)
  x <- x[!miss.xy]
  y <- y[!miss.xy]
  ix <- order(x)
  x <- x[ix]
  y <- y[ix]
  degree <- as.integer(degree)
  if (length(degree) != 1 || is.na(degree) || degree < 0 || 2 < degree)
    stop('\'degree\' must be in {0,1,2}')

  fit.D <- loess(y ~ x, degree = degree, span = span, family = family,
	   control = loess.control(surface = 'direct'))

  fit.I <- loess(y ~ x, degree = degree, span = span, family = family)

  xx <- seq(min(x), max(x), length.out = nout)
  est <- list(x = xx,
              y = predict(fit.I, newdata = data.frame(x = xx)))

  xl <- if (strictlim && is.numeric(xlim) && length(xlim) == 2)
      xlim else {
      xl <- range(x, est$x, xlim)
      xl <- xl + c(-1, 1) * 0.03 * diff(xl)
  }
  yl <- if (strictlim && is.numeric(ylim) && length(ylim) == 2) {
      dy <- 0.05 * diff(ylim)
      ylim
  } else {
      yl <- range(y, est$y, ylim, fitted(fit.D))
      dy <- 0.05 * diff(yl)
      yl + c(-1, 1) * dy
  }
  ## room below for weights
  dy <- 4 * dy
  yl[1] <- yl[1] - dy
  stit <- paste('span = ', span,';  degree = ', degree)
  if (family != 'gaussian')
    stit <- paste(stit, '.  family = \'', family,'', sep = '')

  fitPlot <- function(x, y, w, est, fit.D, xl, yl) {
    pU <- par('usr')
    plot(x, y, pch = pch, xlim = xl, ylim = yl, sub = stit, las = 1, tcl = .2,
         mgp = c(2,0.5,0))
    if (!is.null(w)) {
      w <- w / max(w) # in [0,1]
	    wP <- w > 1e-5
	    nw <- length(xw <- x[wP])
	    if (w.symbols)
	      symbols(xw, y[wP], circles = sqrt(w[wP]),
	              inches = inch.sym, add = TRUE, fg = sym.col)
	    ## scale [0,1] to yl[1] + [0, dy]:
	    y0 <- pU[3]
	    wy <- y0 + (dy + yl[1] - y0) * w[wP]
	    polygon(c(xw[1], xw, xw[nw]), c(y0, wy, y0), col = w.col)
	    segments(xw, rep(y0, nw), xw, wy, col = sym.col)
    }
    lines(x, fitted(fit.D), col = 2, lwd = 2)
    mtext('Exact estimate with linear interpolation between x-values (surface = \'direct\')',
          col = 2, adj = 0, line = 0.5)
    lines(est, col = 3, lwd = 2)
    mtext('Estimate obtained using the default interpolation scheme',
          col = 3, adj = 0, line = 1.5)
    pU
  }
  fitPlot(x, y, w = NULL, est, fit.D, xl, yl)
  
  repeat {
    if (verbose)
      cat('Click left for x0  to predict -- click right to stop ')
    x0 <- locator(1)$x
    if (verbose)
      cat('\n')
    ## right clicking leaves loop
    if (!length(x0))
      break
    if (nearest)
      x0 <- unique(x[abs(x - x0) == min(abs(x - x0))])
    if (verbose)
      cat('x0 =', x0, '\n')
    Dx <- abs(x - x0)
    d <- if (span < 1)
      sort(Dx)[as.integer(span * length(x))] else max(Dx) * sqrt(span)
    w <- rep(0, length(x))
  	s <- Dx <= d
  	## tricube weights
  	w[s] <- (1 - (Dx[s] / d) ^ 3) ^ 3
  	pU <- fitPlot(x, y, w, est, fit.D, xl, yl)
  	
  	if (degree > 0L) {
  	  if(degree == 1L)
  	    abline(lm(y ~ x, weights = w), col = line.col) else
  	      ## (degree == 2)
  	      ## predict(lm( ~ poly()) fails!
  	      lines(xx, predict(lm(y ~ x + I(x ^ 2), weights = w),
  	                        data.frame(x = xx)), col = line.col, err = -1)
  	} else {
  	  ## degree == 0
  	  # lines(x, fitted(lm(y ~ 1, weights = w)), col = line.col, err = -1)
  	  abline(a = sum(w * y) / sum(w), b = 0, col = line.col)
  	}
  	
  	abline(v = x0, col = line.col, lty = 3, lwd = 0.2)
    axis(1, at = x0, labels = formatC(x0, digits = 3), col.axis = line.col,
         tcl = -0.8, lwd.tick = 2, col.tick = line.col)
    if ((x1 <- x0 - d) > xl[1]) {
      abline(v = x1, col = line.col, lty = 2)
      if (shade)
        polygon(c(pU[1], x1, x1, pU[1]), pU[c(3,3,4,4)], density = 5)
    }
    if ((x1 <- x0 + d) < xl[2]) {
      abline(v = x1, col = line.col, lty = 2)
      if (shade)
        polygon(c(x1, pU[c(2,2)], x1), pU[c(3,3,4,4)], density = 5, angle = -45)
    }
  }
}
