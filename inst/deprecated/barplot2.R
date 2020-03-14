#' Stacked, grouped barplot
#' 
#' A special case of \code{\link{barplot}} with stacked and grouped bars.
#' 
#' @param array an \code{\link{array}}
#' @param width the width of the bars
#' @param space either a length 2 vector giving the space between bars and
#' groups, respectively, or a vector giving individual spaces between bars,
#' recycled as needed
#' @param col a vector of colors for each row of \code{array}
#' @param names.arg a vector of names for each bar; alternatively, a named
#' list with group and individual bar names; see examples
#' @param border the color for the border of each bar; use \code{NA} for
#' no border
#' @param axes logical; if \code{TRUE}, axes are drawn
#' @param angle,density the angle and density of shading lines; see
#' \code{\link{rect}}
#' @param panel.first,panel.last expressions to be evaluated before or after
#' drawing bars, respectively
#' @param xlim,ylim the x- and y-axis limits
#' @param xpd logical; if \code{TRUE} (default), bars are allowed to go
#' outside the plotting region
#' @param horiz logical; if \code{TRUE}, bars are drawn horizontally
#' @param plot logical; if \code{FALSE}, no plot is started but return value
#' is calculated and returned
#' @param add logical; if \code{TRUE}, bars are added to an already existing
#' plot
#' 
#' @return
#' A list giving the locations of each bar and the mid-point of each group.
#' 
#' @examples
#' set.seed(1)
#' x <- array(runif(4 * 3 * 3), c(4, 3, 3))
#' p <- apply(x, 2:3, prop.table)
#' t <- with(mtcars, table(vs, am, gear))
#' 
#' barplot(x[, , 1])
#' barplot2(x[, , 1]) ## same
#' 
#' ## group labels
#' barplot2(x, names.arg = list(A = 1:3, B = 4:6, C = 7:9))
#' bp <- barplot2(x)
#' mtext(1:9, side = 1L, at = bp$at, line = 1)
#' mtext(1:3, side = 1L, at = bp$group, line = 3)
#' 
#' ## simplified space
#' barplot2(x, space = c(0, 1, 2) / 2, las = 1L, col = 1:4,
#'          legend.text = sprintf('Factor %s', 1:4),
#'          names.arg = list(A = 1:3, B = 4:6, C = 7:9))
#' 
#' @export

barplot2 <- function(array, width = 1, space = c(0.1, 0.5), names.arg = NULL,
                     legend.text = NULL, beside = FALSE, horiz = FALSE,
                     density = NULL, angle = 45, col = NULL, border = par('fg'),
                     main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                     xlim = NULL, ylim = NULL, xpd = TRUE, log = '', axes = TRUE,
                     axisnames = TRUE,
                     cex.axis = par('cex.axis'), cex.names = par('cex.axis'),
                     inside = TRUE, plot = TRUE, axis.lty = 0, offset = 0,
                     add = FALSE, ann = !add && par('ann'), args.legend = NULL,
                     panel.first = NULL, panel.last = NULL, ...) {
  `%||%` <- function(x, y)
    if (is.null(x)) y else x
  
  nr <- nrow(array)
  nc <- ncol(array)
  nn <- dim(array)[3L]
  
  ht <- apply(array, 2:3, cumsum)
  
  at <- seq.int(nc * nn)
  at <- cumsum(rep_len(width, nc * nn)) - 1L
  sp <- if (length(space) == 2L)
    rep(rep(space, c(nc - 1L, 1L)), nn) else rep_len(space, length(at))
  at <- at + cumsum(c(0, sp[-length(sp)]))
  gr <- sapply(split(at, rep(seq.int(nn), each = nc)), mean)
  
  if (!plot)
    return(list(at = at, group = gr))
  
  l <- at - width / 2
  r <- at + width / 2
  
  col <- if (is.null(col))
    grey.colors(nr) else rep_len(col, nr)
  log <- ''
  
  if (horiz) {
    xlim <- c(0, extendrange(ht)[2L])
    ylim <- range(l, r)
  } else {
    xlim <- range(l, r)
    ylim <- c(0, extendrange(ht)[2L])
  }
  
  xyrect <- function(x1, y1, x2, y2, horizontal = TRUE, ...) {
    if (!horizontal)
      rect(x1, y1, x2, y2, ...) else rect(y1, x1, y2, x2, ...)
  }
  
  op <- if (horiz) 
    par(xaxs = 'i') else par(yaxs = 'i')
  on.exit(par(op))
  
  if (!add) {
    plot.new()
    plot.window(xlim, ylim, log = log)
  }
  
  panel.first
  
  ht <- matrix(ht, nr)
  an <- rep(rep_len(angle, nr), nc * nn)
  dn <- rep(rep(density, nr), nc * nn)
  
  for (ii in seq_along(at))
    xyrect(
      l[ii], 0, r[ii], rev(ht[, ii]), col = rev(col),
      horizontal = horiz, border = border, xpd = xpd,
      ## feature -- separate angle/density for each rect?
      angle = an[ii], density = dn[ii]
    )
  
  if (axes) {
    axis(if (horiz) 2L else 1L, at, lwd = 0,
         unlist(names.arg) %||% rep(colnames(array), nn) %||% FALSE)
    axis(if (horiz) 1L else 2L)
    mtext(names(names.arg) %||% dimnames(array)[[3L]],
          if (horiz) 2L else 1L, at = gr, line = 3)
    box(bty = 'l')
  }
  
  panel.last
  
  invisible(list(at = at, group = gr))
}
