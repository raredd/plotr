##
# ggclip, ggwidths
##

#' ggplot clipping
#' 
#' Toggles ggplot panel clipping.
#' 
#' @usage
#' ggclip(p, action = 'off')
#' 
#' @param p a ggplot object
#' @param action turn clipping \code{'off'} (default) or \code{'on'}
#' 
#' @examples
#' library(ggplot2)
#' library(grid)
#' 
#' (q <- qplot(1:10, 1:10,size = I(10)) + scale_y_continuous(expand = c(0, 0)))
#' ggclip(q)
#' 
#' dat <- data.frame(time = 0:5, y = 0:5)
#' (p <- ggplot(dat, aes(x = time, y = y)) +
#'     geom_area(alpha = .1) + theme_bw() +
#'     scale_y_continuous(expand = c(0, 0)) +
#'     scale_x_continuous(expand = c(0, 0)) +
#'     theme(panel.grid.major = element_blank(), 
#'           panel.grid.minor = element_blank(),
#'           axis.text.x=element_blank(),
#'           axis.ticks.x=element_blank()) +
#'      geom_segment(aes(x = 0, xend = 5 , y = 0, yend = 0), size = 1.5,
#'                   arrow = arrow(length = unit(0.6, 'cm'))))
#' 
#' ggclip(p)
#' 
#' 
#' @export

ggclip <- function(p, action = 'off') {
  if (action %ni% c('off','on'))
    stop('action should be \'on\' or \'off\'')
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- action
  grid.draw(gt)
}

#' Scale ggplot widths
#' 
#' Align two or more ggplots by scaling widths to equal size.
#' 
#' @param ... two or more ggplots
#' @param moreArgs a list of additional arguments passed to
#' \code{\link[gridExtra]{arrangeGrob}}
#' 
#' @examples
#' xx <- 0:100
#' dd <- data.frame(x = xx, y1 = sin(xx * pi / 10), y2 = xx ** 2, y3 = xx ** 3)
#' 
#' library('ggplot2')
#' p1 <- ggplot(dd, aes(x = x)) +
#'   geom_line(aes(y = y1))
#' p2 <- ggplot(dd, aes(x = x)) +
#'   geom_bar(aes(y = y2), stat = 'identity')
#' p3 <- ggplot(dd, aes(x = x)) +
#'   geom_line(aes(y = x)) +
#'   coord_cartesian(xlim = c(0,100))
#' 
#' library('gridExtra')
#' grid.arrange(p1, p2)
#' ggwidths(p1, p2, moreArgs = list(heights = c(1, 2)))
#' 
#' grid.arrange(p1, p2, p3)
#' ggwidths(p1, p2, p3)
#' 
#' @export

ggwidths <- function(..., moreArgs) {
  if (missing(moreArgs))
    moreArgs <- NULL else if (!is.list(moreArgs))
      stop('\'moreArgs\' should be a list')
  l <- list(...)
  l <- lapply(l, function(x) ggplot_gtable(ggplot_build(x)))
  widths <- do.call('unit.pmax', lapply(l, function(x) x$widths[2:3]))
  l <- lapply(l, function(x) {x$widths[2:3] <- widths; x})
  do.call('grid.arrange', c(l, moreArgs))
}
