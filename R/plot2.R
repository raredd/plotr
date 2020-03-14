### some less useful plots
# waffle, histr, shist, propfall, bibar, dose_esc, boxline, toxplot, tableplot,
# barplot2
###


#' waffle
#' 
#' A waffle chart.
#' 
#' If \code{mat} is given, all other arguments except \dots are ignored, and
#' \code{mat} is used to specify dimensions, layout, and colors for the plot;
#' see examples.
#' 
#' @param x an integer vector with counts for each group
#' @param rows number of rows
#' @param horiz logical; orientation of the chart and pattern of coloring
#' boxes; default is \code{TRUE} (horiztonal)
#' @param cols colors for each group, should be of length \code{length(x)}
#' @param mat an optional matrix giving the layout; see details
#' @param pct the percent the square spanned along the x- and y-axes; can be
#' length one (recycled) or two
#' @param ... additional graphical parameters passed to \code{par}
#' 
#' @examples
#' waffle(c(3, 10), 2)
#' waffle(c(3, 10), 2, pct = 1)
#' waffle(c(3, 10), 2, pct = 0.5)
#' waffle(c(3, 10), 2, pct = c(0.95, 0.5))
#' 
#' waffle(c(15, 700), rows = 40, horiz = FALSE, cols = c('salmon2', 'grey90'))
#' 
#' cols <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
#' waffle(c(80, 30, 20, 10), rows = 8, cols = cols, mar = c(0,0,0,7))
#' legend('right', legend = LETTERS[1:4], pch = 15, col = cols, pt.cex = 2,
#'        bty = 'n')
#' 
#' ## using mat
#' mat <- rep(c(cols, NA), times = c(80, 30, 20, 10, 4))
#' waffle(mat = matrix(mat, 8))
#' 
#' 
#' \dontrun{
#' tf <- tempfile(fileext = '.pdf')
#' pdf(tf, width = 7, height = 3)
#' savings <- c('Mortgage ($84,911)' = 84911,
#'              'Auto and\ntuition loans ($14,414)'=14414,
#'              'Home equity loans ($10,062)' = 10062,
#'              'Credit cards\n($8,565)' = 8565)
#' w <- waffle(savings / 392, rows = 7, cols = c("#c7d4b6", "#a3aabd", "#a0d0de", "#97b5cf"),
#'             bg = 'cornsilk', mar = c(0,0,0,3))
#' 
#' xx <- c(-.05, .73, .85, .93)
#' yy <- c(-.05, -.05, -.35, -.05)
#' segments(x0 = xx, y0 = .05, y1 = yy, lty = 'dotted',
#'          lwd = 1, xpd = NA, col = 'grey50')
#' text(xx, yy + .05, labels = names(savings), xpd = NA, cex = .6, col = 'grey50', pos = 1)
#' 
#' p <- par('usr')
#' mtext('Average household savings each year', at = xx[1], font = 2,
#'       col = 'grey50', adj = 0, line = 1)
#' mtext('Source: Federal Reserve', side = 1, font = 3, at = xx[1],
#'       col = 'grey50', adj = 0, cex = .6, line = 2)
#' legend(.87, 1.3, legend = '$392', xpd = NA, bty = 'n', bg = 'cornsilk',
#'        col = 'orange', text.col = 'grey50', pch = 15, cex = .8, pt.cex = 1.5)
#' dev.off()
#' 
#' system(paste(getOption('pdfviewer'), tf))
#' unlink(tf)
#' }
#' 
#' @export

waffle <- function(x, rows, horiz = TRUE, cols = seq_along(x), mat,
                   pct = 0.85, ...) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  m <- if (!missing(mat)) {
    # mat[rev(seq.int(nrow(mat))), ]
    mat
  } else {
    xx <- rep(cols, times = x)
    lx <- length(xx)
    m <- matrix(nrow = rows, ncol = (lx %/% rows) + (lx %% rows != 0))
    m[seq.int(length(xx))] <- xx
    
    if (!horiz) {
      m <- matrix(c(m), nrow = rows, byrow = TRUE)
      m <- m[rev(seq.int(nrow(m))), ]
    }
    
    m
  }
  
  par(...)
  plot.new()
  o <- cbind(c(row(m)), c(col(m))) + 1L
  pct <- rep_len(pct, 2L)
  plot.window(
    c(0L, max(o[, 2L]) + 1L), c(0L, max(o[, 1L]) + 1L),
    asp = 1, xaxs = 'i', yaxs = 'i'
  )
  rect(
    xleft = o[, 2L], ybottom = o[, 1L],
    xright = o[, 2L] + pct[1L], ytop = o[, 1L] + pct[2L],
    col = c(m), border = NA
  )
  
  invisible(list(m = m, o = o))
}

#' histr
#' 
#' A histogram with density curve and rug.
#' 
#' @param x a vector of values for which the histogram is desired
#' @param ... additional parameters passed to \code{\link{hist}} or graphical
#' parameters passed to \code{\link{par}}
#' @param lines.pars optional list of additional parameters passed to
#' \code{\link{line}}
#' @param rug.pars optional list of additional parameters passed to
#' \code{\link{rug}}
#' @param poly.pars optional list of additional parameters passed to
#' \code{\link{polygon}}
#' @param reset_par logical; if \code{TRUE}, resets par settings to state
#' before function call; setting \code{reset_par = FALSE} is useful for adding
#' to a plot
#' 
#' @return
#' A list of length two containing the return value of \code{\link{hist}}
#' and \code{\link{density}} for \code{x}.
#' 
#' @examples
#' set.seed(1)
#' histr(x <- rnorm(100), xlim = c(-3, 3), las = 1)
#' plot(density(x), xlim = c(-3, 3))
#' 
#' histr(x + 50, main = 'Age at diagnosis', xlab = 'Age',
#'       poly.pars = list(col = 'dodgerblue2', density = 30),
#'       lines.pars = list(lty = 'dashed', lwd = 3),
#'       rug.pars = list(side = 3, col = 'red'))
#' 
#' @export

histr <- function(x, ..., lines.pars, rug.pars, poly.pars, reset_par = TRUE) {
  op <- par(no.readonly = TRUE)
  if (reset_par)
    on.exit(par(op))
  
  m <- match.call()
  
  if (length(xx <- grep('pars', names(m))))
    m[xx] <- lapply(m[xx], function(ii) as.list(ii)[-1L])
  
  par(mar = par('mar')[c(1,2,3,2)])
  d <- density(x)
  h <- if (is.null(match.call()$xlab))
    hist(x, ..., xlab = sprintf('N = %s  Bandwidth = %.3f', d$n, d$bw)) else
      hist(x, ...)
  rs <- max(h$counts) / max(d$y)
  
  do.call('lines', c(list(x = d$x, y = d$y * rs, type = 'l'), m$lines.pars))
  do.call('rug', c(list(x = x), m$rug.pars))
  do.call('polygon', 
          c(list(x = c(d$x, rev(d$x)),
                 y = c(d$y * rs, rep(par('usr')[3L], length(d$y))),
                 col = adjustcolor(m$poly.pars$col %||% NA, alpha.f = 0.25),
                 border = NA),
            m$poly.pars[-which(names(m$poly.pars) == 'col')]))
  
  par(new = TRUE)
  plot(d, type = 'n', ann = FALSE, axes = FALSE)
  
  text(coords(side = 4L, line = par('mgp')[1L]),
       mean(par('usr')[3:4]),
       labels = 'Density', srt = -90, xpd = NA)
  axis(4L)
  
  invisible(list(h = h, d = d))
}

#' shist
#' 
#' A stacked histogram with density curve.
#' 
#' @param x a list of numeric vectors for which the histogram is desired
#' @param ... additional parameters passed to \code{\link{hist}} or graphical
#' parameters passed to \code{\link{par}}
#' @param col a vector of colors for each histogram
#' @param loess logical; if \code{TRUE}, a \code{\link{loess}} curve is
#' fit to each histogram
#' @param total logical; if \code{TRUE}, an aggregate histogram of \code{x}
#' is plotted first, and each element of \code{x} is plotted below
#' @param xlim,ylim the x- and y-axis limits (if given, applied to all plots)
#' @param heights an optional vector of values, usually in \code{(0,1)}, for
#' the heights of each plot on the device; see \code{\link{layout}}
#' @param heights.main if \code{total = TRUE}, the height of the aggregate
#' histogram plot relative to the device, usually in \code{(0,1)}
#' @param reset_par logical; if \code{TRUE}, resets par settings to state
#' before function call; setting \code{reset_par = FALSE} is useful for adding
#' to a plot
#' 
#' @return
#' A list of length \code{length(x)} containing the return value of
#' \code{\link{hist}} for each element of \code{x}.
#' 
#' @examples
#' set.seed(1)
#' x <- lapply(sample(1:10, 4), rpois, n = 500)
#' 
#' shist(x)
#' shist(x, heights.main = 0.75)
#' shist(x, heights = c(5,1,1,1,3))
#' shist(x, col = terrain.colors(length(x)), total = FALSE,
#'       ylim = c(0, 200), las = 1L, breaks = 10)
#' 
#' @export

shist <- function(x, ..., col = grey.colors(length(x) + 1L),
                  loess = TRUE, total = TRUE, xlim = NULL, ylim = NULL,
                  heights = NULL, heights.main = 0.5, reset_par = TRUE) {
  op <- par(no.readonly = TRUE)
  if (reset_par)
    on.exit(par(op))
  
  x <- as.list(x)
  if (is.null(names(x)))
    names(x) <- seq_along(x)
  
  x <- if (total)
    c(list(total = unname(unlist(x))), x)
  else {
    heights <- 1L
    x
  }
  
  h <- hist(unlist(x), plot = FALSE)
  col  <- rep_len(col, length(x))
  xlim <- xlim %||% range(h$breaks)
  # ylim <- ylim %||% range(h$counts)
  
  heights <- heights %||%
    c(heights.main, rep((1 - heights.main) / (length(x) - 1L), length(x) - 1L))
  heights <- rep_len(heights, length(x))
  
  layout(seq_along(x), heights = heights)
  par(mar = c(0, 0, 0, 0), oma = c(5, 4, 4, 2) + 0.1)
  suppressWarnings({
    par(...)
  })
  
  res <- vector('list', length(x))
  for (ii in seq_along(x)) {
    xii <- x[[ii]]
    
    suppressWarnings({
      res[[ii]] <- h <- hist(
        xii, ..., xpd = NA, ann = FALSE, axes = FALSE,
        xlim = xlim, ylim = ylim, col = col[ii]
      )
    })
    
    axis(2L)
    
    if (ii == length(x))
      axis(1L)
    
    if (loess) {
      nx <- seq(min(xii), max(xii), length.out = 1000L)
      lo <- data.frame(counts = h$counts, mids = h$mids)
      
      lo <- loess(counts ~ mids, lo)
      pr <- predict(lo, data.frame(mids = nx), se = TRUE)
      
      lines(pr$fit ~ nx, col = col[ii], lwd = 2, xpd = NA)
    }
  }
  
  invisible(setNames(res, names(x)))
}

#' propfall
#' 
#' A barplot of proportions.
#' 
#' @param data a data frame (or object to be coerced)
#' @param group a vector with length \code{nrow(data)} defining the group to
#' which each row of \code{data} belongs
#' @param order a vector of column names of \code{data} defining how numeric
#' values should be sorted
#' @param group.order an optional vector of unique values of \code{group}
#' giving the desired order for groups
#' @param col a vector of colors for each of \code{order}, recycled as needed
#' @param ... additional parameters passed to \code{\link{barplot}} or
#' graphical parameters passed to \code{\link{par}}
#' 
#' @return
#' A list of length two with the result of the call to \code{\link{barplot}}
#' (\code{bp}) giving the x-axis positions for each bar and the \code{o}rder
#' that each observation has been sorted in the barplot.
#' 
#' @seealso
#' \code{\link[rawr]{waterfall}}
#' 
#' @examples
#' dat <- within(mtcars, {
#'   disp <- disp / 10
#'   wt <- wt * 10
#' })
#' 
#' vars <- c('mpg', 'disp', 'wt')
#' dat[, vars] <- t(apply(dat[, vars], 1L, function(x) x / sum(x)))
#' 
#' dat <- dat[, vars]
#' dat$group <- colnames(dat)[max.col(dat)]
#' 
#' cols <- c('grey', 'lightpink', 'indianred1')
#' propfall(dat[, 1:3])
#' propfall(dat[, 1:3], group = dat$group, col = cols)
#' 
#' ## use the return value to add labels or identify observations
#' bp <- propfall(
#'   dat[, 1:3], col = cols, group = dat$group,
#'   order = c('disp', 'wt', 'mpg'),
#'   group.order = c('mpg', 'wt', 'disp')
#' )
#' text(bp$bp, 0, labels = rownames(dat)[bp$o], srt = 90, col = 0, adj = 0)
#' 
#' @export

propfall <- function(data, group, order, group.order = unique(group),
                     col = NULL, ...) {
  if (missing(order))
    order <- colnames(data)
  col <- if (is.null(col))
    gray.colors(length(order))
  else rep_len(col, length(order))
  
  group <- if (missing(group))
    rep_len(1L, nrow(data))
  else if (is.character(group) & length(group) == 1L)
    data[, group]
  else rep_len(group, nrow(data))
  
  data <- as.data.frame(data)
  data <- data[, order, drop = FALSE]
  
  f <- function(x) do.call('order', as.list(x))
  
  sp <- split(data, group)[group.order]
  
  sp <- lapply(seq_along(sp), function(x) {
    rbind(sp[[x]][f(sp[[x]][, order]), ], NA)
  })
  
  sp <- do.call('rbind', sp)
  # o <- rownames(sp[-cumsum(lengths(sp)), ])
  o <- match(rownames(sp), rownames(data))
  rownames(sp) <- NULL
  
  mm <- t(as.matrix(sp[, seq_along(col)]))
  
  tt <- table(factor(group, group.order))
  times <- rawr::interleave(
    which = 'rbind', matrix(tt), matrix(1L, length(tt))
  )
  
  mm[is.na(mm)] <- 0
  
  op <- par(mar = c(3, 4.5, 2, 1))
  on.exit(par(op))
  
  bp <- barplot(mm, col = col, space = 0, ..., border = NA)
  
  text(
    c(0, head(cumsum(tt), -1L)) + seq_along(group.order), par('usr')[4L],
    group.order, xpd = NA, adj = 0, pos = 3L
  )
  legend(
    0, par('usr')[3L], xpd = NA, horiz = TRUE, bty = 'n',
    lty = 1L, lwd = 7, legend = order, col = col
  )
  
  invisible(list(bp = bp, o = o))
}

#' Bi-barplot
#' 
#' Plot a bi-directional bar plot with optional side bars.
#' 
#' @param x a table- or matrix-like object
#' @param left,right a vector of integers giving the column indices of
#' \code{x} to draw on the left and right sides \emph{from} the origin
#' \emph{to} either side
#' @param sleft,sright as above but bars drawn \emph{from} the left or
#' right side \emph{to} the origin
#' @param col a vector of colors for each column
#' @param xlim,ylim x- and y-axis limits
#' @param axes logical; if \code{TRUE}, the figure is framed and the x- and
#' y-axes are drawn
#' 
#' @seealso
#' \code{\link{prettybars2}}
#' 
#' @examples
#' set.seed(1)
#' x <- datasets::ability.cov$cov
#' x <- x[sample(seq.int(nrow(x)), 20, TRUE), ]
#' 
#' bibar(x, left = 1:3, right = 4:6, xlim = c(-250, 250))
#' 
#' palette(c('grey90', 'cornflowerblue', 'blue', 'tomato', 'tomato3'))
#' bibar(x, left = 2:3, right = 4:5, sleft = 1, sright = 6)
#' legend('topleft', inset = c(0, -0.2), xpd = NA, fill = 3:2,
#'        legend = colnames(x)[3:2], horiz = TRUE, bty = 'n')
#' legend('topright', inset = c(0, -0.2), xpd = NA, fill = 4:5,
#'        legend = colnames(x)[4:5], horiz = TRUE, bty = 'n')
#' palette('default')
#' 
#' @export

bibar <- function(x, left = NULL, right = NULL, sleft = NULL, sright = NULL,
                  col = NULL, xlim = NULL, ylim = NULL, axes = TRUE) {
  bp <- function(x, ...) {
    barplot(t(x), ..., horiz = TRUE, add = TRUE,
            axes = FALSE, axisnames = FALSE)
  }
  
  if (is.null(xlim))
    xlim <- max(rowSums(x)) * c(-1, 1)
  col <- rep_len(col %||% seq.int(ncol(x)), ncol(x))
  
  yat <- barplot(t(x), horiz = TRUE, xlim = xlim, ylim = ylim %||% NULL,
                 col = NA, axes = FALSE, axisnames = FALSE, border = NA)
  
  if (length(sleft)) {
    nx <- x[, sleft, drop = FALSE]
    nx <- -cbind(abs(xlim[1L]) - rowSums(nx), nx)
    bd <- c(0, rep_len('black', length(sleft)))
    bp(nx, col = c(0, col[sleft]), border = bd)
  }
  
  if (length(sright)) {
    nx <- x[, sright, drop = FALSE]
    nx <- cbind(abs(xlim[2L]) - rowSums(nx), nx)
    bd <- c(0, rep_len('black', length(sright)))
    bp(nx, col = c(0, col[sright]), border = bd)
  }
  
  if (length(left)) {
    lx <- x[, left, drop = FALSE]
    bp(-lx, col = col[left])
  }
  
  if (length(right)) {
    rx <- x[, right, drop = FALSE]
    bp(rx, col = col[right])
  }
  
  if (axes) {
    xat <- pretty(par('usr'), n = 6L)
    axis(1L, xat, abs(xat))
    axis(2L, yat, rownames(x), las = 1L)
    box(bty = par('bty'))
  }
  
  invisible(yat)
}

#' Plot dose escalation
#' 
#' Plot results of a dose-escalation study.
#' 
#' @param dose,col.dose vector of dose levels for each observation; colors
#' should correspond to DLT or similar
#' @param nstep number entered at each step, recycled as necessary; for 3+3
#' or 4+2 studies for example, the value should be \code{3} or \code{c(4,2)},
#' respectively
#' @param dose.exp,col.exp optional vectors for expansion cohort
#' @param xlab,ylab x- and y-axis label for each dose level
#' @param xlim,ylim x- and y-axis limits
#' @param squish numeric value describing a squishing factor; larger values
#' result in plot being compressed
#' 
#' @examples
#' ## 3 + 3
#' dose_esc(d33 <- c(1,1,1,2,2,2,3,3,3,3,3,3,4,4,4),
#'          c33 <- c(3,3,3,3,3,3,3,3,2,3,3,3,3,2,2))
#' legend(0, 4, col = 2:3, pch = 16, pt.cex = 4, xpd = NA, bty = 'n',
#'        legend = paste(c('','Non-'), 'DLT'), y.intersp = 1.5)
#' 
#' ## 3 + 3 with expansion
#' dose_esc(d33, c33, dose.exp = rep(4, 10), col.exp = rep(3, 10))
#' dose_esc(d33, c33, dose.exp = rep(3, 4), col.exp = rep(3, 4))
#' 
#' ## 4 + 4
#' dose_esc(c(1,1,1,1, 2,2,2,2, 2,2,2,2, 3,3,3), nstep = 4,
#'          col.dose = c(3,3,3,3, 3,3,2,3, 3,3,3,3, 3,2,2),
#'          ylab = parse(text = paste0(1:2, '~mg^2')))
#' 
#' ## 4 + 2
#' dose_esc(c(1,1,1,1,1,1, 2,2,2,2,2), nstep = c(4,2),
#'          col.dose = c(3,3,3,2,3,3, 2,3,3,3,2))
#'          
#' @export

dose_esc <- function(dose, col.dose, nstep = 3L, dose.exp, col.exp,
                     xlab = 'Time', ylab = 'Dose',
                     xlim = NULL, ylim = NULL, squish = length(dose) %/% 3L) {
  nlevel <- table(dose)
  dose.exp <- if (missing(dose.exp))
    NULL else c(0, dose.exp)
  col.exp  <- if (missing(col.exp)) {
    if (is.null(dose.exp))
      NULL else {
        warning('\'dose.exp\' given without \'exp.col\'', domain = NA)
        rep(NA, length(dose.exp) + 1L)
      }
  } else c(NA, col.exp)
  
  n <- sum(nlevel)
  N <- n + if (!is.null(dose.exp))
    length(dose.exp) else 0
  y <- c(dose, dose.exp)
  x <- seq.int(N) + c(0, cumsum(diff(y) * 0.1))
  col <- c(col.dose, col.exp)
  
  x <- x + cumsum(ave(y, y, FUN = seq_along) %in%
                    (cumsum(rep(nstep, length(y))) + 1)) * 0.1
  pls_idx <- which(!diff(y) > 0 & diff(x) > 1 | y[-length(y)] == 0L)
  pls_idx <- pls_idx[pls_idx <= n + 1]
  x <- rescaler(x, c(1, max(x) - squish)) - 1 / squish
  pls <- sapply(Map(`:`, pls_idx, pls_idx + 1L), function(ii)
    mean(x[ii]))
  arr_idx <- which(diff(y) > 0) + 1L
  arr <- rawr::roll_fun(x, 2L, mean)[arr_idx[arr_idx <= n]]
  
  plot.new()
  plot.window(
    xl <- xlim %||% c(0, max(x)),
    yl <- ylim %||% c(0, max(y) + 1)
  )
  p <- par('usr')
  arrows2(c(0, 0), c(0, 0), c(0, max(xl)), c(max(yl), 0), lwd = 3,
          size = 0.5, width = 0.5)
  text(p[2L], 0, pos = 1L, xlab, xpd = NA, font = 2L)
  text(0, p[4L], pos = 2L, ylab, xpd = NA, font = 2L)
  
  points(x, y, pch = 16L, col = col, cex = 3.5, xpd = NA)
  points(pls, y[pls_idx + 1L], pch = '+', cex = 1.5, xpd = NA)
  arrows2(arr, seq_along(arr) + 0.2, arr, seq_along(arr) + 0.8,
          size = 0.5, width = 0.5, lwd = 4, curve = 1.2,
          sadj = c(0, 0, 0, -.1))
  
  if (!is.null(dose.exp))
    if (diff(y[wh <- which(y == 0L) + c(-1, 1)]) < 0) {
      carrows(c(x[wh[1L]], y[wh[1L]]), c(x[wh[1L]] * 1.01, y[wh[2L]]),
              col = 2L, size = .5, arc = pi / 3 * c(-1, 1), lwd = 4,
              width = 0.7, dir = c(1, 0), pad = c(.1, .3))
    } else arrows2(mean(x[wh[1:2]]), max(dose.exp), mean(x[wh[1:2]]),
                   size = 0.7, width = 1, lwd = 4, col = 3L, curve = 1.3)
  
  invisible(list(x, y, col, arr, pls))
}

#' Boxline
#' 
#' An alternative to \code{\link{boxplot}} with lines connecting median and
#' quantiles of each box.
#' 
#' @param x a list of vectors
#' @param probs a vector of probabilities; see \code{\link{quantile}}
#' @param col.probs a vector of colors for each \code{probs}, recycled as needed
#' @param alpha optional vector of opacity for each \code{probs}; see
#' \code{\link{adjustcolor}}
#' @param col.med,lwd.med color and line width used for median line
#' @param pch optional plotting character used for the median point of each
#' @param err optional error bars around each median value; possible values
#' are \code{"none"} (default), \code{"sd"} (standard deviation), \code{"se"}
#' (standard error), \code{"ci"} (confidence interval), and \code{"quantile"}
#' (quantile)
#' @param err.alpha the alpha level used when \code{err} is \code{"ci"} or
#' \code{"quantile"}
#' @param col.err,lwd.err color and line width used for error bars
#' @param at a vector of x-coodinates for each element of \code{x}
#' @param add logical; if \code{TRUE}, adds to existing plot
#' @param ... additional arguments passed to \code{\link{boxplot}} or further
#' to \code{\link{par}}
#' 
#' @return
#' See \code{\link{boxplot}}
#' 
#' @examples
#' set.seed(1)
#' x <- lapply(0:10, function(x) rnorm(25, x / 2, sd = 0.5))
#' boxplot(x)
#' boxline(x, add = TRUE)
#' 
#' boxline(x, col.probs = c('red', 'orange', 'yellow'), alpha = 0.5,
#'         pch = 16L, col.med = 'red')
#' boxline(x, probs = NULL, err = 'sd', add = TRUE, col.err = 'cyan')
#' boxplot(x, add = TRUE, axes = FALSE, col = 'transparent')
#' 
#' @export

boxline <- function(x, probs = c(0.75, 0.90, 0.99),
                    col.probs = 2L, alpha = NULL,
                    col.med = 1L, lwd.med = 2, pch = NULL,
                    err = c('none', 'sd', 'se', 'ci', 'quantile'),
                    err.alpha = 0.05, col.err = col.med, lwd.err = lwd.med,
                    at = seq_along(x), add = FALSE, ...) {
  probs <- unique(sort(c(0.5, probs), decreasing = TRUE))
  lprob <- length(probs)
  
  ## complete x
  cx <- lapply(x, sort)
  hi <- sapply(cx, function(xx) quantile(xx, probs))
  lo <- sapply(cx, function(xx) quantile(xx, 1 - probs))
  
  alp <- if (is.null(alpha))
    seq(0.25, 0.75, length.out = lprob)
  else rep_len(alpha, lprob)
  
  ## vector of color/alpha for each quantile polygon
  col.probs <- rep_len(col.probs, lprob)
  col.probs <- Vectorize(adjustcolor, c('col', 'alpha.f'))(
    col = col.probs, alpha.f = alp)
  
  med <- sapply(cx, function(xx) quantile(xx, 0.5))
  col.med <- rep_len(col.med, length(x))
  
  ## optional error bars around each element of x
  err <- match.arg(err)
  err.alpha <- err.alpha[1L]
  if (!err %in% 'none') {
    pm <- switch(err,
      sd = sd,
      se = function(xx) sd(xx) / sqrt(length(xx)),
      ci = function(xx) {
        qt((1 - err.alpha) / 2 + 0.5, length(xx) - 1) *
          sd(xx) / sqrt(length(xx))
      },
      quantile = TRUE
    )
    
    if (isTRUE(pm)) {
      hi <- sapply(cx, function(xx) quantile(xx, 1 - err.alpha))
      lo <- sapply(cx, function(xx) quantile(xx, err.alpha))
      ylim <- extendrange(c(unlist(hi), unlist(lo)))
    } else {
      z <- sapply(cx, pm)
      ylim <- extendrange(c(med + z, med - z))
    }
  }
  
  bp <- if (!err %in% 'none')
    boxplot(x, border = NA, at = at, plot = !add, ylim = ylim, ...)
  else boxplot(x, border = NA, at = at, plot = !add, ...)
  
  for (ii in seq_along(probs[-1L])) {
    polygon(c(at, rev(at)), c(lo[ii, ], rev(lo[ii + 1L, ])),
            col = col.probs[ii], border = NA)
    polygon(c(at, rev(at)), c(hi[ii, ], rev(hi[ii + 1L, ])),
            col = col.probs[ii], border = NA)
  }
  
  if (!err %in% 'none') {
    if (isTRUE(pm))
    arrows(at, hi, at, lo, col = col.err,
         code = 3L, angle = 90, length = 0.1, lwd = lwd.err)
    else
      arrows(at, med + z, at, med - z, col = col.err,
             code = 3L, angle = 90, length = 0.1, lwd = lwd.err)
  }
  
  ## median points for each element of x
  lines(at, med, col = col.med, lwd = lwd.med, pch = pch,
        type = if (is.null(pch)) 'l' else 'o')
  
  invisible(bp)
}

#' Inset barplot
#' 
#' Draw a \code{\link{barplot}} with inset bars.
#' 
#' @inheritParams graphics::barplot.default
#' @param height a matrix of data describing the bars which make up the plot,
#' usually a main/total bar and one or more smaller or subsets of the total;
#' bars are grouped by columns; the first row will be the main bar and other
#' rows will be minor bars
#' @param r distance (in user coordinates) each inset bar is from the edge
#' of the main bar
#' @param alpha numeric value in \code{[0,1]} for the alpha transparency;
#' either a vector equal to \code{nrow(height)} (recycled as needed) or a
#' matrix having the same dimensions of \code{height}
#' @param col colors for each bar; if a vector, each \code{col} is applied to
#' a group of bars with \code{alpha} transparency added to each minor bar;
#' alternatively, a matrix of colors having the same dimensions of
#' \code{height} to set each bar color
#' @param group.col logical; if \code{TRUE} (default), columns of \code{height}
#' are treated as groups and mapped to one color; if \code{FALSE}, each row
#' is mapped to \code{col} and recycled for each column of \code{height}
#' 
#' @return
#' A numeric vector with the midpoint of each main bar (i.e., identical to a
#' call to \code{barplot(..., beside = FALSE)}).
#' 
#' Additionally, a list attribute (\code{attr(., "coords")}) where each list
#' element corresponds to a group of bars. The matrices contain coordinates
#' of each rectangle plus midpoints.
#' 
#' @seealso
#' \code{\link{tracebar}}
#' 
#' @examples
#' set.seed(1)
#' tbl <- sapply(1:3, function(x) sort(rpois(3, 10), decreasing = TRUE))
#' barplot(tbl, col = 1:3)
#' inbar(tbl, col = 1:3)
#' inbar(tbl, col = matrix(rainbow(length(tbl)), nrow(tbl)))
#' 
#' ## compare group.col = TRUE (default)
#' inbar(tbl, col = 1:3)
#' ## compare group.col = FALSE
#' inbar(tbl, col = 1:3, group.col = FALSE)
#' 
#' inbar(
#'   tbl, col = 1:3, r = c(0.5, 0.8), alpha = c(1, 0.5, 0.25),
#'   horiz = TRUE, names.arg = 1:3,
#'   legend.text = 1:3, xlim = c(0, 20), border = NA
#' )
#' 
#' 
#' ## inbar returns rect coordinates for future use
#' co <- inbar(tbl)
#' co <- do.call('rbind', attr(co, 'coords'))
#' points(co[, 'midpoint'], co[, 'ytop'], xpd = NA)
#' 
#' @export

inbar <- function(height, width = 1, r = NULL,
                  alpha = if (group.col) r else 1, space = NULL,
                  names.arg = NULL, legend.text = NULL, horiz = FALSE,
                  density = NULL, angle = 45, col = NULL, group.col = TRUE,
                  border = par('fg'),
                  main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                  xlim = NULL, ylim = NULL, xpd = TRUE, log = '',
                  axes = TRUE, axisnames = TRUE, cex.axis = par('cex.axis'),
                  cex.names = par('cex.axis'), plot = TRUE, axis.lty = 0,
                  offset = 0, add = FALSE, ann = !add && par('ann'),
                  args.legend = NULL, ...) {
  stopifnot(
    is.matrix(height),
    is.character(log)
  )
  beside <- FALSE
  force(alpha)
  
  if (is.null(space))
    space <- if (is.matrix(height) && beside)
      c(0, 1) else 0.2
  space <- space * mean(width)
  
  if (plot && axisnames && is.null(names.arg))
    names.arg <- if (is.matrix(height))
      colnames(height) else names(height)
  
  if (is.logical(legend.text)) 
    legend.text <- if (legend.text && is.matrix(height))
      rownames(height)
  
  logx <- logy <- FALSE
  if (log != '') {
    logx <- length(grep('x', log)) > 0L
    logy <- length(grep('y', log)) > 0L
  }
  if ((logx || logy) && !is.null(density))
    stop('Cannot use shading lines in bars when log scale is used')
  
  NR <- nrow(height)
  NC <- ncol(height)
  
  if (is.matrix(r)) {
    stopifnot(
      ncol(r) == NC,
      nrow(r) == NR
    )
  } else {
    r <- c(0, if (is.null(r))
      (seq.int(NR)[-1L]) / 10 else rep_len(r, NR - 1L))
    r <- matrix(r, NR, NC)
  }
  
  alpha <- if (is.null(alpha)) {
    1 - r
  } else if (is.matrix(alpha)) {
    stopifnot(
      ncol(alpha) == NC,
      nrow(alpha) == NR
    )
    alpha
  } else {
    matrix(rep_len(alpha, NR), NR, NC)
  }
  
  if (is.matrix(col)) {
    stopifnot(
      ncol(col) == NC,
      nrow(col) == NR
    )
    ocol <- col[1L, ]
  } else {
    col <- ocol <- if (is.null(col))
      gray.colors(NC) else rep_len(col, NC)
    col <- if (group.col)
      sapply(seq_along(col), function(ii)
        colorRampPalette(c('white', col[ii]))(100)[(alpha[, ii]) * 100])
    else sapply(seq.int(NC), function(ii)
      Vectorize(adjustcolor)(col, alpha[, ii]))
    if (!is.matrix(col))
      col <- matrix(col, NR, NC)
  }
  
  width <- rep_len(width, NC)
  
  offset <- rep_len(as.vector(offset), length(width))
  delta <- width / 2
  w.r <- cumsum(space + width)
  w.m <- w.r - delta
  w.l <- w.m - delta
  
  log.dat <- (logx && horiz) || (logy && !horiz)
  rAdj <- offset + (if (log.dat) 0.9 * height else -0.01 * height)
  
  rectbase <- if (log.dat) {
    if (min(height + offset, na.rm = TRUE) <= 0) 
      stop('log scale error: at least one \'height + offset\' value <= 0')
    if (logx && !is.null(xlim) && min(xlim) <= 0) 
      stop('log scale error: \'xlim\' <= 0')
    if (logy && !is.null(ylim) && min(ylim) <= 0) 
      stop('log scale error: \'ylim\' <= 0')
    if (logy && !horiz && !is.null(ylim)) 
      ylim[1L]
    else if (logx && horiz && !is.null(xlim)) 
      xlim[1L]
    else 0.9 * min(height, na.rm = TRUE)
  } else 0
  
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
  
  xyrect <- function(x1, y1, x2, y2, horizontal = TRUE, plot = TRUE, ...) {
    res <- if (horizontal) {
      if (plot)
        rect(x1, y1, x2, y2, ...)
      c(x1, y1, x2, y2, (y2 - y1) / 2 + y1)
    } else {
      if (plot)
        rect(y1, x1, y2, x2, ...)
      c(y1, x1, y2, x2, (y2 - y1) / 2 + y1)
    }
    
    setNames(res, c('xleft', 'ybottom', 'xright', 'ytop', 'midpoint'))
  }
  
  if (plot) {
    dev.hold()
    opar <- if (horiz)
      par(xaxs = 'i', xpd = xpd) else par(yaxs = 'i', xpd = xpd)
    on.exit({
      dev.flush()
      par(opar)
    })
    
    if (!add) {
      plot.new()
      plot.window(xlim, ylim, log = log, ...)
    }
    
    co <- lapply(seq.int(NC), function(cc)
      t(sapply(seq.int(NR), function(rr)
        xyrect(
          rectbase + offset[cc], w.l[cc], height[rr, cc] + offset[cc],
          w.r[cc] - r[rr, cc], horizontal = horiz, angle = angle,
          density = density, col = col[rr, cc], border = border
        )
      ))
    )
    
    if (axisnames && !is.null(names.arg)) {
      at.l <- if (length(names.arg) != length(w.m)) {
        if (length(names.arg) == NC)
          colMeans(w.m)
        else stop('incorrect number of names')
      } else w.m
      
      axis(if (horiz) 2 else 1, at = at.l, labels = names.arg,
           lty = axis.lty, cex.axis = cex.names, ...)
    }
    
    if (!is.null(legend.text)) {
      legend.col <- rep_len(ocol, length(legend.text))
      if ((horiz & beside) || (!horiz & !beside)) {
        legend.text <- rev(legend.text)
        legend.col <- rev(legend.col)
        density <- rev(density)
        angle <- rev(angle)
      }
      xy <- par('usr')
      if (is.null(args.legend)) {
        legend(
          xy[2L] - xinch(0.1), xy[4L] - yinch(0.1), legend.text, angle = angle,
          density = density, fill = legend.col, xjust = 1, yjust = 1
        )
      } else {
        args.legend1 <- list(
          x = xy[2L] - xinch(0.1), y = xy[4L] - yinch(0.1),
          legend = legend.text, angle = angle, density = density,
          fill = legend.col, xjust = 1, yjust = 1
        )
        args.legend1[names(args.legend)] <- args.legend
        
        do.call('legend', args.legend1)
      }
    }
    if (ann)
      title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
    if (axes)
      axis(if (horiz) 1 else 2, cex.axis = cex.axis, ...)
    
    invisible(structure(w.m, coords = co))
  } else {
    co <- lapply(seq.int(NC), function(cc)
      t(sapply(seq.int(NR), function(rr)
        xyrect(
          rectbase + offset[cc], w.l[cc], height[rr, cc] + offset[cc],
          w.r[cc] - r[rr, cc], horizontal = horiz, plot = FALSE
        )
      ))
    )
    structure(w.m, coords = co)
  }
}

#' Tracing barplot
#' 
#' Draw a \code{\link{barplot}} with polygons tracing each row.
#' 
#' @param height a matrix of data describing the bars which make up the plot,
#' usually a main/total bar and one or more smaller or subsets of the total;
#' bars are grouped by columns; the first row will be the main bar and other
#' rows will be minor bars
#' @param col colors for each row of \code{height}
#' @param alpha numeric value in \code{[0,1]} for the alpha transparency of
#' polygons
#' @param pad numeric vector of length one or two controlling the horizontal
#' and vertical padding, respectively
#' @param ... additional arguments passed to \code{\link{barplot}}
#' 
#' @inherit inbar return
#' 
#' @seealso
#' \code{\link{inbar}}
#' 
#' @examples
#' set.seed(1)
#' tbl <- sapply(1:3, function(x) sort(rpois(3, 10), decreasing = TRUE))
#' tracebar(tbl)
#' 
#' tracebar(replace(tbl, 5, 0), col = 1:3, space = 0.5)
#' 
#' @export

tracebar <- function(height, col = NULL, pad = 0.05, alpha = 0.75, ...) {
  ht <- apply(height, 2L, function(x) cumsum(x))
  cd <- inbar(ht, r = 0, plot = FALSE, ...)
  co <- attr(cd, 'coords')
  ht <- rbind(0, ht)
  
  if (is.null(col))
    col <- gray.colors(nrow(ht))
  
  barplot(height, col = col, beside = FALSE, horiz = FALSE, plot = TRUE, ...)
  
  pad <- rep_len(pad, 2L)
  
  for (ii in seq_along(co[-1L])) {
    ## current and next group
    x <- cbind(co[[ii]], co[[ii + 1L]])
    
    for (jj in seq.int(nrow(height))) {
      y  <- x[jj, ]
      
      ## get padding for x and y
      px <- pad[1L] * abs(y[[3L]] - y[[6L]])
      py <- pad[2L] * max(height)
      
      xx <- c(y[3L] + px, y[6L] - px, y[6L] - px, y[3L] + px, y[3L] + px)
      yy <- c(ht[jj, ii] + py, ht[jj, ii + 1L] + py, ht[jj + 1L, ii + 1L] - py,
              ht[jj + 1L, ii] - py, ht[jj + 1L, ii] - py)
      
      ## handle overlap
      if (diff(yy[2:3]) < 0)
        yy[2:3] <- mean(yy[2:3])
      if (diff(yy[c(1L, 5L)]) < 0)
        yy[c(1L, 4:5)] <- mean(yy[c(1L, 5L)])
      
      polygon(xx, yy, col = adjustcolor(col[jj], alpha.f = alpha), border = NA)
    }
  }
  
  invisible(cd)
}

#' toxplot
#' 
#' Draw a toxicity summary plot.
#' 
#' @param ftable a two- or three-dimensional \code{\link{ftable}}
#' @param total total number of patients (for percentages)
#' @param digits number of places past the decimal to keep for percentages
#' @param headers a vector of labels for text column headers
#' @param xlim the x-axis limits for the bar plot
#' @param order optional ordering for the rows, one of \code{"none"} (default),
#' \code{"decreasing"}, or \code{"increasing"}; note that 3-d tables should
#' not be re-ordered
#' @param label.bars optional text on each bar, one of \code{"n"} (default),
#' \code{"percent"}, or \code{"none"}
#' @param col.bars a vector of colors for each column of \code{ftable}
#' @param digits.bars number of places past the decimal to keep for
#' \code{label.bars} text
#' @param widths the relative width of each column
#' @param space additional space between final text column and bar plot
#' @param xaxis.at x-axis tick positions
#' @param col.bg,alpha.bg background color for text columns with alternating
#' \code{alpha.bg} transparency
#' @param legend logical; if \code{TRUE}, a legend for the \code{col.bars} is
#' shown
#' @param args.legend an optional \emph{named} list of arguments passed to
#' \code{\link{legend}}
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @examples
#' ## example from rawr::tabler_by
#' set.seed(1)
#' 
#' f <- function(x, ...) sample(x, 100, replace = TRUE, ...)
#' tox <- data.frame(
#'   id = rep(1:10, 10), phase = 1:2,
#'   code = f(rawr::ctcae_v4$tox_code[1:100]),
#'   grade = f(1:3, prob = c(.6, .3, .1)),
#'   stringsAsFactors = FALSE
#' )
#' tox <- cbind(tox, rawr::match_ctc(tox$code)[, c('tox_cat', 'tox_desc')])
#' 
#' t1 <- ftable(
#'   Category = tox$tox_cat,
#'   Description = tox$tox_desc,
#'   Grade = tox$grade
#' )
#' t2 <- ftable(
#'   Description = tox$tox_desc,
#'   Grade = tox$grade
#' )
#' n <- 25
#' 
#' 
#' ## basic usage
#' toxplot(t1, n) ## three column
#' toxplot(t2, n) ## two column
#' 
#' 
#' toxplot(
#'   t2, n, widths = c(1, 3), label.bars = 'percent',
#'   headers = c('Toxicity description', 'N (%)'),
#'   order = 'dec', xlim = c(0, 10), xaxis.at = c(0, 5, 10),
#'   args.legend = list(x = 'bottomright', horiz = FALSE, cex = 2)
#' )
#' 
#' @export

toxplot <- function(ftable, total, digits = 0L, headers = NULL, xlim = NULL,
                    order = c('none', 'decreasing', 'increasing'),
                    label.bars = c('n', 'percent', 'none'),
                    col.bars = NULL, digits.bar = digits,
                    widths = c(1, 1, 2), space = 1, xaxis.at = NULL,
                    col.bg = 'grey', alpha.bg = c(0.25, 0.5),
                    legend = TRUE, args.legend = list(), ...) {
  if (!inherits(ftable, 'ftable'))
    ftable <- ftable(ftable)
  
  rv <- attr(ftable, 'row.vars')
  ng <- length(rv) + 1L
  extra <- ng == 3L
  
  order <- match.arg(order)
  if (extra)
    order <- 'none'
  
  headers <- if (is.null(headers))
    c(names(rv), sprintf('N=%s (%%)', total)) else rep_len(headers, ng)
  
  ## ftable processing
  ord <- if (order == 'none')
    seq.int(nrow(ftable))
  else order(rowSums(ftable), decreasing = order == 'decreasing')
  
  ftbl <- rawr::writeftable(ftable)
  ftbl <- as.matrix(rawr::locf(as.data.frame(ftbl)))[ord, ]
  ftbl <- ftbl[rowSums(ftable) > 0, ]
  ftbl[, 1L] <- paste(' ', ftbl[, 1L])
  
  if (order == 'none')
    ftbl[, 1L][duplicated(ftbl[, 1L])] <- ''
  
  tbl <- ftbl[, -seq.int(ng)]
  tbl <- matrix(as.integer(tbl), nrow(tbl))
  ftbl[, ng] <- sprintf('%s (%s) ', rowSums(tbl),
                        rawr::roundr(rowSums(tbl) / total * 100, digits))
  
  
  ## layout/widths of panels
  widths <- rep_len(widths, ng)
  widths <- c(head(widths, -1L), space, tail(widths, 1L))
  layout(t(seq.int(ng + 1L)), widths = widths)
  
  mm <- c(4, 2, 3, 2)
  op <- par(yaxs = 'i', mar = mm)
  on.exit(par(op))
  par(...)
  
  dat <- as.matrix(tbl)
  dat <- dat[seq.int(nrow(dat)), ]
  dat <- t(dat)
  
  
  ## text panels
  if (extra) {
    barplot(
      dat, horiz = TRUE, col = NA, border = NA, space = 0,
      axes = FALSE, axisnames = FALSE
    )
    co1 <- plotr::coords(to = 'ndc')
  }
  
  barplot(
    dat, horiz = TRUE, col = NA, border = NA, space = 0,
    axes = FALSE, axisnames = FALSE
  )
  co2 <- plotr::coords(to = 'ndc')
  
  par(mar = mm - c(0, 0, 0, 2))
  bp <- barplot(
    dat, horiz = TRUE, col = NA, border = NA, space = 0,
    axes = FALSE, axisnames = FALSE
  )
  co3 <- plotr::coords(to = 'ndc')
  
  
  ## adjustments if only two text columns
  if (!extra) {
    headers <- c('', headers)
    co1 <- co2
  }
  
  
  ## background bars
  ndcx <- function(x) grconvertX(x, 'ndc')
  ndcy <- function(x) grconvertY(x, 'ndc')
  
  pad <- 0
  rect(
    ndcx(co1$plot$x[1L] - pad), ndcy(co1$plot$y[1L]),
    ndcx(co3$plot$x[2L]), ndcy(co3$plot$y[2L]), xpd = NA
  )
  
  df <- diff(bp)[1L] / 2
  yb <- c(ndcy(co1$plot$y[1L]), unique(bp) - df, ndcy(co3$plot$y[2L]))
  aa <- rep_len(alpha.bg, 2L)
  
  for (ii in seq.int(nrow(tbl) + 1L)) {
    rect(
      ndcx(co1$plot$x[1L] - pad), yb[ii],
      ndcx(co3$plot$x[2L]), yb[ii + 1L],
      col = adjustcolor(col.bg[1L], ifelse(ii %% 2L == 0L, aa[1L], aa[2L])),
      xpd = NA, border = NA
    )
  }
  
  
  ## text columns
  text(ndcx(co1$plot$x[1L]), bp, rev(ftbl[, 1L]), xpd = NA, adj = 0)
  if (extra)
    text(ndcx(co2$plot$x[1L]), bp, rev(ftbl[, 2L]), xpd = NA, adj = 0)
  text(ndcx(co3$plot$x[2L]), bp, rev(ftbl[, ng]), xpd = NA, adj = 1)
  
  
  ## headers
  text(c(ndcx(co1$plot$x[1L]), ndcx(co2$plot$x[1L])),
       grconvertY(nrow(tbl) + 0.5), headers[1:2],
       xpd = NA, adj = 0, font = 2L)
  text(ndcx(co3$plot$x[2L]), grconvertY(nrow(tbl) + 0.5), headers[3L],
       xpd = NA, adj = 1, font = 2L)
  
  
  ## barplot
  if (is.null(col.bars))
    col.bars <- c('lightblue', 'dodgerblue2', 'mediumpurple1', 'purple3', 'tomato')
  # col.bars <- rep_len(col.bars, ncol(tbl))
  
  par(mar = mm - c(0, 2, 0, 0))
  dat <- dat[, rev(seq.int(ncol(dat)))]
  xlim <- xlim %||% c(0, max(pretty(colSums(dat))))
  barplot(
    dat, horiz = TRUE, col = col.bars, border = 'white', space = 0,
    axes = FALSE, axisnames = FALSE, xlim = xlim
  )
  
  xaxis.at <- xaxis.at %||% c(0, xlim[2L])
  xaxis.at <- sort(unique(c(0, xaxis.at)))
  lbl <- sprintf('%s (%s)', xaxis.at,
                 round(xaxis.at / total * 100, max(digits, digits.bar)))
  lbl[1L] <- '0 (%)'
  axis(1L, xaxis.at, lbl, mgp = par('mgp') + c(0, 0.5, 0.5))
  
  
  label.bars <- match.arg(label.bars)
  if (label.bars != 'none') {
    fmt <- sprintf('%%.%sf', digits.bar)
    lbl <- if (label.bars == 'percent')
      sprintf(fmt, dat / total * 100) else dat
    
    xx <- apply(dat, 2L, function(x) head(c(0, cumsum(x)), -1L) + x / 2)
    yy <- bp[col(dat)]
    text(xx, yy, lbl, col = ifelse(dat == 0, 'transparent', 'black'))
  }
  
  
  if (legend) {
    largs <- list(
      x = par('usr')[2L], y = grconvertY(nrow(tbl) + 2.5), xpd = NA,
      legend = seq_along(col.bars), title = expression(bold(Grade)),
      fill = col.bars, border = NA, horiz = TRUE, bty = 'n', xjust = 1
    )
    do.call('legend', modifyList(largs, args.legend))
  }
  
  invisible(ftbl)
}

#' tableplot
#' 
#' Add a table to a base plot.
#' 
#' @param x,y the x- and y-coordinates to be used or a keyword; see
#' \code{\link{xy.coords}}
#' @param table a data frame or matrix
#' @param title optional title for the table
#' @param cex,cex.title text size for table, title
#' @param bg background color for each table cell, recycled as needed
#' @param xjust,yjust table position relative to \code{x} and \code{y}
#' @param xpad,ypad padding around text in cells as a proportion of the max
#' width and height of the strings in each column
#' @param col.table,col.title,col.colnames,col.rownames colors for table,
#' title, column names, and row names
#' @param font.table,font.title,font.colnames,font.rownames fonts for table,
#' title, column names, and row names
#' @param show.colnames,show.rownames logical; if \code{TRUE}, column and row
#' names of \code{table} are added
#' @param hlines,vlines logical; if \code{TRUE}, horizontal and/or vertical
#' lines are drawn between table cells
#' @param frame.colnames,frame.title,frame.table logical; if \code{TRUE},
#' draws a frame around the object
#' @param frame.type the type of framing for column names, title, and table,
#' respectively; options are \code{"box"} or \code{"line"}
#' 
#' @return
#' A list with components \code{table}, \code{colnames}, \code{title}, and
#' \code{outline} giving the coordinates of the rectangle for each.
#' 
#' @seealso
#' \code{\link[plotrix]{addtable2plot}}
#' 
#' @examples
#' plot(mpg ~ wt, mtcars)
#' tableplot(
#'   'topright', table = head(mtcars, 3),
#'   title = 'mtcars data set', cex.title = 2
#' )
#' tableplot(
#'   par('usr')[1], 35, head(mtcars, 3)[, 1:3],
#'   show.rownames = TRUE, col.rownames = 'red',
#'   font.colnames = 2, hlines = TRUE
#' )
#' 
#' @export

tableplot <- function(x, y = NULL, table, title = NULL,
                      bg = 'transparent', cex = 1, cex.title = cex,
                      xjust = 0, yjust = 1, xpad = 0.25, ypad = 0.75,
                      col.table = 1L, col.title = col.table,
                      col.colnames = col.table, col.rownames = col.table,
                      font.table = 1L, font.title = font.table,
                      font.colnames = font.table, font.rownames = font.table,
                      show.colnames = TRUE, show.rownames = FALSE, 
                      hlines = FALSE, vlines = FALSE,
                      frame.colnames = FALSE, frame.title = FALSE, frame.table = FALSE,
                      frame.type = c('box', 'box', 'line')) {
  if (is.null(y)) {
    if (is.character(x)) {
      pos <- plotrix::get.tablepos(x)
      x <- pos$x
      y <- pos$y
      xjust <- pos$xjust
      yjust <- pos$yjust
      
    } else {
      y <- x$y %||% x$x
      x <- x$x
    }
  }
  
  tdim <- dim(table)
  ## lower legend if keyword position is on top row
  drop <- any(grepl('top', x))
  
  ## framing table, column names, title
  frame.type <- rep_len(frame.type, 3L)
  
  if (tdim[1L] == 1L)
    hlines <- FALSE
  if (tdim[2L] == 1L)
    vlines <- FALSE
  if (is.null(dim(bg)))
    bg <- matrix(bg, tdim[1L], tdim[2L])
  
  cnames <- colnames(table)
  if (is.null(cnames) && show.colnames)
    cnames <- seq.int(tdim[2L])
  rnames <- rownames(table)
  if (is.null(rnames) && show.rownames) 
    rnames <- seq.int(tdim[1L])
  
  if (par('xlog'))
    x <- log10(x)
  cellwidth <- rep_len(0, tdim[2L])
  
  ## calculate cell heights/widths
  if (show.colnames) {
    for (cc in seq.int(tdim[2L])) {
      cw <- strwidth(c(cnames[cc], format(table[, cc])), cex = cex)
      cellwidth[cc] <- max(cw) * (1 + xpad)
    }
    nvcells <- tdim[1L] + 1L
  } else {
    nvcells <- tdim[1L]
    for (cc in seq.int(tdim[2L])) {
      cw <- strwidth(format(table[, cc]), cex = cex)
      cellwidth[cc] <- max(cw) * (1 + xpad)
    }
  }
  
  if (show.rownames) {
    nhcells <- tdim[2L] + 1L
    rowname.width <- max(strwidth(rnames, cex = cex)) * (1 + xpad)
  } else {
    nhcells <- tdim[2L]
    rowname.width <- 0
  }
  
  cellheight  <- strheight(c(cnames, rnames, c(unlist(table))), cex = cex)
  cellheight  <- max(cellheight) * (1 + ypad)
  titleheight <- strheight(title, cex = cex.title) * (1 + ypad)
  
  if (par('ylog'))
    y <- log10(y)
  if (!is.null(title) & drop)
    y <- y - cellheight - titleheight / 2
  ytop <- y + yjust * nvcells * cellheight
  
  op <- par(xlog = FALSE, ylog = FALSE, xpd = TRUE)
  on.exit(par(op))
  
  ## x-coodinates of center of each column
  xat <- x + show.rownames * rowname.width - xjust *
    (sum(cellwidth) + rowname.width)
  xat <- xat + c(0, cumsum(cellwidth[-length(cellwidth)])) + cellwidth / 2
  
  if (show.colnames)
    text(
      xat, ytop - 0.5 * cellheight, cnames, cex = cex, col = col.colnames,
      font = font.colnames, adj = c(0.5, 0.5)
    )
  
  ## plot each table row
  for (rr in seq.int(tdim[1L])) {
    xleft <- x - xjust * (sum(cellwidth) + rowname.width)
    
    if (show.rownames) {
      text(
        xleft + 0.5 * rowname.width, ytop - (rr + show.colnames - 0.5) * cellheight,
        rnames[rr], cex = cex, col = col.rownames, font = font.rownames
      )
      xleft <- xleft + rowname.width
    }
    
    for (cc in seq.int(tdim[2L])) {
      rect(
        xleft, ytop - (rr + show.colnames - 1) * cellheight,
        xleft + cellwidth[cc], ytop - (rr + show.colnames) * cellheight,
        col = bg[rr, cc], border = bg[rr, cc]
      )
      xleft <- xleft + cellwidth[cc]
    }
    
    text(
      xat, ytop - (rr + show.colnames - 0.5) * cellheight, table[rr, ],
      cex = cex, col = col.table, font = font.table
    )
  }
  
  xleft <- x + show.rownames * rowname.width - xjust *
    (sum(cellwidth) + rowname.width)
  xright <- xleft + sum(cellwidth)
  
  ## grid lines in table
  if (vlines)
    segments(
      xleft + cumsum(cellwidth[-tdim[2L]]),
      ytop - show.colnames * cellheight,
      xleft + cumsum(cellwidth[-tdim[2L]]),
      ytop - (show.colnames + tdim[1L]) * cellheight
    )
  if (hlines)
    segments(
      xleft, ytop - show.colnames * cellheight -
        cumsum(rep(cellheight, tdim[1L] - 1)),
      xright, ytop - show.colnames * cellheight -
        cumsum(rep(cellheight, tdim[1L] - 1))
    )
  
  if (!is.null(title)) {
    text(
      xat[1L] - cellwidth[1L] / 2 * 0.75, ytop + titleheight / 2, title,
      cex = cex.title, col = col.title, adj = 0, font = font.title
    )
  }
  
  co <- list(
    table = {
      yt <- if (frame.type[3L] == 'line')
        ytop - (tdim[1L] + show.colnames) * cellheight
      else ytop - cellheight
      c(xleft, yt, xright, ytop - (tdim[1L] + show.colnames) * cellheight)
    },
    colnames = {
      yt <- if (frame.type[1L] == 'line')
        ytop - cellheight else ytop
      c(xleft, yt, xright, ytop - cellheight)
    },
    title = {
      yt <- if (frame.type[[2L]] == 'line')
        ytop else ytop + titleheight
      c(xleft, yt, xright, ytop)
    }
  )
  co$outline <- c(
    xleft, min(do.call('rbind', co)[, 2L]), xright, ytop + titleheight
  )
  co <- lapply(co, setNames, c('xleft', 'ybottom', 'xright', 'ytop'))
  
  if (frame.table)
    do.call('rect', as.list(co$table))
  if (frame.colnames & show.colnames)
    do.call('rect', as.list(co$colnames))
  if (frame.title & !is.null(title))
    do.call('rect', as.list(co$title))
  
  invisible(co)
}

#' barplot2
#' 
#' Extension of \code{\link{barplot.default}} which accepts multi-way arrays
#' and tables, simplifies grouping and spacing, and adds \code{panel.first} and
#' \code{panel.last} functionality.
#' 
#' @inheritParams graphics::barplot.default
#' @param height a vector, matrix, table, or \code{\link{array}}
#' 
#' @return
#' A list giving the locations of each bar, \code{.$at}, and the mid-point of
#' each group, \code{.$group}.
#' 
#' @examples
#' set.seed(1)
#' x <- array(runif(4 * 3 * 3), c(4, 3, 3))
#' 
#' barplot(x[, , 1])
#' barplot2(x[, , 1]) ## same
#' 
#' barplot2(with(mtcars, table(cyl, gear, vs)))
#' 
#' 
#' ## group labels
#' barplot2(x, names.arg = list(A = 1:3, B = 4:6, C = 7:9))
#' 
#' bp <- barplot2(x)
#' mtext(1:9, side = 1L, at = bp$at, line = 1)
#' mtext(1:3, side = 1L, at = bp$group, line = 3)
#' 
#' 
#' ## simplified space argument
#' barplot2(x, space = c(0.1, 1, 2) / 2, las = 1L, col = 1:4,
#'          legend.text = sprintf('Factor %s', 1:4),
#'          args.legend = list(horiz = TRUE, bty = 'n'),
#'          names.arg = list(A = 1:3, B = 4:6, C = 7:9))
#' 
#' @export

barplot2 <- function(height, width = 1, space = NULL, names.arg = NULL,
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
    if (is.null(x) || is.na(x)) y else x
  
  if (length(d <- dim(height)) == 3L) {
    sp <- rep(space, 2L)[1:2] %||% c(0.1, 0.5)
    s <- rep_len(sp[1L], d[2L] * d[3L])
    i <- which(seq_along(s) %% d[2L] == 0L) + 1L
    s[i[-length(i)]] <- sp[2L]
    if (length(space) > 2L)
      s <- c(0, rep_len(space, length(s))[-length(s)])
    
    m <- matrix(height, d[1L])
    
    bp <- barplot(
      m, width = width, space = s, names.arg = names.arg, plot = plot,
      legend.text = FALSE, beside = FALSE, horiz = horiz, density = NULL,
      angle = angle, col = NA, border = NA, main = main, sub = sub,
      xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xpd = xpd,
      log = log, axes = FALSE, axisnames = FALSE, cex.axis = cex.axis,
      cex.names = cex.names, axis.lty = axis.lty, offset = offset,
      add = FALSE, ann = FALSE, args.legend = args.legend
    )
    gr <- sapply(split(bp, rep(seq.int(d[3L]), each = d[2L])), mean)
    
    res <- list(at = bp, group = gr)
    
    if (!plot)
      return(res)
    
    panel.first
    barplot(
      m, width = width, space = s, axisnames = FALSE,
      legend.text = legend.text, beside = FALSE, horiz = horiz,
      density = density, angle = angle, col = col, border = border,
      xpd = xpd, log = log, axes = FALSE, add = TRUE, ann = ann,
      args.legend = args.legend
    )
    panel.last
    
    if (axes) {
      axis(if (horiz) 2L else 1L, bp, lwd = 0, cex.axis = cex.axis,
           unlist(names.arg) %||% rep(colnames(height), d[3L]) %||% FALSE, ...)
      axis(if (horiz) 1L else 2L, cex.axis = cex.axis, ...)
      if (ann)
        mtext(names(names.arg) %||% dimnames(height)[[3L]], at = gr,
              line = 3, if (horiz) 2L else 1L, cex = cex.names, ...)
      box(bty = 'l')
    }
  } else {
    if (!is.null(space)) {
      space <- rep_len(space, ncol(height) %||% length(height))
      space <- c(0, space[-length(space)])
    }
    
    bp <- barplot(
      height, width = width, space = space, names.arg = names.arg,
      plot = plot, legend.text = FALSE, beside = beside, horiz = horiz,
      density = NULL, angle = angle, col = NA, border = NA,
      main = main, sub = sub, xlab = xlab, ylab = ylab, xlim = xlim,
      ylim = ylim, xpd = xpd, log = log, axes = axes, axisnames = axisnames,
      cex.axis = cex.axis, cex.names = cex.names, axis.lty = axis.lty,
      offset = offset, add = add, ann = ann, args.legend = args.legend
    )
    
    gr <- if (is.null(dim(bp)))
      1L else rep(seq.int(nrow(height)), each = ncol(height))
    gr <- sapply(split(bp, gr), mean)
    res <- list(at = bp, group = gr)
    
    if (!plot)
      return(res)
    
    panel.first
    barplot(
      height, width = width, space = space, axisnames = FALSE,
      legend.text = legend.text, beside = beside, horiz = horiz,
      density = density, angle = angle, col = col, border = border,
      xpd = xpd, log = log, add = TRUE, ann = ann, args.legend = args.legend
    )
    panel.last
  }
  
  invisible(res)
}
