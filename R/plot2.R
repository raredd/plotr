### some less useful plots
# waffle, histr, shist, propfall, bibar, dose_esc, boxline
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
#' shist(x, col = rainbow(5), total = FALSE,
#'       ylim = c(0, 150), las = 1L, breaks = 10)
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
  par(mar = c(0,0,0,0), oma = par('mar'))
  
  res <- vector('list', length(x))
  for (ii in seq_along(x)) {
    xii <- x[[ii]]
    
    res[[ii]] <- h <- hist(
      xii, ..., xpd = NA,
      main = '', xlab = '', ylab = '',
      xlim = xlim, ylim = ylim,
      axes = FALSE, col = col[ii]
    )
    
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
#' cols <- c('grey', 'lightpink', 'indianred1', 'indianred3')
#' propfall(dat[, 1:3])
#' propfall(dat[, 1:3], group = dat$group, col = cols)
#' 
#' ## use the return value to add labels or identify observations
#' bp <- propfall(
#'   dat[, 1:3], col = cols, group = dat$group, order = c('disp', 'wt', 'mpg'),
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
  times <- rawr::interleave(which = 'rbind',
                            matrix(tt), matrix(1L, length(tt))
  )
  
  mm[is.na(mm)] <- 0
  
  par(mar = c(3,4.5,2,1))
  bp <- barplot(mm, col = col, space = 0, ..., border = NA)
  
  text(c(0, head(cumsum(tt), -1L)) + seq_along(group.order), par('usr')[4L],
       group.order, xpd = NA, adj = 0, pos = 3L)
  legend(0, par('usr')[3L], xpd = NA, horiz = TRUE,
         bty = 'n', lty = 1, lwd = 7,
         legend = order, col = col)
  
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
  arr <- roll_fun(x, 2L, mean)[arr_idx[arr_idx <= n]]
  
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
  
  bp <- boxplot(x, border = NA, at = at, plot = !add, ...)
  
  for (ii in seq_along(probs[-1L])) {
    polygon(c(at, rev(at)), c(lo[ii, ], rev(lo[ii + 1L, ])),
            col = col.probs[ii], border = NA)
    polygon(c(at, rev(at)), c(hi[ii, ], rev(hi[ii + 1L, ])),
            col = col.probs[ii], border = NA)
  }
  
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
      arrows(at, hi, at, lo, col = col.err,
             code = 3L, angle = 90, length = 0.1, lwd = lwd.err)
    } else {
      z <- sapply(cx, pm)
      arrows(at, med + z, at, med - z, col = col.err,
             code = 3L, angle = 90, length = 0.1, lwd = lwd.err)
    }
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
#' 
#' @return
#' A numeric vector with the midpoint of each main bar (i.e., identical to a
#' call to \code{barplot(..., beside = FALSE)}).
#' 
#' Additionally, a list attribute (\code{attr(., "coords")}) where each list
#' element corresponds to a group of bars. The matrices contain coordinates
#' of each rectangle plus midpoints.
#' 
#' @examples
#' set.seed(1)
#' tbl <- sapply(1:3, function(x) sort(rpois(3, 10), decreasing = TRUE))
#' barplot(tbl, col = 1:3)
#' inbar(tbl, col = 1:3)
#' inbar(tbl, col = matrix(rainbow(length(tbl)), nrow(tbl)))
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

inbar <- function(height, width = 1, r = NULL, alpha = r, space = NULL,
                  names.arg = NULL, legend.text = NULL, horiz = FALSE,
                  density = NULL, angle = 45, col = NULL, border = par('fg'),
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
      gray.colors(NC) else seq.int(NC)
    col <- sapply(seq_along(col), function(ii)
      colorRampPalette(c('white', col[ii]))(100)[(alpha[, ii]) * 100])
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
