### some useful plots
# scattergram, dimr, plot.dimr
# 
# unexported:
# int
###


#' Scattergram
#' 
#' A scattergram plot with density curves along axes.
#' 
#' @param x,y,g the x-, y-, and group variables
#' @param col a vector of colors or integers for each unique value of \code{g}
#' @param ... additional arguments passed to \code{plot} (e.g., \code{xlim},
#' \code{panel.first}) or further to \code{par}
#' @param xlim,ylim the x- and y-axis limits
#' @param axs the style(s) of axis interval calculation; see \code{\link{par}}
#' @param legend logical; if \code{TRUE}, a \code{group} legend is drawn
#' @param args.legend a \emph{named} list of \code{\link{legend}} arguments to
#' override defaults
#' @param main the main figure title
#' 
#' @examples
#' with(mtcars, {
#'   scattergram(mpg, wt)
#' })
#' 
#' with(mtcars, {
#'   scattergram(mpg, wt, cyl, col = rainbow(3), pch = 16)
#' })
#' 
#' with(mtcars, {
#'   scattergram(
#'     mpg, wt, c('Four', 'Six', 'Eight')[cyl / 2 - 1],
#'     xlab = 'MPG', ylab = 'Weight', las = 1,
#'     panel.first = abline(lm(wt ~ mpg), col = 5, lwd = 3),
#'     panel.last = abline(v = c(20, 25))
#'   )
#' })
#' 
#' @export

scattergram <- function(x, y, g = NULL, col = NULL, ...,
                        xlim = NULL, ylim = NULL, axs = 'i',
                        main = NULL, legend = TRUE, args.legend = list()) {
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    palette('default')
  })
  
  if (is.character(col))
    palette(col)
  
  if (is.null(g)) {
    legend <- FALSE
    g <- rep_len(1L, length(x))
  }
  g <- as.factor(g)
  i <- as.integer(g)
  
  xg <- lapply(split(x, g), function(d) density(d))
  yg <- lapply(split(y, g), function(d) density(d))
  xl <- xlim %||% extendrange(x)
  yl <- ylim %||% extendrange(y)
  
  lo <- matrix(c(2, 1, 4, 3), 2L)
  lo <- layout(lo, heights = c(1, 3), widths = c(3, 1))
  
  axs <- rep_len(axs, 2L)
  par(mar = c(par('mar')[1:2], 0, 0), xaxs = axs[1L], yaxs = axs[2L])
  plot(x, y, col = i, xlim = xl, ylim = yl, ...)
  text(grconvertX(0.975, 'ndc'), grconvertY(0.95, 'ndc'), main,
       adj = 1, xpd = NA, font = 3L, cex = 1.5)
  
  p <- par('usr')
  m <- par('mar')
  
  par(mar = c(0, m[2L], 1, 0))
  plot.new()
  plot.window(p[1:2], c(0, max(unlist(lapply(xg, '[[', 'y')))))
  
  sapply(seq_along(xg), function(ii) {
    xx <- xg[[ii]]
    # lines(xx$x, xx$y, col = ii)
    polyShade(
      xx$x, xx$y, p[1L], p[2L], n = 1e3, border = rm_alpha(palette())[ii],
      col = adjustcolor(rm_alpha(palette())[ii], alpha.f = 1/3)
    )
  })
  
  par(mar = c(m[1L], 0, 0, 5))
  plot.new()
  plot.window(c(0, max(unlist(lapply(yg, '[[', 'y')))), p[3:4])
  
  sapply(seq_along(yg), function(ii) {
    xx <- yg[[ii]]
    # lines(xx$y, xx$x, col = ii)
    polyShade(
      xx$x, xx$y, n = 1e3, horiz = TRUE, border = rm_alpha(palette())[ii],
      col = adjustcolor(rm_alpha(palette())[ii], alpha.f = 1/3)
    )
  })
  
  plot.new()
  largs <- list(
    # 'bottomright',
    x = grconvertX(0, 'npc'), y = grconvertY(0, 'npc'),
    fill = rm_alpha(palette()[sort(unique(i))]), legend = levels(g),
    bty = 'n', xpd = NA, cex = 1.5
  )
  if (!islist(args.legend))
    args.legend <- list()
  if (legend)
    do.call('legend', modifyList(largs, args.legend))
  
  invisible(NULL)
}

#' Dimension reduction
#' 
#' Wrapper function to perform and plot methods of dimensionality reduction
#' including \code{\link[Rtsne]{Rtsne}}, \code{\link[rsvd]{rpca}}, and
#' \code{\link[stats]{prcomp}} with optional \code{\link[stats]{kmeans}}
#' clusters.
#' 
#' @param data an \code{r x c} matrix-like object with \code{r} features and
#' \code{c} samples
#' @param type the type of algorithm to run
#' @param k an integer representing the number of desired clusters, passed to
#' \code{\link[stats]{kmeans}}; note this is only used for convenience and
#' does not affect the algorithm determied by \code{type}
#' 
#' note, if \code{k} is passed in \code{plot.dimr}, a new k-means clustering
#' is performed; this is useful for on-the-fly clustering
#' @param ... for \code{dimr}, additional arguments passed to the algorithm
#' determined by \code{type}; for \code{plot.dimr}, additional arguments
#' passed to plotting functions or further to \code{\link{par}}
#' @param group a length \code{c} vector identifying the group of each sample;
#' use \code{FALSE} for none or \code{TRUE} to use k-means clusters
#' @param group2 a list of data to plot, usually samples in \code{data}, each
#' to be shown as a scatter plot
#' @param col a vector of colors for each group
#' @param labels argument controlling labels for points; if \code{FALSE}, no
#' labels are drawn; if a numeric value in \code{(0,1)}, points outside
#' quantile are labelled; otherwise, a vector of labels for each point
#' @param vegan logical; if \code{TRUE}, ordination diagrams are drawn for each
#' k-means cluster; see \code{\link[vegan]{ordihull}}
#' @param legend logical; if \code{TRUE}, a legend will be drawn
#' @param args.legend a \emph{named} list of \code{\link{legend}} arguments to
#' override defaults
#' @param plot3d logical; if \code{TRUE}, the first three dimensions are drawn
#' as a 3D scatterplot; see \code{\link[rgl]{plot3d}}
#' 
#' @return
#' \code{dimr} returns a list of class "\code{dimr}" with the following
#' components:
#' 
#' \item{\code{object}}{the object return by the algorithm determined via
#' \code{type}}
#' \item{\code{x}}{a matrix with the first three dimensions}
#' \item{\code{kmeans}}{a k-means clustering performed on the first two
#' dimensions of \code{object}}
#' 
#' @seealso
#' \code{\link[Rtsne]{Rtsne}}; \code{\link[rsvd]{rpca}};
#' \code{\link[stats]{pca}}; \code{\link[stats]{kmeans}}
#' 
#' @examples
#' ## feature x sample matrix
#' dat <- t(unique(iris[, 1:4]))
#' grp <- unique(iris)$Species
#' 
#' tsne <- dimr(dat)
#' pca  <- dimr(dat, type = 'pca')
#' rpca <- dimr(dat, type = 'rpca')
#' pca2 <- dimr(dat, type = 'pca', scale. = TRUE) ## same as rpca
#' 
#' plot(tsne)        ## k-means cluster groups
#' plot(tsne, grp)   ## sample groups
#' plot(tsne, k = 4) ## new k-means
#' 
#' plot(pca, vegan = TRUE)
#' plot(pca, k = 5, vegan = TRUE)$kmeans$cluster
#' 
#' 
#' ## use group2 to show scatter plots of numeric data
#' plot(pca, group2 = as.list(data.frame(t(dat))))
#' plot(pca, pch = 16, cex = 3, col = adjustcolor(palette()[1:3], alpha.f = 0.5),
#'      group2 = as.list(data.frame(t(rbind(dat, dat)))))
#' 
#' \dontrun{
#' plot(tsne, k = 5, plot3d = TRUE)
#' }
#' 
#' @export

dimr <- function(data, type = c('tsne', 'pca', 'rpca'), k = 3L, ...) {
  type <- match.arg(type)
  data <- t(data)
  
  set.seed(1L)
  res <- switch(
    type,
    tsne = {
      object <- Rtsne::Rtsne(
        stats::dist(data), dims = 3L, is_distance = TRUE, ...
      )
      list(object = object, x = object$Y[, 1:3], type = 't-SNE')
    },
    pca = {
      object <- stats::prcomp(data, ...)
      list(obect = object, x = object$x[, 1:3], type = 'PC')
    },
    rpca = {
      object <- rsvd::rpca(data, ...)
      list(object = object, x = object$x[, 1:3], type = 'rPC')
    }
  )
  
  km <- stats::kmeans(res$x[, 1:2], k, 1e4L)
  
  res <- structure(
    c(res, list(kmeans = km)),
    class = 'dimr'
  )
  
  invisible(res)
}

#' @rdname dimr
#' @export
plot.dimr <- function(x, group = TRUE, group2 = NULL, k = NULL,
                      col = NULL, labels = FALSE, vegan = FALSE,
                      legend = TRUE, args.legend = list(),
                      plot3d = FALSE, ...) {
  on.exit({
    palette('default')
  })
  
  object <- x
  type <- object$type
  
  if (!is.null(k)) {
    set.seed(1L)
    object$kmeans <- stats::kmeans(object$x[, 1:2], k, 1e4L)
  }
  
  if (is.numeric(col))
    col <- palette()[as.integer(col)]
  if (is.character(col))
    palette(col)
  
  x <- object$x[, 1L]
  y <- object$x[, 2L]
  z <- object$x[, 3L]
  
  group <- if (identical(group, FALSE))
    rep_len(1L, length(x))
  else if (isTRUE(group))
    object$kmeans$cluster
  else rep_len(group, length(x))
  
  group <- as.factor(group)
  colii <- as.integer(group)
  
  if (isTRUE(labels))
    labels <- FALSE
  
  object$lbl <- if (is.numeric(labels) & length(labels) == 1L) {
    q <- c(labels / 2, 1 - labels / 2)
    ifelse(x %inside% quantile(x, q) & y %inside% quantile(y, q),
           '', colnames(data))
  } else if (identical(labels, FALSE))
    NULL else rep_len(labels, length(x))
  
  if (plot3d) {
    rgl::plot3d(
      x, y, z,
      paste(type, '1'), paste(type, '2'), paste(type, '3'),
      type = 's', col = colii, size = 1, scale = 0.2, ...
    )
  } else {
    ## set up plotting regions for extra panel
    if (!is.null(group2)) {
      op <- par(no.readonly = TRUE)
      on.exit(par(op), add = TRUE)
      group2 <- if (islist(group2))
        group2 else list(group2)
      
      n <- length(group2)
      m <- n2mfrow(n)
      m <- matrix(seq.int(prod(m)), m[1L], m[2L])
      l <- cbind(1L, m + 1L)
      l <- layout(l, heights = 1, widths = c(2, rep(0.5, n)))
      par(oma = par('mar'), mar = c(0,0,0,1))
    }
    
    plot(
      x, y, col = colii, ...,
      xlab = paste(type, '1'), ylab = paste(type, '2')
    )
    text(x, y, labels = object$lbl, pos = 3L, xpd = NA, cex = 0.7)
    
    if (vegan) {
      xy <- cbind(x, y)
      gr <- factor(object$kmeans$cluster)
      vegan::ordispider(
        xy, gr, label = TRUE, col = adjustcolor('grey', alpha.f = 0.5)
      )
      vegan::ordihull(
        xy, gr, lty = 'dashed', col = 'darkgrey'
      )
    }
    
    xy <- unlist(coords(0.51, 0.95)$device)
    xy <- par('usr')[c(1L, 4L)]
    largs <- list(
      x = xy[1L], y = xy[2L], bty = 'n', horiz = TRUE, yjust = 0,
      col = unique(colii), pch = 16L, xpd = NA, legend = levels(group)
    )
    if (!islist(args.legend))
      args.legend <- list()
    if (legend)
      do.call('legend', modifyList(largs, args.legend))
    
    if (type == 't-SNE')
      mtext(sprintf('perplexity = %s', object$object$perplexity),
            adj = 1, line = 0.5)
    
    if (!is.null(group2)) {
      xy <- par('usr')[c(2L, 4L)]
      col <- c('blue', 'red')
      largs <- list(
        x = xy[1L], y = xy[2L], bty = 'n', horiz = TRUE, yjust = 0, xjust = -0.1,
        col = c(col, 'black', 'black'), pch = 16L, pt.cex = c(3, 3, 1, 4) / 2,
        legend = c('Low', 'High', 'Less', 'More'), xpd = NA
      )
      
      if (legend)
        do.call('legend', modifyList(largs, list()))
      
      int(group2, x, y, dim = m)
    }
  }
  
  invisible(object)
}

# n <- 9
# d <- as.list(mtcars[, rep_len(c('mpg', 'wt', 'hp'), n)])
# par(mfrow = c(3, 3), oma = c(5, 5, 4, 2))
# int(d, mtcars$mpg, mtcars$wt, legend = TRUE)
# title(xlab = 'MPG', ylab = 'Weight', outer = TRUE, cex.lab = 2)

int <- function(data, x, y, col = c('blue', 'red'), cex = c(0.5, 2),
                legend = FALSE, alpha = 0.5, dim = par('mfrow'),
                opad = 0, ipad = 0, vec = NULL, args.legend = list()) {
  data <- if (islist(data))
    data else list(data)
  
  op <- par(no.readonly = TRUE)
  op$fig <- abs(op$fig)
  on.exit(par(op))
  
  opad <- rep_len(opad, 4L)
  ipad <- rep_len(ipad, 2L)
  
  p <- lapply(seq_along(data), function(ii) {
    dd <- data[[ii]]
    cc <- if (!is.null(vec))
      rawr:::col_scaler2(dd, col, rep_len(vec, length(data))[ii], alpha = alpha)
    else rawr::col_scaler(dd, col, alpha = alpha)
    cx <- rawr::rescaler(dd, cex)
    
    mar <- switch(
      fig(ii, dim),
      c(0, ipad[1L], opad[3L], 0),
      c(0, 0, opad[3L], 0),
      c(0, 0, opad[3L], ipad[2L]),
      c(0, ipad[1L], 0, 0),
      c(0, 0, 0, 0),
      c(0, 0, 0, ipad[2L]),
      c(opad[1L], ipad[1L], 0, 0),
      c(opad[1L], 0, 0, 0),
      c(opad[1L], 0, 0, ipad[2L]),
      c(0, opad[2:4]),
      c(0, opad[2L], 0, opad[4L]),
      c(opad[1:2], 0, opad[4L]),
      c(opad[1:3], 0),
      c(opad[1L], 0, opad[3L], 0),
      c(opad[1L], 0, opad[3:4])
    )
    par(mar = mar)
    plot.new()
    plot.window(extendrange(x), extendrange(y))
    box()
    
    points(x, y, col = cc, cex = cx, pch = 16L)
    mtext(names(data)[ii], line = -1)
    
    p <- par('usr')
    c(grconvertX(p[1:2], 'user', 'ndc'), grconvertY(p[3:4], 'user', 'ndc'))
  })
  
  p <- do.call('rbind', p)
  
  xy <- unlist(coords(0.51, 0.95)$device)
  xy <- c(min(c(min(p[, 1L]), max(p[, 2L]))), max(p[, 4L]))
  xy <- c(grconvertX(xy[1L], 'ndc', 'user'), grconvertY(xy[2L], 'ndc', 'user'))
  
  largs <- list(
    x = xy[1L], y = xy[2L], bty = 'n', horiz = TRUE,
    col = c(col, 'black', 'black'), pch = 16L, pt.cex = c(3, 3, 1, 4) / 2,
    legend = c('Low', 'High', 'Less', 'More'), xpd = NA
  )
  
  if (legend)
    do.call('legend', modifyList(largs, args.legend))
  
  invisible(NULL)
}
