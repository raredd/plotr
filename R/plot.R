### some useful plots
# scattergram, dimr, plot.dimr, loess_smooth, jmplot, dsplot, dsplot.default,
# dsplot.formula
# 
# s3 methods
# loess_smooth.default, loess_smooth.formula, plot.loess_smooth,
# print.loess_smooth, dsplot
# 
# unexported:
# gridplot, caxis_pca, wss, guess_k, do_poly
###


#' Scattergram
#' 
#' A scattergram plot with density curves along axes.
#' 
#' @param x,y,g the x-, y-, and group variables
#' @param col a vector of colors or integers for each unique value of \code{g}
#' @param ... additional arguments passed to \code{plot} (e.g., \code{xlim},
#' \code{panel.first}) or further to \code{par}
#' @param add logical; if \code{TRUE}, plots will be added to an existing
#' or multi-panel figure
#' @param xlim,ylim the x- and y-axis limits
#' @param axs the style(s) of axis interval calculation; see \code{\link{par}}
#' @param heights for \code{add = TRUE}, the relative height of each density
#' panel compared to the figure region
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
#' 
#' ## to add multiple figures on a plot, use add = TRUE
#' sp <- split(mtcars, mtcars$am)
#' op <- par(mfrow = 1:2, mar = c(5, 5, 5, 4), oma = c(0, 0, 0, 2))
#' lapply(sp, function(d) {
#'   scattergram(d$mpg, d$wt, d$cyl, col = 1:3, add = TRUE)
#' })
#' par(op)
#' 
#' @export

scattergram <- function(x, y, g = NULL, col = NULL, ..., add = FALSE,
                        xlim = NULL, ylim = NULL, axs = 'i', heights = 0.25,
                        main = NULL, legend = TRUE, args.legend = list()) {
  if (add) {
    scattergram2(x, y, g, col = col, ..., heights = heights,
                 xlim = xlim, ylim = ylim, main = main)
    return(invisible(NULL))
  }
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  col <- if (is.character(col))
    col else if (is.null(col)) palette() else palette()[col]
  col2 <- rm_alpha(col)
  
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
  plot(x, y, col = if (length(col) == length(i)) col else col[i],
       xlim = xl, ylim = yl, ...)
  text(grconvertX(0.975, 'nfc'), grconvertY(0.95, 'nfc'), main,
       adj = 1, xpd = NA, font = 3L, cex = 1.5)
  
  p <- par('usr')
  m <- par('mar')
  
  par(mar = c(0, m[2L], 1, 0))
  plot.new()
  plot.window(p[1:2], c(0, max(unlist(lapply(xg, '[[', 'y')))))
  
  sapply(seq_along(xg), function(ii) {
    xx <- xg[[ii]]
    polyShade(
      xx$x, xx$y, p[1L], p[2L], n = 1e3, border = col2[ii],
      col = adjustcolor(col2[ii], alpha.f = 1/3)
    )
  })
  
  par(mar = c(m[1L], 0, 0, 5))
  plot.new()
  plot.window(c(0, max(unlist(lapply(yg, '[[', 'y')))), p[3:4])
  
  sapply(seq_along(yg), function(ii) {
    xx <- yg[[ii]]
    polyShade(
      xx$x, xx$y, n = 1e3, horiz = TRUE, border = col2[ii],
      col = adjustcolor(col2[ii], alpha.f = 1/3)
    )
  })
  
  plot.new()
  largs <- list(
    x = grconvertX(0, 'npc'), y = grconvertY(0, 'npc'),
    fill = sort(unique(col2[i])), legend = levels(g),
    bty = 'n', xpd = NA, cex = 1.5
  )
  if (!islist(args.legend))
    args.legend <- list()
  if (legend)
    do.call('legend', modifyList(largs, args.legend))
  
  invisible(NULL)
}

scattergram2 <- function(x, y, g = NULL, col = NULL, ..., heights = 0.25,
                         xlim = NULL, ylim = NULL, main = NULL) {
  col <- if (is.character(col))
    col else if (is.null(col)) palette() else palette()[col]
  
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
  
  plot(x, y, col = if (length(col) == length(i)) col else col[i],
       xlim = xl, ylim = yl, ...)
  
  p <- par('usr')
  d <- c(diff(p[1:2]), diff(p[3:4]))
  h <- rep_len(heights, 2L)
  h <- c(diff(p[3:4]), diff(p[1:2])) * heights + p[c(4L, 2L)]
  
  sapply(seq_along(xg), function(ii) {
    xx <- xg[[ii]]
    yy <- rescale(xx$y, c(p[4L], h[1L]))
    polyShade(
      xx$x, yy, p[1L], p[2L], n = 1e3, border = rm_alpha(col)[ii],
      xpd = NA,
      col = adjustcolor(rm_alpha(col[ii]), alpha.f = 1/3)
    )
  })
  
  sapply(seq_along(yg), function(ii) {
    xx <- yg[[ii]]
    yy <- rescale(xx$y, c(p[2L], h[2L]))
    polyShade(
      xx$x, yy, p[3L], p[4L], n = 1e3, border = rm_alpha(col[ii]),
      horiz = TRUE, xpd = NA,
      col = adjustcolor(rm_alpha(col[ii]), alpha.f = 1/3)
    )
  })
  
  invisible(NULL)
}

#' Dimension reduction
#' 
#' Wrapper function to perform and plot methods of dimensionality reduction
#' including \code{\link[Rtsne]{Rtsne}}, \code{\link[rsvd]{rpca}},
#' \code{\link[stats]{prcomp}}, and\code{\link[umap]{umap}}, with optional
#' \code{\link[stats]{kmeans}} clustering.
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
#' @param x an object of class \code{"dimr"}
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
#' @param loadings for PCA objects, a length 1 or 2 vector giving the number
#' of loadings to plot for each component; the highest loadings will be shown
#' as a proportion of the axes with the sign of the eigenvector
#' @param legend logical; if \code{TRUE}, a legend will be drawn
#' @param args.legend a \emph{named} list of \code{\link{legend}} arguments to
#' override defaults
#' @param plot3d logical; if \code{TRUE}, the first three dimensions are drawn
#' as a 3D scatterplot; see \code{\link[rgl]{plot3d}}
#' @param iplot logical; if \code{TRUE}, the first two dimensions are drawn
#' as an interactive scatter plot; see \code{\link[iplotr]{iscatter}}
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
#' \code{\link[stats]{prcomp}}; \code{\link[umap]{umap}},
#' \code{\link[stats]{kmeans}}
#' 
#' @examples
#' ## feature x sample matrix
#' dat <- t(unique(iris[, 1:4]))
#' grp <- unique(iris)$Species
#' 
#' tsne <- dimr(dat)
#' umap <- dimr(dat, type = 'umap')
#' pca  <- dimr(dat, type = 'pca')
#' rpca <- dimr(dat, type = 'rpca')
#' pca2 <- dimr(dat, type = 'pca', scale. = TRUE) ## same as rpca
#' 
#' plot(tsne)        ## use k-means cluster groups
#' plot(tsne, grp)   ## use groups from data
#' plot(tsne, k = 3) ## perform new k-means
#' 
#' plot(pca, vegan = TRUE)
#' plot(pca, loadings = c(3, 4))
#' plot(rpca, loadings = c(3, 4))
#' plot(pca, k = 4, vegan = TRUE)$kmeans$cluster
#' 
#' 
#' ## use group2 to show scatter plots of numeric data
#' plot(pca, group2 = as.list(data.frame(t(dat))))
#' plot(pca, pch = 16, cex = 3, col = adjustcolor(palette()[1:3], alpha.f = 0.5),
#'      group2 = as.list(data.frame(t(rbind(dat, dat)))))
#' 
#' 
#' ## use plot3d = TRUE for a 3D, interactive figure
#' \dontrun{
#' plot(tsne, plot3d = TRUE)
#' 
#' plot(tsne, iplot = TRUE, labels = data.frame(t(dat)))
#' 
#' 
#' ## make a gridplot (called from plot.dimr)
#' n <- 9
#' d <- as.list(mtcars[, rep_len(c('mpg', 'wt', 'hp'), n)])
#' par(mfrow = n2mfrow(n), oma = c(5, 5, 4, 2))
#' plotr:::gridplot(d, mtcars$mpg, mtcars$wt, legend = TRUE)
#' title(xlab = 'MPG', ylab = 'Weight', outer = TRUE, cex.lab = 2)
#' }
#' 
#' @export

dimr <- function(data, type = c('tsne', 'pca', 'rpca', 'umap'), k = NULL, ...) {
  type <- match.arg(type)
  dims <- pmin(nrow(data), 3L)
  data <- t(data)
  
  set.seed(1L)
  res <- switch(
    type,
    tsne = {
      object <- Rtsne::Rtsne(
        stats::dist(data), dims = dims, is_distance = TRUE, ...
      )
      list(object = object, x = object$Y[, seq.int(dims)], type = 't-SNE')
    },
    umap = {
      no <- list(...)
      no$n_components <- no$n_components %||% dims
      oo <- umap::umap.defaults
      
      object <- umap::umap(data, modifyList(oo, no))
      list(object = object, x = object$layout[, seq.int(dims)], type = 'UMAP')
    },
    pca = {
      object <- stats::prcomp(data, ...)
      list(object = object, x = object$x[, seq.int(dims)], type = 'PC')
    },
    rpca = {
      object <- rsvd::rpca(data, ...)
      list(object = object, x = object$x[, seq.int(dims)], type = 'rPC')
    }
  )
  
  k <- k %||% guess_k(data, 'wss')
  if (is.na(k))
    k <- 3L
  
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
                      loadings = 0L, legend = TRUE, args.legend = list(),
                      plot3d = FALSE, iplot = FALSE, ...) {
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
  z <- if (ncol(object$x) > 2L)
    object$x[, 3L] else NULL
  
  if (is.null(z) && plot3d) {
    message('not enough dimensions for 3d plot', domain = NA)
    plot3d <- FALSE
  }
  
  group <- if (identical(group, FALSE))
    rep_len(1L, length(x))
  else if (isTRUE(group) || is.null(group))
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
  } else if (iplot) {
    lbl <- if (identical(labels, FALSE) || is.null(labels))
      seq_along(x) else labels
    ip <- iplotr::iscatter(
      x, y, group = colii, col = palette()[unique(sort(colii))],
      labels = lbl, ...
    )
    return(ip)
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
    
    if (inherits(object$object, c('prcomp', 'rpca'))) {
      loadings <- rep_len(loadings, 2L)
      caxis_pca(3L, object$object, 1L, loadings[1L])
      caxis_pca(4L, object$object, 2L, loadings[2L])
    }
    
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
      col = unique(sort(colii)), pch = 16L, xpd = NA, legend = levels(group)
    )
    if (!islist(args.legend))
      args.legend <- list()
    if (legend)
      do.call('legend', modifyList(largs, args.legend))
    
    if (type == 't-SNE')
      mtext(sprintf('perplexity = %s', object$object$perplexity),
            adj = 1, line = 0.5)
    
    if (!is.null(group2)) {
      xy  <- par('usr')[c(2L, 4L)]
      col <- c('blue', 'red')
      
      largs <- list(
        x = xy[1L], y = xy[2L], bty = 'n', horiz = TRUE, yjust = 0, xjust = -0.1,
        col = c(col, 'black', 'black'), pch = 16L, pt.cex = c(3, 3, 1, 4) / 2,
        # legend = c('Low', 'High', 'Less', 'More'), xpd = NA
        legend = c('Lower', 'Higher'), xpd = NA
      )
      
      if (legend)
        do.call('legend', modifyList(largs, list()))
      
      gridplot(group2, x, y, dim = m)
    }
  }
  
  invisible(object)
}

gridplot <- function(data, x, y, col = c('blue', 'red'), cex = c(0.5, 2),
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
    # cx <- rawr::rescaler(dd, cex)
    cx <- 1
    
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
    # legend = c('Low', 'High', 'Less', 'More'), xpd = NA
    legend = c('Lower', 'Higher'), xpd = NA
  )
  
  if (legend)
    do.call('legend', modifyList(largs, args.legend))
  
  invisible(NULL)
}

caxis_pca <- function(side, pca, which, n = 5L, label = TRUE) {
  if (n < 1L)
    return(invisible(NULL))
  
  l <- pca$rotation ^ 2
  i <- seq.int(nrow(l))
  o <- setNames(i, rownames(l))
  l <- l[ord <- order(l[, which], decreasing = TRUE), ]
  s <- c('-', '+')[(pca$rotation[ord, which] > 0) + 1L]
  
  n <- pmin(n, nrow(l))
  x <- l[, which]
  x <- if (n < nrow(l)) {
    c <- c(o[ord][seq.int(n)], rep_len(NA, nrow(l) - n))
    c(x[seq.int(n)], sum(x[-seq.int(n)]))
  } else {
    c <- o[ord][seq.int(n)]
    x
  }
  length(x) <- nrow(l)
  x[is.na(x)] <- 0
  
  x <- caxis(side, x, c, 5, lend = 1L)
  l <- sprintf('%s (%s)', rownames(l), s)
  s[is.na(c)] <- l[is.na(c)] <- ''
  
  if (label)
    switch(
      side, NA, NA,
      text(x$end, par('usr')[4L], l, col = c,
           xpd = NA, adj = c(1, -1), cex = 0.8),
      text(par('usr')[2L], x$end - (x$end - x$start) / 2, s,
           col = c, xpd = NA, pos = 4L)
    )
}

wss <- function(data, n = pmin(20L, nrow(data)), p = 0.01, return_wss = FALSE) {
  wss <- (nrow(data) - 1L) * sum(apply(data, 2L, stats::var, na.rm = TRUE))
  wss <- c(wss, sapply(seq.int(n)[-1L], function(ii)
    sum(kmeans(data, ii)$withinss)))
  
  if (return_wss)
    wss else which(abs(diff(rescale(wss))) < p)[1L] - 1L
}

guess_k <- function(data, how = c('wss'), ...) {
  how <- match.arg(how)
  
  switch(
    how,
    ## silhouette
    wss = wss(data, ...)
  )
}

#' Fit loess curves
#' 
#' Add a smooth curve computed by \code{\link{loess}} to a scatter plot.
#' 
#' @param x,y the x- and y-axes for the plot
#' @param ... additional arguments passed to or from other methods
#' @param formula a formula of the form \code{y ~ x} giving the x- and y-
#' axes for the plot
#' @param data a data frame (or list) from which the variables in
#' \code{formula} should be taken
#' @param conf the confidence interval to calculate
#' @param n the number of points at which to evaluate the smooth curve
#' @param sd logical; if \code{TRUE}, the standard deviation of the square
#' of the residuals are used to calculate the confidence intervals; otherwise,
#' the standard errors from \code{\link{predict.loess}} are used
#' @param fix_extremes logical; if \code{TRUE}, the extremes of the fitted
#' lines will run through \code{{x_1, y_1}} and \code{{x_n, y_n}}
#' @param col.line,col.ci colors for the smoothed curve and confidence
#' intervals (lines or band)
#' @param alpha.ci alpha transparency for the confidence band
#' @param xlab,ylab the x- and y-axis labels
#' @param ci type of confidence interval to plot, one of \code{"none"} for
#' no plotting, \code{"lines"} for lines only, or \code{"band"} for a
#' semi-transparent color band
#' 
#' @seealso
#' \code{\link{loess.smooth}}
#' 
#' @examples
#' plot(mpg ~ wt, mtcars)
#' lo <- loess_smooth(mpg ~ wt, mtcars)
#' lines(lo$x, lo$y)
#' lines(lo$x, lo$upper, lty = 2)
#' lines(lo$x, lo$lower, lty = 2)
#' 
#' plot(lo)
#' plot(lo, ci = 'lines')
#' plot(lo, ci = 'band')
#' 
#' @export

loess_smooth <- function(x, ...) {
  UseMethod('loess_smooth')
}

#' @rdname loess_smooth
#' @export
loess_smooth.formula <- function(formula, data, ...) {
  xy <- all.vars(formula)
  if (length(xy) != 2L)
    stop('\'formula\' should be of the form y ~ x')
  
  d <- as.list(data)
  x <- d[[xy[2L]]]
  y <- d[[xy[1L]]]
  
  loess_smooth(x, y, ...)
}

#' @rdname loess_smooth
#' @export
loess_smooth.default <- function(x, y = NULL, ..., conf = 0.95, n = 50L,
                                 sd = TRUE, fix_extremes = FALSE) {
  xy <- xy.coords(x, y)
  na <- is.na(xy$x) | is.na(xy$y)
  
  xy$x <- xy$x[!na]
  xy$y <- xy$y[!na]
  
  xx <- xy$x
  yy <- xy$y
  x0 <- seq(min(xx), max(xx), length.out = n)
  
  ci <- qnorm((1 - conf) / 2, lower.tail = FALSE)
  lo <- loess(yy ~ xx, ...)
  pr <- predict(lo, data.frame(xx = x0), se = TRUE)
  
  se <- if (sd) {
    lr <- loess(I(residuals(lo) ^ 2) ~ xx, ...)
    sd <- sqrt(pmax(0, predict(lr, data.frame(xx = x0))))
  } else pr$se.fit
  
  if (fix_extremes) {
    x0[c(1L, n)] <- xy$x[c(1L, length(xy$x))]
    pr$fit[c(1L, n)] <- xy$y[c(1L, length(xy$y))]
  }
  
  res <- list(
    model = lo, x = x0, y = unname(pr$fit), sd = sd, xy = xy,
    lower = pr$fit - ci * se, upper = pr$fit + ci * se,
    call = match.call()
  )
  
  invisible(
    structure(res, class = 'loess_smooth')
  )
}

#' @rdname loess_smooth
#' @export
print.loess_smooth <- function(x, ...) {
  print(x$model)
  
  invisible(x)
}

#' @rdname loess_smooth
#' @export
plot.loess_smooth <- function(x, y, ..., col.line = 1L, col.ci = col.line,
                              alpha.ci = 0.5, xlab = NULL, ylab = NULL,
                              ci = c('none', 'lines', 'band')) {
  stopifnot(
    inherits(x, 'loess_smooth')
  )
  
  x$xy$xlab <- xlab %||% x$xy$xlab %||% 'x'
  x$xy$ylab <- ylab %||% x$xy$ylab %||% 'y'
  
  plot(x$xy, xlab = x$xy$xlab, ylab = x$xy$ylab, ...)
  lines(x, col = col.line)
  
  switch(
    match.arg(ci),
    none = NULL,
    lines = {
      lines(x$x, x$upper, col = col.ci, lty = 2L)
      lines(x$x, x$lower, col = col.ci, lty = 2L)
    },
    band = do_poly(x$x, x$lower, x$upper, col = col.ci, alpha = alpha.ci)
  )
  
  invisible(NULL)
}

do_poly <- function(x, y1, y2, col = 'grey', alpha = 0.5) {
  polygon(
    c(x, rev(x)), c(y1, rev(y2)), border = NA,
    col = adjustcolor(col, alpha.f = alpha)
  )
}

#' Joint-marginal plot
#' 
#' Joint and marginal distributions scatterplots with \code{\link{tplot}}s on
#' margins.
#' 
#' @param x,y x- and y-axis variables
#' @param z grouping variable
#' @param main,sub main- and sub-titles (below x-axis) for the plot
#' @param xlab,ylab x- and y-axis labels
#' @param names labels for \code{x} and \code{y} variables
#' @param xlim,ylim x- and y-axis limits
#' @param axes logical; draw axes
#' @param frame.plot logical; draw box around \code{x-y} plot
#' @param log \code{"x"}, \code{"y"}, or \code{"xy"} for logarithmic scale or
#' \code{""} for none (default); sets negative values to \code{\link{NA}} and
#' gives a warning; see \code{\link{xy.coords}}
#' @param xratio,yratio proportion of x- and y-axes allotted for scatterplots;
#' see \code{widths} and \code{heights} in \code{\link{layout}}
#' @param show.n,show.na logical; show total and missing in each group
#' @param cex.n size of \code{show.n} and \code{show.na} text
#' @param ann logical; annotate plot
#' @param asp numeric, giving the \strong{asp}ect ratio \emph{y/x}; see
#' \code{\link{plot.window}}
#' @param panel.first an "expression" to be evaluated after the plot axes are
#' set up but before any plotting takes place; this can be useful for drawing
#' background grids or scatterplot smooths; note that this works by lazy
#' evaluation: passing this argument from other plot methods may well not work
#' since it may be evaluated too early; see also \code{\link{plot.default}}
#' @param panel.last an expression to be evaluated after plotting has taken
#' place but before the axes, title, and box are added; see the comments about
#' \code{panel.first}
#' @param ... further arguments passed to \code{par} (\code{las}, \code{pch},
#' etc) and/or \code{\link[rawr]{tplot}} (\code{group.col}, \code{group.pch}, etc)
#' 
#' @return
#' A list with elements \code{x} and \code{y} corresponding to boxplots
#' on x- and y-axes, respectively. See \code{\link{boxplot}} or
#' \code{\link[rawr]{tplot}} for a detailed description of each list.
#' 
#' @references
#' \href{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}{Tatsuki
#' \code{jmplot}}; \code{\link[rawr]{tplot}}; \code{\link{boxplot}}
#'
#' @examples
#' set.seed(1)
#' dat <- data.frame(
#'   x = rnorm(100, 20, 5),
#'   y = rexp(100),
#'   z = c('M', 'F'),
#'   zz = c(LETTERS[1:4])
#' )
#' 
#' with(dat, {
#'   jmplot(x, y, zz, type = 'db', jit = 0.02, col = 1:4, las = 1, cex.n = 0.5,
#'          group.col = TRUE, pch = 1:4, group.pch = TRUE, boxcol = grey(0.9))
#' })
#'
#' @export

jmplot <- function(x, y, z,
                   ## labels/aesthetics
                   main = '', sub = '', xlab = NULL, ylab = NULL, names = NULL,
                   
                   ## additional aesthetics
                   xlim = NULL, ylim = NULL, axes = TRUE, frame.plot = axes,
                   log = '', xratio = 0.8, yratio = xratio,
                   
                   ## n/missing for each group
                   show.n = TRUE, show.na = show.n, cex.n = 1,
                   
                   ## extra stuff
                   ann = par('ann'), asp = NA,
                   panel.first = NULL, panel.last = NULL, ...) {
  ## helpers
  localTplot <- function(..., type = 'b', horizontal = FALSE)
    rawr::tplot(..., type = type, axes = FALSE, horizontal = horizontal)
  eliminateTplot <- function(fn, ..., type, dist, jit, names, group.col,
                             boxcol, boxborder, group.pch, median.line,
                             mean.line, median.pars, mean.pars, boxplot.pars,
                             border.col, axes, frame.plot, add, horizontal)
    fn(...)
  localPlot   <- function(xy, ...,                         lwd)
    eliminateTplot(plot.xy, xy, 'p', ...)
  localAxis   <- function(    ..., col, bg, pch, cex, lty, lwd)
    eliminateTplot(axis, ...)
  localBox    <- function(    ..., col, bg, pch, cex, lty, lwd)
    eliminateTplot(box, ...)
  localWindow <- function(    ..., col, bg, pch, cex, lty, lwd)
    eliminateTplot(plot.window, ...)
  localTitle  <- function(    ..., col, bg, pch, cex, lty, lwd)
    eliminateTplot(title, ...)
  
  ## calculate xlim, ylim
  lim <- function(z) {
    range(z, na.rm = TRUE, finite = TRUE)
  }
  
  z  <- as.factor(z)
  xy <- xy.coords(x, y, deparse(substitute(x)), deparse(substitute(y)), log)
  
  ## defaults
  names <- names %||% levels(z)
  xlab <- xlab %||% xy$xlab
  ylab <- ylab %||% xy$ylab
  xlim <- xlim %||% lim(xy$x)
  ylim <- ylim %||% lim(xy$y)
  
  op <- par(no.readonly = TRUE)
  mar <- op$mar
  ## set the layout
  layout(matrix(c(1, 3, 0, 2), 2L), widths = c(xratio, 1 - xratio),
         heights = c(1 - yratio, yratio))
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, mar[3L], mar[4L]) + op$oma)
  
  ## plot x distribution on top
  par(mar = c(0, mar[2L], 0, 0))
  X <- localTplot(x ~ z, ylim = xlim, horizontal = TRUE,
                  show.n = FALSE, show.na = FALSE, ...)
  if (axes) 
    localAxis(side = 2, at = 1:nlevels(z), labels = names, ...)
  
  ## plot y distribution on right
  par(mar = c(mar[1L], 0, 0, 0))
  Y <- localTplot(y ~ z, ylim = ylim, horizontal = FALSE,
                  show.n = show.n, show.na = show.na, cex.n = cex.n, ...)
  if (axes) 
    localAxis(side = 1L, at = seq.int(nlevels(z)), labels = names, ...)
  
  ## plot xy points
  par(mar = c(mar[1L], mar[2L], 0, 0))
  plot.new()
  localWindow(xlim, ylim, log, asp, ...)
  panel.first
  localPlot(xy, xlim = xlim, ylim = ylim, ...)
  panel.last
  
  ## plot options
  if (axes) {
    localAxis(side = 1L, ...)
    localAxis(side = 2L, ...)
  }
  if (frame.plot)
    localBox(...)
  if (ann) {
    localTitle(sub = sub, xlab = xlab, ylab = ylab, ...)
    localTitle(main = main, outer = TRUE, ...)
  }
  
  invisible(list(x = X, y = Y))
}

#' Discrete scatter plot
#' 
#' This creates a scatter plot (sort of) for discrete, bivariate data; an
#' alternative to sunflower plots for integer-valued variables.
#' 
#' @param formula a \code{\link{formula}}, such as \code{y ~ group}, where y
#' is a numeric vector of data values to be split into groups according to the
#' grouping variable \code{group} (usually a factor)
#' @param data a data frame (or list) from which the variables in formula
#' should be taken
#' @param subset an optional vector specifying a subset of observations to be
#' used for plotting
#' @param na.action a function which indicates what should happen when the data
#' contain \code{\link{NA}}s; the default is to ignore missing values in either
#' the response or the group
#' @param x,y x- and y-axis variables
#' @param ... for the \code{formula} method, named arguments to be passed to
#' the default method
#' 
#' for the default method, unnamed arguments are additional data vectors
#' (unless x is a list when they are ignored), and named arguments are
#' arguments and \code{\link{par}}s to be passed to \code{\link{plot}}
#' @param bg.col logical or color name to fill boxes with based on density; if
#' \code{FALSE}, gridlines are drawn to distinguish boxes; if \code{TRUE},
#' grayscale is used as the default color
#' @param col plotting color
#' @param xlab,ylab x- and y-axis labels
#' @param pch \strong{p}lotting \strong{ch}aracter
#' @param cex numerical value giving the amount by which plotting text and
#' symbols should be magnified relative to the default; this starts as 1
#' when a device is opened and is reset when the layout is changed, e.g.,
#' by setting \code{mfrow}
#' 
#' @return
#' A table (invisibly) corresponding to the plot cell counts.
#' 
#' @references
#' \href{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}{Tatsuki
#' \code{dsplot}}
#' 
#' @examples
#' set.seed(1)
#' x <- round(rnorm(400, 100, 4))
#' y <- round(rnorm(400, 200, 4))
#' sex <- sample(c('Female', 'Male'), 400, replace = TRUE)
#' 
#' dsplot(x, y, col = (sex %in% 'Female') + 1L, bg.col = FALSE)
#' dsplot(x, y, col = (sex %in% 'Female') + 1L, bg.col = 'tomato')
#' 
#' dsplot(y ~ x, pch = 19, col = (sex %in% 'Female') + 1L,
#'        cex = 0.6, bty = 'l', bg.col = 'tomato',
#'        xlab = 'measurement 1', ylab = 'measurement 2')
#' legend('bottomright', pch = 19, col = 1:2, bty = 'n',
#'        legend = c('Male', 'Female'))
#'        
#' @export

dsplot <- function(x, ...) {
  UseMethod('dsplot')
}

#' @rdname dsplot
#' @export
dsplot.default <- function(x, y, ..., bg.col = TRUE, col = par('col'),
                           xlab = NULL, ylab = NULL,
                           pch = par('pch'), cex = par('cex')) {
  # if (any(x != round(x), na.rm = TRUE) | any(y != round(y), na.rm = TRUE))
  #   stop('\'x\' must be integer values', '\n')
  
  ## helpers
  localAxis   <- function(..., bg, bty, cex, log, lty, lwd, pos)
    axis(...)
  
  square.coordinates <- function(box.size) {
    x.c <- y.c <- 1
    for (i in 2:box.size)
      x.c <- c(x.c, every.other.element.x(i))
    for (j in 2:box.size)
      y.c <- c(y.c, every.other.element.y(j))
    data.frame(x.c, y.c)
  }
  
  ## 1, n, 2, n, 3, n, ...,  n, n for x
  ## n, 1, n, 2, n, 3, ..., n-1, n for y
  every.other.element.x <- function(n) head(c(rbind(seq.int(n), n)), -1L)
  every.other.element.y <- function(n) head(c(rbind(n, seq.int(n))), -1L)
  
  xy <- xy.coords(x, y, deparse(substitute(x)), deparse(substitute(y)))
  L <- length(x)
  cc <- complete.cases(x, y)
  
  pch <- rep_len(pch, L)
  col <- rep_len(col, L)
  cex <- rep_len(cex, L)
  
  x <- x[cc]
  y <- y[cc]
  X <- range(x) + c(0, 1)
  Y <- range(y) + c(0, 1)
  pch <- pch[cc]
  col <- col[cc]
  cex <- cex[cc]
  
  x.levels <- sort(unique(x))
  y.levels <- sort(unique(y))
  tab <- table(x, y)
  max.freq <- max(tab)
  box.size <- ceiling(sqrt(max.freq))
  
  
  plot(X, Y, type = 'n', xaxs = 'i', yaxs = 'i',
       ann = FALSE, xaxt = 'n', yaxt = 'n', ...)
  title(xlab = xlab %||% xy$xlab, ylab = ylab %||% xy$ylab)
  
  localAxis(1L, pretty(x) + 0.5, pretty(x), ...)
  localAxis(2L, pretty(y) + 0.5, pretty(y), ...)
  
  if (identical(bg.col, FALSE)) {
    abline(h = y.levels, col = grey(0.9))
    abline(v = x.levels, col = grey(0.9))
  }
  
  sc <- square.coordinates(box.size)
  coord <- (1:box.size) / (box.size + 1)
  off.set <- coord[1L] / 4
  alpha <- seq(0.2, 0.9, length = max.freq)
  bg.col <- if (isTRUE(bg.col))
    'grey' else bg.col[1L]
  
  dat <- data.frame(id = seq.int(length(x)), x, y)
  dat <- dat[order(dat$x, dat$y), ]
  within <- c(t(tab))
  within <- within[within > 0L]
  idx <- hm <- NULL
  
  for (i in within) {
    ## index within category
    idx <- c(idx, seq.int(i))
    hm  <- c(hm, rep(i, i))
  }
  dat$idx <- idx
  
  ## local offset
  dat$ly <- (box.size - ceiling(sqrt(hm))) / (box.size + 1) / 2
  dat$lx <- dat$ly + ((ceiling(sqrt(hm - 1)) ^ 2 == hm - 1) & (hm > 1)) /
    (box.size + 1) / 2
  dat <- dat[order(dat$id), ]
  dat$col <- col
  dat$pch <- pch
  
  if (!identical(bg.col, FALSE)) {
    for (ii in x.levels) {
      for (jj in y.levels) {
        n <- sum(x == ii & y == jj)
        if (n > 0) {
          col <- adjustcolor(bg.col, alpha[n])
          rect(ii + off.set, jj + off.set, ii + 1 - off.set, jj + 1 - off.set,
               border = col, col = col)
        }
      }
    }
  }
  
  points(
    dat$x + coord[sc[dat$idx, 1L]] + dat$lx,
    dat$y + coord[sc[dat$idx, 2L]] + dat$ly,
    pch = dat$pch, col = dat$col, cex = cex
  )
  
  invisible(
    table(factor(y, rev(min(y):max(y))), factor(x, min(x):max(x)))
  )
}

#' @rdname dsplot
#' @export
dsplot.formula <- function(formula, data = NULL, ...,
                           subset, na.action = NULL) {
  if (missing(formula) || (length(formula) != 3L))
    stop('\'formula\' missing or incorrect')
  
  enquote <- function(x) {
    as.call(list(as.name('quote'), x))
  }
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame(1L))))
    m$data <- as.data.frame(data)
  args <- lapply(m$..., eval, data, parent.frame(1L))
  nmargs <- names(args)
  
  form <- as.character(as.list(formula))
  args <- modifyList(list(xlab = form[3L], ylab = form[2L]), args)
  
  if ('main' %in% nmargs) args[['main']] <- enquote(args[['main']])
  if ('sub' %in% nmargs)  args[['sub']]  <- enquote(args[['sub']])
  if ('xlab' %in% nmargs) args[['xlab']] <- enquote(args[['xlab']])
  if ('ylab' %in% nmargs) args[['ylab']] <- enquote(args[['ylab']])
  
  # m$na.action <- na.pass
  m$... <- NULL
  
  m[[1L]] <- as.name('model.frame')
  m <- as.call(c(as.list(m), list(na.action = NULL)))
  mf <- eval(m, parent.frame(1L))
  n <- nrow(mf)
  response <- attr(attr(mf, 'terms'), 'response')
  
  do.call('dsplot', c(list(mf[[-response]], mf[[response]]), args))
}
