### some useful plots
# scattergram, dimr, plot.dimr, loess_smooth
# 
# s3 methods
# loess_smooth.default, loess_smooth.formula, plot.loess_smooth,
# print.loess_smooth
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
