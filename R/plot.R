### some useful plots
# scattergram, tsne, pca
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

#' t-SNE plot
#' 
#' Wrapper function to plot an \code{\link[Rtsne]{Rtsne}} object with optional
#' \code{\link[stats]{kmeans}} groupings. Alternatively, plot a 3D scatterplot
#' of the first three t-SNE dimensions.
#' 
#' @param data an \code{r x c} matrix-like object with \code{r} features and
#' \code{c} samples
#' @param group a length \code{c} vector identifying the group of each sample
#' @param col a vector of colors for each group
#' @param ... additional arguments passed to \code{\link{plot}} or further to
#' \code{\link{par}}
#' @param perplexity perplexity parameter passed to \code{\link[Rtsne]{Rtsne}}
#' @param centers an integer value for the number of desired clusters, passed
#' to \code{\link[stats]{kmeans}}
#' @param plot3d logical; if \code{TRUE}, the first three dimensions are drawn
#' as a 3D scatterplot; see \code{\link[rgl]{plot3d}}
#' @param labels argument controlling labels for points; if \code{FALSE}, no
#' labels are drawn; if a numeric value in \code{(0,1)}, points outside
#' quantile are labelled; otherwise, a vector of labels for each point
#' @param legend logical; if \code{TRUE}, a legend will be drawn
#' @param args.legend a \emph{named} list of \code{\link{legend}} arguments to
#' override defaults
#' 
#' @seealso
#' \code{\link{pca}}
#' 
#' @examples
#' ## feature x sample matrix
#' dat <- t(unique(iris[, 1:4]))
#' grp <- unique(iris)$Species
#' 
#' tsne(dat)
#' tsne(dat, grp)
#' tsne(dat, grp, centers = 3)$km$perplexity30$cluster
#' tsne(dat, grp, centers = 3, perplexity = c(5, 10, 20, 30), pch = 16)
#' tsne(dat, grp, centers = 3, perplexity = 20, col = tcol(3:5, alpha = 0.5),
#'      pch = 16, cex = 1.5, las = 1L, bty = 'l', labels = 0.001,
#'      args.legend = list(x = 'topleft', title = 'Species'))
#' 
#' @export

tsne <- function(data, group = rep(1L, ncol(data)), col = NULL, ...,
                 perplexity = 30, centers = NULL, plot3d = FALSE, labels = FALSE,
                 legend = length(perplexity) == 1L, args.legend = list()) {
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    palette('default')
  })
  
  if (is.numeric(col))
    col <- palette()[as.integer(col)]
  if (is.character(col))
    palette(col)
  
  group <- as.factor(group)
  colii <- as.integer(group)
  
  ## run tsne for each perplexity, create labels
  set.seed(1)
  res <- lapply(seq_along(perplexity), function(ii) {
    ts <- Rtsne::Rtsne(
      stats::dist(t(data)), dims = 3L, is_distance = TRUE,
      perplexity = perplexity[ii]
    )
    
    x <- ts$Y[, 1L]
    y <- ts$Y[, 2L]
    
    if (isTRUE(labels))
      labels <- FALSE
    lbl <- if (is.numeric(labels) & length(labels) == 1L) {
      q <- c(labels / 2, 1 - labels / 2)
      ifelse(x %inside% quantile(x, q) & y %inside% quantile(y, q),
             '', colnames(data))
    } else if (identical(labels, FALSE))
      NULL else rep_len(labels, length(x))
    
    structure(ts, labels = lbl)
  })
  
  if (plot3d) {
    which <- 1L
    co <- res[[which]]$Y[, 1:3]
    gr <- if (is.null(centers))
      colii else kmeans(co, centers, 10000L)$cluster
    
    rgl::plot3d(co, col = gr, type = 's', size = 1, scale = 0.2)
    return(invisible(NULL))
  }
  
  ## plot each tsne
  par(mfrow = rev(n2mfrow(length(perplexity))))
  kml <- vector('list', length(res))
  
  for (ii in seq_along(res)) {
    co <- res[[ii]]
    x <- co$Y[, 1L]
    y <- co$Y[, 2L]
    plot(x, y, col = colii, xlab = 't-SNE 1', ylab = 't-SNE 2', ...)
    text(x, y, labels = attr(co, 'labels'), pos = 3L, xpd = NA, cex = 0.7)
    
    if (!is.null(centers)) {
      xy <- cbind(x, y)
      kml[[ii]] <- km <- kmeans(xy, centers, 10000L)
      vegan::ordispider(
        xy, factor(km$cluster), label = TRUE, col = tcol('grey', alpha = 0.5))
      vegan::ordihull(xy, factor(km$cluster), lty = 'dashed', col = 'darkgrey')
    }
    
    p <- par('usr')
    largs <- list(
      x = p[1L], y = p[4L] + diff(p[3:4]) * 0.1, bty = 'n', horiz = TRUE,
      col = unique(colii), pch = 16, xpd = NA, legend = levels(group)
    )
    if (!islist(args.legend))
      args.legend <- list()
    if (legend)
      do.call('legend', modifyList(largs, args.legend))
    
    mtext(sprintf('perplexity = %s', perplexity[ii]), adj = 1)
  }
  
  res <- setNames(res, paste0('perplexity', perplexity))
  kml <- if (!is.null(centers))
    setNames(kml, paste0('perplexity', perplexity)) else NULL
  
  invisible(list(tsne = res, km = kml))
}

#' PCA plot
#' 
#' Wrapper function to plot a \code{\link{prcomp}} or \code{\link[rsvd]{rpca}}
#' object with optional \code{\link[stats]{kmeans}} groupings. Alternatively,
#' plot a 3D scatterplot of the first three principal components.
#' 
#' @param data an \code{r x c} matrix-like object with \code{r} features and
#' \code{c} samples
#' @param group a length \code{c} vector identifying the group of each sample
#' @param col a vector of colors for each group
#' @param ... additional arguments passed to \code{\link{plot}} or further to
#' \code{\link{par}}
#' @param centers an integer value for the number of desired clusters, passed
#' to \code{\link[stats]{kmeans}}
#' @param plot3d logical; if \code{TRUE}, the first three dimensions are drawn
#' as a 3D scatterplot; see \code{\link[rgl]{plot3d}}
#' @param labels argument controlling labels for points; if \code{FALSE}, no
#' labels are drawn; if a numeric value in \code{(0,1)}, points outside
#' quantile are labelled; otherwise, a vector of labels for each point
#' @param rpca logical; if \code{TRUE}, \code{\link[rsvd]{rpca}} is used to
#' calculate the principal components
#' @param legend logical; if \code{TRUE}, a legend will be drawn
#' @param args.legend a \emph{named} list of \code{\link{legend}} arguments to
#' override defaults
#' 
#' @seealso
#' \code{\link{tsne}}
#' 
#' @examples
#' ## feature x sample matrix
#' dat <- t(unique(iris[, 1:4]))
#' grp <- unique(iris)$Species
#' 
#' pca(dat)
#' pca(dat, grp)
#' pca(dat, grp, rpca = TRUE)
#' pca(dat, grp, centers = 3, pch = 16)$km$cluster
#' pca(dat, grp, centers = 3, col = tcol(3:5, alpha = 0.5),
#'     pch = 16, cex = 1.5, las = 1L, bty = 'l', labels = 0.001,
#'     args.legend = list(x = 'topleft', title = 'Species'))
#' 
#' @export

pca <- function(data, group = rep(1L, ncol(data)), col = NULL, ...,
                centers = NULL, plot3d = FALSE, labels = FALSE, rpca = FALSE,
                legend = TRUE, args.legend = list()) {
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    palette('default')
  })
  
  if (is.numeric(col))
    col <- palette()[as.integer(col)]
  if (is.character(col))
    palette(col)
  
  group <- as.factor(group)
  colii <- as.integer(group)
  
  ## choose which pca fn to use
  res <- if (rpca)
    rsvd::rpca(t(data), retx = TRUE, k = 3L)
  else prcomp(t(data), retx = TRUE, center = TRUE, scale. = TRUE)
  
  x <- res$x[, 1L]
  y <- res$x[, 2L]
  
  if (isTRUE(labels))
    labels <- FALSE
  lbl <- if (is.numeric(labels) & length(labels) == 1L) {
    q <- c(labels / 2, 1 - labels / 2)
    ifelse(x %inside% quantile(x, q) & y %inside% quantile(y, q),
           '', colnames(data))
  } else if (identical(labels, FALSE))
    NULL else rep_len(labels, length(x))
  
  res <- structure(res, labels = lbl)
  
  if (plot3d) {
    co <- res$x[, 1:3]
    gr <- if (is.null(centers))
      colii else kmeans(co, centers, 10000L)$cluster
    
    rgl::plot3d(co, col = gr, type = 's', size = 1, scale = 0.2)
    return(invisible(NULL))
  }
  
  par(mfrow = c(1, 1))
  plot(x, y, col = colii, xlab = 'PC 1', ylab = 'PC 2', ...)
  text(x, y, labels = attr(res, 'labels'), pos = 3L, xpd = NA, cex = 0.7)
  
  km <- NULL
  if (!is.null(centers)) {
    xy <- cbind(x, y)
    km <- kmeans(xy, centers, 10000L)
    vegan::ordispider(
      xy, factor(km$cluster), label = TRUE, col = tcol('grey', alpha = 0.5)
    )
    vegan::ordihull(xy, factor(km$cluster), lty = 'dashed', col = 'darkgrey')
  }
  
  p <- par('usr')
  largs <- list(
    x = p[1L], y = p[4L] + diff(p[3:4]) * 0.1, bty = 'n', horiz = TRUE,
    col = unique(colii), pch = 16, xpd = NA, legend = levels(group)
  )
  if (!islist(args.legend))
    args.legend <- list()
  if (legend)
    do.call('legend', modifyList(largs, args.legend))
  
  invisible(list(pca = res, km = km))
}
