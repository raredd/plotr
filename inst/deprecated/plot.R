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
#' tsne(dat, grp, centers = 3, perplexity = 20, col = adjustcolor(3:5, alpha.f = 0.5),
#'      pch = 16, cex = 1.5, las = 1L, bty = 'l', labels = 0.001,
#'      args.legend = list(x = 'topleft', title = 'Species'))
#' 
#' @export

# op <- par(no.readonly = TRUE)
# g2 <- rep(as.list(data.frame(t(dat))), 5)[1]
# m <- n2mfrow(length(g2))
# m <- matrix(seq.int(prod(m)), m)
# m <- cbind(1, m + 1L)
# l <- layout(m, heights = 1, widths = c(2, rep(0.5, 4)))
# l <- layout(m, heights = 1, widths = c(2, 2))
# par(oma = par('mar'), mar = c(0,0,0,1))
# tsne(dat, grp, group2 = g2, legend = FALSE, perplexity = 20)
# par(op)

tsne <- function(data, group = rep(1L, ncol(data)), col = NULL, ..., group2 = NULL,
                 perplexity = 30, centers = NULL, plot3d = FALSE, labels = FALSE,
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
  perplexity <- perplexity[1L]
  
  group <- as.factor(group)
  colii <- as.integer(group)
  
  ## run tsne for each perplexity, create labels
  set.seed(1)
  res <- Rtsne::Rtsne(
    stats::dist(t(data)), dims = 3L, is_distance = TRUE, perplexity = perplexity
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
  
  res <- structure(ts, labels = lbl, xyz = ts$Y[, 1:3])
  
  if (plot3d) {
    co <- res[[1L]]$Y[, 1:3]
    gr <- if (is.null(centers))
      colii else kmeans(co, centers, 1e4L)$cluster
    
    rgl::plot3d(co, col = gr, type = 's', size = 1, scale = 0.2)
    return(invisible(list(tsne = res, km = NULL)))
  }
  
  ## plot each tsne
  if (length(perplexity) < 1L)
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
      kml[[ii]] <- km <- kmeans(xy, centers, 1e4L)
      vegan::ordispider(
        xy, factor(km$cluster), label = TRUE, col = adjustcolor('grey', alpha.f = 0.5))
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
  
  if (!is.null(group2))
    int(group2, x, y, legend = TRUE)
  
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
#' pca(dat, grp, centers = 3, col = adjustcolor(3:5, alpha.f = 0.5),
#'     pch = 16, cex = 1.5, las = 1L, bty = 'l', labels = 0.001,
#'     args.legend = list(x = 'topleft', title = 'Species'))
#' 
#' 
#' op <- par(no.readonly = TRUE)
#' n <- nrow(dat)
#' n <- 1
#' m <- n2mfrow(n)
#' m <- matrix(seq.int(prod(m)), m[1L], m[2L])
#' m <- cbind(1, m + 1L)
#' l <- layout(m, heights = 1, widths = c(2, 1, 1))
#' par(oma = par('mar'), mar = c(0,0,0,1))
#' pca(dat, grp, group2 = as.list(data.frame(t(dat))[seq.int(n)]))
#' par(op)
#' 
#' op <- par(no.readonly = TRUE)
#' g2 <- rep(as.list(data.frame(t(dat))), 5)
#' m <- n2mfrow(length(g2))
#' m <- matrix(seq.int(prod(m)), m)
#' m <- cbind(1, m + 1L)
#' l <- layout(m, heights = 1, widths = c(2, rep(0.5, 4)))
#' par(oma = par('mar'), mar = c(0,0,0,1))
#' pca(dat, grp, group2 = g2, legend = FALSE)
#' par(op)
#' 
#' @export

pca <- function(data, group = rep(1L, ncol(data)), col = NULL, ..., group2 = NULL,
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
      colii else kmeans(co, centers, 1e4L)$cluster
    
    rgl::plot3d(co, col = gr, type = 's', size = 1, scale = 0.2)
    return(invisible(NULL))
  }
  
  plot(x, y, col = colii, xlab = 'PC 1', ylab = 'PC 2', ...)
  text(x, y, labels = attr(res, 'labels'), pos = 3L, xpd = NA, cex = 0.7)
  
  km <- NULL
  if (!is.null(centers)) {
    xy <- cbind(x, y)
    km <- kmeans(xy, centers, 1e4L)
    vegan::ordispider(
      xy, factor(km$cluster), label = TRUE, col = adjustcolor('grey', alpha.f = 0.5)
    )
    vegan::ordihull(xy, factor(km$cluster), lty = 'dashed', col = 'darkgrey')
  }
  
  p <- par('usr')
  largs <- list(
    x = p[1L], y = p[4L] + diff(p[3:4]) * 0.1, bty = 'n', horiz = TRUE,
    col = unique(colii), pch = 16L, xpd = NA, legend = levels(group)
  )
  if (!islist(args.legend))
    args.legend <- list()
  if (legend)
    do.call('legend', modifyList(largs, args.legend))
  
  if (!is.null(group2))
    int(group2, x, y, legend = TRUE)
  
  invisible(list(pca = res, km = km))
}

int <- function(data, x, y, col = c('blue', 'red'), cex = c(0.5, 2),
                legend = FALSE, alpha = 0.5, ...) {
  data <- if (islist(data))
    data else list(data)
  ok <- length(data) > 1L
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  if (ok) {
    p <- op$mar
    pad <- c(0,0)
    pad <- rep_len(pad, 2L)
    mat <- n2mfrow(length(data))
    legend <- FALSE
  }
  
  legend <- rep_len(legend, length(data))
  lapply(seq_along(data), function(ii) {
    dd <- data[[ii]]
    cc <- rawr::col_scaler(dd, col, alpha = alpha)
    cx <- rawr::rescaler(dd, cex)
    if (!ok)
      plot(x, y, xlab = 'PC 1', ylab = 'PC 2', ..., type = 'n')
    else {
      mar <- switch(
        fig(ii, m[, -1L, drop = FALSE] - 1L),
        c(0, pad[1L], p[3L], 0),
        c(0, 0, p[3L], 0),
        c(0, 0, p[3L], pad[2L]),
        c(0, pad[1L], 0, 0),
        c(0, 0, 0, 0),
        c(0, 0, 0, pad[2L]),
        c(p[1L], pad[1L], 0, 0),
        c(p[1L], 0, 0, 0),
        c(p[1L], 0, 0, pad[2L])
      )
      par(mar = mar)
      plot.new()
      plot.window(extendrange(x), extendrange(y))
      box()
    }
    points(x, y, col = cc, cex = cx, pch = 16L)
    mtext(names(data)[ii], line = -1)
    
    p <- par('usr')
    largs <- list(
      x = p[1L], y = p[4L] + diff(p[3:4]) * 0.1, bty = 'n', horiz = TRUE,
      col = c(col, 'black', 'black'), pch = 16L, pt.cex = c(3, 3, 1, 4) / 2,
      legend = c('Low', 'High', 'Less', 'More'), xpd = NA
    )
    
    if (legend[ii])
      do.call('legend', largs)
  })
  
  xy <- unlist(coords(0.51, 0.95)$device)
  largs <- list(
    x = xy[1L], y = xy[2L], bty = 'n', horiz = TRUE,
    col = c(col, 'black', 'black'), pch = 16L, pt.cex = c(3, 3, 1, 4) / 2,
    legend = c('Low', 'High', 'Less', 'More'), xpd = NA
  )
  
  if (1)
    do.call('legend', largs)
  
  invisible(NULL)
}

int <- function(data, x, y, col = c('blue', 'red'), cex = c(0.5, 2),
                legend = FALSE, alpha = 0.5, ...) {
  data <- if (islist(data))
    data else list(data)
  ok <- length(data) > 1L
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  if (ok) {
    p <- op$mar
    pad <- c(0,0)
    pad <- rep_len(pad, 2L)
    mat <- n2mfrow(length(data))
    legend <- FALSE
  }
  
  legend <- rep_len(legend, length(data))
  lapply(seq_along(data), function(ii) {
    dd <- data[[ii]]
    cc <- rawr::col_scaler(dd, col, alpha = alpha)
    cx <- rawr::rescaler(dd, cex)
    if (!ok)
      plot(x, y, xlab = 'PC 1', ylab = 'PC 2', ..., type = 'n')
    else {
      mar <- switch(
        fig(ii, m[, -1L, drop = FALSE] - 1L),
        c(0, pad[1L], p[3L], 0),
        c(0, 0, p[3L], 0),
        c(0, 0, p[3L], pad[2L]),
        c(0, pad[1L], 0, 0),
        c(0, 0, 0, 0),
        c(0, 0, 0, pad[2L]),
        c(p[1L], pad[1L], 0, 0),
        c(p[1L], 0, 0, 0),
        c(p[1L], 0, 0, pad[2L]),
        c(0, p[2:4]),
        c(0, p[2L], 0, p[4L]),
        c(p[1:2], 0, p[4L]),
        c(p[1:3], 0),
        c(p[1L], 0, p[3L], 0),
        c(p[1L], 0, p[3:4])
      )
      par(mar = mar)
      plot.new()
      plot.window(extendrange(x), extendrange(y))
      box()
    }
    points(x, y, col = cc, cex = cx, pch = 16L)
    mtext(names(data)[ii], line = -1)
    
    p <- par('usr')
    largs <- list(
      x = p[1L], y = p[4L] + diff(p[3:4]) * 0.1, bty = 'n', horiz = TRUE,
      col = c(col, 'black', 'black'), pch = 16L, pt.cex = c(3, 3, 1, 4) / 2,
      legend = c('Low', 'High', 'Less', 'More'), xpd = NA
    )
    
    # if (legend[ii])
    #   do.call('legend', largs)
  })
  
  xy <- unlist(coords(0.51, 0.95)$device)
  largs <- list(
    x = xy[1L], y = xy[2L], bty = 'n', horiz = TRUE,
    col = c(col, 'black', 'black'), pch = 16L, pt.cex = c(3, 3, 1, 4) / 2,
    legend = c('Low', 'High', 'Less', 'More'), xpd = NA
  )
  
  if (1)
    do.call('legend', largs)
  
  invisible(NULL)
}
