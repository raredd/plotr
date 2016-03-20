## sort and plot a matrix of integers

## helpers - required
##   psum - pairwise sum of vectors
##   sort_matrix - sort matrix by values in rows or columns
##   waffle - create a "waffle chart"

## misc - required for vis
##   rawr::tcol - add transparency to colors
##   rawr::rescaler - rescale vector of values


psum <- function(..., na.rm = FALSE) {
  ## rawr::psum - pairwise sum of vectors
  ## psum(c(-1, NA, 4, 5), c(NA, NA, 6, -1))
  dat <- do.call('cbind', list(...))
  res <- rowSums(dat, na.rm = na.rm) 
  idx_na <- !rowSums(!is.na(dat))
  res[idx_na] <- NA
  res 
}

insert_matrix <- function(matrix, rowsep, colsep, rrepl = NA, crepl = NA) {
## rawr::insert_matrix
  # insert_matrix(m, 4, c(5, 7))
  # insert_matrix(m, 4, c(5, 7), sample(22), 0)
  im_ <- function(m, idx, repl = NA) {
    nr <- nrow(m)
    ii <- sort(c(1:ncol(m), idx))
    m <- m[, ii]
    ri <- idx + seq_along(idx) - 1
    ii <- cbind(rep(1:nr, length(idx)), rep(ri, each = nr))
    m[ii] <- repl
    if (!is.null(colnames(m)))
      colnames(m)[ri] <- ''
    m
  }
  if (!missing(rowsep))
    matrix <- t(im_(t(matrix), rowsep, crepl))
  if (!missing(colsep))
    matrix <- im_(matrix, colsep, rrepl)
  matrix
}

sort_matrix <- function(m, margin = 1L, order) {
  ## rawr::sort_matrix - sort a matrix of integers by row or column values
  # m <- matrix(rpois(5 * 10, 1), 5)
  # sort_matrix(m)
  stopifnot(sum(1:2 == margin) == 1L)
  m <- if (margin == 1L)
    as.matrix(m) else t(as.matrix(m))
  if (missing(order))
    order <- sort(unique(c(m)), decreasing = TRUE)
  # stopifnot(length(order) != length(unique(m)))
  dd <- data.frame(t(m))
  dd[] <- lapply(dd, factor, levels = order)
  m <- m[, do.call('order', dd)]
  if (margin == 1L)
    m else t(m)
}

waffle <- function(mat, xpad = 0.05, ypad = 0.05, ..., reset_par = TRUE) {
  ## rawr::waffle - create a waffle chart
  ## waffle(matrix(1:4, 1))
  o <- cbind(c(row(mat)), c(col(mat))) - 1
  op <- par(no.readonly = TRUE)
  if (reset_par)
    on.exit(par(op))
  par(list(...))
  plot.new()
  plot.window(xlim = c(0, max(o[, 2]) + 1), ylim = c(0, max(o[, 1]) + 1),
              xaxs = 'i', yaxs = 'i')
  rect(xl <- o[, 2], yb <- o[, 1], xr <- o[, 2] + (1 - xpad),
       yt <- o[, 1] + (1 - ypad), col = c(mat), border = NA)
  invisible(list(matrix = mat, origin = `colnames<-`(o[, 2:1], c('x','y')),
                 centers = cbind(x = psum(xl, xr) / 2, y = psum(yb, yt) / 2)))
}

## worker fn
do_waffle <- function(matrix, rowsep, colsep, reset_par = TRUE,
                      xpad = 0.05, ypad = xpad, ...) {
  mat <- insert_matrix(as.matrix(matrix), rowsep, colsep)
  # mat <- mat[nrow(mat):1, ]
  op <- par(no.readonly = TRUE)
  if (reset_par)
    on.exit(par(op))
  par(mar = c(1,4,8.5,1))
  par(list(...))
  w <- waffle(mat, xpad, ypad, reset_par = FALSE)
  box()
  text(0, unique(w$centers[, 'y']), rownames(mat), xpd = NA, pos = 2, font = 4)
  text(unique(w$centers[, 'x']), par('usr')[4] + .2, labels = colnames(mat),
       xpd = NA, srt = 90, adj = 0, col = pmax(mat['total', ], 1))
  abline(h = 1, lwd = 3)
  invisible(w)
}


# pdf('~/desktop/tmp.pdf', height = 5, width = 10)
## generate data
mat <- +(t(mtcars[, c('cyl','drat','wt','gear','carb')]) > 4)
mat <- sort_matrix(mat)[nrow(mat):1, ]
mat <- rbind(total = colSums(mat), mat)
do_waffle(mat)
do_waffle(mat, colsep = c(8, 29), family = 'mono', xpad = 0)

## custom colors
un <- sort(unique(c(mat)))
mat2 <- setNames(un, c('grey90','tomato','dodgerblue2','pink'))
mat2 <- matrix(names(mat2[c(mat) + 1]), nrow(mat), dimnames = dimnames(mat))
do_waffle(mat2, reset_par = FALSE)
rect(c(1, 21), 0, c(5, 22), c(10.8, 10), xpd = NA)

## for return value, see ?waffle
w <- do_waffle(cbind(mat, mat), colsep = 33, reset_par = FALSE)
abline(v = ncol(w$matrix) / 2, lwd = 10, col = 'lightgrey')

## add additional boxes for whatever
par(fig = c(0,1,.2,1))
w <- do_waffle(cbind(mat, mat), colsep = 33, mar = c(0,4,9,1))
cols <- c('dodgerblue2','tomato','purple')
mat3 <- mtcars[colnames(w$matrix), c('mpg', 'wt', 'hp')]
mat3[] <- lapply(seq_along(mat3), function(x)
  rawr::tcol(cols[x], alpha = rescaler(mat3[, x], c(.2, .8))))
par(fig = c(0,1,0,.2), mar = c(1/2,4,1/2,1), new = TRUE)
w <- waffle(t(mat3), reset_par = FALSE)
text(0, w$centers[1:3, 'y'], c('mpg', 'wt', 'hp'), pos = 2, xpd = NA, cex = .8)

# dev.off()

