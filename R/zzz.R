## utils

#' plotr utils
#' 
#' Some utilities for \code{plotr}.
#' 
#' \code{\%ni\%} is the negation of \code{\link[base]{\%in\%}}.
#' 
#' \code{\%inside\%} returns a logical vector indicating if \code{x} is inside
#' the \code{interval} (inclusive).
#' 
#' \code{\%||\%} is useful for a function, \code{f}, that may return a value
#' or \code{NULL}, but if \code{NULL} is the result of \code{f}, it is
#' desirable to return some other default value without errors.
#' 
#' \code{tcol} adds transparency to colors (vectorized).
#' 
#' @param x vector or \code{NULL}; the values to be matched
#' @param table vector or \code{NULL}; the values to be matched against
#' @param interval numeric vector of length two representing the interval
#' @param a,b raw, logical, "number-like" vectors or objects
#' @param color vector of color names (or hexadecimal)
#' @param trans transparency defined as an integer in the range 
#' \code{[0, 255]} where \code{0} is fully transparent and \code{255} is fully
#' visible
#' 
#' @aliases oror %||% notin %ni% inside %inside% tcol
#' @seealso \code{\link{\%in\%}}, \code{\link{||}}
#' @name plotr_utils
#' 
#' @examples
#' \dontrun{
#' 1:5 %ni% 3:5
#' 
#' c(0,4) %inside% c(0, 4)
#' -5:5 %inside% c(0,5)
#' -5:5 %inside% c(5,0)
#' 
#' f <- function(x0 = TRUE) NULL || x0
#' f() # error
#' f <- function(x0 = TRUE) NULL %||% x0
#' f() # TRUE
#' 
#' cols <- c('red','green','blue')
#' 
#' # a normal plot
#' plot(rnorm(100), col = tcol(cols), pch = 16, cex = 4)
#' 
#' # more transparent
#' plot(rnorm(100), col = tcol(cols, 100), pch = 16, cex = 4)
#' 
#' # hexadecimal colors also work
#' cols <- c('#FF0000','#00FF00','#0000FF')
#' plot(rnorm(100), col = tcol(cols, c(50, 100, 255)), pch= 16, cex = 4)
#' }
NULL

#' @rdname plotr_utils
#' @export
'%ni%' <- function(x, table) !(match(x, table, nomatch = 0) > 0)

#' @rdname plotr_utils
#' @export
'%inside%' <- function(x, interval) {
  interval <- sort(interval)
  x >= interval[1] & x <= interval[2]
}

#' @rdname plotr_utils
#' @export
'%||%' <- function(a, b) if (!is.null(a)) a else b

#' @rdname plotr_utils
#' @export
tcol <- function(color, trans = 255) {
  if (length(color) != length(trans) & 
        !any(c(length(color), length(trans)) == 1)) 
    stop('Vector lengths not correct')
  if (length(color) == 1 & length(trans) > 1) 
    color <- rep(color, length(trans))
  if (length(trans) == 1 & length(color) > 1) 
    trans <- rep(trans, length(color))
  res <- paste0('#', apply(apply(rbind(col2rgb(color)), 2, function(x) 
    format(as.hexmode(x), 2)), 2, paste, collapse = ''))
  res <- unlist(unname(Map(paste0, res, as.character(as.hexmode(trans)))))
  res[is.na(color)] <- NA
  return(res)
}
