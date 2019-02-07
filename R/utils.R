### utils
# assert_class, islist, rescale, rm_alpha
# 
# plotr_utils:
# %ni%, %inside%, %||%
#
# coordinate systems:
# d2r, r2d, p2c, c2p, p2r, p2d
###


assert_class <- function(x, class, which = FALSE,
                         message = NULL, warn = FALSE) {
  name <- substitute(x)
  FUN  <- function(...) {
    if (warn)
      warning(..., call. = FALSE)
    else stop(..., call. = FALSE)
  }
  
  if (is.null(message))
    message <- paste(
      shQuote(name),
      'is not of class',
      toString(shQuote(class))
    )
  
  if (!all(inherits(x, class, which)))
    FUN(message)
  
  invisible(TRUE)
}

islist <- function(x) {
  ## is.list(data.frame()); plotr:::islist(data.frame())
  inherits(x, 'list')
}

rescale <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  (x - from[1L]) / diff(from) * diff(to) + to[1L]
}

rm_alpha <- function(x) {
  gsub('(^#[A-Fa-f0-9]{6})[A-Fa-f0-9]{2}$', '\\1', x)
}

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
#' @param x vector or \code{NULL}; the values to be matched
#' @param table vector or \code{NULL}; the values to be matched against
#' @param interval numeric vector of length two representing the interval
#' @param a,b raw, logical, "number-like" vectors or objects
#' 
#' @aliases oror %||% notin %ni% inside %inside%
#' @seealso \code{\link{\%in\%}}, \code{\link{||}}
#' @name plotr_utils
#' 
#' @examples
#' 1:5 %ni% 3:5
#' 
#' c(0,4) %inside% c(0, 4)
#' -5:5 %inside% c(0, 5)
#' -5:5 %inside% c(5, 0)
#' 
#' # NULL || TRUE ## error
#' NULL %||% TRUE ## TRUE
NULL

#' @rdname plotr_utils
#' @export
'%ni%' <- function(x, table) {
  !(match(x, table, nomatch = 0L) > 0L)
}

#' @rdname plotr_utils
#' @export
'%inside%' <- function(x, interval) {
  interval <- sort(interval)
  x >= interval[1L] & x <= interval[2L]
}

#' @rdname plotr_utils
#' @export
'%||%' <- function(a, b) {
  if (!is.null(a)) a else b
}


## coordinate systems

## convert degrees to radians or vice versa
d2r <- function(degrees = 1) {
  # plotr:::d2r(180)
  degrees * (pi / 180)
}
r2d <- function(radians = 1) {
  # plotr:::r2d(pi)
  radians * (180 / pi)
}

## convert polar to cartesian or vice versa
p2c <- function(radius, theta, degree = FALSE) {
  # plotr:::p2c(plotr:::c2p(0, 1)$r, plotr:::c2p(0, 1)$t)
  if (degree)
    theta <- d2r(theta)
  list(x = radius * cos(theta), y = radius * sin(theta))
}
c2p <- function(x, y, degree = FALSE) {
  # plotr:::c2p(plotr:::p2c(1, 30, TRUE)$x, plotr:::p2c(1, 30, TRUE)$y, TRUE)
  list(radius = sqrt(x ** 2 + y ** 2),
       theta = atan2(y, x) * if (degree) r2d() else 1)
}

## x,y coords to radians/degrees
p2r <- function(x, y, cx = 0, cy = 0) {
  # plotr:::p2r(0, 1)
  atan2(y - cy, x - cx)
  # ifelse(r < 0, pi / 2 + abs(r), r)
}
p2d <- function(x, y, cx = 0, cy = 0) {
  # plotr:::p2d(0, 1)
  r2d(atan2(y - cy, x - cx))
}
