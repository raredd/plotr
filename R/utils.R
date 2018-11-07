### utils
# plotr_utils, %ni%, %inside%, %||%, tcol, rescaler, assert_class, coords,
# islist
#
# coordinate systems: d2r, r2d, p2c, c2p, p2r, p2d
###


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
#' @param alpha the alpha transparency in \code{[0,1]}; \code{trans} is
#' ignored if \code{alpha} is given
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
'%ni%' <- function(x, table) {
  !(match(x, table, nomatch = 0) > 0)
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

#' @rdname plotr_utils
#' @export
tcol <- function(color, trans = 255, alpha) {
  stopifnot(
    trans %inside% c(0,255) | is.na(trans)
  )
  if (!missing(alpha)) {
    stopifnot(alpha %inside% 0:1 | is.na(alpha))
    trans <- round(rescaler(alpha, to = c(0,255), from = 0:1))
  }
  if (length(color) != length(trans) & 
      !any(c(length(color), length(trans)) == 1))
    stop('Vector lengths are not comformable')
  if (length(color) == 1L & length(trans) > 1L)
    color <- rep(color, length(trans))
  if (length(trans) == 1L & length(color) > 1L)
    trans <- rep(trans, length(color))
  
  if (length(nocol <- which(color == 0))) {
    color[nocol] <- 1
    trans[nocol] <- NA
  }
  
  res <- paste0('#', apply(apply(rbind(col2rgb(color)), 2, function(x)
    format(as.hexmode(x), 2)), 2, paste, collapse = ''))
  res <- unlist(Map(paste0, res, as.character(try(as.hexmode(trans)))))
  res[is.na(color) | is.na(trans)] <- NA
  res[color %in% 'transparent'] <- 'transparent'
  
  unname(res)
}

rescaler <- function (x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  ## rawr::rescaler
  zero_range <- function(x, tol = .Machine$double.eps * 100) {
    if (length(x) == 1L)  return(TRUE)
    if (length(x) != 2L)  stop('\'x\' must be length one or two')
    if (any(is.na(x)))    return(NA)
    if (x[1L] == x[2L])   return(TRUE)
    if (all(is.infinite(x))) return(FALSE)
    m <- min(abs(x))
    if (m == 0) return(FALSE)
    abs((x[1L] - x[2L]) / m) < tol
  }
  
  if (zero_range(from) || zero_range(to))
    return(rep(mean(to), length(x)))
  
  (x - from[1L]) / diff(from) * diff(to) + to[1L]
}

col_scaler <- function(x, colors, alpha = 1,
                       alpha.min = min(0.1, x[x >= 0], na.rm = TRUE),
                       to = c(0, 1), from = range(x, na.rm = TRUE)) {
  ## rawr::col_scaler
  pals <- c('rainbow', paste0(c('heat', 'terrain', 'topo', 'cm'), '.colors'))
  colors <- if (is.numeric(colors))
    rep_len(palette(), max(colors, na.rm = TRUE))[as.integer(colors)]
  else if (inherits(colors, 'function'))
    colors
  else if (colors[1L] %in% pals)
    get(colors, mode = 'function')
  else as.character(colors)
  
  x <- if (is.factor(x) || is.character(x) || is.integer(x))
    as.integer(as.factor(x)) else as.numeric(x)
  
  ## add alpha
  if (is.character(colors) & length(colors) == 1L)
    return(tcol(colors, alpha = rescaler(x, c(alpha.min, to[2L]), from)))
  
  ## use interpolation
  n  <- 10000L
  to <- to * n
  x  <- rescaler(x, to, from)
  x  <- as.integer(x) + 1L
  
  colors <- if (inherits(colors, 'function'))
    colors(n + 1L)[x]
  else colorRampPalette(colors)(n + 1L)[x]
  
  if (!all(alpha == 1))
    tcol(colors, alpha = rep_len(alpha, length(colors)))
  else tolower(colors)
}

assert_class <- function(x, class, which = FALSE,
                         message = NULL, warn = FALSE) {
  name <- substitute(x)
  FUN <- if (warn)
    function(...) warning(..., call. = FALSE)
  else function(...) stop(..., call. = FALSE)
  
  if (is.null(message))
    message <- paste(shQuote(name), 'is not of class',
                     toString(shQuote(class)))
  
  if (!all(inherits(x, class, which)))
    FUN(message)
  invisible(TRUE)
}


## coordinate systems

## convert degrees to radians or vice versa
d2r <- function(degrees = 1) {
  degrees * (pi / 180)
}
r2d <- function(radians = 1) {
  radians * (180 / pi)
}

## convert polar to cartesian or vice versa
p2c <- function(radius, theta, degree = FALSE) {
  # p2c(c2p(0, 1)$r, c2p(0, 1)$t)
  if (degree)
    theta <- d2r(theta)
  list(x = radius * cos(theta),
       y = radius * sin(theta))
}
c2p <- function(x, y, degree = FALSE) {
  # c2p(p2c(1, 30, TRUE)$x, p2c(1, 30, TRUE)$y, TRUE)
  list(radius = sqrt(x ** 2 + y ** 2),
       theta = atan2(y, x) * if (degree) r2d() else 1)
}

## x,y coords to radians/degrees
p2r <- function(x, y, cx = 0, cy = 0) {
  # p2r(0,1)
  atan2(y - cy, x - cx)
  # ifelse(r < 0, pi / 2 + abs(r), r)
}
p2d <- function(x, y, cx = 0, cy = 0) {
  # p2d(0,1)
  r2d(atan2(y - cy, x - cx))
}

coords <- function(x = 0:1, y = x, to = 'user', line, side) {
  ## rawr::coords
  xy <- cbind(x, y)
  x  <- xy[, 1L]
  y  <- xy[, 2L]
  
  if (!missing(line) | !missing(side)) {
    lh <- par('cin')[2L] * par('cex') * par('lheight')
    
    sapply(line, function(li) {
      li <- li + 0.5
      x  <- diff(grconvertX(x, 'in', 'user')) * lh * li
      y  <- diff(grconvertY(y, 'in', 'user')) * lh * li
      
      (par('usr')[c(3, 1, 4, 2)] + c(-y, -x, y, x))[match(side, 1:4)]
    })
  } else
    list(
      plot   = list(x = grconvertX(x, 'npc', to), y = grconvertY(y, 'npc', to)),
      figure = list(x = grconvertX(x, 'nfc', to), y = grconvertY(y, 'nfc', to)),
      inner  = list(x = grconvertX(x, 'nic', to), y = grconvertY(y, 'nic', to)),
      device = list(x = grconvertX(x, 'ndc', to), y = grconvertY(y, 'ndc', to))
    )
}

islist <- function(x) {
  ## is.list(data.frame()); plotr:::islist(data.frame())
  inherits(x, 'list')
}
