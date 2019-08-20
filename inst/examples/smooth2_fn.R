### smooth functions
# loess_smooth, plot.loess_smooth
###


loess_smooth <- function(x, y = NULL, ..., conf = 0.95, n = 1000L, sd = TRUE,
                         fix_extremes = FALSE) {
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
  lr <- loess(I(residuals(lo) ^ 2) ~ xx, ...)
  
  se <- if (sd)
    sd <- sqrt(pmax(0, predict(lr, data.frame(xx = x0))))
  else pr$se.fit
  
  if (fix_extremes) {
    x0[c(1L, n)] <- xy$x[c(1L, length(xy$x))]
    pr$fit[c(1L, n)] <- xy$y[c(1L, length(xy$y))]
  }
  
  res <- list(
    model = lo, x = x0, y = pr$fit, sd = sd, xy = xy,
    lower = pr$fit - ci * se,
    upper = pr$fit + ci * se,
    call = match.call()
  )
  
  invisible(
    structure(res, class = 'loess_smooth')
  )
}

plot.loess_smooth <- function(x, y, ..., xlab = NULL, ylab = NULL, gg = FALSE) {
  stopifnot(
    inherits(x, 'loess_smooth')
  )
  
  '%||%' <- function(x, y)
    if (is.null(x)) y else x
  do_poly <- function(x, y1, y2) {
    polygon(c(x, rev(x)), c(y1, rev(y2)), border = NA,
            col = adjustcolor('grey', alpha.f = 0.4))
  }
  
  x$xy$xlab <- xlab %||% x$xy$xlab %||% 'x'
  x$xy$ylab <- ylab %||% x$xy$ylab %||% 'y'
  
  if (gg) {
    require('ggplot2', character.only = TRUE)
    
    dat <- setNames(
      data.frame(x$xy[c('x', 'y')]),
      unlist(x$xy[c('xlab', 'ylab')]) %||% c('x', 'y')
    )
    rib <- data.frame(x[c('x', 'lower', 'upper')])
    names(rib)[1L] <- names(dat)[1L]
    
    gg <- ggplot(dat, aes_string(names(dat)[1L], names(dat)[2L])) +
      labs(x = xlab %||% names(dat)[1L], y = ylab %||% names(dat)[2L]) +
      geom_point() +
      geom_smooth(method = 'loess', se = FALSE) +
      geom_ribbon(data = rib,
                  aes_string(names(rib)[1L], ymin = 'lower', ymax = 'upper'),
                  alpha = 0.2, inherit.aes = FALSE)
    return(gg)
  }
  
  op <- par(mar = c(4, 4, 1, 1) + 0.2)
  on.exit(par(op))
  
  plot(x$xy, xlab = x$xy$xlab, ylab = x$xy$ylab, ...)
  lines(x)
  do_poly(x$x, x$lower, x$upper)
  # lines(x$x, x$upper, lty = 2L)
  # lines(x$x, x$lower, lty = 2L)
  
  invisible(NULL)
}
