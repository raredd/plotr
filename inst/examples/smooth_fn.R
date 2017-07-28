### smooth functions
# do_poly, bezier, bezier.est, spline.est, boot.ci
###


do_poly <- function(x, y1, y2) {
  polygon(c(x, rev(x)), c(y1, rev(y2)), border = NA,
          col = adjustcolor(2, alpha.f = 0.2))
}

bezier <- function(x, y, n = 10L) {
  ## http://stackoverflow.com/a/3480948/2994949
  ## x,y x- and y-coordinates of hull points
  ## n number of points desired
  lx <- length(x) - 1L
  sq <- seq_along(x) - 1L
  ch <- choose(lx, sq)
  zz <- seq(0, 1, length.out = n)
  
  bz <- function(z) {
    z <- ch * ((1 - z) ^ (lx - sq)) * z ^ sq
    colSums(z * cbind(x, y))
  }
  
  as.list(data.frame(t(vapply(zz, bz, numeric(2L)))))
}

bezier.est <- function(x, y, n = 300L) {
  bezier(x, y, n)$y
}

spline.est <- function(x, y, n = 300L) {
  ss <- stats::smooth.spline(x, y, cv = TRUE)
  sq <- seq(min(x), max(x), length.out = n)
  predict(ss, sq)$y
}

boot.ci <- function(x, y, type = c('spline', 'bezier'),
                    alpha = 0.05, n = 300L, nsim = 1000L) {
  ## http://stackoverflow.com/a/23852733/2994949
  FUN <- match.arg(type)
  FUN <- match.fun(paste0(type, '.est'))
  
  ## bootstraps
  bs <- replicate(nsim, {
    idx <- sample(seq_along(x), replace = TRUE)
    FUN(x[idx], y[idx], n)
  })
  
  ## estimate and {1-alpha/2}% CI
  est <- FUN(x, y, n)
  lci <- 2 * est - apply(bs, 1L, quantile, probs = 1 - alpha / 2)
  uci <- 2 * est - apply(bs, 1L, quantile, probs =     alpha / 2)
  
  list(
    x = seq(min(x), max(x), length.out = n),
    fit = est, lci = lci, uci = uci
  )
}



# ## sample data
# set.seed(1)
# dd <- data.frame(x = rnorm(100), y = rnorm(100))
# 
# ## spline fit and CIs
# plot(y ~ x, dd)
# sp.cis <- boot.ci(dd$x, dd$y, type = 'spline')
# lines(sp.cis$x, sp.cis$fit, col = 2, lty = 2, lwd = 2)
# do_poly(sp.cis$x, sp.cis$lci, sp.cis$uci)
# 
# 
# 
# ## bezier fit and CIs
# plot(y ~ x, dd)
# bz <- dd[order(dd$x), ]
# bz <- bezier(bz$x, bz$y, 10)
# lines(bz, col = 1, lty = 2, lwd = 2)
# 
# bz.cis <- boot.ci(bz$x, bz$y, type = 'bezier', alpha = 0.2)
# lines(bz.cis$x, bz.cis$fit, col = 2, lty = 2, lwd = 2)
# do_poly(bz.cis$x, bz.cis$lci, bz.cis$uci)
