## smooth curves, confidence limits
## linear, loess, spline (bootstrap ci), mgcv::gam, bezier (bootstrap ci)


## functions for plots
source(system.file('examples', 'smooth_fn.R', package = 'plotr'))

library('mgcv')
set.seed(1)
dd <- gamSim(1, n = 200, dist = 'normal', scale = 2)
plot(gam(y ~ s(x2), data = dd))


## new x-values for predict
nx <- seq(min(dd$x2), max(dd$x2), length.out = 1000L)

alpha <- 0.05
cv <- qnorm(1 - alpha / 2)


## linear fit
lf <- lm(y ~ x2, dd)
pr <- predict(lf, data.frame(x2 = nx), type = 'response', se.fit = TRUE)
pr <- within(data.frame(pr), {
  nx <- nx
  LCI <- fit - cv * se.fit
  UCI <- fit + cv * se.fit
})

plot(y ~ x2, dd, main = 'linear')
abline(lf, col = 2, lwd = 2, lty = 2)
do_poly(nx, pr$LCI, pr$UCI)



## loess
lo <- loess(y ~ x2, dd)
pr <- predict(lo, data.frame(x2 = nx), se = TRUE)
pr <- within(data.frame(pr), {
  nx <- nx
  LCI <- fit - cv * se.fit
  UCI <- fit + cv * se.fit
})

plot(y ~ x2, dd, main = 'loess')
lines(fit ~ nx, pr, col = 2, lwd = 2, lty = 2)
do_poly(nx, pr$LCI, pr$UCI)


## smooth spline
ss <- smooth.spline(dd$x2, dd$y, spar = 0.9)
ci <- boot.ci(dd$x2, dd$y, alpha = alpha, type = 'spline')

plot(y ~ x2, dd, main = 'smooth spline')
lines(ss, col = 2, lwd = 2, lty = 2)
do_poly(ci$x, ci$lci, ci$uci)


## generalized additive model
gm <- gam(y ~ s(x2), data = dd)
pr <- predict(gm, data.frame(x2 = nx), type = 'response', se.fit = TRUE)
pr <- within(data.frame(pr), {
  nx <- nx
  LCI <- fit - cv * se.fit
  UCI <- fit + cv * se.fit
})

plot(y ~ x2, dd, main = 'gam')
lines(fit ~ nx, pr, lwd = 2, lty = 2, col = 2)
do_poly(nx, pr$LCI, pr$UCI)


## bezier
plot(y ~ x2, dd, main = 'bezier')
bz <- dd[order(dd$x2), ]
bz <- bezier(bz$x2, bz$y, n = 20L)
lines(bz, col = 1, lty = 2, lwd = 2)

bz.cis <- boot.ci(bz$x, bz$y, type = 'bezier')
lines(bz.cis$x, bz.cis$fit, col = 2, lty = 2, lwd = 2)
do_poly(bz.cis$x, bz.cis$lci, bz.cis$uci)
