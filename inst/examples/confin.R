## plotting confidence intervals

## forgot the name of this one
set.seed(1)
xbar <- 159
n    <- 1047
ns   <- 100

## empirical mean:
em <- xbar / n

## generate 100 samples of size n
M <- matrix(rbinom(n * ns, size = 1, prob = em), nrow = n)

## compute the mean and confidence interval:
f <- function(x, alpha = 0.05) {
  n  <- length(x)
  xm <- mean(x)
  cv <- qnorm(alpha / 2, lower.tail = FALSE)
  
  xm + c(-1,1) * cv * sqrt(xm * (1 - xm) / n)
}

cm <- colMeans(M)
CIs <- t(apply(M, 2L, f))


## plot confidence intervals

## red: do not contain the empirical mean
k <- (em < CIs[, 1]) | (em > CIs[, 2])

plot(cm, 1:ns, xlim = range(CIs), axes = FALSE, ann = FALSE, cex = .7,
     pch = c(17, 19)[1 + k], col = c('blue', 'red')[1 + k], 
     panel.first = {
       grid(ny = 0, lty = 1, col = 'grey90')
       abline(v = em)
       axis(1)
     }
)
segments(CIs[, 1L], 1:ns, CIs[, 2L], 1:ns, col = c('blue', 'red')[1L + k])
mtext(round(em, 2), side = 3, at = em)


## linear model with confidence intervals
set.seed(1)
x  <- rnorm(20)
y  <- x + rnorm(20)
dd <- data.frame(x, y)

## model + predict + interval
mod <- lm(y ~ x, data = dd)
nx  <- seq(min(dd$x), max(dd$x), length.out = 100)
pr  <- data.frame(
  predict(mod, newdata = data.frame(x = nx),
          interval = 'confidence')
)

## plot + fill
par(mar = c(3,3,2,2), bty = 'l', las = 1)
plot(y ~ x, data = dd, type = 'n', ann = FALSE)
polygon(c(rev(nx), nx), c(rev(pr$lwr), pr$upr),
        col = 'grey80', border = NA)

## model + CIs + data points
points(y ~ x, data = dd)
lines(nx, pr$lwr, lty = 'dashed', col = 2)
lines(nx, pr$upr, lty = 'dashed', col = 2)

## clip plotting region to keep abline inside polygon
clip(min(nx), max(nx), par('usr')[3L], par('usr')[4L])
abline(mod, col = 2)



library('mgcv')
mod <- gam(y ~ s(x, k = 2))
nx  <- seq(min(x), max(x), 0.1)
pr  <- data.frame(
  x = nx,
  predict(mod, data.frame(x = nx),
          type = 'response', se = TRUE)
)
pr <- within(pr, {
  LCI <- fit - 1.96 * se.fit
  UCI <- fit + 1.96 * se.fit
})

lines(fit ~ x, data = pr, lwd = 2, lty = 2, col = 2)
with(pr, polygon(c(x, rev(x)), c(LCI, rev(UCI)),
                 border = NA, col = adjustcolor(2, alpha.f = 0.1)))
