## plotting confidence intervals

## forgot the name of this one
set.seed(1)
xbar <- 159
n <- 1047
ns <- 100
## empirical mean:
m_ <- xbar / n
## generate 100 samples of size n
M <- matrix(rbinom(n * ns, size = 1, prob = m_), nrow = n)

## compute the mean and confidence interval:
f <- function(x) {
  x_ <- mean(x)
  x_ + c(-1,1) * 1.96 * sqrt(x_ * (1 - x_) / n)
}

M_ <- colMeans(M)
CIs <- t(apply(M, 2, f))

## plot confidence intervals
## red: do not contain the empirical mean

k <- (m_ < CIs[, 1]) | (m_ > CIs[, 2])
plot(M_, 1:ns, xlim = range(CIs), axes = FALSE, ann = FALSE, cex = .7,
     pch = c(17, 19)[1 + k], col = c('blue', 'red')[1 + k], 
     panel.first = grid(ny = 0, lty = 1, col = 'grey90'))
axis(1)
segments(CIs[, 1], 1:ns, CIs[, 2], 1:ns, col = c('blue', 'red')[1 + k])
abline(v = m_)
mtext(round(m_, 2), side = 3, at = m_)

## linear model with confidence intervals
set.seed(1)
x <- rnorm(20)
df <- data.frame(x = x, y = x + rnorm(20))

## model + predict + interval
mod <- lm(y ~ x, data = df)
newx <- seq(min(df$x), max(df$x), length.out = 100)
preds <- predict(mod, newdata = data.frame(x = newx), 
                 interval = 'confidence')

## plot + fill
par(mar = c(3,3,2,2), bty = 'l', las = 1)
plot(y ~ x, data = df, type = 'n', ann = FALSE)
polygon(c(rev(newx), newx), c(rev(preds[, 3]), preds[, 2]),
        col = 'grey80', border = NA)
## model + CIs + data points
points(y ~ x, data = df)
lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
## clip plotting region to keep abline inside polygon
clip(min(newx), max(newx), par('usr')[3], par('usr')[4])
abline(mod, col = 'red')

