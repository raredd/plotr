## boxplot with splines along quantiles
x <- 1
## data
set.seed(1)
n <- 1449
x <- sort(rnorm(n, 0, 4))
s <- spline(quantile(x, seq(0, 1, 1/10)), c(0, .03, -6/10, 1/2, -1/10, 6/10,
                                            1.2, 7/10, 1.4, 1/10, 6/10),
            xout = x, method = 'natural')
# plot(s, type = 'l')
e <- rnorm(length(x), sd = 1)
y <- s$y + e ## ($ interferes with MathJax processing on SE)

## calcs
q <- 2 ^ (-(2:floor(log(n / 10, 2))))
q <- c(rev(q), 1 / 2, 1 - q)
n.bins <- length(q) + 1
bins <- cut(x, quantile(x, probs = c(0, q, 1)))
x.binmed <- by(x, bins, median)
x.bincount <- by(x, bins, length)
x.bincount.max <- max(x.bincount)
x.delta <- diff(range(x))

## plot
par(mfrow = c(1,1), las = 1, mar = c(3,3,2,2))
bp <- boxplot(y ~ bins, varwidth = TRUE, plot = FALSE)
plot(x, y, pch = 19, col = '#00000010', ann = FALSE, bty = 'l', tcl = .2, 
     panel.first = grid())
for (i in 1:n.bins) {
  invisible(bxp(list(stats = bp$stats[, i, drop = FALSE],
                     n=bp$n[i], conf = bp$conf[, i, drop = FALSE],
                     out = bp$out[bp$group == i], group = 1,
                     names = bp$names[i]),
                add = TRUE, axes = FALSE, col = 'blue', at = x.binmed[i],
                boxwex = 2 * x.delta * x.bincount[i] / x.bincount.max / n.bins))
}

colors <- hsv(seq(2/6, 1, 1/6), 3/4, 5/6)
invisible(sapply(1:5, function(i) 
  lines(spline(x.binmed, bp$stats[i, ], method = 'natural'),
        col = adjustcolor(colors[i], alpha.f = .3), lwd = 4)))
