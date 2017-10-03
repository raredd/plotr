## smooth curves, confidence limits
## loess


## functions for plots
source(system.file('examples', 'smooth2_fn.R', package = 'plotr'))


set.seed(1)
n <- 1000L
dd <- data.frame(
  x = sort(runif(n)),
  y = rt(n, 5)
)


library('ggplot2')
ggplot(dd, aes(x, y)) +
  geom_point() +
  geom_smooth(method = 'loess')

## same as above
lo <- with(dd, loess_smooth(x, y, sd = FALSE))
plot(lo, gg = TRUE)


## for data points
lo <- with(dd, loess_smooth(x, y))

plot(lo, gg = TRUE)
plot(lo, gg = TRUE) + theme_light() + labs(title = 'main')

plot(lo, pch = 16L, col = adjustcolor(1L, 0.2))
