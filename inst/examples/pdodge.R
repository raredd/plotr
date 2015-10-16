## dodge points in circle
## similar to jitter or rawr::dodge but spreads points in a circle

## adds random noise to radius and angle
pdodge <- function(x, y) {
  stopifnot((lx <- length(x)) == length(y))
  p2c(x, y, runif(lx), sample(seq.int(360 - 1), lx, replace = TRUE), FALSE)
}

## convert polar to cartesian
p2c <- function(x, y, radius, theta, deg = FALSE) {
  if (deg)
    theta <- theta * pi / 180
  list(x = x + radius * sin(theta), y = y + radius * cos(theta))
}

set.seed(1)
par(mfrow = c(2,2), mar = c(4,4,1,1))
pts <- rep(0, 1000)
plot(pts, pts)
plot(jitter(pts), pts)
# plot(pts, jitter(pts))
plot(jitter(pts), jitter(pts))

pts <- pdodge(pts, pts)
plot(pts$x, pts$y)
