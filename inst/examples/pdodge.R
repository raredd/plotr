## dodge points in circle
## similar to jitter or rawr::dodge but spreads points evenly around a circle

## convert polar to cartesian
p2c <- function(radius, theta, deg = FALSE) {
  if (deg)
    theta <- theta * (pi / 180)
  list(x = radius * cos(theta),
       y = radius * sin(theta))
}

## convert cartesian to polar
c2p <- function(x, y, deg = FALSE) {
  list(radius = sqrt(x ** 2 + y ** 2),
       theta = atan2(y, x) * if (deg) 180 / pi else 1)
}

## convert to polar, add rad to radius and spread points, convert back
pdodge <- function(x, y, rad = 1) {
  stopifnot((lx <- length(x)) == length(y))
  p <- c2p(x, y)
  p <- within(p, {
    radius <- radius + rad
    theta  <- theta + plotr:::rescaler(seq.int(lx + 1), c(0,359))[-(lx + 1)]
  })
  p2c(p$radius, p$theta, TRUE)
}

set.seed(1)
par(mfrow = c(2,2), mar = c(4,4,1,1))
pts <- rep(0, 10)
plot(pts, pts)
plot(jitter(pts), pts)
# plot(pts, jitter(pts))
plot(jitter(pts), jitter(pts))

pts <- pdodge(pts, pts)
plot(pts$x, pts$y, xlim = c(-1,1), ylim = c(-1,1), asp = 1)
