## plot small graphics along x-axis and/or y-axis

pdf('~/desktop/axis_plots', width = 16, height = 6)
npoints <- 15
set.seed(1)
y <- rexp(npoints)
x <- seq(npoints)

# reserve some extra space on bottom margin (outer margin)
par(mar = c(6,4,0,2), fig = c(0, 1, 0, .8))
plot(y, xlab = NA, xaxt = 'n',  pch = 15, cex = 1.2, col = 'grey50', ann = FALSE)
lines(y, col = 'dodgerblue2', lwd = 2)

## convert from plot coordinates to figure coordinates
## x1 and x2 are left and right boundaries of igraphs
x1 <- grconvertX(x = 1:npoints - 0.7, from = 'user', to = 'nfc')
x2 <- grconvertX(x = 1:npoints + 0.7, from = 'user', to = 'nfc')

if (!require('igraph'))
  message('Run: install.packages(\'igraph\')') else
    for (ii in seq_len(npoints)) {
      par(fig = c(x1[ii], x2[ii], 0, .25), new = TRUE, mar = c(1,1,2,1))
      plot(graph.ring(ii), vertex.label = NA)
    }

for (ii in seq_len(npoints)) {
  par(fig = c(x1[ii], x2[ii], .8, 1), new = TRUE, mar = c(0,1,2,1))
  h <- hist(xx <- rnorm(50, mean = y[ii]), breaks = 50, axes = FALSE,
            ann = FALSE, col = 'dodgerblue2', border = NA)
  d <- density(xx)
  rs <- max(h$counts) / max(d$y)
  lines(x = d$x, y = d$y * rs, type = 'l')
  # axis(1, tcl = .2, cex.axis = .5)
  box('plot', col = 'grey50', bty = 'u')
}
box('outer')
dev.off()
