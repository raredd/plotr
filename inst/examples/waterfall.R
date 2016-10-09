## waterfall plot
## plot percent change in some value from baseline

waterfall <- function(x, type = 1L, col = c('red','blue'), ...,
                      panel.first = NULL, panel.last = NULL) {
  m  <- match.call(expand.dots = FALSE)$`...`
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  o   <- order(x, na.last = NA)
  rx  <- range(x, na.rm = TRUE)
  col <- if (type == 2L)
    rep_len(col, 2L) else if (length(col) != length(x))
      colorRampPalette(col)(1000)[(x - rx[1]) / diff(rx) * 999 + 1][o] else col[o]
  bp <- barplot(x[o], plot = FALSE)
  
  plot.new()
  plot.window(range(bp), if (is.null(m$ylim))
    extendrange(if (type == 2L) cumsum(x) else x) else eval(m$ylim))
  
  panel.first
  if (type == 2L) {
    dx <- diff(c(bp))[1] * .4
    rect(bp - dx, y0 <- cumsum(c(0, x[-length(x)])), bp + dx, y1 <- cumsum(x),
         col = col[(x > 0) + 1L], border = NA, xpd = NA)
    arrows(bp, y0, bp, y1, lwd = 2, length = .1, xpd = NA, col = 0)
  } else barplot(x[o], border = NA, col = col, ..., xpd = NA, add = TRUE)
  panel.last
  
  invisible(bp)
}

set.seed(1)
change <- runif(20, -1, 1) * 100
col <- c(PD = 'red', SD = 'blue', CR = 'chartreuse4')

## interpolation
waterfall(change, col = col)

## discrete breaks
waterfall(change, col = as.character(cut(change, c(-Inf, -50, 50, Inf), col)))
legend('top', legend = names(col), fill = col, horiz = TRUE, bty = 'n', border = NA)
title(xlab = 'Patient', ylab = '% change', main = 'waterfall', line = 1)

## type 2
waterfall(change, col = col, type = 2,
          panel.first = grid(),
          panel.last = {
            axis(2, las = 1)
            title(main = 'waterfall - type 2')
          }
)



## generate data
set.seed(1)
n <- 50
ea <- sample(2:10, n, TRUE)
dat <- data.frame(id = id <- rep(seq(n), ea),
                  time = c('Baseline','Cycle')[duplicated(id) + 1L],
                  value = rnorm(length(id), 5))
head(dat)
#   id     time    value
# 1  1 Baseline 4.994233
# 2  1    Cycle 7.404653
# 3  1    Cycle 5.763593
# 4  1    Cycle 4.200991
# 5  2 Baseline 3.852343
# 6  2    Cycle 4.710538


## calculate change in variable from baseline
get_change <- function(data, how = c('change','increase','decrease'),
              from = 'Baseline', default = NA) {
  how <- switch(match.arg(how), change = function(...) TRUE,
                increase = `>`, decrease = `<`)
  
  x <- by(data, data$id, function(x) {
    base <- x$value[x$time %in% from]
    out  <- (x$value[!x$time %in% from] - base) / base * 100
    idx  <- how(out, 0)
    out  <- out[idx]
    if (any(idx))
      out[which.max(abs(out))] else default
  })
  
  c(x)
}

head(sapply(c('change','increase','decrease'), get_change, data = dat))

#      change increase   decrease
# 1 -28.61368       NA -28.613683
# 2  17.36338 17.36338  -9.611202
# 3  68.37222 68.37222         NA
# 4  23.90048 23.90048  -9.567476
# 5  21.72178 21.72178 -11.789877
# 6 -38.50691 15.05659 -38.506905



waterfall(get_change(dat, 'increase'),
          main = 'largest increase from baseline',
          col = c('blue','red')[(unique(dat$id) %% 2 == 0) + 1L])
legend('topleft', fill = c('blue','red'), cex = 1.5, bty = 'n',
       legend = c('Odds','Evens'), border = NA)


waterfall(get_change(dat), names = FALSE, las = 1, tcl = .2,
          ylim = c(-100, 100),
          xlab = 'Patient', ylab = 'Change from baseline (%)',
          panel.first = {
            abline(h = c(-75, 75), col = 'red')
            abline(h = c(-25, 25), col = 'grey')
            grid(0, NULL)
          }
)


library('ggplot2')
dd <- data.frame(id = seq(n), value = get_change(dat))

ggplot(dd, aes(reorder(id, value), value, fill = value)) +
  geom_hline(yintercept = c(75, -75), colour = 'red') +
  geom_bar(stat = 'identity') +
  labs(x = 'Patient', y = 'Change from baseline (%)', fill = '% change') +
  scale_fill_gradient(low = 'red', high = 'blue') +
  coord_cartesian(ylim = c(-100, 100)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

