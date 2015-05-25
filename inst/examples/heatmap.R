## a heat map or something

'%inside%' <- function(x, interval) {
  interval <- sort(interval)
  x >= interval[1] & x <= interval[2]
}

N1 <- 50
N2 <- 50
cutoff <- .5

set.seed(1)
f <- function() {
  ll <- function()
    sample(LETTERS[1:5], size = sample(2:3, 1), replace = TRUE)
  nn <- function()
    sample(0:9, size = sample(1:5, 1), replace = TRUE)
  f2 <- function(...) paste0(..., collapse = '')
  f2(f2(ll()), '-', f2(nn()))
}

## generate some gene names and patient ids
genes <- replicate(N1, f())
pids <- sprintf('FL-%s', seq_len(N2))

## generate some continuous data of some kind
corr <- matrix(runif(N1 * N2, -1, 1), nrow = N1, 
               dimnames = list(genes, pids))
corr[corr %inside% (cutoff * c(-1, 1))] <- 0
cs <- data.frame(rn = rownames(corr), rs = rowSums(abs(corr) > 0))
ord <- rev(with(cs, order(-xtfrm(substr(rn, 1, 1)), rs, decreasing = TRUE)))
m <- corr[ord, ]
cs <- cs[ord, 'rs']
# m <- m[rev(seq_len(nrow(m))), ]

## gene functional categories/colors
grps <- table(substr(rownames(m), 1, 1))
cgrps <- adjustcolor(c('blue','green','red','orange','yellow')[seq_along(grps)],
                     alpha.f = .3)


pdf('~/desktop/tmp.pdf', width = 20, height = 10)
plot.new()
par(mar = c(2, 2, 5, 1), fig = c(0, .9, 0, 1), family = 'HersheySerif')

## color matrix
o <- cbind(c(row(m)), c(col(m))) + 1
plot.window(xlim = c(0, max(o[, 2]) + 1), ylim = c(0, max(o[, 1]) + 1),
            xaxs = 'i', yaxs = 'i')
p <- par('usr')
rect(o[, 2], o[, 1] - .5, o[, 2] + .95, o[, 1] + .35, border = NA,
     col = colorRampPalette(c('grey50','grey90','dodgerblue2'))(1000)[c(m) * 500 + 500])

## gene labels and color functional groups
mtext(side = 2, text = 'Genes', line = 0)
text(x = rep(1.5, nrow(m)), y = seq_len(nrow(m)) + 1, labels = rownames(m),
     cex = .5, xpd = NA, adj = 1)
rect(rep(0, nrow(m)), seq_len(nrow(m)) + 1 - .5, rep(1.75, nrow(m)),
     seq_len(nrow(m)) + 1.35, border = NA,
     col = rep(cgrps, rev(grps)))

## cheap legends
par(family = '')
legend(p[1], p[3], pch = 15, cex = 1, bty = 'n', xpd = NA, pt.cex = 2,
       legend = c('Epigenetic','Immune','JAK-STAT',expression('BCR-NF-'~kappa~'B'),'B-cell')[seq_along(grps)],
       col = rev(cgrps), horiz = TRUE)
legend(p[2] * .5, p[3], pch = 15, cex = 1, bty = 'n', xpd = NA, pt.cex = 1.5,
       legend = c('Down-regulation', 'Up-regulation'),
       col = c('grey50','dodgerblue2'), horiz = TRUE)

## patient labels
par(family = 'HersheySerif')
mtext('FL patients', side = 3, line = 3, font = 2)
text(x = seq_len(ncol(m)) + 1.5, y = nrow(m) + 3, labels = colnames(m),
     pos = 3, xpd = NA, srt = 90, font = 2, cex = .6)
rect(2, nrow(m) + 5, ncol(m) + 2, nrow(m) + 7, col = '#3399FF64', xpd = NA,
     border = NA)

## count bars on right
par(fig = c(.9, .99, 0, 1), mar = c(0, 0, 0, 0))
rect(1, o[, 1] - .5, cs, o[, 1] + .35, col = 'grey50', border = NA, xpd = NA)
axis(3, line = -4, tcl = .2, at = pretty(c(0, cs)), cex.axis = .8)
mtext(side = 3, text = 'No. > cutoff', line = -1.5, cex = 1, adj = 0)

abline(v = 0)
box('outer')
dev.off()
