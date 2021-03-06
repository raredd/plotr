-   [plotr](#plotr)
-   [Installation](#installation)
-   [these are actually useful](#these-are-actually-useful)
    -   [barplot2](#barplot2)
    -   [methods for tsne, pca, umap](#methods-for-tsne-pca-umap)
    -   [loess\_smooth](#loess_smooth)
    -   [tableplot](#tableplot)
    -   [scattergram](#scattergram)
-   [ggplot-style base plots](#ggplot-style-base-plots)
-   [Some useful and not-so-useful plots and
    tools](#some-useful-and-not-so-useful-plots-and-tools)
    -   [shist](#shist)
    -   [propfall](#propfall)
    -   [bibar](#bibar)
    -   [dose\_esc](#dose_esc)
    -   [boxline](#boxline)
    -   [inbar](#inbar)
    -   [tracebar](#tracebar)
    -   [toxplot](#toxplot)
    -   [spider](#spider)
    -   [dotplot](#dotplot)
-   [not so useful](#not-so-useful)
    -   [prettypie2](#prettypie2)
    -   [barmap](#barmap)
    -   [bump](#bump)
    -   [minbars](#minbars)
-   [Session info](#session-info)

## plotr

miscellaneous plots and things

------------------------------------------------------------------------

## Installation

``` r
# install.packages('devtools')
devtools::install_github('raredd/plotr')
```

## these are actually useful

### barplot2

``` r
## with arrays
barplot2(with(mtcars, table(cyl, gear, vs)))
```

<img src="inst/etc/barplot2-1.png" style="display: block; margin: auto;" />

``` r
set.seed(1)
x <- array(runif(4 * 3 * 3), c(4, 3, 3))
barplot2(x)
```

<img src="inst/etc/barplot2-2.png" style="display: block; margin: auto;" />

``` r
## group labels
barplot2(x, names.arg = list(A = 1:3, B = 4:6, C = 7:9))
```

<img src="inst/etc/barplot2-3.png" style="display: block; margin: auto;" />

``` r
bp <- barplot2(x)
mtext(1:9, side = 1L, at = bp$at, line = 1)
mtext(1:3, side = 1L, at = bp$group, line = 3)
```

<img src="inst/etc/barplot2-4.png" style="display: block; margin: auto;" />

``` r
## simplified space argument
barplot2(
  x, space = c(0.1, 1, 2) / 2, las = 1L, col = 1:4,
  legend.text = sprintf('Factor %s', 1:4),
  args.legend = list(horiz = TRUE, bty = 'n'),
  names.arg = list(A = 1:3, B = 4:6, C = 7:9)
)
```

<img src="inst/etc/barplot2-5.png" style="display: block; margin: auto;" />

``` r
## missing arguments from plot.default
barplot2(1:5, panel.first = {grid(0, NULL); abline(h = 4, col = 2)})
```

<img src="inst/etc/barplot2-6.png" style="display: block; margin: auto;" />

### methods for tsne, pca, umap

``` r
## feature x sample matrix
dat <- t(unique(iris[, 1:4]))
grp <- unique(iris)$Species

l <- list(
  tsne = dimr(dat, type = 'tsne'),
  umap = dimr(dat, type = 'umap'),
  pca  = dimr(dat, type = 'pca'),
  rpca = dimr(dat, type = 'rpca')
)

op <- par(mfrow = c(2, 2), mar = c(4, 5, 2, 1))
for (x in l)
  plot(x)
```

<img src="inst/etc/dimr-1.png" style="display: block; margin: auto;" />

``` r
par(op)

plot(l$tsne, group2 = as.list(data.frame(t(dat))))
```

<img src="inst/etc/dimr-2.png" style="display: block; margin: auto;" />

``` r
n <- 9
d <- as.list(mtcars[, rep_len(c('mpg', 'wt', 'hp'), n)])
par(mfrow = n2mfrow(n), oma = c(5, 5, 4, 2))
plotr:::gridplot(d, mtcars$mpg, mtcars$wt, legend = TRUE)
title(xlab = 'MPG', ylab = 'Weight', outer = TRUE, cex.lab = 2)
```

<img src="inst/etc/dimr-3.png" style="display: block; margin: auto;" />

### loess\_smooth

``` r
par(mfrow = c(2, 2), mar = c(4, 5, 2, 2))

plot(mpg ~ wt, mtcars)
lo <- loess_smooth(mpg ~ wt, mtcars)
lines(lo$x, lo$y)
lines(lo$x, lo$upper, lty = 2)
lines(lo$x, lo$lower, lty = 2)

plot(lo)
plot(lo, ci = 'lines', col.line = 'red')
plot(lo, ci = 'band', col.ci = 'grey90')
```

<img src="inst/etc/loess_smooth-1.png" style="display: block; margin: auto;" />

### tableplot

``` r
plot(mpg ~ wt, mtcars)
tableplot(
  'topright', table = head(mtcars, 3),
  title = 'mtcars data set', cex.title = 2
)
tableplot(
  par('usr')[1], 35, head(mtcars, 3)[, 1:3],
  show.rownames = TRUE, col.rownames = 'red',
  font.colnames = 2, hlines = TRUE
)
```

<img src="inst/etc/tableplot-1.png" style="display: block; margin: auto;" />

### scattergram

``` r
with(mtcars, {
  scattergram(mpg, wt, cyl, col = rainbow(3), pch = 16)
})
```

<img src="inst/etc/scattergram-1.png" style="display: block; margin: auto;" />

## ggplot-style base plots

``` r
gplot(1:10, col = ggcols(10), pch = 16, cex = 5)
```

<img src="inst/etc/gplot-1.png" style="display: block; margin: auto;" />

``` r
gmatplot(1:10, matrix(rnorm(100), 10), type = 'l', col = ggcols(10))
```

<img src="inst/etc/gplot-2.png" style="display: block; margin: auto;" />

see all:

``` r
grep('^g[^g]', ls('package:plotr'), value = TRUE)
```

    ##  [1] "gbarplot"       "gboxplot"       "gbxp"           "gcurve"        
    ##  [5] "ghist"          "gmatlines"      "gmatplot"       "gmatpoints"    
    ##  [9] "gpairs"         "gplot"          "gqqnorm"        "gqqplot"       
    ## [13] "grcols"         "gspineplot"     "gstripchart"    "gsunflowerplot"

## Some useful and not-so-useful plots and tools

### shist

``` r
set.seed(1)
x <- lapply(sample(1:10, 4), rpois, n = 500)
shist(x)
```

<img src="inst/etc/shist-1.png" style="display: block; margin: auto;" />

### propfall

``` r
dat <- within(mtcars, {
  disp <- disp / 10
  wt <- wt * 10
})[, c('mpg', 'disp', 'wt')]

dat[] <- t(apply(dat, 1L, function(x) x / sum(x)))

propfall(dat)
```

<img src="inst/etc/propfall-1.png" style="display: block; margin: auto;" />

``` r
propfall(dat, group = colnames(dat)[max.col(dat)],
         col = c('grey', 'lightpink', 'indianred1'))
```

<img src="inst/etc/propfall-2.png" style="display: block; margin: auto;" />

### bibar

``` r
set.seed(1)
x <- datasets::ability.cov$cov
x <- x[sample(seq.int(nrow(x)), 20, TRUE), ]

bibar(x, left = 1:3, right = 4:6, xlim = c(-250, 250))
```

<img src="inst/etc/bibar-1.png" style="display: block; margin: auto;" />

``` r
palette(c('grey90', 'cornflowerblue', 'blue', 'tomato', 'tomato3'))
bibar(x, left = 2:3, right = 4:5, sleft = 1, sright = 6)
legend('topleft', inset = c(0, -0.2), xpd = NA, fill = 3:2,
       legend = colnames(x)[3:2], horiz = TRUE, bty = 'n')
legend('topright', inset = c(0, -0.2), xpd = NA, fill = 4:5,
       legend = colnames(x)[4:5], horiz = TRUE, bty = 'n')
```

<img src="inst/etc/bibar-2.png" style="display: block; margin: auto;" />

``` r
palette('default')
```

### dose\_esc

``` r
d33 <- rep(1:4, c(3, 3, 6, 4))
c33 <- rep(3, length(d33))
c33[c(9, 14, 15)] <- 2

par(mfrow = c(2, 1), mar = c(2, 2, 2, 2))
dose_esc(d33, c33, dose.exp = rep(4, 10), col.exp = rep(3, 10))
dose_esc(d33, c33, dose.exp = rep(3, 10), col.exp = rep(3, 4))
```

<img src="inst/etc/dose_esc-1.png" style="display: block; margin: auto;" />

### boxline

``` r
set.seed(1)
x <- lapply(0:10, function(x) rnorm(25, x / 2, sd = 0.5))
boxplot(x)
boxline(x, add = TRUE)
```

<img src="inst/etc/boxline-1.png" style="display: block; margin: auto;" />

### inbar

``` r
set.seed(1)
tbl <- sapply(1:3, function(x) sort(rpois(3, 10), decreasing = TRUE))
inbar(tbl, col = 1:3)
```

<img src="inst/etc/inbar-1.png" style="display: block; margin: auto;" />

### tracebar

``` r
set.seed(1)
tbl <- sapply(1:3, function(x) sort(rpois(3, 10), decreasing = TRUE))

tracebar(tbl)
```

<img src="inst/etc/tracebar-1.png" style="display: block; margin: auto;" />

``` r
tracebar(replace(tbl, 5, 0), col = 1:3, space = 0.5)
```

<img src="inst/etc/tracebar-2.png" style="display: block; margin: auto;" />

### toxplot

``` r
set.seed(1)
f <- function(x, ...) sample(x, 100, replace = TRUE, ...)
tox <- data.frame(
  id = rep(1:10, 10), phase = 1:2,
  code = f(rawr::ctcae_v4$tox_code[1:100]),
  grade = f(1:3, prob = c(.6, .3, .1)),
  stringsAsFactors = FALSE
)
tox <- cbind(tox, rawr::match_ctc(tox$code)[, c('tox_cat', 'tox_desc')])

t1 <- ftable(
  Category = tox$tox_cat,
  Description = tox$tox_desc,
  Grade = tox$grade
)
t2 <- ftable(
  Description = tox$tox_desc,
  Grade = tox$grade
)
n <- 25


## basic usage
toxplot(t1, n) ## three column
```

<img src="inst/etc/toxplot-1.png" style="display: block; margin: auto;" />

``` r
toxplot(t2, n, widths = c(1, 3)) ## two column
```

<img src="inst/etc/toxplot-2.png" style="display: block; margin: auto;" />

### spider

``` r
with(airquality, spider(Day, Temp, group = Month))
```

<img src="inst/etc/spider-1.png" style="display: block; margin: auto;" />

``` r
with(airquality, {
  spider(Day, Temp - mean(Temp), group = Month, start = 0,
         labels = month.abb[unique(Month)],
         at.labels = par('usr')[2], col.labels = 1:5)
})
```

<img src="inst/etc/spider-2.png" style="display: block; margin: auto;" />

### dotplot

``` r
# https://twitter.com/RandyRenstrom/status/1318053323828756480/photo/1
x <- c(
  39, 55, 36, 47, 32, 58, 57, 17, 14, 17, 43, 49, 40, 38, 28,
  60, 57, 56, 52, 49, 46, 45, 43, 43, 42, 40, 36, 36, 33, 23,
  85, 68, 73, 58, 69, 48, 43, 68, 64, 62, 44, 35, 31, 36, 19
)
y <- c(
  'The coronavirus pandemic',
  'Fairness of presidential elections',
  'Health care',
  'Jobs and employment',
  'Foreign interference in presidential elections',
  'Crime',
  'Terrorism',
  'Racial inequality',
  'Climate change',
  'Growing gap between rich and poor',
  'Appointment of U.S. Supreme Court Justices',
  'Abortion',
  'The federal deficit',
  'Immigration',
  'Trade agreements with other countries'
)
x <- matrix(x, ncol = 3L, dimnames = list(y, c('R', 'All', 'D')))

dotplot(x, col = c('blue4', 'darkgrey', 'tomato2'))
box('outer')
```

<img src="inst/etc/dotplot-1.png" style="display: block; margin: auto;" />

## not so useful

### prettypie2

``` r
prettypie2(mtcars$mpg, group = mtcars$gear)
```

<img src="inst/etc/prettypie2-1.png" style="display: block; margin: auto;" />

### barmap

``` r
barmap(c(1, 1, 1) / 3, region = 'Germany', cols = c('gold', 'red', 'black'))
```

<img src="inst/etc/barmap-1.png" style="display: block; margin: auto;" />

``` r
voteGermany2013 <- structure(
  list(
    Party = c("CDU/CSU", "SPD", "LINKE", "GRUENE"),
    Result = c(49.4, 30.5, 10.2, 10)
  ), class = "data.frame", row.names = c("1", "2", "3", "4")
)

with(voteGermany2013, {
  barmap(Result / 100, region = 'Germany',
         labels = sprintf('%s (%s%%)', Party, Result))
})
```

<img src="inst/etc/barmap-2.png" style="display: block; margin: auto;" />

### bump

``` r
set.seed(1)
mat <- replicate(5, sample(1:10))
dimnames(mat) <- list(rownames(mtcars)[1:nrow(mat)],
                      paste0('time', 1:ncol(mat)))
bump(mat, mar = c(2, 0, 2, 9))
```

<img src="inst/etc/bump-1.png" style="display: block; margin: auto;" />

### minbars

``` r
set.seed(1)

layout(matrix(c(1, 1, 1, 2:4), 3), widths = c(1, 1.5))
op <- par(las = 1L, mar = c(1, 1, 2, 2))

minbars(table(rbinom(500, 15, 0.5)), unit = 'Billion',
        col = adjustcolor('tomato4', alpha.f = 0.5))
mtext('Group 1', at = par('usr')[1L], adj = 0)
for (ii in 1:3) {
  minbars(table(rbinom(500, 15, 0.5)), unit = 'Million', min = 10,
          horiz = FALSE, col = adjustcolor(ii, alpha.f = 0.5))
  if (ii == 2L)
    abline(h = grconvertY(0:1, 'nfc'), xpd = TRUE)
  text(0, mean(par('usr')[3:4]), paste('Group', ii + 1L),
       xpd = NA, srt = 90, adj = c(0.5, -1), cex = 1.5)
}
```

<img src="inst/etc/minbars-1.png" style="display: block; margin: auto;" />

``` r
par(op)
```

## Session info

``` r
within.list(sessionInfo(), loadedOnly <- NULL)
```

    ## R version 4.0.2 (2020-06-22)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Mojave 10.14.6
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] mapdata_2.3.0 maps_3.3.0    knitr_1.31    plotr_0.0.7
