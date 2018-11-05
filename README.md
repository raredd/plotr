source(system.file('examples','barplot.r', package = 'plotr'), echo = TRUE)
source(system.file('examples','confin.r', package = 'plotr'), echo = TRUE)
source(system.file('examples','gg.r', package = 'plotr'), echo = TRUE)
source(system.file('examples','boxplot.r', package = 'plotr'), echo = TRUE)

How to make plots in r

## images: click to see code:

* histograms with density
* boxplots
* barplots
* pie charts
* survival plots
* combining base and grid graphics
* ggplot clipping


## pretty bars


```r
## kinda pretty?
prettybars(mtcars$mpg, y = rownames(mtcars), col.bg = 'snow',
           emph = rownames(mtcars)[grepl('Merc', rownames(mtcars))],
           extra.margin = 1, col.emph = 'cyan2',
           FUN = quantile, probs = c(.25, .5, .75), na.rm = TRUE, 
           fun.lab = c('lower quartile','median','upper quartile'),
           note = "if you buy a Merccedes,\nget ready to pay for lots of gas",
           title = 'motor trend cars', sub = '   (miles per gallon)')
```

<figure><img src='about/figure/prettybars1.png'  style='display:block'><figcaption>Figure 4: prettybars</figcaption></figure>

```r
## a face only a mother could love
set.seed(1618)
f <- function(...) sample(1:5, 100, replace = TRUE, prob = c(...))
dat <- data.frame(q1 = f(.1, .2, .3, .3, .1),
                  q2 = f(.1, .4, .1, .3, .1),
                  q3 = f(.1, .2, .3, .3, .1),
                  q4 = f(.1, .1, .3, .3, .1),
                  q5 = f(.2, .1, .2, .3, .2),
                  q6 = f(.1, .3, .3, .2, .1),
                  q7 = f(.1, .4, .1, .1, .3))
dat <- stack(dat)
dat <- within(dat, {
  values <- factor(values, levels = 1:5, labels = c('NA','SA','A','D','SD'))})

mydata <- table(dat)
cols <- c(grey(.9), tcol(c('lightblue','lightblue','magenta1','magenta1'), 
               c(200, 100, 100, 200)))

prettybars2(mydata, lab.y = paste('Question #', 1:7), extra.margin = 3, 
            col.group = cols)
```

<figure><img src='about/figure/prettybars2.png'  style='display:block'><figcaption>Figure 5: prettybars</figcaption></figure>