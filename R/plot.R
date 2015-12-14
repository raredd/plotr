## some random plot things
# prettybars, prettybars2, zoomin, ptlocator, prettypie, widebars, waffle,
# bump, histr
##


#' prettybars
#' 
#' A barplot
#' 
#' @param x numeric vector; values of bar heights
#' @param y rownames of bars (or extracted from a named vector)
#' @param emph vector of bars to emphasize (must be in \code{y})
#' @param col.bar color of bars
#' @param col.emph color of emphasis bars
#' @param col.y color of y labels
#' @param col.bg background color bars
#' @param cex.y size of y labels
#' @param FUN summary function to apply to \code{x}
#' @param ... additional arguments passed to \code{FUN}
#' @param fun.lab text label for \code{FUN}
#' @param digits numeric; digits after decimal after applying \code{FUN}
#' @param title main title
#' @param sub subtitle
#' @param note optional note (top right)
#' @param col.note color of note
#' @param subnote optional subnote (bottom right)
#' @param extra.margin extra left spacing for long \code{y} labels
#' @param pars additional list of graphical parameters passed to 
#' \code{\link{par}}
#' 
#' @seealso \code{\link{prettybars2}}
#' 
#' @examples
#' set.seed(1618)
#' myrandomdata <- setNames(round(runif(10), 2) * 100, LETTERS[1:10])
#' prettybars(myrandomdata, FUN = NULL, fun.lab = NULL, cex.y = 1.2, 
#'            bg = 'white', emph = 'F', digits = 2)
#' 
#' x <- setNames(mtcars$mpg, rownames(mtcars))
#' prettybars(x, emph = rownames(mtcars)[mtcars$hp < 100], extra.margin = 1,
#'            FUN = median, fun.lab = 'overall median mpg',
#'            title = 'motor trend cars', sub = '   (miles per gallon)',
#'            note = 'vehicles with less than 100 hp in bold')
#'
#' prettybars(mtcars$mpg, y = rownames(mtcars), col.bg = 'snow',
#'            emph = rownames(mtcars)[grepl('Merc', rownames(mtcars))],
#'            extra.margin = 1, col.emph = 'cyan2',
#'            FUN = quantile, probs = c(.25, .5, .75), na.rm = TRUE, 
#'            fun.lab = c('lower quartile','median','upper quartile'),
#'            note = "if you buy a Mercedes,\nget ready to pay for lots of gas",
#'            title = 'motor trend cars', sub = '   (miles per gallon)') 
#' @export

prettybars <- function(x, y = names(x), emph = NULL, 
                       
                       # aesthetics
                       col.bar = grey(.9), col.emph = 'magenta1', col.y = 'black', 
                       col.bg = 'lightblue', cex.y = 0.5,
                       
                       # summary line(s)
                       FUN = mean, ..., fun.lab = 'overall mean', digits = 2, 
                       
                       # labels and text
                       title = paste0('prettybar of ', m$x), sub = NULL, note = NULL, 
                       col.note = col.emph, subnote = 'source: github.com/raredd/rawr',
                       
                       # etc
                       extra.margin = 0, pars = NULL) {
  
  m <- match.call()
  
  ## par settings
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mar = c(3, 4 + extra.margin, 4, 2) + .1, las = 1, bg = 'snow')
  par(pars)
  
  ## error checks
  if (length(x) != length(y))
    stop('variable lengths not equal')
  if (!is.null(emph) && all(emph %in% rownames(y)))
    stop('error: some emph not found in y')
  
  x <- x[order(x)]
  y <- y[order(x)]
  
  ## background bars
  breaks <- pretty(x)
  s <- seq_along(breaks)
  p0 <- barplot(x, horiz = TRUE, axes = FALSE, col = NA, border = NA, 
                xlim = range(breaks), names.arg = FALSE)
  
  rect(breaks[1], 0, breaks[length(breaks)], sum(range(p0)), 
       col = tcol(col.bg, 50), border = NA)
  rect(breaks, 0, breaks + rep(diff(breaks)[1], length(breaks)), sum(range(p0)), 
       col = c(tcol(col.bg, 80), NA), border = NA)
  
  ## data bars
  p1 <- barplot(x, names.arg = FALSE, horiz = TRUE, border = NA, axes = FALSE,
                xlim = range(breaks), ylim = c(0, length(x)), col = col.bar, 
                cex.names = 0.85, add = TRUE, xpd = FALSE)
  
  ## emphasized bars
  x2 <- x * (y %in% emph)
  barplot(x2, names.arg = FALSE, horiz = TRUE, border = NA,
          xlim = range(breaks), col = tcol(col.emph, 200), 
          cex.names = 0.85, axes = FALSE, add = TRUE, xpd = FALSE)
  
  ## FUN line
  if (!is.null(FUN)) {
    at.x <- round(FUN(x, ...), digits)
    #     tcl <- length(p0) / (max(p0) - min(p0)) / 4
    tcl <- min(p0) / mean(p0)
    arrows(x0 = at.x, y0 = 0, x1 = at.x, y1 = sum(range(p0)),
           lwd = 1.5, length = 0, xpd = TRUE, col = col.y, lty = 3)
    arrows(x0 = at.x, y0 = 0, x1 = at.x, y1 = -tcl,
           lwd = 3, length = 0, xpd = TRUE)
    arrows(x0 = at.x, y0 = sum(range(p0)), x1 = at.x, y1 = sum(range(p0)) + tcl,
           lwd = 3, length = 0, xpd = TRUE)
    #     text(x = at.x, y = sum(range(p0)) * 1.25, labels = fun.lab, pos = 2, 
    #          xpd = TRUE, cex = 0.65, font = 3)
    #     text(x = at.x, y = sum(range(p0)) * 1.25, labels = at.x, pos = 4, 
    #          xpd = TRUE, cex = 0.65, font = 4)
    mtext(text = fun.lab, side = 3, line = 0, adj = 0, at = at.x, 
          cex = 0.65, font = 3)
    mtext(text = at.x, side = 3, line = -.5, adj = 0, at = at.x, 
          cex = 0.65, font = 4)
  }
  
  ## axes text
  mtext(text = breaks, at = breaks, side = 1, line = 0, cex = 0.80)
  font <- 1 + (y %in% emph) * rep(1, length(x))
  text(x = breaks[1], y = p1, labels = y, pos = 2, col = col.y,
       xpd = TRUE, adj = 0, cex = cex.y, font = font)
  text(x = breaks[1], y = p1, labels = x, col = col.y,
       xpd = TRUE, adj = 0, cex = cex.y, font = font)
  
  ## plot text (titles, notes)
  mtext(text = title, side = 3, line = 2, adj = 0, cex = 1.2, font = 2)
  mtext(text = sub, side = 3, line = 1, adj = 0, cex = 0.9)
  mtext(text = note, side = 3, line = -.5, adj = 1, cex = 0.65, font = 3, 
        col = col.note)
  #   text(x = breaks[length(breaks)], y = max(p0), labels = note, pos = 3,
  #        xpd = TRUE, cex = 0.65, font = 3)
  mtext(text = subnote, side = 1, line = 1, adj = 1, cex = 0.65, font = 3)
}

#' prettybars2
#' 
#' A barplot
#' 
#' @param x a table of data, typically from \code{\link{table}} or 
#' \code{\link{xtabs}}
#' @param lab.y y-axis labels
#' @param n.y number of y-groups (calculated from \code{x})
#' @param lab.group group labels
#' @param n.group number of groups (calculated from \code{x})
#' @param col.group color for each group level
#' @param col.line color of origin line
#' @param extra.margin extra left spacing for long \code{lab.y} labels
#' @param col.bg background color
#' @param cex.y size of \code{lab.y}
#' @param title main title
#' @param sub subtitle
#' @param note optional note (top right)
#' @param subnote optional subnote (bottom right)
#' @param legend numeric (0, 1, 2); \code{0} for no legend; \code{1} default
#' legend; or \code{2} for a default \code{\link{legend}}
#' @param notext logical; if \code{TRUE}, suppresses all plot text, labels, and
#' axes for user input
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @seealso \code{\link{prettybars}}
#' 
#' @examples
#' set.seed(1618)
#' f <- function(...) sample(1:5, 100, replace = TRUE, prob = c(...))
#' dat <- data.frame(q1 = f(.1, .2, .3, .3, .1),
#'                   q2 = f(.1, .4, .1, .3, .1),
#'                   q3 = f(.1, .2, .3, .3, .1),
#'                   q4 = f(.1, .1, .3, .3, .1),
#'                   q5 = f(.2, .1, .2, .3, .2),
#'                   q6 = f(.1, .3, .3, .2, .1),
#'                   q7 = f(.1, .4, .1, .1, .3))
#' dat <- stack(dat)
#' dat <- within(dat, {
#'   values <- factor(values, levels = 1:5, labels = c('NA','SA','A','D','SD'))})
#' 
#' mydata <- table(dat)
#' cols <- c(grey(.9), tcol(c('lightblue','lightblue','magenta1','magenta1'), 
#'                c(200, 100, 100, 200)))
#'                
#' ## compare:
#' barplot(mydata, horiz = TRUE, las = 1, col = cols, border = NA)
#' prettybars2(mydata, lab.y = paste('Question #', 1:7), extra.margin = 3, 
#'             col.group = cols)
#'
#' @export

prettybars2 <- function(x,
                        lab.y = colnames(x), 
                        n.y = ncol(x),
                        lab.group = rownames(x), 
                        n.group = nrow(x),
                        col.group = tcol(1:n.group, 100),
                        col.line = 'skyblue3',
                        extra.margin = 0,
                        col.bg = 'snow',
                        cex.y = 0.7,
                        title = paste0('prettybar of ', m$x), 
                        sub = paste0('     n = ', sum(x)), 
                        note = NULL, 
                        subnote = 'subnote: here is a subnote',
                        legend = 1,
                        notext = FALSE,
                        ...) {
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  m <- match.call()
  
  if (!is.table(x))
    stop('x must be a table')
  if (nrow(x) != n.group || nrow(x) != length(lab.group))
    stop('check group labels')
  if (ncol(x) != n.y || ncol(x) != length(lab.y))
    stop('check y labels')
  if (length(col.group) != n.group)
    warning('colors will be recycled (length(col.group) != n.group)')
  
  par(mar = c(6, 4 + extra.margin, 4, 2) + .1, bg = 'snow',
      #       lheight = 1.5,
      las = 1)
  par(list(...))
  
  ## data bars
  p0 <- barplot(-rep(100, n.y), names.arg = lab.y, cex.names = cex.y, 
                horiz = TRUE, border = col.bg, xlim = c(-100, n.y * 10), 
                col = col.group[1], axes = FALSE)
  barplot(-(100 - x[1, ]), axisnames = FALSE, horiz = TRUE, border = col.bg, 
          xlim = c(-100, n.y * 10), col = col.bg, axes = FALSE, add = TRUE)
  barplot(-x[3:2, ], axisnames = FALSE, horiz = TRUE, border = NA, 
          xlim = c(-100, n.y * 10), col = col.group[3:2], axes = FALSE, 
          add = TRUE)
  barplot(x[4:5, ], axisnames = FALSE, horiz = TRUE, border = NA, 
          xlim = c(-100, n.y * 10), col = col.group[4:5], axes = FALSE, 
          add = TRUE)
  
  ## legend, axes
  arrows(x0 = 0, y0 = -0.1, x1 = 0, y1 = sum(range(p0)),
         lwd = 2.5, length = 0, xpd = TRUE, col = col.line)
  if (legend == 2) {
    legend(x = -110, y = -0.1, horiz = TRUE, fill = col.group, legend = lab.group, 
           border = col.group, pt.cex = 3, bty = 'n', xpd = TRUE, cex = .8)
  } else if (legend == 1) {
    # if fine-tune
    px <- c(-95,-90, -59, -53, -37)
    tx <- c(-100, -79, -65, -45, -25)
    lab.group <- c('N/A','Strongly agree','Agree','Disagree','Strongly disagree')
    points(x = px, y = rep(-1, n.group), pch = 15, cex = 3, 
           col = col.group, xpd = TRUE)
    text(x = tx, y = rep(-1, n.group), labels = lab.group, xpd = TRUE, font = 3, 
         cex = .8)
  } else if (legend == 0) TRUE
  else warning('legend should be 0, 1, or 2')
  
  if (!notext) {
    ## x-lab
    mtext(c(80, 60, 40, 20, 0, 20, 40, 60), at = c(-80,-60,-40,-20,0,20,40,60), 1,
          line = 0, cex = 0.95)
    
    ## plot text (titles, notes)
    mtext(text = title, side = 3, line = 2, adj = 0, cex = 1.2, font = 2)
    mtext(text = sub, side = 3, line = 1, adj = 0, cex = 0.9, font = 3)
    mtext(text = note, side = 3, line = -.5, adj = 1, cex = 0.65, font = 3)
    #   text(x = breaks[length(breaks)], y = max(p0), labels = note, pos = 3,
    #        xpd = TRUE, cex = 0.65, font = 3)
    mtext(text = subnote, side = 1, line = 1, adj = 1, cex = 0.65, font = 3)
    #   mtext(paste0('N = ', sum(x)), side = 3, line = 0, at = -70, 
    #         cex = 0.8, font = 3)
  }
}

#' prettypie
#' 
#' A pie chart
#' 
#' @param dat data
#' @param file path to output file
#' @param dev device; default is \code{\link{pdf}}
#' @param width width of \code{dev}
#' @param height height of \code{dev}
#' @param main overall title for plot
#' @param sub sub-title for plot
#' @param note note for plot
#' 
#' @examples
#' \dontrun{
#' browsers <- source(system.file('source','browsers.r', package = 'plotr'))$value
#' browsers <- within(browsers, total <- ave(share, browser, FUN = sum))
#' browsers <- browsers[c('browser','version','share','total')]
#' 
#' prettypie(dat = browsers,
#'           file = './donuts.pdf', 
#'           main = 'Browser market share, April 2011',
#'           sub = 'stackoverflow.com:::maryam',
#'           note = '/questions/26748069/ggplot2-pie-and-donut-chart-on-same-plot')
#' 
#' 
#' disease <- source(system.file('source','disease.r', package = 'plotr'))$value
#' disease <- disease[c('Disease','Acosts60','Pcosts60','Patients60','Total60')]
#' 
#' prettypie(dat = disease,
#'           file = './cost.pdf', 
#'           main = 'Cost of getting sick',
#'           sub = 'Inside: Personal Costs, outside: Insurer Costs',
#'           note = 'visualization.geblogs.com/visualization/health_costs/')
#' }
#' @export

prettypie <- function(dat, file, dev = 'pdf', width = 15, height = 11,
                      main, sub, note) {
  m <- match.call()
  if (missing(main)) main <- ''
  if (missing(sub))  sub  <- ''
  if (missing(note)) note <- ''
  if (missing(file)) file <- getwd()
  
  do.call(dev, list(file = file, width = width, height = height, bg = 'snow'))
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  nr <- nrow(dat)
  f0 <- rep(NA, nr)
  ok <- length(unique(dat[, 1])) == nr
  
  if (ok) {
    width <- max(sqrt(dat[, 2])) / 0.8
    tbl <- rep(1, nr)
    cols <- colors()[1:nr]
  } else {
    width <- max(sqrt(dat[, 3])) / 0.8
    tbl <- table(dat[, 1])[order(unique(dat[, 1]))]
    cols <- c('cyan2','red','orange','green','dodgerblue2')
    cols <- unlist(Map(rep, cols, tbl))
  }
  
  plot.new()
  par(omi = c(0.5,0.5,0.75,0.5), mai = c(0.1,0.1,0.1,0.1), las = 1)
  for (ii in 1:nr) {
    par(new = TRUE)
    rgb <- col2rgb(cols[ii])
    f0[ii] <- rgb(rgb[1], rgb[2], rgb[3], alpha = 190 / sequence(tbl)[ii], 
                  maxColorValue = 255)
    lab <- sprintf('%s: %s', dat[, 2], dat[, 3])
    if (dat[, 3][ii] == max(dat[, 3]))
      lab0 <- lab
    else lab0 <- NA
    
    if (ok)
      pie(dat[, 5], border = NA, radius = sqrt(dat[, 2])[ii] / width, 
          col = f0, labels = lab0, cex = 1.8)
    else 
      pie(dat[, 3], border = NA, radius = 5 / width, 
          col = f0, labels = lab0, cex = 1.8)
    
    par(new = TRUE)
    rgb <- col2rgb(cols[ii])
    f0[ii] <- rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
    
    if (ok)
      pie(dat[, 5], border = NA, radius = sqrt(dat[, 3])[ii] / width, 
          col = f0, labels = NA)
    else
      pie(dat[, 3], border = NA, radius = 4 / width, col = f0, labels = NA)
    f0 <- rep(NA, nr)
  }
  
  ## group labels, guess and check?
  if (!ok)
    text(x = c(-.05, -.05, 0.15, .25, .3), y = c(.08, -.12, -.15, -.08, -.02),
         labels = unique(dat[, 1]), col = 'white', cex = 1.2)
  mtext(main, side = 3, line = -1, adj = 0, cex = 3.5, outer = TRUE, font = 2)
  mtext(sub, side = 3, line = -3.5, adj = 0, cex = 1.75, outer = TRUE, font = 3)
  mtext(note, side = 1, line = 0, adj = 1, cex = 1.2, outer = TRUE, font = 3)
  dev.off()
}

#' Map barplot
#' 
#' Creates a barplot in the shape of a map region.
#' 
#' @param x a vector of proportions summing to 1 (approximately)
#' @param db data base; see \code{\link[maps]{map}}
#' @param region the region; see \code{\link[maps]{map}}
#' @param labels optional vector of labels for each section
#' @param cols a vector of colors for each section
#' 
#' @examples
#' par(mar = c(0,0,0,0))
#' barmap(c(1,1,1)/3, region = 'Germany', cols = c('red','black','gold'))
#'  
#' voteGermany2013 <- read.table(header = TRUE, text = "Party Result
#'                               1 CDU/CSU   49.4
#'                               2     SPD   30.5
#'                               3   LINKE   10.2
#'                               4  GRUENE   10.0")
#'  
#' with(voteGermany2013,
#'      barmap(Result / 100, region = 'Germany',
#'             labels = sprintf('%s (%s%%)', Party, Result)))
#' 
#' @export

barmap <- function(x, db = 'worldHires', region, labels, cols) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  require('maps')
  require('mapdata')
  
  ## fill = TRUE !important
  dat <- map(db, region, fill = TRUE)
  
  ## set up plotting window
  p <- par('usr')
  plot.new()
  plot.window(p[1:2], p[3:4])
  
  ## calculate some useful things
  xx <- range(dat$x, na.rm = TRUE)
  yy <- range(dat$y, na.rm = TRUE)
  zz <- diff(yy) * x
  
  if (missing(cols))
    cols <- palette(rainbow(length(x)))
  
  ## draw and color rectangles
  dyy <- rep(0, length(zz))
  dy <- 0
  for (ii in seq_along(zz)) {
    rect(p[1], yy[1] + dy, p[2], yy[1] + (dy <- sum(zz[1:ii])),
         col = cols[ii], border = NA)
    ## label y-coordinates
    dyy[ii] <- yy[1] + c(0, cumsum(zz))[ii] + zz[ii] / 2
  }
  
  map(db, region, col = 'black', add = TRUE)
  
  ## trim around borders
  xb <- xx + c(-1,1)
  yb <- yy + c(-1,1)
  polypath(c(dat$x, NA, c(xb, rev(xb))), c(dat$y, NA, rep(yb, each = 2)),
           col = 'white', rule = 'evenodd')
  
  if (!missing(labels))
    text(max(xx), dyy, labels = labels, pos = 4, xpd = NA)
}

#' Wide bar plots
#' 
#' Create a series of barplots by grouping factor.
#' 
#' @param x vector of grouping values
#' @param y numeric vector of data values
#' @param main overall title for plot
#' @param sub sub-title for plot
#' @param foot footnote for plot
#' @param note note for plot
#' @param col a vector of colors for elements of the form 
#' \code{c(section background, text, negative bars, positive bars)}
#' 
#' @examples
#' \dontrun{
#' pdf('./widebar.pdf', width = 14, height = 7)
#' par(oma = c(5,5,7,5), mar = c(5,0,3,.1), las = 1,
#'     fg = 'transparent', bg = 'grey98')
#' widebars(x <- rep(2004:2015, each = 4), rnorm(48),
#'          main = sprintf('Market value, %s',
#'                         paste0(range(x), collapse = ' - ')),
#'          sub = 'Percent change, quarterly',
#'          foot = sprintf('Values current as of %s',
#'                         format(Sys.time(), '%b %Y')),
#'          note = 'github.com/raredd')
#' dev.off()
#' }
#' 
#' par(oma = c(5,5,7,5), mar = c(5,0,3,.1), las = 1,
#'     fg = 'transparent', bg = 'grey98')
#' widebars(mtcars$gear, mtcars$mpg * sample(c(-1,1), 32, replace = TRUE),
#'          'Motor Trend car road tests', 'Miles per gallon by gear',
#'          'some footnote that isn\'t important', 'blahblah')
#' @export

widebars <- function(x, y, main, sub, foot, note,
                     col = c('grey90','grey30','lightblue2','dodgerblue2')) {
  
  if (missing(main)) main <- ''
  if (missing(sub))  sub  <- ''
  if (missing(foot)) foot <- ''
  if (missing(note)) note <- ''
  
  ## data and aesthetic vars
  bgcol  <- col[1]
  txtcol <- col[2]
  barcol <- col[3:4]
  
  xx <- unique(x)
  rr <- range(y) + c(-1, 1) * diff(range(y)) / 50
  
  par(mfcol = c(1, length(xx)))
  for (ii in 1:length(xx)) {
    xt <- y[x == xx[ii]]
    cols <- ifelse(xt < 0, barcol[1], barcol[2])
    barplot(xt, border = NA, bty = 'n', col = cols, ylim = rr, axes = FALSE,
            panel.first = {
              usr <- par('usr')
              rect(usr[1], usr[3], usr[2], usr[4], col = bgcol)
            })
    if (ii == 1)
      axis(2, lwd = 0, cex.axis = 1.25,
#            labels = paste0(pretty(rr), '%'),
           at = pretty(rr))
    mtext(text = xx[ii], side = 1, line = 2, cex = 1.25, col = txtcol)
  }
  
  mtext(main, line = 2.5, adj = 0, cex = 2, col = txtcol, outer = TRUE)
  mtext(sub, line = -0.5, adj = 0, cex = 1.5, col = txtcol, outer = TRUE)
  mtext(note, side = 1, line = 1, adj = 1, cex = 1.25, font = 3,
        col = txtcol, outer = TRUE)
  mtext(foot, side = 1, line = 1, adj = 0, cex = 1.25, font = 3,
        col = txtcol, outer = TRUE)
}

#' waffle
#' 
#' A waffle chart.
#' 
#' If \code{mat} is given, all other arguments except \dots are ignored, and
#' \code{mat} is used to specify dimensions, layout, and colors for the plot;
#' see examples.
#' 
#' @param x an integer vector with counts for each group
#' @param rows number of rows
#' @param horiz logical; orientation of the chart and pattern of coloring
#' boxes; default is \code{TRUE} (horiztonal)
#' @param cols colors for each group, should be of length \code{length(x)}
#' @param mat an optional matrix giving the layout; see details
#' @param ... additional graphical parameters passed to \code{par}
#' 
#' @examples
#' waffle(c(3, 10), rows = 2, cols = c('red','black'))
#' waffle(c(15, 700), rows = 40, horiz = FALSE, cols = c('salmon2','grey90'))
#' 
#' cols <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
#' waffle(c(80, 30, 20, 10), rows = 8, cols = cols, mar = c(0,0,0,7))
#' legend('right', legend = LETTERS[1:4], pch = 15, col = cols, pt.cex = 2,
#'        bty = 'n')
#'        
#' ## using mat
#' mat <- rep(c(cols, NA), times = c(80, 30, 20, 10, 4))
#' waffle(mat = matrix(mat, 8))
#' 
#' \dontrun{
#' pdf('~/desktop/waffle.pdf', width = 7, height = 3)
#' savings <- c('Mortgage ($84,911)' = 84911,
#'              'Auto and\ntuition loans ($14,414)'=14414,
#'              'Home equity loans ($10,062)' = 10062,
#'              'Credit cards\n($8,565)' = 8565)
#' w <- waffle(savings / 392, rows = 7, cols = c("#c7d4b6", "#a3aabd", "#a0d0de", "#97b5cf"),
#'             bg = 'cornsilk', mar = c(0,0,0,3))
#' xx <- c(-.05, .73, .85, .93)
#' yy <- c(-.05, -.05, -.35, -.05)
#' segments(x0 = xx, y0 = .05, y1 = yy, lty = 'dotted',
#'          lwd = 1, xpd = NA, col = 'grey50')
#' text(xx, yy + .05, labels = names(savings), xpd = NA, cex = .6, col = 'grey50', pos = 1)
#' p <- par('usr')
#' mtext('Average household savings each year', at = xx[1], font = 2,
#'       col = 'grey50', adj = 0, line = 1)
#' mtext('Source: Federal Reserve', side = 1, font = 3, at = xx[1],
#'       col = 'grey50', adj = 0, cex = .6, line = 2)
#' legend(.87, 1.3, legend = '$392', xpd = NA, bty = 'n', bg = 'cornsilk',
#'        col = 'orange', text.col = 'grey50', pch = 15, cex = .8, pt.cex = 1.5)
#' dev.off()
#' }
#' 
#' @export

waffle <- function(x, rows, horiz = TRUE, cols = seq_along(x), mat, ...) {
  
  if (!missing(mat)) {
    # m <- mat[nrow(mat):1, ]
    m <- mat
  } else {
    xx <- rep(cols, times = x)
    lx <- length(xx)
    m <- matrix(nrow = rows, ncol = (lx %/% rows) + (lx %% rows != 0))
    m[1:length(xx)] <- xx
    
    if (!horiz) {
      m <- matrix(c(m), nrow = rows, byrow = TRUE)
      m <- m[nrow(m):1, ]
    }
  }
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  par(list(...))
  plot.new()
  o <- cbind(c(row(m)), c(col(m))) + 1
  plot.window(xlim = c(0, max(o[, 2]) + 1), ylim = c(0, max(o[, 1]) + 1),
              asp = 1, xaxs = 'i', yaxs = 'i')
  rect(o[, 2], o[, 1], o[, 2] + .85, o[, 1] + .85, col = c(m), border = NA)
  
  invisible(list(m = m, o = o))
}

#' bump chart
#' 
#' A bump chart
#' 
#' @param mat an \code{n x t} matrix with \code{n} observations and \code{t}
#' timepoints
#' @param adj label position adjustment
#' @param ... additional graphical parameters passed to \code{par}
#' 
#' @examples
#' mat <- replicate(5, sample(1:10))
#' dimnames(mat) <- list(rownames(mtcars)[1:nrow(mat)], paste0('time', 1:ncol(mat)))
#' bump(mat, mar = c(2,4,2,9), adj = .1, bg = 'cornsilk')
#' 
#' @export

bump <- function(mat, adj = .5, ...) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  par(list(...))
  plot.new()
  plot.window(xlim = c(0, ncol(mat)), ylim = c(0, nrow(mat)),
              xaxs = 'i', yaxs = 'i')
  segments(x0 = 1:ncol(mat), y0 = 1 - adj, y1 = nrow(mat),
           col = 'grey70', lty = 'dashed')
  
  lapply(1:nrow(mat), function(x) lines(mat[x, ], col = x))
  rn <- rownames(mat)
  start <- order(mat[, 1])
  end <- order(mat[, ncol(mat)])
  
  text(1 - adj, 1:nrow(mat), labels = rn[start], col = start, adj = 1, xpd = NA)
  text(ncol(mat) + adj, 1:nrow(mat), labels = rn[end], col = end,
       xpd = NA, adj = 0)
  text(1:ncol(mat), y = 0, labels = colnames(mat), xpd = NA)
}

#' histr
#' 
#' A histogram with density curve and rug.
#' 
#' @param x a vector of values for which the histogram is desired
#' @param ... additional parameters passed to \code{\link{hist}} or graphical
#' parameters passed to \code{\link{par}}
#' @param line.pars optional list of additional parameters passed to
#' \code{\link{line}}
#' @param rug.pars optional list of additional parameters passed to
#' \code{\link{rug}}
#' @param poly.pars optional list of additional parameters passed to
#' \code{\link{polygon}}
#' 
#' @return
#' A list of length two containing the return value of \code{\link{hist}}
#' and \code{\link{density}} for \code{x}.
#' 
#' @examples
#' set.seed(1)
#' histr(x <- rnorm(100), xlim = c(-3, 3), las = 1)
#' plot(density(x), xlim = c(-3, 3))
#' 
#' histr(x + 50, main = 'Age at diagnosis', xlab = 'Age',
#'       poly.pars = list(col = 'dodgerblue2', density = 30),
#'       lines.pars = list(lty = 'dashed', lwd = 3),
#'       rug.pars = list(side = 3, col = 'red'))
#' 
#' @export

histr <- function(x, ..., lines.pars, rug.pars, poly.pars) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  m <- match.call()
  if (length(xx <- grep('pars', names(m))))
    m[xx] <- lapply(m[xx], function(ii) as.list(ii)[-1L])
  
  par(las = 1, mar = c(5,4,4,4.5) + .1, tcl = .2)
  d <- density(x)
  h <- if (is.null(match.call()$xlab))
    hist(x, ..., xlab = sprintf('N = %s  Bandwidth = %.3f', d$n, d$bw)) else
      hist(x, ...)
  rs <- max(h$counts) / max(d$y)
  
  do.call('lines', c(list(x = d$x, y = d$y * rs, type = 'l'), m$lines.pars))
  do.call('rug', c(list(x = x), m$rug.pars))
  do.call('polygon', 
          c(list(x = c(d$x, rev(d$x)),
                 y = c(d$y * rs, rep(par('usr')[3], length(d$y))),
                 col = adjustcolor(m$poly.pars$col %||% NA, alpha.f = .25),
                 border = NA),
            m$poly.pars[-which(names(m$poly.pars) == 'col')]))
  
  par(new = TRUE)
  plot(d, type = 'n', ann = FALSE, axes = FALSE)
  p <- par('usr')
  text(p[2] + .15 * diff(p[1:2]), mean(p[3:4]), labels = 'Density',
       srt = -90, xpd = NA)
  axis(4)
  invisible(list(h = h, d = d))
}
