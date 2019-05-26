### special case plots
# prettybars, prettybars2, prettypie, prettypie2, barmap, widebars, bump,
# minbars
###


#' prettybars
#' 
#' A barplot.
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
#' @seealso
#' \code{\link{prettybars2}}; \code{\link{bibar}}; \code{\link{minbars}}
#' 
#' @examples
#' set.seed(1)
#' x <- setNames(round(runif(10), 2) * 100, LETTERS[1:10])
#' 
#' prettybars(
#'   x, FUN = NULL, fun.lab = NULL, cex.y = 1.2,
#'   bg = 'white', emph = 'F', digits = 2
#' )
#' 
#' x <- setNames(mtcars$mpg, rownames(mtcars))
#' prettybars(
#'   x, emph = rownames(mtcars)[mtcars$hp < 100], extra.margin = 1,
#'   FUN = median, fun.lab = 'overall median mpg',
#'   title = 'motor trend cars', sub = '   (miles per gallon)',
#'   note = 'vehicles with less than 100 hp in bold'
#' )
#'
#' prettybars(mtcars$mpg, y = rownames(mtcars), col.bg = 'snow',
#'   emph = rownames(mtcars)[grepl('Merc', rownames(mtcars))],
#'   extra.margin = 1, col.emph = 'cyan2',
#'   FUN = quantile, probs = c(.25, .5, .75), na.rm = TRUE,
#'   fun.lab = c('lower quartile','median','upper quartile'),
#'   note = "if you buy a Mercedes,\nget ready to pay for lots of gas",
#'   title = 'motor trend cars', sub = '   (miles per gallon)'
#' )
#' 
#' @export

prettybars <- function(x, y = names(x), emph = NULL,
                       
                       ## aesthetics
                       col.bar = grey(.9), col.emph = 'magenta1', col.y = 'black',
                       col.bg = 'lightblue', cex.y = 0.5,
                       
                       ## summary line(s)
                       FUN = mean, ..., fun.lab = 'overall mean', digits = 2L,
                       
                       ## labels and text
                       title = paste0('prettybar of ', m$x),
                       sub = NULL, note = NULL,
                       col.note = col.emph,
                       subnote = 'source: github.com/raredd/rawr',
                       
                       ## etc
                       extra.margin = 0, pars = NULL) {
  m <- match.call()
  
  ## par settings
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mar = c(3, 4 + extra.margin, 4, 2) + .1, las = 1L, bg = 'snow')
  par(pars)
  
  ## error checks
  if (length(x) != length(y))
    stop('variable lengths not equal')
  if (!is.null(emph) && !all(wh <- emph %in% y))
    stop(sprintf('%s not found in \'y\'', toString(shQuote(emph[!wh]))),
         call. = FALSE)
  
  x <- x[order(x)]
  y <- y[order(x)]
  
  ## background bars
  at <- pretty(x)
  s  <- seq_along(at)
  p0 <- barplot(x, horiz = TRUE, axes = FALSE, col = NA, border = NA,
                xlim = range(at), names.arg = FALSE)
  
  rect(at[1L], 0, at[length(at)], sum(range(p0)),
       col = adjustcolor(col.bg, alpha.f = 0.2), border = NA)
  rect(at, 0, at + rep(diff(at)[1L], length(at)), sum(range(p0)),
       col = c(adjustcolor(col.bg, alpha.f = 0.3), NA), border = NA)
  
  ## data bars
  p1 <- barplot(x, names.arg = FALSE, horiz = TRUE, border = NA, axes = FALSE,
                xlim = range(at), ylim = c(0, length(x)), col = col.bar, 
                cex.names = 0.85, add = TRUE, xpd = FALSE)
  
  ## emphasized bars
  x2 <- x * (y %in% emph)
  bp <- barplot(x2, names.arg = FALSE, horiz = TRUE, border = NA,
                xlim = range(at), col = adjustcolor(col.emph, alpha.f = 0.8),
                cex.names = 0.85, axes = FALSE, add = TRUE, xpd = FALSE)
  
  ## FUN line
  if (!is.null(FUN)) {
    at.x <- round(FUN(x, ...), digits)
    # tcl <- length(p0) / (max(p0) - min(p0)) / 4
    tcl <- min(p0) / mean(p0)
    arrows(at.x, 0, at.x, sum(range(p0)), length = 0,
           lwd = 1.5, xpd = TRUE, col = col.y, lty = 3L)
    arrows(at.x, 0, at.x, -tcl, length = 0, lwd = 3, xpd = TRUE)
    arrows(at.x, sum(range(p0)), at.x, sum(range(p0)) + tcl,
           length = 0, lwd = 3, xpd = TRUE)
    # text(at.x, y = sum(range(p0)) * 1.25, labels = fun.lab, pos = 2L,
    #      xpd = TRUE, cex = 0.65, font = 3L)
    # text(at.x, y = sum(range(p0)) * 1.25, labels = at.x, pos = 4L,
    #      xpd = TRUE, cex = 0.65, font = 4L)
    mtext(fun.lab, side = 3L, line = 0, adj = 0, at = at.x, 
          cex = 0.65, font = 3L)
    mtext(at.x, side = 3L, line = -.5, adj = 0, at = at.x, 
          cex = 0.65, font = 4L)
  }
  
  ## axes text
  mtext(at, at = at, side = 1L, line = 0, cex = 0.8)
  font <- 1L + (y %in% emph) * rep(1L, length(x))
  text(at[1L], p1, labels = y, pos = 2L, col = col.y,
       xpd = TRUE, adj = 0, cex = cex.y, font = font)
  text(x = at[1L], y = p1, labels = x, col = col.y,
       xpd = TRUE, adj = 0, cex = cex.y, font = font)
  
  ## plot text (titles, notes)
  mtext(title, side = 3L, line = 2, adj = 0, cex = 1.2, font = 2L)
  mtext(sub, side = 3L, line = 1, adj = 0, cex = 0.9)
  mtext(note, side = 3L, line = -.5, adj = 1, cex = 0.65, font = 3L, 
        col = col.note)
  # text(at[length(at)], max(p0), labels = note, pos = 3L,
  #      xpd = TRUE, cex = 0.65, font = 3L)
  mtext(subnote, side = 1L, line = 1, adj = 1, cex = 0.65, font = 3L)
  
  invisible(bp)
}

#' prettybars2
#' 
#' A barplot.
#' 
#' @param x a table of data, typically from \code{\link{table}} or 
#' \code{\link{xtabs}}
#' @param lab.y y-axis labels
#' @param n.y number of y-groups (calculated from \code{x})
#' @param lab.group group labels
#' @param n.group number of groups (calculated from \code{x})
#' @param col.group color for each group level
#' @param col.line color of origin line
#' @param col.bg background color
#' @param cex.y size of \code{lab.y}
#' @param notext logical; if \code{TRUE}, suppresses all plot text, labels, and
#' axes for user input
#' @param title,sub main and sub titles
#' @param note,subnote optional note (top right) or subnote (bottom right)
#' @param legend integer (0, 1, 2); \code{0} for no legend; \code{1} for a
#' legend with pre-formatting; or \code{2} for a default \code{\link{legend}}
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @seealso
#' \code{\link{prettybars}}; \code{\link{bibar}}; \code{\link{bibar}}
#' 
#' @examples
#' set.seed(1)
#' f <- function(...) sample(1:5, 100, replace = TRUE, prob = c(...))
#' dat <- data.frame(
#'   q1 = f(.1, .2, .3, .3, .1),
#'   q2 = f(.1, .4, .1, .3, .1),
#'   q3 = f(.1, .2, .3, .3, .1),
#'   q4 = f(.1, .1, .3, .3, .1),
#'   q5 = f(.2, .1, .2, .3, .2),
#'   q6 = f(.1, .3, .3, .2, .1),
#'   q7 = f(.1, .4, .1, .1, .3)
#' )
#' dat <- within(stack(dat), {
#'   values <- factor(values, levels = 1:5, labels = c('NA','SA','A','D','SD'))
#' })
#' 
#' tdat <- table(dat)
#' cols <- c(
#'   grey(.9),
#'   adjustcolor(c('cadetblue', 'lightblue', 'lightpink', 'red'), alpha.f = 0.5)
#' )
#' 
#' ## compare:
#' barplot(tdat, horiz = TRUE, las = 1, col = cols, border = NA)
#' prettybars2(tdat, lab.y = paste('Question #', 1:7), col.group = cols, las = 1L)
#' 
#' 
#' ## using plotr::bibar
#' par(mar = c(3, 8, 4, 2))
#' at <- bibar(t(tdat), 3:2, 4:5, 1, col = cols, xlim = c(-80, 50), axes = FALSE)
#' 
#' axis(1, pretty(c(-80, 50)), abs(pretty(c(-80, 50))))
#' abline(v = 0, col = 'lightblue', lwd = 4)
#' text(-85, at, sprintf('Question # %s', 1:7), xpd = NA, adj = 1)
#' 
#' legend('topleft', inset = c(0, -0.1), xpd = NA, fill = cols[2:3],
#'        legend = rownames(tdat)[2:3], horiz = TRUE, bty = 'n')
#' legend('topright', inset = c(0, -0.1), xpd = NA, fill = cols[4:5],
#'        legend = rownames(tdat)[4:5], horiz = TRUE, bty = 'n')
#' 
#' @export

prettybars2 <- function(x, lab.y = colnames(x), n.y = ncol(x),
                        lab.group = rownames(x), n.group = nrow(x),
                        
                        ## aesthetics
                        col.group = adjustcolor(seq.int(n.group), alpha.f = 0.5),
                        col.line = 'skyblue3',
                        col.bg = par('bg'), cex.y = 0.7,
                        
                        ## labels
                        notext = FALSE,
                        title = paste('prettybar of', m$x),
                        sub = paste0('n = ', sum(x)), note = NULL,
                        subnote = 'subnote: here is a subnote', legend = 1L,
                        
                        ...) {
  m <- match.call()
  
  if (!is.table(x))
    stop('\'x\' must be a table')
  if (nrow(x) != n.group || nrow(x) != length(lab.group))
    stop('check group labels')
  if (ncol(x) != n.y || ncol(x) != length(lab.y))
    stop('check y labels')
  if (length(col.group) != n.group)
    warning('colors will be recycled (length(col.group) != n.group)')
  
  # op <- par(mar = c(6, 4 + extra.margin, 4, 2) + .1)
  op <- par(mar = par('mar') + c(0, 2, 0, 0))
  on.exit(par(op))
  par(...)
  
  xlim <- c(-100, n.y * 10)
  
  ## base plot
  p0 <- barplot(
    -rep(100, n.y), names.arg = lab.y, cex.names = cex.y, horiz = TRUE,
    border = col.bg, xlim = xlim, col = col.group[1L], axes = FALSE
  )
  ## left/positive bars
  barplot(
    -(100 - x[1L, ]), axisnames = FALSE, horiz = TRUE, border = col.bg,
    xlim = xlim, col = col.bg, axes = FALSE, add = TRUE
  )
  ## left/negative bars
  barplot(
    -x[3:2, ], axisnames = FALSE, horiz = TRUE, border = NA,
    add = TRUE, xlim = xlim, col = col.group[3:2], axes = FALSE
  )
  ## right/positive bars
  barplot(
    x[4:5, ], axisnames = FALSE, horiz = TRUE, border = NA,
    add = TRUE, xlim = xlim, col = col.group[4:5], axes = FALSE
  )
  
  ## legend, axes
  arrows(0, -0.1, 0, sum(range(p0)), length = 0,
         lwd = 2.5, xpd = TRUE, col = col.line)
  if (legend == 2L) {
    legend(-110, -0.1, horiz = TRUE, fill = col.group, legend = lab.group,
           border = col.group, pt.cex = 3, bty = 'n', xpd = TRUE, cex = 0.8)
  } else if (legend == 1L) {
    ## fine-tune
    px <- c(-95,-90, -59, -53, -37)
    tx <- c(-100, -79, -65, -45, -25)
    lab.group <- c('N/A', 'Strongly agree', 'Agree',
                   'Disagree', 'Strongly disagree')
    points(px, rep(-1, n.group), pch = 15L, cex = 3,
           col = col.group, xpd = TRUE)
    text(tx, rep(-1, n.group), labels = lab.group,
         xpd = TRUE, font = 3L, cex = .8)
  } else if (legend == 0L)
    NULL
  else warning('legend should be 0, 1, or 2')
  
  if (!notext) {
    ## x-lab
    mtext(c(80, 60, 40, 20, 0, 20, 40, 60),
          at = c(-80, -60, -40, -20, 0, 20, 40, 60),
          side = 1L, line = 0, cex = 0.95)
    
    ## plot text (titles, notes)
    mtext(title, side = 3L, line = 2, adj = 0, cex = 1.2, font = 2L)
    mtext(sub, side = 3L, line = 1, adj = 0, cex = 0.9, font = 3L)
    mtext(note, side = 3L, line = -.5, adj = 1, cex = 0.65, font = 3L)
    mtext(subnote, side = 1L, line = 1, adj = 1, cex = 0.65, font = 3L)
  }
  
  invisible(p0)
}

#' prettypie
#' 
#' A pie chart. \code{prettypie} is stupid, ignore it; use \code{prettypie2},
#' 
#' @param dat data
#' @param main,sub,note overall title, sub-title, and note for plot
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @examples
#' browsers <- source(system.file('source','browsers.r', package = 'plotr'))$value
#' browsers <- within(browsers, total <- ave(share, browser, FUN = sum))
#' browsers <- browsers[c('browser', 'version', 'share', 'total')]
#' 
#' m1 <- 'Browser market share, April 2011'
#' s1 <- 'stackoverflow.com:::maryam'
#' n1 <- '/questions/26748069/ggplot2-pie-and-donut-chart-on-same-plot'
#' prettypie(browsers, m1, s1, n1)
#' 
#' 
#' disease <- source(system.file('source','disease.r', package = 'plotr'))$value
#' disease <- disease[c('Disease', 'Acosts60', 'Pcosts60', 'Patients60', 'Total60')]
#' 
#' m2 <- 'Cost of getting sick'
#' s2 <- 'Inside: Personal Costs, outside: Insurer Costs'
#' n2 <- 'visualization.geblogs.com/visualization/health_costs/'
#' prettypie(disease, m2, s2, n2)
#' 
#' \dontrun{
#' pdf('~/desktop/prettypie-browsers.pdf', width = 15, height = 11, bg = 'snow')
#' prettypie(browsers, m1, s1, n1)
#' dev.off()
#' 
#' pdf('~/desktop/prettypie-disease.pdf', width = 15, height = 11, bg = 'snow')
#' prettypie(disease, m2, s2, n2)
#' dev.off()
#' }
#' 
#' @export

prettypie <- function(dat, main = NULL, sub = NULL, note = NULL, ...) {
  m <- match.call()
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  nr <- nrow(dat)
  f0 <- rep_len(NA, nr)
  ok <- length(unique(dat[, 1L])) == nr
  
  if (ok) {
    width <- max(sqrt(dat[, 2L])) / 0.8
    tbl <- rep_len(1L, nr)
    cols <- colors()[-1L][seq.int(nr)]
  } else {
    ## browsers
    width <- max(sqrt(dat[, 3L])) / 0.8
    tbl <- table(dat[, 1L])[order(unique(dat[, 1L]))]
    cols <- c('cyan2', 'red', 'orange', 'green', 'dodgerblue2')
    cols <- unlist(Map(rep, cols, tbl))
  }
  
  plot.new()
  # par(omi = c(0.5,0.5,0.75,0.5), mai = c(0.1,0.1,0.1,0.1), las = 1L)
  par(oma = c(0, 1, 3, 0), mar = c(0, 0, 0, 0), las = 1L)
  par(...)
  
  for (ii in seq.int(nr)) {
    par(new = TRUE)
    rgb <- col2rgb(cols[ii])
    f0[ii] <- rgb(rgb[1L], rgb[2L], rgb[3L], alpha = 190 / sequence(tbl)[ii], 
                  maxColorValue = 255L)
    lab <- sprintf('%s: %s', dat[, 2L], dat[, 3L])
    if (dat[, 3L][ii] == max(dat[, 3L]))
      lab0 <- lab
    else lab0 <- NA
    
    if (ok)
      pie(dat[, 5L], border = NA, radius = sqrt(dat[, 2L])[ii] / width, 
          col = f0, labels = lab0, cex = 1.8)
    else 
      pie(dat[, 3L], border = NA, radius = 5 / width, 
          col = f0, labels = lab0, cex = 1.8)
    
    par(new = TRUE)
    rgb <- col2rgb(cols[ii])
    f0[ii] <- rgb(rgb[1L], rgb[2L], rgb[3L], maxColorValue = 255L)
    
    if (ok)
      pie(dat[, 5L], border = NA, radius = sqrt(dat[, 3L])[ii] / width, 
          col = f0, labels = NA)
    else
      pie(dat[, 3L], border = NA, radius = 4 / width, col = f0, labels = NA)
    f0 <- rep(NA, nr)
  }
  
  ## group labels - guess and check?
  if (!ok)
    text(c(-0.05, -0.05, 0.15, 0.25, 0.3), c(0.08, -0.12, -0.15, -0.08, -0.02),
         unique(dat[, 1L]), col = 'white', cex = 1.2)
  mtext(main, 3L, line = -1, adj = 0, cex = 3.5, outer = TRUE, font = 2L)
  mtext(sub, 3L, line = -3.5, adj = 0, cex = 1.75, outer = TRUE, font = 3L)
  mtext(note, 1L, line = 0, adj = 1, cex = 1.2, outer = TRUE, font = 3L)
  
  invisible(NULL)
}

#' @param x numeric vector representing the size for each slice
#' @param group vector identifying groups of slices (used by \code{col})
#' @param labels vector of labels for individual slices; if \code{NA}, no
#' labels will be drawn; if \code{NULL}, slices will be labeled sequentially
#' @param col colors for each group of slices
#' @param radius radius for inner and outer pie (usually in [0,1])
#' 
#' @examples
#' ## basic usage
#' prettypie2(mtcars$mpg)
#' 
#' prettypie2(mtcars$mpg, group = mtcars$gear)
#' 
#' with(mtcars,
#'   prettypie2(mpg, interaction(gear, cyl), rownames(mtcars), cex = 0.8)
#' )
#' 
#' @rdname prettypie
#' @export

prettypie2 <- function(x, group = 1, labels = NA, col.group = NULL,
                       radius = c(.7, 1), ...) {
  grp <- rep_len(group, length(x))
  ug  <- unique(grp)
  tbl <- table(grp)[order(ug)]
  
  col.group <- if (is.null(col.group))
    seq_along(ug) else rep_len(col.group, length(ug))
  col.main <- Map(rep, col.group[seq_along(tbl)], tbl)
  col.sub <- lapply(col.main, function(x) {
    trans <- head(seq(0, 1, length.out = length(x) + 2L)[-1L], -1L)
    Vectorize(adjustcolor)(x, alpha.f = trans)
  })
  
  plot.new()
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(...)
  
  par(new = TRUE)
  pie(x, border = NA, radius = radius[2L],
      col = unlist(col.sub), labels = labels)
  
  par(new = TRUE)
  pie(x, border = NA, radius = radius[1L],
      col = unlist(col.main), labels = NA)
  
  invisible(NULL)
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
#' op <- par(mar = c(0, 0, 0, 0))
#' barmap(1, region = 'Germany')
#' 
#' barmap(c(1, 1, 1) / 3, region = 'Germany', cols = c('red', 'black', 'gold'))
#'  
#' voteGermany2013 <- read.table(
#'   header = TRUE, text = "Party Result
#'                          1 CDU/CSU   49.4
#'                          2     SPD   30.5
#'                          3   LINKE   10.2
#'                          4  GRUENE   10.0"
#' )
#'  
#' with(voteGermany2013,
#'      barmap(Result / 100, region = 'Germany',
#'             labels = sprintf('%s (%s%%)', Party, Result)))
#' par(op)
#' 
#' @export

barmap <- function(x, db = 'worldHires', region, labels = NULL, cols = NULL) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  ## fill = TRUE !important
  suppressPackageStartupMessages({
    require('mapdata', character.only = TRUE, quietly = TRUE)
  })
  dat <- maps::map(db, region, fill = TRUE)
  
  ## calculate some useful things
  p <- par('usr')
  xx <- range(dat$x, na.rm = TRUE)
  yy <- range(dat$y, na.rm = TRUE)
  zz <- diff(yy) * x
  
  if (is.null(cols))
    cols <- palette(rainbow(pmax(2L, length(x))))
  
  ## draw and color rectangles
  dyy <- rep_len(0L, length(zz))
  dy <- 0
  for (ii in seq_along(zz)) {
    rect(p[1L], yy[1L] + dy, p[2L], yy[1L] + (dy <- sum(zz[1:ii])),
         col = cols[ii], border = NA)
    ## label y-coordinates
    dyy[ii] <- yy[1L] + c(0, cumsum(zz))[ii] + zz[ii] / 2
  }
  
  maps::map(db, region, col = 'black', add = TRUE)
  
  ## trim around borders
  xb <- xx + c(-1, 1)
  yb <- yy + c(-1, 1)
  polypath(c(dat$x, NA, c(xb, rev(xb))), c(dat$y, NA, rep(yb, each = 2L)),
           col = 'white', rule = 'evenodd')
  text(max(xx), dyy, labels, pos = 4L, xpd = NA)
  palette('default')
  
  invisible(NULL)
}

#' Wide bar plots
#' 
#' Create a series of barplots by grouping factor.
#' 
#' @param x vector of grouping values
#' @param y numeric vector of data values
#' @param main,sub main title and sub title for plot
#' @param foot,note footnote (left-adjusted) and note (right) for plot
#' @param col a vector of colors for elements of the form 
#' \code{c(section background, text, negative bars, positive bars)}
#' 
#' @examples
#' op <- par(
#'   oma = c(5,5,7,5), mar = c(5,0,3,.1), las = 1,
#'   fg = 'transparent', bg = 'grey98'
#' )
#' widebars(
#'   mtcars$gear, mtcars$mpg * sample(c(-1,1), 32, replace = TRUE),
#'   main = 'Motor Trend car road tests', sub = 'Miles per gallon by gear',
#'   foot = 'some footnote that is not important', note = 'blahblah'
#' )
#' 
#' par(
#'   oma = c(5,5,7,5), mar = c(5,0,3,.1), las = 1,
#'   fg = 'transparent', bg = 'grey98'
#' )
#' widebars(
#'   x <- rep(2004:2015, each = 4), rnorm(48),
#'   main = paste('Market value,', paste0(range(x), collapse = ' - ')),
#'   sub = 'Percent change, quarterly',
#'   foot = paste('Values current as of', format(Sys.time(), '%b %Y')),
#'   note = 'github.com/raredd'
#' )
#' par(op)
#' 
#' @export

widebars <- function(x, y, main = NULL, sub = NULL, foot = NULL, note = NULL,
                     col = c('grey90', 'grey30', 'lightblue2', 'dodgerblue2')) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  ## data and aesthetic vars
  bgcol  <- col[1L]
  txtcol <- col[2L]
  barcol <- col[3:4]
  
  xx <- unique(x)
  rr <- range(y) + c(-1, 1) * diff(range(y)) / 50
  
  par(mfcol = c(1, length(xx)))
  for (ii in 1:length(xx)) {
    xt <- y[x == xx[ii]]
    cols <- ifelse(xt < 0, barcol[1L], barcol[2L])
    barplot(xt, border = NA, bty = 'n', col = cols, ylim = rr, axes = FALSE,
            panel.first = {
              usr <- par('usr')
              rect(usr[1L], usr[3L], usr[2L], usr[4L], col = bgcol)
            })
    if (ii == 1L)
      axis(2L, lwd = 0, cex.axis = 1.25,
           # labels = paste0(pretty(rr), '%'),
           at = pretty(rr))
    mtext(xx[ii], side = 1L, line = 2, cex = 1.25, col = txtcol)
  }
  
  mtext(main, 3L, line = 2.5, adj = 0, cex = 2, col = txtcol, outer = TRUE)
  mtext(sub, 3L, line = -0.5, adj = 0, cex = 1.5, col = txtcol, outer = TRUE)
  mtext(note, 1L, line = 1, adj = 1, cex = 1.25, font = 3L,
        col = txtcol, outer = TRUE)
  mtext(foot, 1L, line = 1, adj = 0, cex = 1.25, font = 3L,
        col = txtcol, outer = TRUE)
  
  invisible(NULL)
}

#' bump chart
#' 
#' A bump chart
#' 
#' @param mat an \code{n x t} matrix with \code{n} observations and \code{t}
#' timepoints
#' @param adj label position adjustment, larger values move labels farther
#' from start/end positions
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @examples
#' mat <- replicate(5, sample(1:10))
#' dimnames(mat) <- list(rownames(mtcars)[1:nrow(mat)],
#'                       paste0('time', 1:ncol(mat)))
#' bump(mat, mar = c(2, 0, 2, 9))
#' 
#' @export

bump <- function(mat, adj = 0.1, ...) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  plot.new()
  par(...)
  plot.window(c(0, ncol(mat)), c(0, nrow(mat)), xaxs = 'i', yaxs = 'i')
  
  segments(seq.int(ncol(mat)), y0 = 1 - adj, y1 = nrow(mat),
           col = 'grey70', lty = 'dashed')
  
  lapply(seq.int(nrow(mat)), function(x)
    lines(mat[x, ], col = x, xpd = TRUE))
  
  rn <- rownames(mat)
  start <- order(mat[, 1L])
  end <- order(mat[, ncol(mat)])
  
  text(1 - adj, seq.int(nrow(mat)), rn[start],
       col = start, adj = 1, xpd = NA)
  text(ncol(mat) + adj, seq.int(nrow(mat)), rn[end],
       col = end, xpd = NA, adj = 0)
  text(seq.int(ncol(mat)), 0, colnames(mat), xpd = NA)
  
  invisible(NULL)
}

#' Minimal bar plots
#' 
#' A bar plot.
#' 
#' @param height an integer vector of counts for each bar
#' @param unit the units of \code{height}, used to label \code{max(height)}
#' @param horiz logical; if \code{TRUE}, horizontal bars are drawn
#' @param min bars less than \code{min} are not labelled
#' @param ... additional arguments passed to \code{\link{barplot}}
#' 
#' @seealso
#' \code{\link{prettybars}}; \code{\link{prettybars2}}; \code{\link{bibar}}
#' 
#' @examples
#' set.seed(1)
#' 
#' layout(matrix(c(1, 1, 1, 2:4), 3), widths = c(1, 1.5))
#' op <- par(las = 1L, mar = c(1, 1, 2, 2))
#' 
#' minbars(table(rbinom(500, 15, 0.5)), unit = 'Billion',
#'         col = adjustcolor('tomato4', alpha.f = 0.5))
#' mtext('Group 1', at = par('usr')[1L], adj = 0)
#' for (ii in 1:3) {
#'   minbars(table(rbinom(500, 15, 0.5)), unit = 'Million', min = 10,
#'           horiz = FALSE, col = adjustcolor(ii, alpha.f = 0.5))
#'   if (ii == 2L)
#'     abline(h = grconvertY(0:1, 'nfc'), xpd = TRUE)
#'   text(0, mean(par('usr')[3:4]), paste('Group', ii + 1L),
#'        xpd = NA, srt = 90, adj = c(0.5, -1), cex = 1.5)
#' }
#' par(op)
#' 
#' @export

minbars <- function(height, unit = NULL, horiz = TRUE, min = 0, ...) {
  height <- sort(height, decreasing = !horiz)
  
  max <- which.max(abs(height))
  lbl <- as.character(height)
  lbl[max] <- paste(lbl[max], unit)
  lbl <- trimws(lbl)
  lbl[height < min] <- ''
  
  bp <- barplot(
    height, axes = FALSE, horiz = horiz, border = NA, names.arg = FALSE, ...
  )
  if (horiz)
    text(height, bp, lbl, xpd = NA, adj = 1.1)
  else text(bp, height, lbl, xpd = NA, srt = 90, adj = 1.1)
  
  invisible(bp)
}
