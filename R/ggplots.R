### ggplot things
# ggcols, ggclip, ggwidths, ggsurv, survdat, ggmultiplot, facet_adjust,
# print.facet_adjust, ggcaterpillar, ggheat, ggheat2, set_panel_size,
# facet_limits, bar_counts, ggtable
###


#' ggplot colors
#' 
#' A function to replicate default \code{\link[ggplot2]{ggplot}} colors.
#' 
#' @param n number of colors
#' @param c the chroma of the color; the upper bound for chroma depends on hue
#' and luminance
#' @param l a value in the range \code{[0, 100]} giving the luminance of the 
#' color; for a given combination of hue and chroma, only a subset of this 
#' range is possible
#' @seealso \code{\link{hcl}}
#' 
#' @examples
#' plot(rnorm(1000), col = ggcols(1000), pch = 19)
#' 
#' @export

ggcols <- function(n, l = 65, c = 100) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = l, c = c)[1:n]
}

#' ggplot clipping
#' 
#' Toggles ggplot panel clipping.
#' 
#' @usage
#' ggclip(p, action = 'off')
#' 
#' @param p a ggplot object
#' @param action turn clipping \code{'off'} (default) or \code{'on'}
#' 
#' @examples
#' library(ggplot2)
#' library(grid)
#' 
#' (q <- qplot(1:10, 1:10,size = I(10)) + scale_y_continuous(expand = c(0, 0)))
#' ggclip(q)
#' 
#' dat <- data.frame(time = 0:5, y = 0:5)
#' (p <- ggplot(dat, aes(x = time, y = y)) +
#'     geom_area(alpha = .1) + theme_bw() +
#'     scale_y_continuous(expand = c(0, 0)) +
#'     scale_x_continuous(expand = c(0, 0)) +
#'     theme(panel.grid.major = element_blank(), 
#'           panel.grid.minor = element_blank(),
#'           axis.text.x=element_blank(),
#'           axis.ticks.x=element_blank()) +
#'      geom_segment(aes(x = 0, xend = 5 , y = 0, yend = 0), size = 1.5,
#'                   arrow = arrow(length = unit(0.6, 'cm'))))
#' 
#' ggclip(p)
#' 
#' 
#' @export

ggclip <- function(p, action = 'off') {
  if (action %ni% c('off','on'))
    stop('action should be \'on\' or \'off\'')
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- action
  grid.draw(gt)
}

#' Scale ggplot widths
#' 
#' Align two or more ggplots by scaling widths to equal size.
#' 
#' @param ... two or more ggplots
#' @param moreArgs a list of additional arguments passed to
#' \code{\link[gridExtra]{arrangeGrob}}
#' 
#' @examples
#' xx <- 0:100
#' dd <- data.frame(x = xx, y1 = sin(xx * pi / 10), y2 = xx ** 2, y3 = xx ** 3)
#' 
#' library('ggplot2')
#' p1 <- ggplot(dd, aes(x = x)) +
#'   geom_line(aes(y = y1))
#' p2 <- ggplot(dd, aes(x = x)) +
#'   geom_bar(aes(y = y2), stat = 'identity')
#' p3 <- ggplot(dd, aes(x = x)) +
#'   geom_line(aes(y = x)) +
#'   coord_cartesian(xlim = c(0,100))
#' 
#' library('gridExtra')
#' grid.arrange(p1, p2)
#' ggwidths(p1, p2, moreArgs = list(heights = c(1, 2)))
#' 
#' grid.arrange(p1, p2, p3)
#' ggwidths(p1, p2, p3)
#' 
#' @export

ggwidths <- function(..., moreArgs) {
  if (missing(moreArgs))
    moreArgs <- NULL else if (!is.list(moreArgs))
      stop('\'moreArgs\' should be a list')
  l <- list(...)
  l <- lapply(l, function(x) ggplot_gtable(ggplot_build(x)))
  widths <- do.call('unit.pmax', lapply(l, function(x) x$widths[2:3]))
  l <- lapply(l, function(x) {x$widths[2:3] <- widths; x})
  do.call('grid.arrange', c(l, moreArgs))
}

#' Survival curves with ggplot
#' 
#' Plot Kaplan-Meier or Cox proportional hazards models with at-risk table
#' using \code{\link[ggplot2]{ggplot}}.
#' 
#' The argument \code{confband = TRUE} does not plot a confidence band in the 
#' statistical sense, i.e., xx-percent chance of containing entire population 
#' of the survival curve which are wider than the point-wise confidence limits.
#' Rather, it refers to a band of color in \code{ggplot} objects, specifically 
#' the use of a \code{\link[ggplot2]{geom_ribbon}} geometric shape. The band is
#' bounded by the confidence limits calculated in the \code{survfit} object
#' which is passed to \code{ggsurv} in the initial function call.
#' 
#' Long strata labels can mis-align the at risk numbers and plot ticks. If the
#' arguments \code{plot.margin} and \code{table.margin} are \code{NULL}, the
#' function will make a guess based on the number of characters in the strata
#' labels. If this is not perfect, \code{plot.margin} and \code{table.margin}
#' can be specified explicitly by providing a single numeric corresponding to
#' the number of "lines" of padding (see \code{\link{unit}}). Note that the 
#' default for \code{ggplot} is \code{unit(.25, "lines")}.
#' 
#' @param s \code{\link{survfit}} or \code{survfit.cox} object
#' @param col.surv color of survival lines; should be one color or match 
#' number of strata
#' @param lty.surv line type used for survival line; default is 1 (solid line)
#' @param censor logical; if \code{TRUE}, plots censored observations (default)
#' @param col.cens color for censor marks
#' @param mark plotting character for censored observations
#' @param confin logical; plot confidence bounds around survival estimate
#' @param confband logical; plot confidence band; note that this is not a 
#' confidence band in the statistical sense; see details
#' @param col.band band colors; if \code{NULL}, uses default ggplot colors 
#' (default); should be one color or match number of strata
#' @param median logical; if \code{TRUE}, plots line corresponding to median 
#' survival (inherits \code{col.surv})
#' @param atrisk logical; if \code{TRUE}, adds table with number of at-risk
#' observatons at each \code{tick} timepoint; color of text inherits from 
#' \code{col.surv}
#' @param col.atrisk optional color for at risk text, e.g., 'black'
#' @param pval two-element numeric vector corresponding to x- and y-coordinates
#' to plot a p-value; p-value based on log-rank test for significant 
#' differences in Kaplan-Meier curves (see \code{\link[survival]{survdiff}});
#' if \code{NULL}, no test is performed (default)
#' @param basehaz logical; if \code{TRUE}, returns baseline survival curve of a 
#' \code{\link[survival]{coxph}} object; see \code{\link[survival]{basehaz}}
#' @param ticks three-element numeric vector corresponding to the "from," "to,"
#' and "by" arguments of \code{seq}, respectively; if \code{NULL}, \code{ticks}
#' will be the default values; if \code{NULL} and \code{atrist = TRUE}, 
#' defaults to \code{seq(0, max(time), length.out = 10)}
#' @param median.ticks logical; if \code{TRUE}, tick labels will be shown for 
#' median survival times (user should provide \code{ticks} argument to avoid
#' overlapping labels); if \code{atrisk = TRUE}, plot will also provide number
#' at risk at median survival time(s)
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param main title
#' @param xlim two-element numeric vector of x-axis range; see 
#' \code{\link[ggplot2]{xlim}}; if \code{NULL}, default is used
#' @param ylim two-element numeric vector of y-axis range; see 
#' \code{\link[ggplot2]{ylim}}; if \code{NULL}, default is used
#' @param legend legend position (no legend if no strata present); takes values
#' of \code{TRUE}, \code{FALSE}, "none," "right," "left," "top," "bottom," or 
#' two-element numeric vector
#' @param legend.labels labels to use for strata in legend; defaults are made 
#' by \code{\link[survival]{survfit}}
#' @param grid logical; if \code{TRUE}, grid lines are drawn at major and 
#' minor ticks
#' @param ggdefault logical; use default ggplot background; if \code{FALSE}, 
#' white background is used (default)
#' @param plot.margin numeric; extra "lines" added to left margin of plot; see
#' details
#' @param table.margin numeric; extra "lines" added to left margin of at risk
#' table; see details
#' @param ... for backwards compatibility with deprecated arguments
#' 
#' @seealso \code{\link[rawr]{kmplot}}; \code{survival:::plot.survfit}
#'
#' @examples
#' \dontrun{
#' library('ggplot2')
#' library('grid')
#' library('gridExtra')
#' library('survival')
#' data(cancer)
#' 
#' cancer <- within(cancer, {
#'   age.cat <- factor(as.numeric(cut(age, c(-Inf, 50, 60, 70, Inf))))
#'   meal.cat <- factor(as.numeric(cut(meal.cal, 
#'     c(-Inf,quantile(meal.cal, c(.25,.5,.75), na.rm = TRUE), Inf))))
#'   wt.cat <- factor(as.numeric(cut(wt.loss, c(-Inf,quantile(wt.loss, 
#'     c(.25,.5,.75), na.rm = TRUE),Inf))))
#' })
#' 
#' ## fitting models
#' # kaplan-meier
#' kmfit0 <- survfit(Surv(time = time, event = status) ~ 1, data = cancer, 
#'   conf.type = 'log-log')
#' kmfit1 <- survfit(Surv(time = time, event = status) ~ sex, data = cancer, 
#'   conf.type = 'log-log')
#'   
#' # cox proportional hazards
#' coxfit0 <- survfit(coxph(Surv(time = time, event = status) ~ strata(age.cat),
#'   data = cancer))
#' coxfit1 <- survfit(coxph(Surv(time = time, event = status) ~ strata(I(age > 45)),
#'   data = cancer))
#' 
#' ggsurv(kmfit0)
#' 
#' ggsurv(kmfit1, confin = FALSE, lty.surv = 1:2,
#'   grid = FALSE, pval = c(500, .75))
#' 
#' ggsurv(kmfit1, confin = FALSE, lty.surv = 1:2,
#'   confband = TRUE, col.band = c('blue','red'),
#'   legend.labels = c('Male', 'Female'),
#'   median = TRUE, ticks = c(0, 1000, 200))
#'   
#' ggsurv(coxfit0, basehaz = TRUE, col.surv = 1:4)
#' 
#' ## this long label mis-aligns the table numbers, so we can use plot.margin
#' ## to adjust; it may be easier to adjust plot.margin instead of table.margin
#' ggsurv(coxfit0, confin = FALSE, col.atrisk = 'black',
#'   col.surv = c('red','green','blue','black'),
#'   legend.labels = c('Less than 50','50-60','60-70','70+'),
#'   plot.margin = 3)
#' 
#' 
#' png('plot.png', height = 600, width = 750)
#' ggsurv(coxfit1, confin = FALSE, median = TRUE, confband = FALSE,
#'   legend.labels = c('< 45','> 45'), legend = FALSE,
#'   col.surv = c('red','green'), mark = '#')
#' dev.off()
#' }
#' @export

ggsurv <- function(s, 
                   # basic plot options
                   col.surv = 1, lty.surv = 1,
                   censor = TRUE, col.cens = 1, mark = 3,
                   
                   # confidence options
                   confin = TRUE, confband = FALSE, col.band = NA, 
                   
                   # extra plot options
                   median = FALSE, atrisk = TRUE, col.atrisk,
                   pval, basehaz = FALSE,
                   
                   # aesthetics
                   ticks, median.ticks = TRUE,
                   xlab, ylab, main, xlim, ylim,
                   legend = 'right', legend.labels,
                   grid = FALSE, ggdefault = FALSE,
                   
                   # other options
                   plot.margin = NULL, table.margin = NULL, ...) {
  
  ## to do:
  # y axis ticks
  # specific axes ticks?, eg, at = c(.5, 1, 4, 10)
  # ticks labels with atrisk and median?
  
  #### error checks
  if (!inherits(s, 'survfit')) 
    stop('s must be a survfit object')
  if (basehaz && !inherits(s, 'survfit.cox')) 
    stop('s must be a survfit.cox object')
  if (confin & confband) 
    warning('choose confidence interval or confidence band')
  if (atrisk && missing(ticks)) 
    message('ticks not specified: \ntick marks defaulting to seq(0, ',
            'max(time), length.out = 10)\n')
  if (missing(xlab)) xlab <- 'Time'
  if (missing(ylab)) ylab <- 'Probability of survival'
  if (missing(main)) main <- ''
  
  #### allow for backwards compatibility
  m <- match.call(expand.dots = FALSE)
  if (!is.null(m$...)) {
    depr <- c('surv.col','surv.lty','cens.col',
              'cens.shape','band.col','atrisk.col')
    if (any(depr %in% names(m$...))) {
      warning('\n NOTE: using deprecated arguments: ', 
              paste(depr[depr %in% names(m$...)], collapse = ' '))
    }
    if (!is.null(m$...) && !all(names(m$...) %in% depr))
      warning('\n NOTE: unused arguments: ', 
              paste(names(m$...)[names(m$...) %ni% depr], collapse = ' '))
    
    try(list(if (!is.null(m$...$surv.col)) col.surv <- eval(m$...$surv.col),
             if (!is.null(m$...$surv.lty)) lty.surv <- eval(m$...$surv.lty),
             if (!is.null(m$...$cens.col)) col.cens <- eval(m$...$cens.col),
             if (!is.null(m$...$cens.shape)) mark <- eval(m$...$cens.shape),
             if (!is.null(m$...$band.col)) col.band <- eval(m$...$band.col),
             if (!is.null(m$...$atrisk.col)) 
               col.atrisk <- eval(m$...$atrisk.col)), 
        silent = TRUE)
  }
  
  survdat <- outdata <- survdat_(s)
  # if (data) 
  #   return(outdata)
  
  #### for custom ribbon color with no strata present
  if (is.null(survdat$strata) && !missing(col.band)) 
    survdat$col.band <- col.band
  
  #### change levels in strata
  if (!is.null(survdat$strata) && !(missing(legend.labels))) {
    if (length(unique(levels(survdat$strata))) != length(legend.labels)) {
      warning('legend labels not equal to number of strata')
    }
    recodes <- list(original = levels(survdat$strata),
                    replaced = legend.labels)
    cat('\nstrata labels recoded as follows:\n\n')
    print(do.call(data.frame, recodes))
    if (inherits(res <- try(rawr::recoder), 'try-error')) {
      survdat$strata <- factor(survdat$strata, 
                               levels = levels(survdat$strata),
                               labels = legend.labels)
    } else { 
      survdat$strata <- rawr::recoder(object = survdat$strata, 
                                      pattern = levels(survdat$strata),
                                      replacement = legend.labels)
    }
    survdat$strata <- droplevels(survdat$strata)
  }
  
  #### graph with no strata ####
  
  if (is.null(survdat$strata)) {
    if (length(col.band) > 1) warning('more colors chosen than bands')
    if (length(col.surv) > 1) warning('more colors chosen than lines')
    
    if (is.null(col.surv)) col.surv <- 'black'
    
    # step plot
    tmp <- ggplot(data = survdat, aes(x = time, y = surv))
    if (is.null(col.surv)) {
      tmp <- tmp + geom_step(colour = 'black', lty = lty.surv, direction = 'hv')
    } else {
      tmp <- tmp + geom_step(colour = col.surv, lty = lty.surv,
                             direction = 'hv')
    }
    # add censored observations
    if (censor) {
      if (is.null(col.cens) & !is.null(col.surv)) {
        tmp <- tmp + geom_point(data = subset(survdat, n.censor > 0), 
                                aes(x = time, y = surv), 
                                colour = col.surv, shape = mark) 
      } else { 
        if (is.null(col.cens) & is.null(col.surv)) {
          tmp <- tmp + geom_point(data = subset(survdat, n.censor > 0),
                                  aes(x = time, y = surv),
                                  colour = 'black', shape = mark)
        } else {
          tmp <- tmp + geom_point(data = subset(survdat, n.censor > 0),
                                  aes(x = time, y = surv),
                                  colour = col.cens, shape = mark)
        }
      }
    }
    # add confidence interval
    if (confin) {
      tmp <- tmp + 
        geom_step(aes(x = time, y = upper), 
                  direction = 'hv', linetype = 2, colour = col.surv) + 
        geom_step(aes(x = time, y = lower), 
                  direction = 'hv', linetype = 2, colour = col.surv)
    }
    # add confidence band
    if (confband) {
      if (is.null(survdat$col.band)) {
        tmp <- tmp + geom_ribbon(aes(x = time, ymax = upper, ymin = lower), 
                                 directions = 'hv',alpha = 0.25)
      } else {
        tmp <- tmp + geom_ribbon(aes(x = time, ymax = upper, ymin = lower, 
                                     fill = col.band), 
                                 direction = 'hv', alpha = 0.25) + 
          scale_fill_manual(values = col.band)
      }
    }
    # median survival line
    if (median) {
      options(survfit.rmean = 'individual')
      tmp.med <- summary(s)$table['median']
      cat('median survival time:',tmp.med)
      tmp.med <- data.frame(time = rep(unname(tmp.med), 2), 
                            quant = c(.5, 0))
      tmp.med <- subset(tmp.med, !is.na(time))
      if (nrow(tmp.med) == 0) {
        cat('\nmedian survival not reached\n')
      } else {
        tmp <- tmp + geom_line(data = tmp.med, aes(x = time, y = quant), 
                               colour = col.surv, linetype = 3) + 
          geom_point(data = tmp.med, aes(x = time, y = quant), 
                     colour = col.surv)
      }
    }
    
  } else { 
    
    #### graph with strata ####
    
    # make sure options are compatible
    # line colors
    col.survs <- if (length(col.surv == 1)) {
      scale_colour_manual(values = rep(col.surv, 
                                       length(unique(survdat$strata))))
    } else {
      scale_colour_manual(values = col.surv)
    } 
    if (is.null(col.surv)) col.survs <- NULL
    # line types
    lty.survs <- if (length(lty.surv == 1)) {
      scale_linetype_manual(values = rep(lty.surv, 
                                         length(unique(survdat$strata))))
    } else {
      scale_linetype_manual(values = lty.surv)
    }
    # censor shapes
    marks <- rep(mark, times = length(unique(survdat$strata)))
    
    # step plot
    tmp <- ggplot(data = survdat, aes(x = time, y = surv, 
                                      group = strata, colour = strata)) +
      geom_step(aes(colour = strata, group = strata, linetype = strata), 
                direction = 'hv') + col.survs + lty.survs
    # add censored observations
    if (censor) {
      if (is.null(col.cens)) {
        tmp <- tmp + geom_point(data = subset(survdat, n.censor > 0), 
                                aes(x = time, y = surv, colour = strata, 
                                    group = strata), shape = mark) 
      } else {
        tmp <- tmp + geom_point(data = subset(survdat, n.censor > 0), 
                                aes(x = time, y = surv, colour = strata, 
                                    group = strata, shape = strata)) +
          # scale_colour_manual(values = col.cens) +
          scale_shape_manual(values = marks)
      }
    }
    # add confidence interval
    if (confin) {      
      tmp <- tmp + 
        geom_step(aes(x = time, y = upper), direction = 'hv', 
                  linetype = 2, alpha = 0.5) + 
        geom_step(aes(x = time, y = lower), direction = 'hv', 
                  linetype = 2, alpha = 0.5)  
    }
    # add confidence band
    if (confband) {
      tmp <- tmp + geom_ribbon(aes(x = time, ymax = upper, ymin = lower, 
                                   fill = strata), 
                               directions = "hv", linetype = 0 ,alpha = 0.25)
      # custom conf band fill colors
      if (!missing(col.band)) 
        tmp <- tmp + scale_fill_manual(
          values = rep(col.band, length(unique(survdat$strata))))
    }
    
    # median survival line
    if (median) {
      options(survfit.rmean = 'individual')
      tmp.med <- summary(s)$table[ ,'median']
      cat('median survival times:\n')
      print(tmp.med)
      cols <- if (is.null(col.surv)) {
        rep(ggcols(length(names(tmp.med))), each = 2)
      } else {
        rep(col.surv, each = 2)
      }
      tmp.med <- data.frame(time = rep(tmp.med, each = 2), 
                            quant = rep(c(.5,0), times = length(tmp.med)), 
                            group = rep(names(tmp.med), each = 2),
                            col.surv = cols)
      tmp.med <- subset(tmp.med, !is.na(time))
      if (nrow(tmp.med) == 0) {
        cat('\nmedian survival not reached\n')
      } else {
        tmp <- tmp + geom_line(data = tmp.med, aes(time, quant, group = group),
                               colour = tmp.med$col.surv, linetype = 3) + 
          geom_point(data = tmp.med, aes(time, quant, group = group), 
                     colour = tmp.med$col.surv)
      }
    }
  }
  
  tmp <- tmp + xlab('Time') + ylab('Survival')
  
  ### test
  ### plotting baseline hazard instead of survival
  if (basehaz && inherits(s, 'survfit.cox')) {
    tmp.haz <- basehaz(eval(s$call$formula))
    
    if (is.null(tmp.haz$strata)) {
      tmp <- ggplot(data = tmp.haz, aes(x = time, y = hazard)) + 
        geom_line() + xlab('Time') + ylab('Hazard')
    } else {
      tmp <- ggplot(data = tmp.haz, aes(x = time, y = hazard, 
                                        colour = strata)) + 
        geom_line() + xlab('Time') + ylab('Hazard') + col.survs + lty.survs
    }
  }
  ### / test
  
  # label options
  if (!missing(xlab)) tmp <- tmp + xlab(xlab)
  if (!missing(ylab)) tmp <- tmp + ylab(ylab)
  if (!missing(main)) tmp <- tmp + ggtitle(main)
  
  # background options
  if (ggdefault == FALSE) tmp <- tmp + theme_bw()
  if (!grid) 
    tmp <- tmp + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.line = element_line(colour = 'black'))
  
  # need to remove legend if custom band color supplied for no strata plot
  if (is.null(s$strata)) {
    tmp <- tmp + theme(legend.position = 'none')
  } else { 
    if (is.logical(legend)) {
      legend <- if (!legend) 'none' else 'right'
    }
    tmp <- tmp + theme(legend.position = legend)
  }
  
  if (!missing(pval)) {
    # log-rank/Mantel-Haenszel (rho = 0)
    sdiff <- survival::survdiff(eval(s$call$formula), data = eval(s$call$data))
    pval.chisq <- pchisq(sdiff$chisq, length(sdiff$n) - 1, lower.tail = FALSE)
    pvaltxt <- ifelse(pval.chisq < 0.001, 'p < 0.001', 
                      paste('p =', signif(pval.chisq, 3)))
    tmp <- tmp + annotate('text', x = pval[1], y = pval[2], label = pvaltxt)
    print(sdiff)
  }
  
  ## added labels here
  if (!missing(ticks)) 
    tmp <- tmp + scale_x_continuous(breaks = seq(ticks[1], ticks[2], 
                                                 by = ticks[3]),
                                    labels = format(seq(ticks[1], ticks[2], 
                                                        by = ticks[3]), 
                                                    nsmall = 0))
  ## here
  if (!missing(ticks) && median && median.ticks) 
    tmp <- tmp + 
    scale_x_continuous(
      breaks = sort(c(tmp.med$time, seq(ticks[1], ticks[2], by = ticks[3]))),
      labels = format(sort(c(tmp.med$time, seq(ticks[1], ticks[2],
                                               by = ticks[3]))), nsmall = 0))
  if (!missing(xlim)) tmp <- tmp + xlim(xlim)
  if (!missing(ylim)) tmp <- tmp + ylim(ylim)
  tmp <- tmp + theme(legend.title = element_blank())
  
  if (atrisk) {
    
    # fix legend
    if (!(legend %in% c(FALSE, 'none', 'bottom', 'top'))) {
      legend <- 'bottom'
      message("when atrisk == TRUE, legend should be FALSE, 'none', ",
              "'bottom', or 'top'\nposition defaulting to 'bottom'")
    }
    
    # set up tick marks for atrisk alignment
    options(survfit.rmean = 'individual')
    if (missing(ticks)) {
      tick.seq <- seq(0, max(s$time), length = 10)
      if (median && median.ticks) 
        tick.seq <- sort(c(tmp.med$time, tick.seq))
      tmp <- tmp + scale_x_continuous(breaks = round(tick.seq))
    } else {
      tick.seq <- seq(ticks[1], ticks[2], ticks[3])
      if (median && median.ticks) tick.seq <- sort(c(tmp.med$time, tick.seq))
    }
    n.ticks <- length(tick.seq)
    
    # create data of at risk
    tmp.risk <- summary(s, times = tick.seq)
    risk.table <- data.frame(
      time = tmp.risk$time,
      n.risk = tmp.risk$n.risk)
    if (is.null(s$strata)) {
      risk.table$strata <- '1'
    } else risk.table$strata <- tmp.risk$strata
    
    # reverse order of risk.table by group
    risk.table <- do.call(rbind, 
                          lapply(rev(unique(risk.table$strata)), 
                                 function(x) 
                                   risk.table[risk.table$strata == x, ]))
    risk.table$strata <- factor(risk.table$strata, 
                                levels = rev(levels(risk.table$strata)))
    if (is.null(s$strata)) {
      risk.table$strata <- factor(' ')
      survdat$strata <- factor(' ')
    }
    
    # increase margins with long labels in risk.table
    if (missing(plot.margin))
      plot.margin <- .5 * (max(nchar(levels(survdat$strata))) - 1) - .3
    
    tmp <- tmp + 
      theme(plot.margin = unit(c(.25, .25, .4, plot.margin), 'lines'))
    
    # create table in ggplot
    gg.table <- ggplot(risk.table, 
                       aes(x = time, y = strata, colour = strata, 
                           label = format(n.risk, nsmall = 0))) + 
      theme_bw() + 
      scale_y_discrete(breaks = as.character(levels(risk.table$strata)), 
                       labels = rev(unique(survdat$strata))) +
      # scale_y_discrete(breaks = as.character(levels(risk.table$strata)), 
      #                  labels = rep('', length(unique(survdat$strata)))) +
      scale_x_continuous('Number at risk', breaks = tick.seq, 
                         limits = c(0, max(s$time))) + 
      theme(axis.title.x = element_text(size = 10, vjust = 1),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(), 
            axis.text.x = element_blank(),
            axis.ticks = element_blank(), 
            axis.text.y = element_text(face = 'bold', hjust = 1))
    # annotate('text', x = 0, y = length(unique(risk.table$strata)) + .5, 
    #          label = 'Number at risk')
    # ggtitle('Number at risk') 
    # theme(plot.title = element_text(size = rel(1)))
    if (!missing(col.atrisk))
      gg.table <- gg.table + geom_text(size = 3.5, colour = col.atrisk)
    else gg.table <- gg.table + geom_text(size = 3.5)
    
    if (missing(table.margin))
      table.margin <- 2
    gg.table <- gg.table + 
      theme(plot.margin = unit(c(-2, .5, .1, table.margin), 'lines'),
            legend.position = 'none') + xlab(NULL) + ylab(NULL)
    
    if (is.null(col.surv)) {
      gg.table <- gg.table + 
        scale_colour_manual(values = rev(ggcols(length(unique(risk.table$strata)))))
    } else {
      if (length(col.surv) == 1) 
        col.surv <- rep(col.surv, length(unique(survdat$strata)))
      gg.table <- gg.table + 
        scale_colour_manual(values = rev(col.surv))
    }
    
    # blank plot for place-holding
    blank.plot <- ggplot(data = survdat, aes(x = time, y = surv)) +
      geom_blank() + theme_bw() +
      theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(), panel.border = element_blank())
    
    # left/right legend location throws off at risk table alignment
    if (legend %ni% FALSE)
      tmp <- tmp + theme(legend.position = legend)
    if (is.null(s$strata)) 
      tmp <- tmp + theme(legend.position = 'none')
    
    tmp <- arrangeGrob(tmp, blank.plot, gg.table,
                       clip = FALSE, nrow = 3, ncol = 1,
                       heights = unit(c(2, .1, .25), c('null','null','null')))
  }
  grid.arrange(tmp)
}

#### create data from survfit object in proper format for ggplot
survdat_ <- function(s) {
  survdat <- NULL
  if (is.null(s$strata)) {   
    # if no strata are defined
    # initiate 100% survival at time == 0
    survdat <- data.frame(time = c(0, s$time),
                          n.risk = c(s$n, s$n.risk),
                          n.event = c(0, s$n.event),
                          n.censor = c(0, s$n.censor),
                          surv = c(1, s$surv),
                          lower = c(1, s$lower),  
                          upper = c(1, s$upper))
  } else {
    # if there is one or more strata
    surv.strata <- NULL
    for (i in 1:length(s$strata)) 
      surv.strata <- c(surv.strata, rep(names(s$strata)[i], s$strata[i]))
    survdat <- data.frame(time = s$time, 
                          n.risk = s$n.risk, 
                          n.event = s$n.event, 
                          n.censor = s$n.censor, 
                          surv = s$surv,
                          lower = s$lower, 
                          upper = s$upper, 
                          strata = factor(surv.strata))
    # initiate 100% survival at time == 0
    tmp <- NULL
    for (i in 1:length(s$strata)) {
      tmp0 <- survdat[survdat$strata == names(s$strata)[i], ]
      tmp1 <- data.frame(time = 0, 
                         n.risk = s[i]$n, 
                         n.event = 0, 
                         n.censor = 0,
                         surv = 1,
                         lower = 1,
                         upper = 1,
                         strata = names(s$strata)[i])
      tmp1 <- rbind(tmp1, tmp0)
      tmp <- rbind(tmp, tmp1)
    }
    survdat <- tmp[order(tmp$strata, tmp$time), ]
    rownames(survdat) <- NULL   
  }
  survdat
}

#' Draw multiple ggplot objects in a single layout
#' 
#' Uses functions in \pkg{grid} to arrange one or more 
#' \code{\link[ggplot2]{ggplot}} objects into a single layout.
#' 
#' @param ... ggplot objects
#' @param plotlist list of ggplot objects (optional)
#' @param ncol number of columns in layout
#' @param layout matrix specifying the layout; if present, \code{cols} is 
#' ignored; 
#' 
#' if layout is \code{matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE)},
#' for example, then plot 1 will go in the upper left, 2 will go in the upper 
#' right, and 3 will go all the way across the bottom
#' 
#' @examples
#' library('ggplot2')
#' gg <- ggplot(mtcars, aes(factor(1), fill = factor(cyl))) +
#'   geom_bar(width = 1) + theme_minimal()
#' gg1 <- gg + coord_polar()
#' gg2 <- gg + coord_polar(theta = 'y')
#' gg3 <- ggplot(mtcars, aes(factor(cyl), fill = factor(cyl))) +
#'   geom_bar(width = 1) + coord_polar() + theme_minimal()
#' 
#' ggmultiplot(gg)
#' ggmultiplot(gg1, gg2, gg3, ncol = 2)
#' ggmultiplot(
#'   gg1, gg2, gg3,
#'   layout = matrix(c(1, 2, 3, 3, 3, 3), ncol = 2, byrow = TRUE)
#' )
#'   
#' @export

ggmultiplot <- function(..., plotlist = NULL, ncol = 1L, layout = NULL) {
  pl <- c(list(...), plotlist)
  if (!(np <- length(pl)))
    stop('No plots')
  
  layout <- if (is.null(layout))
    matrix(seq.int(ncol * ceiling(np / ncol)), ceiling(np / ncol), ncol)
  else layout
  
  if (np > 1L) {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (ii in seq.int(np)) {
      matchidx <- as.data.frame(which(layout == ii, arr.ind = TRUE))
      print(pl[[ii]],
            vp = viewport(layout.pos.row = matchidx$row,
                          layout.pos.col = matchidx$col))
    }
  } else print(pl[[1L]])
  invisible(pl)
}

#' Facet labeling 
#' 
#' Adjusts labels on x-axes when using \code{\link[ggplot2]{facet_wrap}}.
#'   
#' @param x \code{\link[ggplot2]{ggplot}} object
#' @param pos position of labels
#' @param newpage draw new (empty) page first; see 
#' \code{\link[ggplot2]{print.ggplot}}
#' @param vp viewport to draw plot in; see \code{\link[ggplot2]{print.ggplot}}
#' 
#' @return
#' \code{facet_adjust} object that inherits \code{\link[gtable]{gtable}} class
#' 
#' @examples
#' \dontrun{
#' library('ggplot2')
#' ## missing some labels 
#' p <- ggplot(diamonds[1:100, ], aes(carat, price, colour = clarity)) +
#'   geom_point() + facet_wrap( ~ cut)
#' p
#' 
#' facet_adjust(p)
#' facet_adjust(p, pos = 'down')
#' }
#' 
#' @export

facet_adjust <- function(x, pos = c('up', 'down'), 
                         newpage = is.null(vp), vp = NULL) {
  pos <- match.arg(pos)
  p <- ggplot_build(x)
  gtable <- ggplot_gtable(p)
  # dev.off() ## this prevented plots from being rendered in knitr
  
  ## finding dimensions
  # dims <- apply(p$panel$layout[2:3], 2L, max)
  dims <- apply(p$layout$layout[2:3], 2L, max)
  nrow <- dims[1L]
  ncol <- dims[2L]
  
  ## number of panels in plot
  # panels <- sum(grepl('panel', names(gtable$grobs)))
  panels <- sum(grepl('panel', gtable$layout$name))
  space <- ncol * nrow
  
  ## missing panels
  n <- space - panels
  
  ## check if modifications are needed
  if (panels != space) {
    ## indices of panels to fix
    idx <- (space - ncol - n + 1L):(space - ncol)
    
    ## copy x-axis of last existing panel to the chosen panels in row above
    gtable$grobs[paste0('axis_b', idx)] <-
      list(gtable$grobs[[paste0('axis_b', panels)]])
    
    if (pos == 'down') {
      ## shift labels down to same level as x-axis of last panel
      rows <- grep(sprintf('axis_b\\-[%s-%s]', idx[1L], idx[n]),
                   gtable$layout$name)
      lastAxis <- grep(paste0('axis_b\\-', panels), gtable$layout$name)
      gtable$layout[rows, c('t', 'b')] <- gtable$layout[lastAxis, 't']
    }
  }
  
  structure(gtable, class = c('facet_adjust', 'gtable', 'ggplot', 'gg'))
}

#' facet_adjust print method
#' 
#' @param x object from \code{\link{facet_adjust}}
#' @param newpage draw new (empty) page first; see 
#' \code{\link[ggplot2]{print.ggplot}}
#' @param vp viewport to draw plot in; see \code{\link[ggplot2]{print.ggplot}}
#' 
#' @seealso \code{\link{facet_adjust}}

print.facet_adjust <- function(x, newpage = is.null(vp), vp = NULL) {
  if (newpage)
    grid.newpage()
  if (is.null(vp)){
    grid.draw(x)
  } else {
    if (is.character(vp)) 
      seekViewport(vp)
    else pushViewport(vp)
    grid.draw(x)
    upViewport()
  } 
  invisible(x)
}

#' Caterpillar plot
#' 
#' Caterpillar plots for random effects models using \code{\link{ggplot}}.
#' 
#' Behaves like \code{\link[lattice]{qqmath}} and 
#' \code{\link[lattice]{dotplot}} from the lattice package; also handles
#' models with multiple correlated random effects
#' 
#' @param re random effects from lmer object
#' @param qq if \code{TRUE}, returns normal q/q plot; else returns caterpillar
#' dotplot
#' @param likeDotplot if \code{TRUE}, uses different scales for random effects,
#' i.e., \code{\link[ggplot2]{facet_wrap}}
#' 
#' @examples
#' \donttest{
#' library('lme4')
#' fit <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' 
#' ggcaterpillar(ranef(fit, condVar = TRUE))
#' 
#' ## compare (requires lattice package)
#' lattice::qqmath(ranef(fit, condVar = TRUE))
#' }
#' @export

ggcaterpillar <- function(re, qq  =  TRUE, likeDotplot  =  TRUE) {
  
  f <- function(x) {
    pv   <- attr(x, 'postVar')
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + 
      rep((0:(ncol(x) - 1)) * nrow(x), each = nrow(x))
    pDf  <- data.frame(y = unlist(x)[ord],
                       ci = 1.96 * se[ord],
                       nQQ = rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID = factor(rep(rownames(x), ncol(x))[ord], 
                                   levels = rownames(x)[ord]),
                       ind = gl(ncol(x), nrow(x), labels = names(x)))
    if (qq) {
      ## normal q/q plot
      p <- ggplot(pDf, aes(nQQ, y)) + 
        facet_wrap(~ ind, scales = 'free') +
        xlab('Standard normal quantiles') + 
        ylab('Random effect quantiles')
    } else {
      ## caterpillar dotplot
      p <- ggplot(pDf, aes(x = ID, y = y)) + coord_flip()
      if (likeDotplot) {
        ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap( ~ ind)
      } else {
        ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales = 'free_y')
      }
      p <- p + xlab('Levels') + ylab('Random effects')
    }
    
    p + theme_bw() + 
      theme(legend.position = 'none') + 
      geom_hline(yintercept = 0) + 
      geom_errorbar(aes(ymin = y - ci, ymax = y + ci), 
                    width = 0, colour = 'black') + 
      geom_point(aes(size = 1.2), colour = 'blue')
  }
  lapply(re, f)
}

#' ggheat
#' 
#' Function to plot a heat map using \code{\link{ggplot}}.
#' 
#' \code{gradn} takes a vector of \code{n} colors: either a list of names, a 
#' list of hexadecimal values, an existing color palette (i.e., 
#' \code{\link{heat.colors}}, \code{\link{rainbow}}, etc); see also 
#' \code{\link{palette}}, \code{\link{colors}}, the \code{RColorBrewer} 
#' package, etc.
#' 
#' @param cors matrix (or matrix-like object) of correlations
#' @param data data frame of data (in progress)
#' @param limits range of color scale in legend; see 
#' \code{\link[ggplot2]{discrete_scale}}
#' @param gradn vector of \code{n} colors to use, from low to high; see details
#' @param gradc vector of length two, low color and high color; see details
#' 
#' \code{gradc} takes a low color and a high color, respectively, and generates
#' a continuous scale between those colors; see 
#' \code{\link[ggplot2]{scale_fill_gradient}}
#' 
#' @seealso
#' \code{\link{cor}}, \code{\link{ggheat2}}, \code{\link[iplotr]{icorr}}
#' 
#' @examples
#' library('ggplot2')
#' tmp <- rawr::rescaler(matrix(1:25, 5))
#' diag(tmp) <- 1
#' colnames(tmp) <- rownames(tmp) <- LETTERS[1:5]
#' ggheat(cors = tmp, limits = c(0, 1))
#' ggheat(cors = tmp, gradn = NULL, gradc = c('white', 'red'))
#' 
#' @export

ggheat <- function(cors = NULL, data = NULL, limits = c(-1, 1),
                   gradn = rev(heat.colors(10)),
                   gradc = c('white', 'steelblue')) {
  
  zzz <- as.data.frame(cors)
  zzz <- stack(zzz)
  zzz$ind1 <- rownames(cors)
  names(zzz) <- c('corr', 'x', 'y')
  zzz <- within(zzz, {
    x <- factor(x, levels = colnames(cors), ordered = TRUE)
    y <- factor(y, levels = rownames(cors), ordered = TRUE)
  })
  
  p <- ggplot(zzz, aes(x = x, y = rev(y), fill = corr)) + 
    geom_tile() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(labels = rev(unique(zzz$y)), expand = c(0, 0)) +
    coord_fixed() + 
    theme_bw() + 
    theme(legend.position = 'right',
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  if (!is.null(gradc)) {
    p <- p + 
      scale_fill_gradient(limits = limits, low = gradc[1], high = gradc[2])
  }
  if (!is.null(gradn))
    p <- p + scale_fill_gradientn(limits = limits, colours = gradn)
  
  p
}

#' ggheat2
#'
#' Function to plot a heat map using \code{\link{ggplot}}.
#' 
#' Default cluster method is \code{stats::hclust(dist(x), method = 'average')}
#' which will return a list containing a named vector, "order", which is used
#' to reorder the variables.
#'
#' In order to pass a custom clustering function to \code{cluster}, the
#' function must take a single input (a correlation matrix) and return either
#' a vector or a list with a named vector, \code{"order"}.
#' 
#' @param data a data frame or matrix (observations x variables) of numeric
#' values
#' @param corr a correlation matrix
#' @param cluster logical or function; if \code{TRUE}, the variables will be
#' clustered and reordered; if \code{FALSE}, no reordering will be done;
#' otherwise, a custom clustering function may be given; see details
#' @param nbreaks number of breaks to categorize the correlations (default is
#' \code{NULL}, ie, a continuous scale)
#' @param palette for a continuous scale, a vector of length three giving the
#' low, mid, and high colors of the gradient (default is
#' \code{c('blue','white',''red)}, see \code{\link{scale_colour_gradient2}});
#' for a discrete scale, a character string of the palette name or an integer
#' giving the index of the palette, see \code{\link{scale_colour_brewer}}
#' @param legend_name the legend name; see \code{\link{discrete_scale}}
#' @param pch (optional) plotting character; if \code{missing},
#' \code{\link{geom_tile}} is used instead of plotting characters
#' @param cex size of \code{pch}; a vector of length one (all \code{pch} will
#' be the same size) or two (size will be proportional to the correlation);
#' default is c(2,6); see\code{\link{scale_size_identity}}
#' @param label logical; if \code{TRUE}, adds correlation coefficients on top
#' of each \code{pch}
#' @param label_alpha logical, if \code{TRUE}, adds alpha transparency when
#' \code{label} is \code{TRUE} (correlations closer to 0 will be less visible)
#' @param label_color color of correlations (default is \code{'black'})
#' @param label_digits number of digits in correlation labels
#' @param midpoint the midpoint value for continuous scaling of correlations
#' (default is \code{0})
#' @param clim vector of length two giving the limits of correlation
#' coefficients (default is \code{-1,1})
#' @param ... additional arguments passed to \code{\link{geom_text}} for the
#' diagonal labels
#' 
#' @seealso
#' \code{\link{cor}}, \code{\link{ggheat}}, \code{\link[iplotr]{icorr}},
#' \code{\link[arm]{corrplot}}, \url{https://github.com/briatte/ggcorr}
#' 
#' @examples
#' library('ggplot2')
#' ggheat2(mtcars)
#'
#' ggheat2(mtcars, label = TRUE, label_alpha = TRUE, cluster = FALSE,
#'         ## additional args passed to diagonal labels
#'         colour = 'red', angle = 45, size = 7)
#'
#' ggheat2(mtcars, pch = 19, nbreaks = 6, cex = c(2,10),
#'         palette = 'PuOr',         ## colorblind palette
#'         size = 5, hjust = 0.75) + ## passed to diag text
#'     labs(title = 'Correlation Matrix')
#'
#' ## custom clustering function
#' ggheat2(data = NULL, corr = cor(mtcars, use = 'pairwise'),
#'         nbreaks = 5, palette = 'Blues',
#'         cluster = function(...) sample(ncol(mtcars)))
#' 
#' @export

ggheat2 <- function(data, corr = cor(data, use = 'pairwise.complete'),
                    cluster = TRUE, nbreaks = NULL,
                    palette = if (is.null(nbreaks)) c('blue','white','red') else 1,
                    legend_name = expression(rho), pch, cex = c(2, 6),
                    label = FALSE, label_alpha = FALSE, label_color = 'black',
                    label_digits = 2, midpoint = 0, clim = c(-1, 1), ...) {
  
  ## clustering
  stopifnot(class(cluster) %in% c('logical','function'))
  ord <- if (is.function(cluster)) cluster(corr) else
    if (cluster) stats::hclust(dist(corr), method = 'average')$order else
      seq.int(ncol(corr))
  
  ord <-  tryCatch(if (is.vector(ord, mode = 'integer')) ord else ord$order,
                   error = function(e) {
                     warning('Variables not reordered; see \'details\' ',
                             'section on the use of \'cluster\'', domain = NA)
                     seq.int(ncol(corr))
                   },
                   warning = function(w) {
                     warning('Variables not reordered; see \'details\' ',
                             'section on the use of \'cluster\'', domain = NA)
                     seq.int(ncol(corr))
                   })
  corr <- corr[ord, ord]
  
  if (label_alpha)
    label <- TRUE
  colnames(corr) <- rownames(corr) <- make.names(colnames(corr))
  cex <- c(cex, cex)
  dd <- round(corr, label_digits)
  dd <- as.data.frame(dd * lower.tri(dd))
  rn <- names(dd)
  dd <- data.frame(row = rn, dd)
  dd <- reshape2::melt(dd, varying = list(2:ncol(dd)))
  
  corr <- as.data.frame(corr * lower.tri(corr))
  corr <- cbind.data.frame(row = rn, corr)
  corr <- reshape2::melt(corr, varying = list(2:ncol(corr)))
  corr$value[corr$value == 0] <- NA
  
  if (!is.null(nbreaks)) {
    s <- seq(-1, 1, length.out = nbreaks + 1)
    if (!nbreaks %% 2)
      s <- unique(sort(c(s, 0)))
    corr <- within(corr, {
      value <- droplevels(cut(value, breaks = s, include.lowest = TRUE))
      value <- factor(value,
                      levels = unique(cut(s, breaks = s, include.lowest = TRUE)))
    })
  }
  
  if (is.null(midpoint)) {
    midpoint <- median(corr$value, na.rm = TRUE)
    message('Color gradient midpoint set to median of correlation, ',
            round(midpoint, 2), domain = NA)
  }
  
  corr <- within(corr, {
    row <- factor(row, levels = unique(as.character(variable)))
    num <- as.numeric(value)
    num <- as.numeric(factor(abs(num - median(unique(num), na.rm = TRUE))))
    num <- seq(cex[1], cex[2], length.out = length(na.omit(unique(num))))[num]
    variable <- factor(variable, levels = levels(row))
  })
  
  diag  <- corr[with(corr, row == variable), ]
  corr <- corr[complete.cases(corr), ]
  p <- ggplot(corr, aes(x = row, y = variable))
  
  ## if pch is given, use geom_point
  if (!missing(pch)) {
    p <- p + geom_point(aes(size = num, colour = value), shape = pch)
    if (is.null(nbreaks))
      p <- p + scale_size_continuous(range = c(cex[1], cex[2])) +
        scale_colour_gradient2(legend_name, limits = clim, low = palette[1],
                               mid = palette[2], high = palette[3],
                               midpoint = midpoint) +
        guides(size = FALSE)
    else
      p <- p + scale_size_identity(legend_name) +
        scale_colour_brewer(legend_name, palette = palette, drop = FALSE) +
        guides(colour = guide_legend(
          legend_name, override.aes = list(size = (cex[1] + cex[2]) / 2))
        )
    ## use geom_tile otherwise
  } else {
    p <- p + geom_tile(aes(fill = value), colour = 'white') +
      if (is.null(nbreaks))
        scale_fill_gradient2(legend_name, low = palette[1], mid = palette[2],
                             high = palette[3], midpoint = midpoint,
                             limits = clim)
    else 
      scale_fill_brewer(legend_name, palette = palette, drop = FALSE)
  }
  
  ## corr labels
  if (label) {
    dd <- dd[dd$value != 0, ]
    p <- p +
      if (label_alpha)
        geom_text(data = dd, aes(row, variable, label = value,
                                 alpha = abs(as.numeric(value))),
                  show_guide = FALSE, colour = label_color)
    else geom_text(data = dd, aes(row, variable, label = value),
                   colour = label_color)
  }
  
  ## diagonal text and options
  p <- p  +
    geom_text(data = diag, aes(label = variable), ...) +
    scale_x_discrete(breaks = NULL, limits = levels(corr$row)) +
    scale_y_discrete(breaks = NULL, limits = levels(corr$variable)) +
    labs(x = NULL, y = NULL) +
    coord_equal() +
    theme(panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.key       = element_blank(),
          legend.title	   = element_text(size = 15),
          axis.text.x      = element_text(angle = -90))
  p
}

#' Set panel size
#' 
#' Use strict size for panels when number of panels in rows/columns are not
#' equal.
#' 
#' @param p a \code{\link[ggplot2]{ggplot}} object
#' @param file optional file name if result should be saved to file, passed
#' to \code{\link[ggplot2]{ggsave}}
#' @param margin a \code{\link[grid]{unit}} object giving the plot margins
#' @param width,height a \code{\link[grid]{unit}} object giving height and
#' width for each plot
#' 
#' @author
#' Baptiste Auguie
#' 
#' @references
#' \href{http://stackoverflow.com/questions/32580946/setting-absolute-size-of-facets-in-ggplot2}{SO question}
#' 
#' @examples
#' library('ggplot2')
#' library('gridExtra')
#' p1 <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_wrap(~ vs)
#' p2 <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_wrap(~ gear)
#' grid.arrange(p1, p2)
#' 
#' g1 <- set_panel_size(p1)
#' g2 <- set_panel_size(p2)
#' grid.arrange(g1, g2)
#' 
#' @export

set_panel_size <- function(p = NULL, file = NULL, margin = unit(1, 'cm'),
                           width = unit(4, 'cm'), height = unit(4, 'cm')) {
  g <- ggplotGrob(p)
  panels <- grep('panel', g$layout$name)
  panel_index_w <- unique(g$layout$l[panels])
  panel_index_h <- unique(g$layout$t[panels])
  nw <- length(panel_index_w)
  nh <- length(panel_index_h)
  
  if (getRversion() < numeric_version('3.3.0')) {
    # the following conversion is necessary because there is no `[<-`.unit
    # method so promoting to unit.list allows standard list indexing
    g$widths <- grid:::unit.list(g$widths)
    g$heights <- grid:::unit.list(g$heights)
    
    g$widths[panel_index_w] <-  rep(list(width),  nw)
    g$heights[panel_index_h] <- rep(list(height), nh)
  } else {
    g$widths[panel_index_w] <-  rep(width,  nw)
    g$heights[panel_index_h] <- rep(height, nh)
  }
  if (!is.null(file))
    ggsave(file, g,
           width = convertWidth(sum(g$widths) + margin, 'in', TRUE),
           height = convertHeight(sum(g$heights) + margin, 'in', TRUE))
  g
}

#' Custom facet limits
#' 
#' @description
#' Modify x-axis limits for \code{\link{facet_wrap}}.
#' 
#' When melting a data set of factor variables, the desired levels are dropped
#' during the reshaping to a single variable. Therefore, variables will either
#' show all unique values as levels or only levels where data is present. This
#' function gives ability to set custom x limits for each variable (including
#' the side effect of working for continuous variables).
#' 
#' @param pl a \code{\link{ggplot}} object
#' @param limits a named list containing one for more limits for each subplot
#' @param useNA how to handle \code{NA} values
#' 
#' @examples
#' library('ggplot2')
#' datf <- datn <- mtcars[, c(2, 8:11)]
#' datf[] <- lapply(datf, factor)
#' 
#' datfl <- reshape2::melt(datf, id.vars = 'am')
#' datnl <- reshape2::melt(datn, id.vars = 'am')
#' 
#' datfl$value[datfl$value == 4] <- NA
#' # datfl <- na.omit(datfl)
#' 
#' pf <- ggplot(datfl, aes(value, fill = factor(am))) +
#'   # facet_wrap(~ variable) +
#'   facet_wrap(~ variable, scales = 'free') +
#'   geom_bar(position = 'dodge')
#' 
#' ## facets either show all levels for each plot or only those with data
#' pf
#' 
#' facet_limits(pf, list(gear = c(3,4,5), vs = 0:6), useNA = 'no')
#' 
#' facet_limits(pf, lapply(datf, levels), useNA = 'no')
#' facet_limits(pf, lapply(datf, levels), useNA = 'ifany')
#' facet_limits(pf, lapply(datf, levels), useNA = 'always')
#' 
#' pn <- ggplot(datnl, aes(value, fill = factor(am))) +
#'   facet_wrap(~ variable, scales = 'free_x') +
#'   geom_bar(position = 'dodge')
#' pn
#' 
#' facet_limits(pn, list(carb = c(0,100)))
#' 
#' facet_limits(pn, lapply(datn, extendrange, f = 1))
#' facet_limits(pn, list(c(0,10), c(-1,5), c(2,8), c(0,20)))
#' 
#' @export

facet_limits <- function(pl, limits, useNA = c('ifany','no','always')) {
  if (missing(limits) || !is.list(limits))
    return(pl)
  ## at least scale_x needs to be free
  pl$facet$free$x <- TRUE
  useNA <- match.arg(useNA)
  
  ## add 'NA' as level and replace NA with 'NA'
  addNA2 <- function(x, ifany = FALSE) {
    if (!is.factor(x)) {
      x[is.na(x)] <- 'NA'
      x <- factor(x)
    }
    if (ifany & !anyNA(x))
      return(x)
    ll <- levels(x)
    if (!anyNA(ll))
      ll <- unique(c(ll, 'NA'))
    factor(x, levels = ll)
  }
  
  xvar <- tail(as.character(pl$mapping$x), 1L)
  yvar <- tail(as.character(pl$mapping$y), 1L)
  fvar <- as.character(pl$facet$facets %||% names(pl$facet$params$facets))
  
  data <- pl$data
  lvl  <- unique(data[, fvar])
  
  olimits <- lapply(split(data[, xvar], data[, fvar]), function(x)
    if (is.factor(x)) levels(droplevels(x)) else unique(x))
  if (length(limits) < length(lvl))
    limits <- modifyList(olimits, limits)
  
  if (class(pl$data[, xvar]) %in% c('character', 'factor') & useNA != 'no')
    pl$data[, xvar] <- addNA2(pl$data[, xvar], useNA == 'ifany')
  
  limits <- if (all(lvl %in% names(limits)))
    limits[as.character(lvl)]
  else if (length(limits) == length(lvl))
    setNames(limits, lvl)
  else stop('unable to match \'limits\' with facet panels', call. = FALSE)
  
  fun.na <- if (fac <- class(data[, xvar]) %in% c('factor', 'character')) {
    nas <- sapply(split(data[, xvar], data[, fvar]), anyNA) | useNA == 'always'
    if (any(nas))
      limits[nas] <- Map(c, limits[nas], 'NA')
    switch(useNA, ifany = function(x) addNA2(x, TRUE),
           no = function(x) x[x != 'NA'], always = addNA2)
  } else identity
  
  dummy <- lapply(seq_along(limits), function(x) {
    wh <- names(limits[x])
    if (!wh %in% lvl) return(NULL)
    lim <- fun.na(factor(limits[[x]]))
    tmp <- data[rep(1L, length(lim)), ]
    tmp[, yvar] <- Inf
    tmp[, fvar] <- factor(wh, lvl)
    tmp[, xvar] <- if (fac) lim else na.omit(limits[[x]])
    geom_blank(data = tmp)
  })
  
  ## add geom_blank before data layers keeps NA
  ## simply pl + geom_blank _without dropping NA_ seems to be limitation
  # pl$layers <- c(dummy, pl$layers)
  pl + dummy
}

#' Add counts to bar plots
#' 
#' Add a text layer to a bar plot with the count and percentage.
#' 
#' @param data data set to use for plot and \code{x}, \code{y}
#' @param x,fill,facet the variables used for \code{x} (required), \code{fill}
#' (optional), and \code{\link{facet_wrap}} (optional - currently only
#' \code{facet_wrap} and a single variable are supported) in the original plot
#' @param position a character string of \code{"stack"} or \code{"fill"}; this
#' will be the same value used in \code{\link{geom_bar}}
#' @param rev logical; controls the reversal of text labels along the y-axis;
#' depending on the version of \code{ggplot}, the levels of \code{fill} and
#' order in the plot may be reversed (this was only fixed recently in
#' \pkg{ggplot})
#' @param fmt a character string giving the format for the text; "n" and "p"
#' will be replaced with counts and percentages, respectively; all other text
#' will be left unchanged including newlines (\code{\\n}) and percent signs;
#' see examples
#' @param ... additional arguments passed to \code{\link{geom_text}}
#' 
#' @examples
#' mt <- within(mtcars, {
#'   vs <- factor(vs)
#'   gear <- factor(gear)
#' })
#' 
#' library('ggplot2')
#' ggplot(mt, aes(vs)) +
#'   geom_bar() +
#'   bar_counts(mt, vs, fmt = 'N = n', colour = 'white')
#' 
#' ggplot(mt, aes(vs, fill = gear)) +
#'   geom_bar() +
#'   facet_wrap(~ am) +
#'   bar_counts(mt, vs, gear, am, fmt = 'N = n\np%')
#' 
#' ggplot(mt, aes(vs, fill = gear)) +
#'   geom_bar(position = 'fill') +
#'   bar_counts(mt, vs, gear, position = 'fill',
#'              colour = 'white', size = 5, family = 'HersheySerif',
#'              fontface = 'bold.italic')
#' 
#' 
#' ## for ggplot2 >= 2.2.0 use geom_col with pre-computed stats
#' dat <- data.frame(prop.table(with(mt, table(vs, gear))))
#' dat <- within(dat, {
#'   lbl <- sprintf('Gear: %s\n(%s%%)', gear, Freq * 100)
#' })
#' 
#' ggplot(dat, aes(vs, Freq, fill = gear)) +
#'   geom_col() +
#'   geom_text(aes(label = gear), position = position_stack(vjust = 0.5))
#' 
#' ggplot(dat, aes(vs, Freq, fill = gear)) +
#'   geom_col(position = 'fill') +
#'   geom_text(aes(label = lbl), position = position_fill(vjust = 0.5))
#' 
#' 
#' ## or use geom_bar with stat = 'identity' and position = 'fill'
#' ggplot(dat, aes(vs, Freq, fill = gear)) +
#'   geom_bar(stat = 'identity', position = 'fill') +
#'   geom_text(aes(label = lbl), position = position_fill(vjust = 0.5))
#' 
#' @export

bar_counts <- function(data, x, fill, facet, position = c('stack', 'fill'),
                       rev = FALSE, fmt = 'n (p%)', ...) {
  x <- deparse(substitute(x))
  y <- if (missing(fill))
    NULL else deparse(substitute(fill))
  
  facet <- if (missing(facet))
    NULL else deparse(substitute(facet))
  position <- match.arg(position)
  
  data$n_alt <- ave(seq(nrow(data)),
                    as.list(data[, c(x, y, facet), drop = FALSE]),
                    FUN = length)
  data <- data[!duplicated(data[, c(x, y, facet)]), ]
  data <- data[order(data[, x], if (!is.null(y)) {
    if (rev) xtfrm(data[, y]) else -xtfrm(data[, y])
  } else seq_along(data[, x])), ]
  
  ind <- as.list(data[, c(x, facet), drop = FALSE])
  data <- within(data, {
    pos_alt <- ave(n_alt, ind, FUN = function(ii)
      ii / if (position == 'stack') 1 else sum(ii))
    pct_alt <- ave(pos_alt, ind, FUN = function(ii) cumsum(ii) - 0.5 * ii)
    n_pct   <- round(ave(n_alt, ind, FUN = function(ii) ii / sum(ii)) * 100)
    # lbl     <- sprintf(fmt, n_alt, n_pct)
  })
  
  fmt2 <- as.list(strsplit(gsub('[^np]', '', fmt), '')[[1]])
  fmt2 <- lapply(fmt2, function(x) {
    if (x == 'n')
      data$n_alt else if (x == 'p') data$n_pct
  })
  fmt <- gsub('%+', '%%', fmt)
  fmt <- gsub('n|p', '%s', fmt)
  data$lbl <- do.call('sprintf', c(list(fmt = fmt), fmt2))
  
  list(
    geom_text(aes_string(label = 'lbl', x = x, y = 'pct_alt'), data, ...)
  )
}

#' Table plot
#' 
#' @param data a matrix, table, or data frame
#' @param caption the table caption
#' @param row.names logical, \code{NULL}, or a character vector of row names
#' for the table
#' @param newpage logical; if \code{TRUE} the table is drawn on a new page
#' 
#' @seealso
#' \code{\link[gridExtra]{grid.table}}
#' 
#' @examples
#' library('grid')
#' library('gridExtra')
#' ggtable(head(mtcars))
#' ggtable(head(mtcars), row.names = FALSE)
#' 
#' @export

ggtable <- function(data, caption = dname, row.names, newpage = TRUE) {
  dname <- deparse(substitute(data))
  pad <- unit(2, 'line')
  
  row.names <- if (missing(row.names) || isTRUE(row.names))
    rownames(data) else if (is.null(row.names)) NULL else
      if (identical(row.names, FALSE)) FALSE else row.names
  
  theme <- list(
    bg_params = list(fill = '#d3d3d3'),
    fg_params = list(fontface = 2L,
                     col = if (identical(row.names, FALSE)) {
                       row.names <- NULL
                       0
                     } else 1)
  )
  
  theme <- ttheme_minimal(
    rowhead = theme,
    colhead = modifyList(theme, list(fg_params = list(col = 1)))
  )
  
  rownames(data) <- row.names
  gt <- tableGrob(data, theme = theme)
  
  cap <- textGrob(caption, gp = gpar(fontsize = 15))
  
  gt <- gtable::gtable_add_rows(gt, grobHeight(cap) + pad, 0)
  gt <- gtable::gtable_add_grob(gt, cap, 1, 1, 1, ncol(gt), Inf, 'off')
  
  if (newpage)
    grid.newpage()
  grid.draw(gt)
}
