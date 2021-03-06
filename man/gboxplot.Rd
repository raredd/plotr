\name{gboxplot}
\alias{gboxplot}
\alias{gboxplot.default}
\alias{gboxplot.formula}
\title{gboxplots}
\usage{
gboxplot(x, ...)

\method{gboxplot}{default}(x, ..., range = 1.5, width = NULL,
  varwidth = FALSE, notch = FALSE, outline = TRUE, names, plot = TRUE,
  border = par("fg"), col = NULL, log = "", pars = list(boxwex = 0.8,
  staplewex = 0.5, outwex = 0.5), horizontal = FALSE, add = FALSE,
  at = NULL, grid = TRUE, col.grid = "grey90", col.acc = "white")

\method{gboxplot}{formula}(formula, data = NULL, ..., subset,
  na.action = NULL, grid = TRUE, col.grid = "grey90", col.acc = "white")
}
\arguments{
\item{x}{for specifying data from which the boxplots are to be
    produced. Either a numeric vector, or a single list containing such
    vectors. Additional unnamed arguments specify further data
    as separate vectors (each corresponding to a component boxplot).
    \code{\link{NA}}s are allowed in the data.}

\item{...}{For the \code{formula} method, named arguments to be passed to
    the default method.

    For the default method, unnamed arguments are additional data
    vectors (unless \code{x} is a list when they are ignored), and named
    arguments are arguments and \link{graphical parameters} to be passed
    to \code{\link{bxp}} in addition to the ones given by argument
    \code{pars} (and override those in \code{pars}). Note that
    \code{bxp} may or may not make use of graphical parameters it is
    passed: see its documentation.
  }

\item{range}{this determines how far the plot whiskers extend out
    from the box.  If \code{range} is positive, the whiskers extend
    to the most extreme data point which is no more than
    \code{range} times the interquartile range from the box. A value
    of zero causes the whiskers to extend to the data extremes.}

\item{width}{a vector giving the relative widths of the boxes making
    up the plot.}

\item{varwidth}{if \code{varwidth} is \code{TRUE}, the boxes are
    drawn with widths proportional to the square-roots of the number
    of observations in the groups.}

\item{notch}{if \code{notch} is \code{TRUE}, a notch is drawn in
    each side of the boxes.  If the notches of two plots do not
    overlap this is \sQuote{strong evidence} that the two medians differ
    (Chambers \emph{et al}, 1983, p. 62).  See \code{\link{boxplot.stats}}
    for the calculations used.}

\item{outline}{if \code{outline} is not true, the outliers are
    not drawn (as points whereas S+ uses lines).}

\item{names}{group labels which will be printed under each boxplot.
    Can be a character vector or an \link{expression} (see
    \link{plotmath}).}

\item{plot}{if \code{TRUE} (the default) then a boxplot is
    produced.  If not, the summaries which the boxplots are based on
    are returned.}

\item{border}{an optional vector of colors for the outlines of the
    boxplots.  The values in \code{border} are recycled if the
    length of \code{border} is less than the number of plots.}

\item{col}{if \code{col} is non-null it is assumed to contain colors
    to be used to colour the bodies of the box plots. By default they
    are in the background colour.}

\item{log}{character indicating if x or y or both coordinates should
    be plotted in log scale.}

\item{pars}{a list of (potentially many) more graphical parameters,
    e.g., \code{boxwex} or \code{outpch}; these are passed to
    \code{\link{bxp}} (if \code{plot} is true); for details, see there.}

\item{horizontal}{logical indicating if the boxplots should be
    horizontal; default \code{FALSE} means vertical boxes.}

\item{add}{logical, if true \emph{add} boxplot to current plot.}

\item{at}{numeric vector giving the locations where the boxplots should
    be drawn, particularly when \code{add = TRUE};
    defaults to \code{1:n} where \code{n} is the number of boxes.}

\item{grid}{logical; if \code{TRUE}, a background grid will be drawn}

\item{col.grid}{\code{grid} color}

\item{col.acc}{\code{grid} accent color}

\item{formula}{a formula, such as \code{y ~ grp}, where \code{y} is a
    numeric vector of data values to be split into groups according to
    the grouping variable \code{grp} (usually a factor).}

\item{data}{a data.frame (or list) from which the variables in
    \code{formula} should be taken.}

\item{subset}{an optional vector specifying a subset of observations
    to be used for plotting.}

\item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is to ignore missing
    values in either the response or the group.}
}
\description{
Produce a box-and-whisker plot(s) of the given (grouped) values.
}
