% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gplots.R
\name{ghist}
\alias{ghist}
\alias{ghist.default}
\title{ghistograms}
\usage{
ghist(x, ...)

\method{ghist}{default}(
  x,
  breaks = "Sturges",
  freq = NULL,
  probability = !freq,
  include.lowest = TRUE,
  right = TRUE,
  density = NULL,
  angle = 45,
  col = "white",
  border = NULL,
  main = paste("Histogram of", xname),
  xlim = range(breaks),
  ylim = NULL,
  xlab = xname,
  ylab,
  axes = TRUE,
  plot = TRUE,
  labels = FALSE,
  nclass = NULL,
  warn.unused = TRUE,
  grid = TRUE,
  col.grid = "grey90",
  col.acc = "white",
  ...
)
}
\arguments{
\item{x}{a vector of values for which the histogram is desired.}

\item{...}{further arguments and \link[graphics]{graphical parameters} passed to
    \code{\link[graphics]{plot.histogram}} and thence to \code{\link[graphics]{title}} and
    \code{\link[graphics]{axis}} (if \code{plot = TRUE}).}

\item{breaks}{one of:
    \itemize{
      \item a vector giving the breakpoints between histogram cells,
      \item a function to compute the vector of breakpoints,
      \item a single number giving the number of cells for the histogram,
      \item a character string naming an algorithm to compute the
      number of cells (see \sQuote{Details}),
      \item a function to compute the number of cells.
    }
    In the last three cases the number is a suggestion only; as the
    breakpoints will be set to \code{\link{pretty}} values, the number
    is limited to \code{1e6} (with a warning if it was larger).  If
    \code{breaks} is a function, the \code{x} vector is supplied to it
    as the only argument (and the number of breaks is only limited by
    the amount of available memory).
  }

\item{freq}{logical; if \code{TRUE}, the histogram graphic is a
    representation of frequencies, the \code{counts} component of
    the result; if \code{FALSE}, probability densities, component
    \code{density}, are plotted (so that the histogram has a total area
    of one).  Defaults to \code{TRUE} \emph{if and only if} \code{breaks} are
    equidistant (and \code{probability} is not specified).}

\item{probability}{an \emph{alias} for \code{!freq}, for S compatibility.}

\item{include.lowest}{logical; if \code{TRUE}, an \code{x[i]} equal to
    the \code{breaks} value will be included in the first (or last, for
    \code{right = FALSE}) bar.  This will be ignored (with a warning)
    unless \code{breaks} is a vector.}

\item{right}{logical; if \code{TRUE}, the histogram cells are
    right-closed (left open) intervals.}

\item{density}{the density of shading lines, in lines per inch.
    The default value of \code{NULL} means that no shading lines
    are drawn. Non-positive values of \code{density} also inhibit the
    drawing of shading lines.}

\item{angle}{the slope of shading lines, given as an angle in
    degrees (counter-clockwise).}

\item{col}{a colour to be used to fill the bars.
    The default used to be \code{NULL} (unfilled bars) in \R versions before 4.0.0.}

\item{border}{the color of the border around the bars.  The default
    is to use the standard foreground color.}

\item{main, xlab, ylab}{main title and axis labels: these arguments to
    \code{\link[graphics]{title}()} get \dQuote{smart} defaults here, e.g., the default
    \code{ylab} is \code{"Frequency"} iff \code{freq} is true.}

\item{xlim, ylim}{the range of x and y values with sensible defaults.
    Note that \code{xlim} is \emph{not} used to define the histogram (breaks),
    but only for plotting (when \code{plot = TRUE}).}

\item{axes}{logical.  If \code{TRUE} (default), axes are draw if the
    plot is drawn.}

\item{plot}{logical.  If \code{TRUE} (default), a histogram is
    plotted, otherwise a list of breaks and counts is returned.  In the
    latter case, a warning is used if (typically graphical) arguments
    are specified that only apply to the \code{plot = TRUE} case.}

\item{labels}{logical or character string.  Additionally draw labels on top
    of bars, if not \code{FALSE}; see \code{\link[graphics]{plot.histogram}}.}

\item{nclass}{numeric (integer).  For S(-PLUS) compatibility only,
    \code{nclass} is equivalent to \code{breaks} for a scalar or
    character argument.}

\item{warn.unused}{logical.  If \code{plot = FALSE} and
    \code{warn.unused = TRUE}, a warning will be issued when graphical
    parameters are passed to \code{hist.default()}.}

\item{grid}{logical; if \code{TRUE}, a background grid will be drawn}

\item{col.grid}{\code{grid} color}

\item{col.acc}{\code{grid} accent color}
}
\description{
The generic function \code{\link{ghist}} computes a histogram of the given 
data values. If \code{plot = TRUE}, the resulting object of 
\code{\link{class}} "\code{histogram}" is plotted by \code{\link{gplot.histogram}}
before it is returned.
}
