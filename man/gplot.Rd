\name{gplot}
\alias{gplot}
\alias{gplot.default}
\alias{gplot.data.frame}
\title{Generic X-Y plotting}
\usage{
gplot(x, y, ...)

\method{gplot}{default}(x, y = NULL, type = "p", xlim = NULL,
  ylim = NULL, log = "", main = NULL, sub = NULL, xlab = NULL,
  ylab = NULL, ann = par("ann"), axes = TRUE, frame.plot = axes,
  panel.first = NULL, panel.last = NULL, asp = NA, ..., grid = TRUE,
  bty = "n", col.grid = "grey90", col.acc = "white",
  col.axis = "grey50", col.ticks = col.axis)

\method{gplot}{data.frame}(x, grid = TRUE, col.grid = "grey90",
  col.acc = "white", ...)
}
\arguments{
\item{x}{the coordinates of points in the plot. Alternatively, a
      single plotting structure, function or \emph{any \R object with a
        \code{plot} method} can be provided.}

\item{y}{the y coordinates of points in the plot, \emph{optional}
    if \code{x} is an appropriate structure.}

\item{...}{Arguments to be passed to methods, such as
    \link{graphical parameters} (see \code{\link{par}}).
    Many methods will accept the following arguments:
    \describe{
    \item{\code{type}}{what type of plot should be drawn.  Possible types are
      \itemize{
        \item \code{"p"} for \bold{p}oints,
        \item \code{"l"} for \bold{l}ines,
        \item \code{"b"} for \bold{b}oth,
        \item \code{"c"} for the lines part alone of \code{"b"},
        \item \code{"o"} for both \sQuote{\bold{o}verplotted},
        \item \code{"h"} for \sQuote{\bold{h}istogram} like (or
        \sQuote{high-density}) vertical lines,
        \item \code{"s"} for stair \bold{s}teps,
        \item \code{"S"} for other \bold{s}teps, see \sQuote{Details} below,
        \item \code{"n"} for no plotting.
      }
      All other \code{type}s give a warning or an error; using, e.g.,
      \code{type = "punkte"} being equivalent to \code{type = "p"} for S
      compatibility.  Note that some methods,
      e.g.\code{\link{plot.factor}}, do not accept this.
    }
    \item{\code{main}}{an overall title for the plot: see \code{\link{title}}.}
    \item{\code{sub}}{a sub title for the plot: see \code{\link{title}}.}
    \item{\code{xlab}}{a title for the x axis: see \code{\link{title}}.}
    \item{\code{ylab}}{a title for the y axis: see \code{\link{title}}.}
    \item{\code{asp}}{the \eqn{y/x} aspect ratio,
      see \code{\link{plot.window}}.}
    }
  }

\item{grid}{logical; if \code{TRUE}, a background grid will be drawn}

\item{col.grid}{\code{grid} color}

\item{col.acc}{\code{grid} accent color}
}
\description{
Generic plotting for \code{R} objects with default grid background. For 
simple scatter plots, \code{\link{plot.default}} will be used. However, 
there are \code{plot} methods for many \code{R} objects including
\code{\link{function}s}, \code{\link{data.frame}s}, 
\code{\link{density}} objects, etc. Use \code{methods(gplot)} and the 
documentation for these.
}
\examples{
gplot(1:10)
gplot(mtcars)
gplot(mpg ~ wt, data = mtcars)

}
