% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplots.R
\name{ggtable}
\alias{ggtable}
\title{Table plot}
\usage{
ggtable(data, caption = dname, row.names, newpage = TRUE)
}
\arguments{
\item{data}{a matrix, table, or data frame}

\item{caption}{the table caption}

\item{row.names}{logical, \code{NULL}, or a character vector of row names
for the table}

\item{newpage}{logical; if \code{TRUE} the table is drawn on a new page}
}
\description{
Table plot
}
\examples{
library('grid')
library('gridExtra')
ggtable(head(mtcars))
ggtable(head(mtcars), row.names = FALSE)

}
\seealso{
\code{\link[gridExtra]{grid.table}}
}
