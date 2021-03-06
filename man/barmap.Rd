% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_special.R
\name{barmap}
\alias{barmap}
\title{Map barplot}
\usage{
barmap(x, db = "worldHires", region, labels = NULL, cols = NULL)
}
\arguments{
\item{x}{a vector of proportions summing to 1 (approximately)}

\item{db}{data base; see \code{\link[maps]{map}}}

\item{region}{the region; see \code{\link[maps]{map}}}

\item{labels}{optional vector of labels for each section}

\item{cols}{a vector of colors for each section}
}
\description{
Creates a barplot in the shape of a map region.
}
\examples{
op <- par(mar = c(0, 0, 0, 0))
barmap(1, region = 'Germany')

barmap(c(1, 1, 1) / 3, region = 'Germany', cols = c('gold', 'red', 'black'))
 
voteGermany2013 <- read.table(
  header = TRUE, text = "Party Result
                         1 CDU/CSU   49.4
                         2     SPD   30.5
                         3   LINKE   10.2
                         4  GRUENE   10.0"
)
 
with(voteGermany2013, {
  barmap(Result / 100, region = 'Germany',
         labels = sprintf('\%s (\%s\%\%)', Party, Result))
})
par(op)

}
