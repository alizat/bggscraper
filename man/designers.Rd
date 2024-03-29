% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/designers.R
\name{designers}
\alias{designers}
\title{Designers of Board Games}
\usage{
designers(wait = 10, verbose = FALSE)
}
\arguments{
\item{wait}{number of seconds to wait between pages while scraping to avoid
being blocked by BGG (default is 10 seconds)}

\item{verbose}{whether to print supplementary text (shows intermediate
progress)}
}
\value{
Data frame containing list of designers from BGG website.
}
\description{
\code{designers()} retrieves the board game designers that are
  present at \href{http://boardgamegeek.com}{Board Game Geek}.
}
\examples{
designers(verbose = TRUE)
}
\seealso{
\code{\link{categories}} \code{\link{mechanics}}
  \code{\link{families}}
}
