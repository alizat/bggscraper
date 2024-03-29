% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/last_page_index.R
\name{last_page_index}
\alias{last_page_index}
\title{Last Page Index}
\usage{
last_page_index(link)
}
\arguments{
\item{link}{BGG link for which to get the index of the last page}
}
\value{
Last page index for the given BGG link.
}
\description{
\code{last_page_index()} gets the index of the last page on a
  given BGG link
}
\examples{
link <- 'https://boardgamegeek.com/browse/boardgamedesigner'
indx <- last_page_index(link)
indx
}
