% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_publish.R
\name{get_destination}
\alias{get_destination}
\title{Get a publishing destination by name}
\usage{
get_destination(session, name, ...)
}
\arguments{
\item{session}{viya_connection object, obtained through \code{session} function}

\item{name}{destination name}

\item{...}{additional parameters to be passed to \code{httr::GET} such as \code{httr::add_headers}}
}
\value{
A \code{data.frame} with the list of projects
}
\description{
Returns a publishing destination
}
\examples{

\dontrun{
destination <- get_destination(sess, 'maslocal')
destination
}

}
