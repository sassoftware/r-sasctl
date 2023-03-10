% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_repository.R
\name{project_exists}
\alias{project_exists}
\title{Test if a project exists}
\usage{
project_exists(session, project, ...)
}
\arguments{
\item{session}{viya_connection object, obtained through \code{session} function}

\item{project}{\code{MMproject} object, project ID or project name.}

\item{...}{additional parameters to be passed to \code{httr::GET} such as \code{httr::add_headers}}
}
\value{
boolean
}
\description{
Test if the project exists inside SAS Model Manager
}
\examples{

\dontrun{
project_exists(sess, project = ModelProj)
}

}
