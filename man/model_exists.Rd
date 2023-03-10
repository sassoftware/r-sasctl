% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_repository.R
\name{model_exists}
\alias{model_exists}
\title{Test if a model exists}
\usage{
model_exists(session, model, ...)
}
\arguments{
\item{session}{viya_connection object, obtained through \code{session} function}

\item{model}{\code{MMmodel} object, model ID or model name.}

\item{...}{additional parameters to be passed to \code{httr::GET} such as \code{httr::add_headers}}
}
\value{
boolean
}
\description{
Test if the model exists inside SAS Model Manager
}
\examples{

\dontrun{
model_exists(sess, model = ModelProj)
}

}
