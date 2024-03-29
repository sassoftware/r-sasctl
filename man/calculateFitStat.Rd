% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/json_files.R
\name{calculateFitStat}
\alias{calculateFitStat}
\title{Write dmcas_fistat Json}
\usage{
calculateFitStat(
  targetName,
  targetPredicted,
  validadedf = NULL,
  traindf = NULL,
  testdf = NULL,
  type = "binary",
  targetEventValue = 1,
  path = "./",
  label.ordering = c(0, 1),
  cutoff = 0.5,
  noFile = FALSE
)
}
\arguments{
\item{targetName}{target variable column name (actuals)}

\item{targetPredicted}{target variable column name. When \code{type = "binary"} it should be a probability.}

\item{validadedf}{\code{data.frame} where the first column in the yActual (labels/value) and the second is yPrediction (target probability)}

\item{traindf}{\code{data.frame} where the first column in the yActual (labels/value) and the second is yPrediction (target probability)}

\item{testdf}{\code{data.frame} where the first column in the yActual (labels/value) and the second is yPrediction (target probability)}

\item{type}{\code{"binary"} or \code{"interval"}}

\item{targetEventValue}{if \code{type = "binary"} target class name for fit stat reference, if model is nominal, all other class will be counted as "not target"}

\item{path}{default to current work dir}

\item{label.ordering}{The default ordering (cf.details) of the classes can be changed by supplying a vector containing the negative and the positive class label. See \code{\link[ROCR:prediction]{ROCR::prediction()}}}

\item{cutoff}{cutoff to be used for calculation of miss classification for binary}

\item{noFile}{if you don't want to write to a file, only the output}
}
\value{
\itemize{
\item \code{list} that reflects the 'dmcas_fitstat.json'
\item 'dmcas_fitstat.json' file written to \code{path}
}
}
\description{
Calculates fit statistics from user data and writes it to a JSON file for
importing into the common model repository.
}
\examples{

 
df <- data.frame(label = sample(c(1,0), 6000, replace = TRUE),
                 prob = runif(6000),
                 partition = rep_len(1:3, 6000))
               
calculateFitStat(targetName = "label",
                 targetPredicted = "prob",
                 df[df$partition == 1, ], 
                 df[df$partition == 2, ],
                 df[df$partition == 3, ],
                 noFile = TRUE)
                                    
                                    
df2 <- data.frame(actual = rnorm(6000, 1000, 100),
                  predicted = rnorm(6000, 1000, 100),
                  partition = rep_len(1:3, 6000))
 
calculateFitStat(targetName = "actual",
                 targetPredicted = "predicted",
                 df2[df2$partition == 1, ],
                 df2[df2$partition == 2, ],
                 df2[df2$partition == 3, ],
                 type = "interval",
                 noFile = TRUE)
 
}
