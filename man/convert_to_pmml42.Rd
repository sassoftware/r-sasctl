% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_repository.R
\name{convert_to_pmml42}
\alias{convert_to_pmml42}
\title{Convert pmml 4.x to 4.2}
\usage{
convert_to_pmml42(file_in, file_out)
}
\arguments{
\item{file_in}{path to a .pmml file}

\item{file_out}{path to write the converted .pmml file}
}
\value{
nothing
}
\description{
Converts a pmml header text file from 4.x version to 4.2
}
\examples{
 
\dontrun{
hmeq <- read.csv("https://support.sas.com/documentation/onlinedoc/viya/exampledatasets/hmeq.csv",
                 stringsAsFactors = TRUE)

hmeq[hmeq == ""] <- NA
hmeq <- na.omit(hmeq)
hmeq$BAD <- as.factor(hmeq$BAD)

model1 <- glm(BAD ~ ., hmeq, family = binomial("logit"))
summary(model1)

XML::saveXML(pmml::pmml(model1, model.name = "General_Regression_Model",
                        app.name = "Rattle/PMML",
                        description = "Linear Regression Model"),
             "dev/my_model44.pmml")
             
convert_to_pmml42("my_model.pmml", "my_model_conv.pmml")
}

}
