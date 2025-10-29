
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![r-universe status](https://sassoftware.r-universe.dev/sasctl/badges/version)](https://sassoftware.r-universe.dev/sasctl)
[![r-universe status](https://sassoftware.r-universe.dev/sasctl/badges/checks)](https://sassoftware.r-universe.dev/sasctl)

# R sasctl

<!-- badges: start -->
<!-- badges: end -->

## Table of Contents

1.  [Overview](#overview)
2.  [Installation](#installation)  
3.  [Session](#session)  
4.  [Examples](#examples)
    - [PMML to SAS Example](#pmml-to-sas-example)  
    - [A native R model example](#a-native-r-model-example)  
    - [vPOST and vGET convenient
      functions](#vpost-and-vget-convenient-functions)
5.  [Model Management helpers](#model-management-helpers)

## Overview

The goal of sasctl is to provide tools for easy access of SAS Viya APIs
from an R perspective. It has useful tools to make model management
easier.

## Installation

### Install from R

``` r

## sassoftware R-universe repository
install.packages('sasctl', repos = c('https://sassoftware.r-universe.dev', 'https://cloud.r-project.org'))

## dev version
remotes::install_git("https://github.com/sassoftware/r-sasctl")

## released version
## You first have to install the dependencies

install.packages(c("jsonlite", "httr", "uuid", "furrr", "ROCR", "reshape2", "base64enc", "dplyr", "glue"))

## then the package
install.packages("https://github.com/sassoftware/r-sasctl/releases/download/X.X.X/r-sasctl_X.X.X.tar.gz", type = "source", repos = NULL)

library("sasctl")
```

### Install from terminal

The SASCTL package for R is available from SAS as a tar.gz file. You can
download releases from
<https://github.com/sassoftware/r-sasctl/releases>.

After you download the package, you can install the package with a
command that is similar to the following:

``` bash
R CMD INSTALL r-sasctl-X.X.X.tar.gz
```

## Session

You have a few options on how to make a connections to the SAS Viya
server. The first example uses password authentication.

``` r
sess <- session(hostname = "http://myserver.sas.com",
                username = "sasuser",
                password = "s3cr3t")

sess
```

You may also use the .authinfo file.

``` r
## authinfo file (recommended)

sess <- session(hostname = "http://myserver.sas.com",
                authinfo = "./_authinfo")

# authinfo file structure:
# default login sasuser password s3cr3t

## or for mutiple hosts in a single file but hostname must match otherwise will fail
# host http://server1.sas.com login sasuser password s3cr3t
# host https://server2.sas.com login sasuser password s3cr3t
```

If you were provided access tokens or client_id and client_secret, you
may follow one of the following methods.

``` r
## cient_id and client_secret
sess <- session(hostname = "http://myserver.sas.com",
                client_id = "client_id",
                client_secret = "client_s3cr3t")

## token type

sess2 <- session(hostname = "https://myserver.sas.com",
                 oauth_token = token$access_token 
                  )

sess2


## if you want to use authorization code from Viya 4
## will open a browser, login and then  copy and paste the code in the R prompt terminal
## set client secret as "", only a client_id with authorization_code permission

sess2 <- session(hostname = "https://myserver.sas.com",
                 username = "username" # not required, you will be prompt on browser
                 client_id = "client_id", # only if default was removed
                 auth_code = TRUE 
                )
```

## Examples

The following examples offer different options of the model management
life cycle. First, the model is created. Then the model is registered
and published. Finally, the model is scored. The code samples include
expected responses inline as well.

### PMML to SAS Example

``` r
hmeq <- read.csv("https://support.sas.com/documentation/onlinedoc/viya/exampledatasets/hmeq.csv")

## removing missing data
hmeq[hmeq == ""] <- NA # empty strings to NA
hmeq <- na.omit(hmeq)
hmeq$BAD <- as.factor(hmeq$BAD)
hmeq$REASON <- as.factor(hmeq$REASON)
hmeq$JOB <- as.factor(hmeq$JOB)

## creating logistic regression
model1 <- glm(BAD ~ ., hmeq, family = binomial("logit"))
summary(model1)
## saving model as pmml
XML::saveXML(pmml::pmml(model1, model.name = "General_Regression_Model",
             app.name = "Rattle/PMML",
             description = "Logistic Regression Model"),
             "my_model.pmml")


## registering the model
mod <- register_model(
  session = sess,
  file = "my_model.pmml",
  name = "R_model_pmml",
  type = "pmml",
  project = "rsasctl_auto",
  force = TRUE
  )

module <- publish_model(sess, mod, "R_model_pmml") ## defaults to maslocal

## 10 rows
## see documentation for parallel request

scored <- masScore(sess, module, hmeq[1:10,-1])

scored

## deleteing a project delete all associated models
delete_project(sess, "rsasctl_auto")

## delete the published model
delete_masmodule(sess, "R_model_pmml")
```

### A native R model example

``` r

hmeq <- read.csv("https://support.sas.com/documentation/onlinedoc/viya/exampledatasets/hmeq.csv")

hmeq[hmeq == ""] <- NA
hmeq <- na.omit(hmeq) ### probably you don't want to do that, by for sake of simplicity
hmeq$BAD <- as.factor(hmeq$BAD)
hmeq$REASON <- as.factor(hmeq$REASON)
hmeq$JOB <- as.factor(hmeq$JOB)

### creating train/test/val
partition <- sample(c(1,2,3), replace = TRUE, prob = c(0.7, 0.2, 0.1), size = nrow(hmeq))


### logistic regression
model1 <- glm(formula = BAD ~ .,
              family = binomial(link = 'logit'),
              data = hmeq[partition == 1,]
              )

### model summary
summary(model1)

dir.create("myModel")
path <- "myModel/"


## model saved
saveRDS(model1, paste0(path, 'rlogistic.rda'), version = 2)


## creating the score code 
code <- codegen(model1, path = paste0(path, "scoreCode.R"), rds = "rlogistic.rda")

## The following function to creates a sample if you don't want to use the generated code
# create_scoreSample(path, openFile = FALSE)

## scoring the whole table

## running the generated scoring code for testing
codeExpression <- str2expression(code)
eval(codeExpression)

rdsPath <- path
result <- scoreFunction(LOAN = hmeq[, 'LOAN'],
                        MORTDUE = hmeq[, 'MORTDUE'],
                        VALUE = hmeq[, 'VALUE'],
                        REASON = hmeq[, 'REASON'],
                        JOB = hmeq[, 'JOB'],
                        YOJ = hmeq[, 'YOJ'],
                        DEROG = hmeq[, 'DEROG'],
                        DELINQ = hmeq[, 'DELINQ'],
                        CLAGE = hmeq[, 'CLAGE'],
                        NINQ = hmeq[, 'NINQ'],
                        CLNO = hmeq[, 'CLNO'],
                        DEBTINC = hmeq[, 'DEBTINC'])

scoreddf <- as.data.frame(result)
scoreddf$Actual <- as.numeric(hmeq$BAD) - 1
scoreddf$partition <- partition

### diagnostics requires the true Target column name defined in "targetName"
### and the predicted probability column name defined in "targetPredicted"

diags <- diagnosticsJson(validadedf = scoreddf[scoreddf$partition == 3,],
                         traindf = scoreddf[scoreddf$partition == 1,],
                         testdf = scoreddf[scoreddf$partition == 2,],
                         targetEventValue = 1,
                         targetName = "Actual",
                         targetPredicted = "EM_EVENTPROBABILITY",
                         path = path) ## safely ignore warning, knitr bug

## writing other files
write_in_out_json(hmeq[,-1], input = TRUE, path = path)

write_in_out_json(scoreddf[-c(4, 8, 9)], input = FALSE, path = path)

write_fileMetadata_json(scoreCodeName = "scoreCode.R",
                        scoreResource = "rlogistic.rda",
                        path = path)

write_ModelProperties_json(modelName = "Rlogistic",
                           modelFunction = "Classification",
                           trainTable = "hmeq",
                           algorithm = "Logistic Regression",
                           numTargetCategories = 2,
                           targetEvent = "1",
                           targetVariable = "BAD",
                           eventProbVar = "P_BAD1",
                           modeler = "sasctl man",
                           path = path)

files_to_zip <- list.files(path, "*.json|*.R|*.rda", full.names = T)
zip(paste0(path, "Rmodel.zip"), 
    files = files_to_zip)


mod <- register_model(
  session = sess,
  file = "myModel/Rmodel.zip",
  name = "RzipModel",
  type = "zip",
  project = "R_sasctl",
  force = TRUE
  )



## deleteing a project delete all associated models
delete_project(sess, "R_sasctl")
```

### vPOST and vGET convenient functions

#### MAS call example

You can make generic calls to endpoints with minimal effort.

``` r
models <- vGET(sess, 
                  "microanalyticScore/modules/")

models$items[c(2:3, 8)]
```

Next, we need to create the transform table using the correct JSON
payload for a MAS call, which doesn’t have a standard format.

``` json
### Payload for Viya MAS

 {"inputs": [
             {"name": "<input1_name>", "value": 123}, 
             {"name": "<input2_name>", "value": "string_value"}, 
             {"name": "<input3_name>", "value": null} ## if value: NA
             ] 
 }

### Payload for SCR on Viya 2021.1.5 or higher

  {
    "metadata": {
      "<metadata_1>": 1,
      "<metadata_2>": "any metadata string",
      "<metadata_3>": 3
    },
    "data": {
      "<input1_name>": 5.1,
      "<input2_name>": 0.2,
      "<input3_name>": "string_value"
    }
  }
  
### Payload for SCR batch mode on Viya 2024.7 or higher

  {
    "data": [
      [
        1,
        {
          "varnumeric1": 5.1,
          "varnumeric2": 0.2,
          "varcharacter": "string_value"
        }
      ],
      [
        2,
        {
          "varnumeric1": 5.1,
          "varnumeric2": 0.2,
          "varcharacter": "string_value"
        }
      ]
    ]
  }
```

There is a helper function that transform all the rows in a vector of
strings, where each string is a JSON payload, since you cannot send data
for batch scoring.

``` r
hmeq <- read.csv("https://support.sas.com/documentation/onlinedoc/viya/exampledatasets/hmeq.csv")

## Use argument scr = TRUE for SCR format
## Use scr_batch = TRUE for single JSON for SCR batch
hmeq_json <- format_data_json(head(hmeq)) 

jsonlite::prettify(hmeq_json[1])
```

Then you can make a call to a Model.

``` r
output <- sasctl::vPOST(sess, 
                 path = "microanalyticScore/modules/dt_hmeq/steps/score",
                 payload = hmeq_json[3], ## choose a row
                 # content_type used to be hard coded, but we would have less flexibility
                 httr::content_type("application/json") 
                 )

output
```

### Model Management helpers

``` r
## to write inputVar.json
## removing BAD column
write_in_out_json(hmeq[,2:ncol(hmeq)], input = FALSE)
```

``` r
## to write outputVar.json
## you should create your own output dataframe since it will be what you
## put in your score code, which usually means EM_PROBABILITY, EM_CLASSIFICATION,
## or whaterver names you create P_BAD1, P_BAD0, etc.

out_example <- data.frame(P_BAD0 = 0.78,
                           P_BAD1 = 0.22,
                           BAD = '1')

write_in_out_json(out_example)
```

``` r
## to write fileMetadata.json
## defaults should be fine, unless you use different file names

write_fileMetadata_json()
```

``` r
## to write ModelProperties.json
## defaults should be fine, unless you use different file names

write_ModelProperties_json(modelName = "My R Model", 
                           modelDescription = "Awesome Description", 
                           modelFunction = "Classification",
                           trainTable = " ",
                           algorithm = "Logistic Regression",
                           numTargetCategories = 2,
                           targetEvent = "BAD",
                           targetVariable = "P_BAD1",
                           eventProbVar = "P_BAD1",
                           modeler = "John SAS")
```
