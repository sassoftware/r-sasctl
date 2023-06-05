# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

#' Write variable json
#'
#' Writes a variable descriptor JSON file for input or output variables, based
#' on an input dataframe containing predictor and prediction columns.
#'
#' @param data `data.frame` to map the correct variable types to Json
#' @param input `TRUE` to write inputVar.json and `FALSE` to write `outputVar.json`
#' @param path default to current work dir
#' @param noFile if you don't want to write to a file, only the output
#' @return 
#' - `list` of the mapped types and sizes.
#' - 'inputVar.json' or 'outputVar.json' file written to `path`       
#' @examples
#'
#' write_in_out_json(iris[,5, drop = FALSE], input = FALSE, noFile = TRUE)
#' write_in_out_json(iris[,1:4], input = TRUE, noFile = TRUE)
#'
#' @export


write_in_out_json <- function(data, input = TRUE, path = './', noFile = FALSE){
  
  if (!is.data.frame(data)) {
    stop('data must be a data.frame, if you sliced a single column try data[,col_number, drop = FALSE] to keep as data.frame')
  }
  
  vars <- sapply(data, class)
  
  ### transform factors to character, otherwise it will miss identify the
  ### correct variable length
  
  data <- data.frame(lapply(data, FUN = 
                              function(x) {
                                if (is.factor(x)) as.character(x) else x }))
  
  ### variable lengths, 8 is default for numeric
  size <- sapply(data,
                 FUN = function(x){
                   if (is.character(x)) max(sapply(x, nchar)) else {8}}
  )
  
  df <- data.frame(name = names(vars),
                   length = size,
                   type = vars,
                   row.names = NULL)
  
  df$type <- ifelse(df$type %in% c('numeric', 'integer'), 'decimal', 'string')
  df$level <- ifelse(df$type == 'decimal', 'interval', 'nominal')
  
  path <- ifelse(grepl("\\/$", path), path, paste0(path, "/"))
  out_file <- paste0(path, ifelse(input, 'inputVar.json', 'outputVar.json'))
  
  if (!noFile) {
  jsonlite::write_json(df, out_file, pretty = T)
  print(paste0('File written to ', out_file))
  }
  
  df$role <- ifelse(input, 'input', 'output')
  
  attr(df, 'sasctl.attr') <- "variable.frame"
  return(df)
}


#' Write ModelProperties json
#'
#' Writes a descriptor JSON file for ModelProperties, it will configure
#' the model properties within Model Manager
#'
#' @param modelName Name of the model
#' @param modelDescription String describing the model
#' @param modelFunction Classification, Prediction, Segmentation, Analytical or Clustering.
#' @param trainTable Name of the training table
#' @param algorithm Algorithm name (Random Forest, GLM, Linear Regression, etc.)
#' @param numTargetCategories number of possible classes for classification
#' @param targetEvent Target event label eg: "1", "versicolor" etc.
#' @param targetVariable Target variable name
#' @param eventProbVar Variable name that has the `targetEvent` probability Chance
#' @param modeler Modeler's name
#' @param tool Name of the tool used to build the model
#' @param toolVersion Version of the tool used to build the model
#' @param noFile if you don't want to write to a file, only list the output
#' @param path file path where to write the json (don't include the filename)
#' 
#' 
#' @return 
#' - `list` of the mapped properties and values.
#' - 'ModelProperties.json' file written to `path`     
#'   
#' @examples
#' 
#' write_ModelProperties_json(modelName = "My R Model", 
#'                            modelDescription = "Awesome Description", 
#'                            modelFunction = "Classification",
#'                            trainTable = " ",
#'                            algorithm = "Logistic Regression",
#'                            numTargetCategories = 2,
#'                            targetEvent = "BAD",
#'                            targetVariable = "P_BAD1",
#'                            eventProbVar = "P_BAD1",
#'                            modeler = "John SAS",
#'                            noFile = TRUE)
#'
#' @export

write_ModelProperties_json <- function(modelName, 
                                       modelDescription = "R model", 
                                       modelFunction,
                                       trainTable = " ",
                                       #modelPredictors,
                                       algorithm,
                                       numTargetCategories,
                                       targetEvent,
                                       targetVariable,
                                       eventProbVar,
                                       modeler = " ",
                                       tool = "R",
                                       toolVersion = "default",
                                       path = './',
                                       noFile = FALSE){
  

targetLevel <- ifelse(numTargetCategories > 2, 
                      "Nominal", 
                      "Binary") 

toolVersion <- ifelse(toolVersion == "default",
                      paste0(utils::sessionInfo()[1]$R.version$major, ".",
                             utils::sessionInfo()[1]$R.version$minor),
              toolVersion
)

properties <- list(
    "name" = modelName,
    "description" = modelDescription,
    "function" = modelFunction,
    "scoreCodeType" = tool,
    "trainTable" = trainTable,
    "trainCodeType" = tool,
    "algorithm" = algorithm,
    "targetVariable" = targetVariable,
    "targetEvent" = targetEvent,
    "targetLevel" = targetLevel,
    "eventProbVar" = eventProbVar,
    "modeler" = modeler,
    "tool" = tool,
    "toolVersion" = as.character(toolVersion)
  )

path <- ifelse(grepl("\\/$", path), path, paste0(path, "/"))
out_file <- paste0(path, "ModelProperties.json")


if (!noFile) {
  jsonlite::write_json(properties, out_file, pretty = T, auto_unbox = T)
  print(paste0('File written to ', out_file))
}

return(properties)

}


#' Write fileMetadata json
#'
#' Writes a variable descriptor JSON file for fileMetadata, it will configure
#' the models files metadata within Model Manager in the first upload
#'
#' @param scoreCodeName Name of the scoring code file
#' @param scoreResource rda file name or other score resources.
#' @param additionalFilesNames additional files names.
#' @param additionalFilesRoles additional files role names.
#' @param noFile if you don't want to write to a file, only list the output
#' @param path filepath where to write the json (don't include the filename)
#' 
#' 
#' @return 
#' - `list` of the mapped properties and values.
#' - 'ModelProperties.json' file written to `path`     
#'   
#' @examples
#' 
#' ## Using default names and files
#' write_fileMetadata_json(noFile = TRUE)
#' 
#' ## addition file resources
#' ## send 2 vectors with file names and role name, must be of same length
#' 
#' write_fileMetadata_json(additionalFilesNames = c("myFileName.ext", "myFileName2.ext"),
#'                         additionalFilesRoles = c("scoreResource", "scoreResource"),
#'                         noFile = TRUE
#'                         )
#' 
#'
#' @export

write_fileMetadata_json <- function(scoreCodeName = "scoreCode.R",
                                    scoreResource = "model.rda",
                                    additionalFilesNames = c(),
                                    additionalFilesRoles = c(),
                                    path = './',
                                    noFile = FALSE){
  
  if (length(additionalFilesRoles) != length(additionalFilesNames)) {
    stop("additionalFilesRoles and additionalFilesNames must have the same length")
  }
    
  metadata <- list(
                    list(
                      "role" = "inputVariables",
                      "name" = "inputVar.json"
                    ),
                    list(
                      "role" = "outputVariables",
                      "name" = "outputVar.json"
                    ),
                    list(
                      "role" = "score",
                      "name" = scoreCodeName
                    ),
                    list(
                      "role" = "scoreResource",
                      "name" = scoreResource
                    )
                  )



  ### adding additional file configurations
  if (length(additionalFilesRoles) > 0) {
    
      for (i in 1:length(additionalFilesRoles)) {
          metadata <- append(metadata, 
                             list(
                             list("role" = additionalFilesRoles[i],
                                  "name" = additionalFilesNames[i])
                             )
                             )
      }
    
    }
  
  
    if (!noFile) {
      
    path <- ifelse(grepl("\\/$", path), path, paste0(path, "/"))
    out_file <- paste0(path, "fileMetadata.json")
    
    
    jsonlite::write_json(metadata, 
                         out_file, 
                         pretty = T, 
                         auto_unbox = T)
    
    print(paste0('File written to ', out_file))
    
    }
  
    return(metadata)
  
}

#' Format Data.Frame rows to json format
#' 
#' Viya MAS requires a very specific json format which is the goal of this function
#' to create
#' 
#' 
#' @param df data frame to be transformed in JSON format rows
#' @param scr boolean, if `TRUE` will write the new json format with metadata 
#' @param metadata_columns columns names to be used as metadata. If scr is set to `FALSE`, metatada_columns is ignored 
#' @return a vector of JSON strings
#' @examples 
#' 
#' json_output <- format_data_json(mtcars)
#' json_output
#' 
#' @export
#' 


format_data_json <- function(df, scr = FALSE, metadata_columns = NULL){
  
  
  ## check is any is factor, otherwise print in write_json_row 
  ## will write factor as numeric string

    if (any(sapply(df, is.factor) == TRUE)) {
      df <- as.data.frame(
        lapply(df, 
               function(x) {
                 if (is.factor(x)) as.character(x) else x
               }
        )
      )
    }
    

  if (scr) {
    
    outs <- by(df, list(seq_len(nrow(df))), write_json_row_scr, 
               metadata_columns = metadata_columns)  
    
  } else {
    
    outs <- by(df, list(seq_len(nrow(df))), write_json_row)
    
  }
  
  
  return(outs)
}

#' Format row to json
#' 
#' will create a single Json row
#' This is a vectorized version, without data manipulation
#' Convert all factors columns to string otherwise `ifelse()` will coerce to integer
#'  
#' @param row data frame row
#' @return a JSON string
#' @examples 
#' 
#' 
#' json_output <- write_json(mtcars[1,])
#' json_output
#' 
#' @noRd


write_json_row <- function(row) {
  
  ### goal is to create a son in the following format
  ### MAS requires that you send only ONE ROW
  #
  # '{"inputs": [
  #             {"name": "<input1_name>", "value": <decimal_value>}, \
  #             {"name": "<input2_name>", "value": "<string_value>"} \
  #             {"name": "<input3_name>"} ## if value: NA
  #             ] }'  
  
  var_vect <- paste0('{"name": "', colnames(row), '"', 
                     ifelse(sapply(row, is.na), paste0(', "value": null'),
                            ifelse(sapply(row, is.character), paste0(', "value": ', '"', row, '"'),   
                                   ## the "error" string output may give a silent error
                                   ## it is here for placeholder because if I add stop() it
                                   ## will halt the function, even though it is never returning "error"
                                   ifelse(sapply(row, is.numeric), paste0(', "value": ', row), "error"))), ' }')
  
  out <- paste0('{"inputs": [ ', paste0(var_vect, collapse = ", "), '] }')
  
  return(out)
  
  
}


#' Format row to SCR json
#' 
#' will create a single Json row in a newer supported format (Viya 2021.1.5 or above)
#' This is a vectorized version, without data manipulation
#' Convert all factors columns to string otherwise `ifelse()` will coerce to integer
#' 
#' @param row data frame row
#' @param metadata_column name of a column that is a list of metadatas
#' @return a JSON string
#' @examples 
#'
#' json_output <- write_json(mtcars[1,])
#' json_output
#' 
#' @noRd
#' 

write_json_row_scr <- function(row, metadata_columns = NULL) {
  
  ### goal is to create a son in the following format
  ### MAS requires that you send only ONE ROW
  ### This is the new format since viya 2021.1.5, easier to produce
  ### accepts metadata 
  # {
  #   "metadata": {
  #     "meta1": 1,
  #     "meta2": 2,
  #     "meta3": 3
  #   },
  #   "data": {
  #     "varnumeric1": 5.1,
  #     "varnumeric2": 0.2,
  #     "varcharacter": "setosa"
  #   }
  # } 
  
  if (!is.null(metadata_columns)) {
    
    if (is.character(metadata_columns)) {
      metadata_columns <- which(colnames(row) %in% metadata_columns)
    }
    
    row_meta <- row[ , metadata_columns, drop = FALSE]  
    
    row <- row[ , -metadata_columns, drop = FALSE]
    
    meta_vect <- paste0('"', colnames(row_meta), '": ', 
                        ifelse(sapply(row_meta, is.na), paste0('null'),
                               ifelse(sapply(row_meta, is.character), paste0('"', row_meta, '"'),   
                                      ## the "error" string output may give a silent error
                                      ## it is here for placeholder because if I add stop() it
                                      ## will halt the function, even though it is never returning "error"
                                      ifelse(sapply(row_meta, is.numeric), paste0(row_meta), "error"))), '')
  } else {
    meta_vect <- list()
  }
  
  var_vect <- paste0('"', colnames(row), '": ', 
                     ifelse(sapply(row, is.na), paste0('null'),
                            ifelse(sapply(row, is.character), paste0('"', row, '"'),   
                                   ## the "error" string output may give a silent error
                                   ## it is here for placeholder because if I add stop() it
                                   ## will halt the function, even though it is never returning "error"
                                   ifelse(sapply(row, is.numeric), paste0(row), "error"))), ' ')
  
  out <- paste0('{"metadata": {', paste0(meta_vect, collapse = ", "), "}, " ,
                
                '"data": {', paste0(var_vect, collapse = ", "), '} }')
  
  return(out)
  
}

#' Write dmcas_fistat Json
#'
#' Calculates fit statistics from user data and writes it to a JSON file for
#' importing into the common model repository. 
#'
#' @param validadedf `data.frame` where the first column in the yActual (labels/value) and the second is yPrediction (target probability)
#' @param testdf `data.frame` where the first column in the yActual (labels/value) and the second is yPrediction (target probability)
#' @param traindf `data.frame` where the first column in the yActual (labels/value) and the second is yPrediction (target probability)
#' @param targetName target variable column name (actuals)
#' @param targetPredicted target variable column name. When `type = "binary"` it should be a probability.
#' @param targetEventValue if `type = "binary"` target class name for fit stat reference, if model is nominal, all other class will be counted as "not target"
#' @param type `"binary"` or `"interval"`
#' @param cutoff cutoff to be used for calculation of miss classification for binary
#' @param path default to current work dir
#' @param label.ordering The default ordering (cf.details) of the classes can be changed by supplying a vector containing the negative and the positive class label. See [ROCR::prediction()]
#' @param noFile if you don't want to write to a file, only the output
#' @return 
#' - `list` that reflects the 'dmcas_fitstat.json'
#' - 'dmcas_fitstat.json' file written to `path`       
#' @examples
#'
#'
#' ## partition will be ignored since it is 3rd column
#'  
#' df <- data.frame(label = sample(c(1,0), 6000, replace = TRUE),
#'                 prob = runif(6000),
#'                 partition = rep_len(1:3, 6000))
#'                
#' calculateFitStat(df[df$partition == 1, ], 
#'                  df[df$partition == 2, ],
#'                  df[df$partition == 3, ],
#'                  targetName = "label",
#'                  targetPredicted = "prob",
#'                  noFile = TRUE)
#'                                     
#'                                     
#' df2 <- data.frame(actual = rnorm(6000, 1000, 100),
#'                   predicted = rnorm(6000, 1000, 100),
#'                   partition = rep_len(1:3, 6000))
#'  
#' calculateFitStat(df2[df2$partition == 1, ],
#'                  df2[df2$partition == 2, ],
#'                  df2[df2$partition == 3, ],
#'                  targetName = "actual",
#'                  targetPredicted = "predicted",
#'                  type = "interval",
#'                  noFile = TRUE)
#'  
#' @export

calculateFitStat <- function(validadedf, 
                             traindf, 
                             testdf,
                             targetName,
                             targetPredicted,
                             type = "binary",
                             targetEventValue = 1,
                             path = "./", 
                             label.ordering = c(0, 1),
                             cutoff = 0.5, 
                             noFile = FALSE) {
  
  
    
    
  ## TODO GAMMA 
  ## see: https://go.documentation.sas.com/doc/en/pgmsascdc/v_034/casstat/casstat_assess_details02.htm
  
  if (missing(validadedf)) {
    validadedf <- data.frame(target = numeric(), label = numeric())
    colnames(validadedf) <- c(targetPredicted, targetName)
  }
  
  if (missing(traindf)) {
    traindf <- data.frame(target = numeric(), label = numeric())
    colnames(traindf) <- c(targetPredicted, targetName)
  }
  
  if (missing(testdf)) {
    testdf <- data.frame(target = numeric(), label = numeric())
    colnames(testdf) <- c(targetPredicted, targetName)
  }
  
  data <- list(validadedf[, c(targetName, targetPredicted)], 
               traindf[, c(targetName, targetPredicted)],
               testdf[, c(targetName, targetPredicted)])
  
  if (all(sapply(data, nrow) == 0)) {
    stop('validadedf, traindf or testdf must not be an empty data.frame')
  }
  
  if (type == "binary") {
  
  ## renaming labels to 1 and 0 for calculations
  
  if (targetEventValue != 1 ) {
    data <- lapply(data, 
                   function(x) {
                     x[,targetName] <- ifelse(x[,targetName] == targetEventValue, 1, 0)
                     return(x) }
    )
  }
  
  
  if (any(sapply(data, function(x) length(unique(x[,targetName])) > 2))) {
    stop('Output must be binary, if you set targetclassname it will convert non target labels to 0')
  }
  
  
  outputJSON  <- jsonlite::fromJSON(system.file("null_dmcas_fitstat.json",
                                                package = "sasctl"))
  
  for (i in 1:length(data)) {
    outputJSON$data$dataMap[['_PartInd_']][i]     <- i - 1
    
    if (nrow(data[[i]] != 0)) {
      
      predictions <- ROCR::prediction(data[[i]][,2], data[[i]][,1], label.ordering = label.ordering)
      accuracyRoc <- ROCR::performance(predictions, 'acc')
      tpr_fpr <- ROCR::performance(predictions,"tpr","fpr")
      
      # which accuracy fits the chosen cutoff
      cutoffrow <- which.min(abs(accuracyRoc@x.values[[1]] - cutoff)) ## Accuracy based on cutoff
      acc <- accuracyRoc@y.values[[1]][cutoffrow]
      ase <- mean((data[[i]][,1][[1]] - data[[i]][,2][[1]])^2)
      
      outputJSON$data$dataMap[['_RASE_']][i]          <- sqrt(ase)
      
      outputJSON$data$dataMap[['_NObs_']][i]          <- nrow(data[[i]])
      
      outputJSON$data$dataMap[['_GINI_']][i]          <- (2 * ROCR::performance(predictions,"auc")@y.values[[1]]) - 1
      
      outputJSON$data$dataMap[['_GAMMA_']][i]         <- NA
      
      outputJSON$data$dataMap[['_MCE_']][i]           <- 1 - acc
      
      outputJSON$data$dataMap[['_ASE_']][i]           <- ase
      
      outputJSON$data$dataMap[['_MCLL_']][i]          <- (-1/nrow(data[[i]])) * sum(data[[i]][,1][[1]] * log(pmax(pmin(data[[i]][,2][[1]], 1 - 1e-15), 1e-15)))
      
      outputJSON$data$dataMap[['_KS_']][i]            <- max(tpr_fpr@y.values[[1]] - tpr_fpr@x.values[[1]])
      
      outputJSON$data$dataMap[['_KSPostCutoff_']][i]  <- NA
      
      outputJSON$data$dataMap[['_DIV_']][i]           <- nrow(data[[i]])
      
      outputJSON$data$dataMap[['_KSCut_']][i]         <- cutoff
      
      outputJSON$data$dataMap[['_C_']][i]             <- ROCR::performance(predictions,"auc")@y.values[[1]]
      
      outputJSON$data$dataMap[['_TAU_']][i]           <- stats::cor(data[[i]][,1],data[[i]][,2], method = "kendall")
      
      outputJSON$data$dataMap[['_TargetName_']][i]  <- ifelse(is.null(targetName), NA, targetName)
      
    }
  }
  
  }
  
  if (type == "interval") {
    
    outputJSON  <- jsonlite::fromJSON(system.file("null_dmcas_fitstat_interval.json",
                                                  package = "sasctl"))
    
    for (i in 1:length(data)) {
      outputJSON$data$dataMap[['_PartInd_']][i]     <- i - 1
      
      if (nrow(data[[i]] != 0)) { 
        
        nObs <- nrow(data[[i]])
        ase <- mean((data[[i]][,1] - data[[i]][,2])^2)
        msle <- mean((log(data[[i]][,1]) - log(data[[i]][,2]))^2)
        mae <- mean(abs(data[[i]][,1] - data[[i]][,2]))
        
        outputJSON$data$dataMap[['_ASE_']][i]     <- ase
        
        outputJSON$data$dataMap[['_DIV_']][i]     <- nObs
        
        outputJSON$data$dataMap[['_MAE_']][i]     <- mae
        
        outputJSON$data$dataMap[['_MSLE_']][i]    <- msle
        
        outputJSON$data$dataMap[['_NObs_']][i]    <- nObs
        
        outputJSON$data$dataMap[['_RASE_']][i]    <- sqrt(ase)
        
        outputJSON$data$dataMap[['_RMAE_']][i]    <- sqrt(mae)
        
        outputJSON$data$dataMap[['_RMSLE_']][i]   <- sqrt(msle)
        
        outputJSON$data$dataMap[['_TargetName_']][i]  <- ifelse(is.null(targetName), NA, targetName)
        }
      }
    
  }
  
  
  if (!noFile) {
    
    path <- ifelse(grepl("\\/$", path), path, paste0(path, "/"))
    out_file <- paste0(path, "dmcas_fitstat.json")
    
    
    jsonlite::write_json(outputJSON, 
                         out_file, 
                         pretty = TRUE, 
                         auto_unbox = TRUE)
    
    print(paste0('File written to ', out_file))
  }
  
  return(outputJSON)
}

#' Write dmcas_roc Json
#'
#' Calculates the ROC curve from user data and writes it to a JSON file for
#' importing into the common model repository. Binary response only.
#'
#' @param validadedf `data.frame` where the first column in the yActual (labels/value) and the second is yPrediction (target probability)
#' @param testdf `data.frame` where the first column in the yActual (labels/value) and the second is yPrediction (target probability)
#' @param traindf `data.frame` where the first column in the yActual (labels/value) and the second is yPrediction (target probability)
#' @param targetName target variable column name (actuals)
#' @param targetPredicted target variable predicted probability column name
#' @param targetEventValue target class name for ROC reference, if model is nominal, all other class will be counted as "not target"
#' @param path default to current work dir
#' @param label.ordering The default ordering (cf.details) of the classes can be changed by supplying a vector containing the negative and the positive class label. See [ROCR::prediction()]
#' @param noFile if you don't want to write to a file, only the output
#' @return 
#' - `list` that reflects the 'dmcas_roc.json'
#' - 'dmcas_roc.json' file written to `path`
#'    
#' @examples
#'
#' df <- data.frame(label = sample(c(1,0), 6000, replace = TRUE),
#'                  prob = runif(6000),
#'                  partition = rep_len(1:3, 6000)) ## partition will be ignored since it is 3rd column
#'                
#' calculateROCStat(df[df$partition == 1, ], 
#'                       df[df$partition == 2, ],
#'                       df[df$partition == 3, ],
#'                       targetName = "label",
#'                       targetPredicted = "prob",
#'                       noFile = TRUE)
#'                                     
#'                                     
#'  
#' @export

calculateROCStat <- function(validadedf, 
                             traindf, 
                             testdf,
                             targetName, 
                             targetPredicted,
                             targetEventValue = 1,
                             label.ordering = c(0, 1),
                             path = "./", 
                             noFile = FALSE) {
  
  
  if (missing(validadedf)) {
    validadedf <- data.frame(target = numeric(), label = numeric())
    colnames(validadedf) <- c(targetPredicted, targetName)
  }
  
  if (missing(traindf)) {
    traindf <- data.frame(target = numeric(), label = numeric())
    colnames(traindf) <- c(targetPredicted, targetName)
  }
  
  if (missing(testdf)) {
    testdf <- data.frame(target = numeric(), label = numeric())
    colnames(testdf) <- c(targetPredicted, targetName)
  }
  
  data <- list(validadedf[, c(targetName, targetPredicted)], 
               traindf[, c(targetName, targetPredicted)],
               testdf[, c(targetName, targetPredicted)])
  
  if (all(sapply(data, nrow) == 0)) {
    stop('validadedf, traindf or testdf must not be an empty data.frame')
  }
  
  if (targetEventValue != 1 ) {
    data <- lapply(data, 
                   function(x) {
                     x[,targetName] <- ifelse(x[,targetName] == targetEventValue, 1, 0)
                     return(x) }
                   
    )
  }
  
  
  if (any(sapply(data, function(x) length(unique(x[,1])) > 2))) {
    stop('Output must be binary, if you set targetclassname it will convert non target labels to 0')
  }
  
  
  parametersDf = data.frame(parameter    = c("_DataRole_","_PartInd_","_PartInd__f","_Column_",
                                             "_Event_","_Cutoff_","_Sensitivity_","_Specificity_","_FPR_",
                                             "_OneMinusSpecificity_","_TargetName_"),
                            type         = c("char","num","char","char","char","num","num","num","num","num","char"),
                            label        = c("Data Role","Partition Indicator","Formatted Partition","Analysis Variable",
                                             "Event","Cutoff","Sensitivity","Specificity","False Positive Rate",
                                             "1 - Specificity","Target Name"),
                            length       = c(10,8,12,32,8,8,8,8,8,8,20),
                            order        = c(1:11),
                            values       = c("_DataRole_","_PartInd_","_PartInd__f","_Column_",
                                             "_Event_","_Cutoff_","_Sensitivity_","_Specificity_","_FPR_",
                                             "_OneMinusSpecificity_","_TargetName_"),
                            preformatted = FALSE
  )
  
  row.names(parametersDf) <- c("_DataRole_","_PartInd_","_PartInd__f","_Column_",
                               "_Event_","_Cutoff_","_Sensitivity_","_Specificity_","_FPR_",
                               "_OneMinusSpecificity_","_TargetName_")
  
  factorCols <- sapply(parametersDf, is.factor)
  parametersDf[factorCols] <- lapply(parametersDf[factorCols], as.character)
  
  parameterMap <- list()
  for (i in 1:nrow(parametersDf)) {
    parameterMap[[i]] <- lapply(parametersDf, function(x) {x[i]})
  }
  
  names(parameterMap) <- row.names(parametersDf)
  
  rocDf <- data.frame(DataRole            = character(),
                      PartInd             = double(),
                      formattedPartition  = character(),
                      Column              = character(),
                      Event               = character(),
                      Cutoff              = double(),
                      Sensitivity         = double(),
                      Specificity         = double(),
                      FPR                 = double(),
                      OneMinusSpecificity = double(),
                      TargetName          = character(),
                      stringsAsFactors = FALSE) 
  
  names(rocDf) <- paste0('_',names(rocDf),'_') 
  
  for (i in 1:length(data)) {
    PartInd     <- i - 1
    if (i == 1)         {partition = "VALIDATE"}
    else if (i == 2)    {partition = "TRAIN"}
    else if (i == 3)    {partition = "TEST"}
    else                {partition = ""}
    
    if (nrow(data[[i]] != 0)) {
      
      
      # Calculate the ROC coordinates (in the ROCR library)
      predictions <- ROCR::prediction(data[[i]][,2], data[[i]][,1], label.ordering = label.ordering)
      roc_coordinate <- ROCR::performance(predictions, measure = "tpr", x.measure = "fpr")
      
      roc_fpr <- unlist(roc_coordinate@x.values)
      roc_tpr <- unlist(roc_coordinate@y.values)
      roc_cutoff <- unlist(roc_coordinate@alpha.values)
      roc_ncoord <- length(roc_cutoff)
      
      
      rocDf.partition <- data.frame(DataRole            = as.character(rep(partition, roc_ncoord)),
                                    PartInd             = rep(PartInd, roc_ncoord),
                                    formattedPartition  = rep(paste0("           ",PartInd), roc_ncoord),
                                    Column              = rep(paste0("P_",targetName,targetEventValue),roc_ncoord),
                                    Event               = rep(targetEventValue, roc_ncoord),
                                    Cutoff              = roc_cutoff,
                                    Sensitivity         = roc_tpr,
                                    Specificity         = 1 - roc_fpr,
                                    FPR                 = roc_fpr,
                                    OneMinusSpecificity = roc_fpr,
                                    TargetName          = rep(targetName, roc_ncoord)
      ) 
      
      names(rocDf.partition) <- paste0('_',names(rocDf.partition),'_') 
      
      factorCols <- sapply(rocDf.partition, is.factor)
      rocDf.partition[factorCols] <- lapply(rocDf.partition[factorCols], as.character)
      
      rocDf <- rbind(rocDf,rocDf.partition)
    }
  }
  
  dataMap <- list()
  for (i in 1:nrow(rocDf)) {
    dataMap[[i]] <- list(dataMap   = lapply(rocDf, function(x) {x[i]}),
                         rowNumber = i)
  }
  
  outputJSON <- list(name         = c("dmcas_roc"),
                     revision     = 0,
                     order        = 0,
                     parameterMap = parameterMap,
                     data         = dataMap,
                     version      = 1,
                     yInteger     = FALSE,
                     xInteger     = FALSE
  )
  
  if (!noFile) {
    
    path <- ifelse(grepl("\\/$", path), path, paste0(path, "/"))
    out_file <- paste0(path, "dmcas_roc.json")
    
    
    jsonlite::write_json(outputJSON, 
                         out_file, 
                         pretty = TRUE, 
                         auto_unbox = TRUE)
    
    print(paste0('File written to ', out_file))
  }
  
  return(outputJSON) 
}


#' Write dmcas_lift Json
#'
#' Calculates the lift curves from user data and writes to a JSON file for
#' importing into the common model repository. Binary response only.
#'
#' @param validadedf `data.frame` where the first column in the yActual (labels/value) and the second is yPrediction (target probability)
#' @param testdf `data.frame` where the first column in the yActual (labels/value) and the second is yPrediction (target probability)
#' @param traindf `data.frame` where the first column in the yActual (labels/value) and the second is yPrediction (target probability)
#' @param targetName target variable column name (actuals)
#' @param targetPredicted target variable predicted probability column name
#' @param targetEventValue target class name for ROC reference, if model is nominal, all other class will be counted as "not target"
#' @param path default to current work dir
#' @param noFile if you don't want to write to a file, only the output
#' @return 
#' - `list` that reflects the 'dmcas_roc.json'
#' - 'dmcas_roc.json' file written to `path`       
#' @examples
#'
#' df <- data.frame(label = sample(c(1,0), 6000, replace = TRUE),
#'                  prob = runif(6000),
#'                  partition = rep_len(1:3, 6000)) ## partition will be ignored since it is 3rd column
#'                
#' calculateLiftStat(df[df$partition == 1, ], 
#'                    df[df$partition == 2, ],
#'                    df[df$partition == 3, ],
#'                    targetName = "label",
#'                    targetPredicted = "prob",
#'                    noFile = TRUE)
#'                                     
#'                                     
#'  
#' @export

calculateLiftStat <- function(validadedf, 
                              traindf, 
                              testdf,
                              targetName, 
                              targetPredicted,
                              targetEventValue = 1,
                              path = "./", 
                              noFile = FALSE) {
  
  if (missing(validadedf)) {
    validadedf <- data.frame(target = numeric(), label = numeric())
    colnames(validadedf) <- c(targetPredicted, targetName)
  }
  
  if (missing(traindf)) {
    traindf <- data.frame(target = numeric(), label = numeric())
    colnames(traindf) <- c(targetPredicted, targetName)
  }
  
  if (missing(testdf)) {
    testdf <- data.frame(target = numeric(), label = numeric())
    colnames(testdf) <- c(targetPredicted, targetName)
  }
  
  data <- list(validadedf[, c(targetName, targetPredicted)], 
               traindf[, c(targetName, targetPredicted)],
               testdf[, c(targetName, targetPredicted)])
  
  if (all(sapply(data, nrow) == 0)) {
    stop('validadedf, traindf or testdf must not be an empty data.frame')
  }
  
  
  if (targetEventValue != 1 ) {
    data <- lapply(data, 
                   function(x) {
                     x[,targetName] <- ifelse(x[,targetName] == targetEventValue, 1, 0)
                     return(x) }
                   
    )
  }
  
  if (any(unlist(lapply(data, function(x) any(class(x) %in% c("tbl_df", "tbl")))))) {
    data <- lapply(data, as.data.frame)
  }
  
  if (any(sapply(data, function(x) length(unique(x[,targetName])) > 2))) {
    stop('Output must be binary, if you set targetclassname it will convert non target labels to 0')
  }
  
  parametersDf = data.frame(parameter    = c("_DataRole_","_PartInd_","_PartInd__f","_Column_",
                                             "_Event_","_Depth_","_NObs_","_Gain_", "_Lift_","_CumLift_",
                                             "_Resp_","_CumResp_", "_PctResp_", "_CumPctResp_","_TargetName_"),
                            type         = c("char","num","char","char","char",
                                             "num","num","num","num","num","num",
                                             "num","num","num","char"),
                            label        = c("Data Role","Partition Indicator","Formatted Partition","Analysis Variable",
                                             "Event","Depth","Sum of Frequencies","Gain","Lift","Cumulative Lift",
                                             "Captured Response Percentage", "Cumulative Captured Response Percentage", 
                                             "% Response", "Cumulative % Response","Target Name"),
                            length       = c(10,8,12,32,8,8,8,8,8,8,8,8,8,8,20),
                            order        = c(1:15),
                            values       = c("_DataRole_","_PartInd_","_PartInd__f","_Column_",
                                             "_Event_","_Depth_","_NObs_","_Gain_", "_Lift_","_CumLift_",
                                             "_Resp_","_CumResp_", "_PctResp_", "_CumPctResp_","_TargetName_"),
                            preformatted = FALSE
  )
  
  row.names(parametersDf) <- c("_DataRole_","_PartInd_","_PartInd__f","_Column_",
                               "_Event_","_Depth_","_NObs_","_Gain_", "_Lift_","_CumLift_",
                               "_Resp_","_CumResp_", "_PctResp_", "_CumPctResp_","_TargetName_")
  
  factorCols <- sapply(parametersDf, is.factor)
  parametersDf[factorCols] <- lapply(parametersDf[factorCols], as.character)
  
  parameterMap <- list()
  for (i in 1:nrow(parametersDf)) {
    parameterMap[[i]] <- lapply(parametersDf, function(x) {x[i]})
  }
  
  names(parameterMap) <- row.names(parametersDf)
  
  liftDf <- data.frame(DataRole            = character(),
                       PartInd             = double(),
                       formattedPartition  = character(),
                       Column              = character(),
                       Event               = character(),
                       Depth               = double(),
                       NObs                = double(),
                       Gain                = double(),
                       Lift                = double(),
                       CumLift             = double(),
                       Resp                = double(),
                       CumResp             = double(),
                       PctResp             = double(),
                       CumPctResp          = double(),
                       TargetName          = character(),
                       stringsAsFactors = FALSE) 
  
  names(liftDf) <- paste0('_',names(liftDf),'_') 
  
  for (i in 1:length(data)) {
    PartInd     <- i - 1
    
    if (i == 1) {partition = "VALIDATE"} else 
    if (i == 2) {partition = "TRAIN"} else 
    if (i == 3) {partition = "TEST"} else
                {partition = ""}
    
    if (nrow(data[[i]] != 0)) {
      
      lift_coordinates <- compute_lift_coordinates(DepVar = data[[i]][,1],
                                                   EventValue = 1, 
                                                   EventPredProb = data[[i]][,2],
                                                   resolution = 1/20)
      ncoords <- nrow(lift_coordinates)
      
      
      liftDf.partition <- data.frame(DataRole           = as.character(rep(partition,ncoords)),
                                     PartInd             = rep(PartInd,ncoords),
                                     formattedPartition  = rep(paste0("           ",PartInd),ncoords),
                                     Column              = rep(paste0("P_",targetName,targetEventValue),ncoords),
                                     Event               = rep(targetEventValue,ncoords),
                                     Depth               = lift_coordinates[,"accquantilePct"],
                                     NObs                = lift_coordinates[,"quantileN"],
                                     Gain                = lift_coordinates[,"gainN"],
                                     Lift                = lift_coordinates[,"lift"],
                                     CumLift             = lift_coordinates[,"accLift"],
                                     Resp                = lift_coordinates[,"gainPct"],
                                     CumResp             = lift_coordinates[,"accGainPct"],
                                     PctResp             = lift_coordinates[,"responsePct"],
                                     CumPctResp          = lift_coordinates[,"accResponsePct"],
                                     TargetName          = rep(targetName,ncoords)) 
      
      names(liftDf.partition) <- paste0('_',names(liftDf.partition),'_') 
      
      factorCols <- sapply(liftDf.partition, is.factor)
      liftDf.partition[factorCols] <- lapply(liftDf.partition[factorCols], as.character)
      
      liftDf <- rbind(liftDf,liftDf.partition)
    }
  }
  
  dataMap <- list()
  for (i in 1:nrow(liftDf)) {
    dataMap[[i]] <- list(dataMap   = lapply(liftDf, function(x) {x[i]}),
                         rowNumber = i)
    
  }
  
  outputJSON <- list(name         = c("dmcas_lift"),
                     revision     = 0,
                     order        = 0,
                     parameterMap = parameterMap,
                     data         = dataMap,
                     version      = 1,
                     yInteger     = FALSE,
                     xInteger     = FALSE
  )
  
  
  if (!noFile) {
    
    path <- ifelse(grepl("\\/$", path), path, paste0(path, "/"))
    out_file <- paste0(path, "dmcas_lift.json")
    
    
    jsonlite::write_json(outputJSON, 
                         out_file, 
                         pretty = T, 
                         auto_unbox = T)
    
    print(paste0('File written to ', out_file))
  }
  
  return(outputJSON)
}

#' calculate Lift coordinates
#' 
#' @param depvar The column that holds the dependent variable's values
#' @param EventValue Value of the dependent variable that indicates an event
#' @param EventPredProb The column that holds the predicted probability values
#' @param resolution lift chart resolution
#' @return `matrix` with computed lift values
#' @noRd
#' 

compute_lift_coordinates <- function(DepVar,
                                     EventValue,
                                     EventPredProb,
                                     resolution = 1/10) {
  
  # Find out the number of observations
  nObs <- length(EventPredProb)
  
  # Get the deciles
  quantileCutOff <- stats::quantile(EventPredProb, probs = seq(0.0, 1.0, by = resolution))
  nQuantile <- length(quantileCutOff)
  
  quantileIndex <- vector(mode = "numeric", length = nObs)
  for (i in c(1:nObs))
  {
    iQ <- nQuantile
    EPP <- EventPredProb[i]
    for (j in c(1:(nQuantile - 1)))
    {
      if (EPP > quantileCutOff[nQuantile - j + 1]) iQ <- iQ - 1
    }
    quantileIndex[i] <- iQ
  }
  
  # Construct the Lift chart table
  countTable = table(quantileIndex, DepVar)
  #print(countTable)
  
  quantileN = rowSums(countTable)
  quantilePct = 100.0 * (quantileN / nObs)
  gainN = countTable[,toString(EventValue)]
  totalNResponse = sum(gainN)
  gainPct = 100.0 * (gainN / totalNResponse)
  responsePct = 100.0 * (gainN / quantileN)
  overallResponsePct = 100.0 * (totalNResponse / nObs)
  lift = responsePct / overallResponsePct
  
  LiftCoordinates = cbind(quantileN, quantilePct, gainN, gainPct, responsePct, lift)
  
  # Construct the Accumulative Lift chart table
  accCountTable = apply(countTable, 2, cumsum)
  accquantileN = rowSums(accCountTable)
  accquantilePct = 100.0 * (accquantileN / nObs)
  accGainN = accCountTable[,toString(EventValue)]
  accGainPct = 100.0 * (accGainN / totalNResponse)
  accResponsePct = 100.0 * (accGainN / accquantileN)
  accLift = accResponsePct / overallResponsePct
  
  LiftCoordinates = cbind(quantileN, quantilePct, gainN, gainPct, responsePct, lift,
                          accquantileN, accquantilePct, accGainN, accGainPct, accResponsePct, accLift)
  
  return(LiftCoordinates)
  
}


#' Write all diagnostic Json files
#'
#' Calculates and writes fit statistics, roc and lift for binary and fit statistics for
#' interval.
#'
#' @param validadedf `data.frame` where the first column in the yActual (labels/value) and the second is yPrediction (target probability)
#' @param testdf `data.frame` where the first column in the yActual (labels/value) and the second is yPrediction (target probability)
#' @param traindf `data.frame` where the first column in the yActual (labels/value) and the second is yPrediction (target probability)
#' @param type `"binary"` or `"interval"`
#' @param targetName target variable name (actuals)
#' @param targetPredicted target variable probability column name
#' @param targetEventValue if `type = "binary"` target class name for fit stat reference, if model is nominal, all other class will be counted as "not target"
#' @param cutoff cutoff to be used for calculation of miss classification for binary
#' @param label.ordering The default ordering (cf.details) of the classes can be changed by supplying a vector containing the negative and the positive class label. See [ROCR::prediction()]
#' @param path default to current work dir
#' @param noFile if you don't want to write to a file, only the output
#' @return 
#' - `list` of lists that reflects the 'dmcas_fitstat.json', 'dmcas_roc.json' and 'dmcas_lift.json'
#' - 'dmcas_fitstat.json', 'dmcas_roc.json' and 'dmcas_lift.json' files written to `path`       
#' 
#' @seealso 
#' 
#' All parameters are passed to [sasctl::calculateLiftStat()], [sasctl::calculateLiftStat()] and [sasctl::calculateLiftStat()] for matching parameters.
#' 
#' @examples
#'
#' df <- data.frame(label = sample(c(1,0), 6000, replace = TRUE),
#'                 prob = runif(6000),
#'                partition = rep_len(1:3, 6000)) ## partition will be ignored since it is 3rd column
#'                
#' diagnosticsJson(df[df$partition == 1, ], 
#'                  df[df$partition == 2, ],
#'                  df[df$partition == 3, ],
#'                  targetName = "label",
#'                  targetPredicted = "prob",
#'                  noFile = TRUE
#'                  )
#'                                     
#'                                     
#'  
#' @export


diagnosticsJson <- function(validadedf, 
                            traindf, 
                            testdf,
                            targetName,
                            targetPredicted,
                            type = "binary",
                            targetEventValue = 1,
                            cutoff = 0.5, 
                            label.ordering = c(0, 1),
                            path = "./", 
                            noFile = FALSE
) {
  
  all_outputs <- list()
  
  if (type == "binary") {
    
    all_outputs$lift <- calculateLiftStat(validadedf = validadedf, 
                                   traindf = traindf, 
                                   testdf = testdf,
                                   targetName = targetName, 
                                   targetPredicted = targetPredicted,
                                   targetEventValue = targetEventValue,
                                   path = path, 
                                   noFile = noFile)
    
    
    all_outputs$roc <- calculateROCStat(validadedf = validadedf, 
                                 traindf = traindf, 
                                 testdf = testdf,
                                 targetName = targetName, 
                                 targetPredicted = targetPredicted,
                                 targetEventValue = targetEventValue,
                                 label.ordering = label.ordering,
                                 path = path, 
                                 noFile = noFile) 
  }
  
  all_outputs$fitStat <- calculateFitStat(validadedf = validadedf, 
                                   traindf = traindf, 
                                   testdf = testdf,
                                   type = type,
                                   targetName = targetName, 
                                   targetPredicted = targetPredicted,
                                   targetEventValue = targetEventValue,
                                   path = path, 
                                   label.ordering = label.ordering,
                                   cutoff = cutoff, 
                                   noFile = noFile)
  
  
  return(all_outputs)
  
}
