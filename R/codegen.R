
#' SAS standard score code generation Generic Function \strong{(EXPERIMENTAL)}
#' 
#' @description
#' 
#' \strong{EXPERIMENTAL STATE - MAY NOT WORK AS INTENDED}
#' 
#' Score code will only be generated successfully for supported models. 
#' Other models and frameworks will be added in due time. 
#' Use [sasctl::create_scoreSample()] to get a structure sample
#' 
#' Disclaimer: The score code that is generated is designed to be a working template for an R model, 
#' but is not guaranteed to work out of the box for scoring, publishing, or validating the model.
#' 
#' @param model model object (lm, glm ...)
#' @param path file name and path to write
#' @param libs vector of libraries to be added to the code. Some may be guessed from the type.
#' @param rds .rds file name to be called
#' @param inputs define inputs as the passed vector instead of guessed
#' @param ... to be passes to individual code generators
#' 
#' @return a code string
#' @examples
#' 
#' \dontrun{
#' # SAS viya doesn't play nice with variables with '.' in the names
#' colnames(iris) <- gsub("\\.", "_", colnames(iris))
#' 
#' # simple regression
#' model <- lm(Petal.Length ~ ., data = iris)
#' 
#' codegen(model)
#' } 
#' 
#' @export

codegen <- function(model, path, rds, libs, inputs, ...) {
  UseMethod("codegen")
}

#' @describeIn codegen Code generator for `lm` class models
#' @export 

codegen.lm <- function(model, path = "scoreCode.R", rds = "model.rds", libs = c()) {
  
  inputs <- attr(terms(model), "term.labels")
  target <- terms(model)[[2]]
  
  libs <- unique(c("stats", libs))
  
  libsCode <- paste0('library("',libs, '")', collapse = "\n")
  
  scorecode <- glue::glue({
    
    '
    <<libsCode>>
    
    scoreFunction <- function(<<paste(inputs, collapse = ", ")>>)
    {
      #output: EM_PREDICTION, P_<<target>>
      
      if (!exists("sasctlRmodel"))
      {
        assign("sasctlRmodel", readRDS(file = paste(rdsPath, "<<rds>>", sep = "")), envir = .GlobalEnv)
      }
      
      data <- data.frame(<<paste(inputs," = ", inputs, collapse = ",\n                         ")>>)
  
      pred <- predict(sasctlRmodel, newdata = data, type = "response")
    
      output_list <- list(EM_PREDICTION = pred, P_<<target>> = pred)
    
      return(output_list)
    }
    '
  },
  .open = "<<",
  .close = ">>")
  
  message(paste("File written to:", path))
  writeLines(scorecode, path)
  
  invisible(scorecode)
}

#' @describeIn codegen generator for `glm` class models, specifically logistic regression
#' @export 

codegen.glm <- function(model, path = "scoreCode.R", rds = "model.rds", cutoff = 0.5, libs = c()) {
  
  inputs <- attr(terms(model), "term.labels")
  target <- terms(model)[[2]]
  
  libs <- unique(c("stats", libs))
  
  libsCode <- paste0('library("',libs, '")', collapse = "\n")
  
  ## SAS does not expect the following outputs necessarily
  ## but if you follow that structure it will play nice with other SAS Features
  #
  # EM_CLASSIFICATION - Predicted for target
  # EM_EVENTPROBABILITY - Probability target=1 
  # EM_PROBABILITY - Probability of Classification
  # I_<<target>> - Into: BAD
  # I_<<target>><<level>> eg: I_BAD1 - predicted level
  
  scorecode <- glue::glue({
    
    '
    <<libsCode>>
    
    scoreFunction <- function(<<paste(inputs, collapse = ", ")>>)
    {
      #output: EM_CLASSIFICATION, EM_EVENTPROBABILITY, EM_PROBABILITY, <<target>>, I_<<target>>, P_<<target>>1, P_<<target>>0
      
      if (!exists("sasctlRmodel"))
      {
        assign("sasctlRmodel", readRDS(file = paste(rdsPath, "<<rds>>", sep = "")), envir = .GlobalEnv)
      }
      
      data <- data.frame(<<paste(inputs," = ", inputs, collapse = ",\n                         ")>>)
  
      predictions <- predict(sasctlRmodel, newdata = data, type = "response")
    
      P_<<target>>1 <- predict(sasctlRmodel, newdata = data, type = "response")
      P_<<target>>0 <- 1 - P_<<target>>1
      <<target>> <- ifelse(P_<<target>>1 >= <<cutoff>>, 1, 0)
    
      output_list <- list(EM_CLASSIFICATION = <<target>>, 
                          EM_EVENTPROBABILITY = P_<<target>>1,
                          EM_PROBABILITY = ifelse(P_<<target>>1 >= <<cutoff>>, P_<<target>>1, P_<<target>>0),
                          <<target>> = <<target>>,
                          I_<<target>> = <<target>>,
                          P_<<target>>1 = P_<<target>>1,
                          P_<<target>>0 = P_<<target>>0)
    
      return(output_list)
    }
    '
  },
  .open = "<<",
  .close = ">>")
  
  
  message(paste("File written to:", path))
  writeLines(scorecode, path)
  
  invisible(scorecode)
}


#' @describeIn codegen generator for [tidymodels] `workflow` class models
#' @export 

codegen.workflow <- function(tm_workflow, path = "scoreCode.R", rds = "model.rds", inputs = NULL,
                             libs = c(), referenceLevel = NULL) {
    
  if (!is.null(inputs)) {
    
    if (!is.vector(names(hmeqTrain))) stop("inputs must be a vector names")
    
    predictors <- inputs
    
  } else {
    
    predictorsTable <- tm_workflow[['pre']][['actions']][['recipe']][['recipe']][['var_info']] 
    predictors <- predictorsTable[predictorsTable$role == "predictor",][["variable"]]
  
  }
  
  target <- colnames(tm_workflow[["pre"]][["mold"]][["outcomes"]])
  mode <- tm_workflow[["fit"]][["fit"]][["spec"]][["mode"]]
  mlibs <- tm_workflow[["fit"]][["fit"]][["spec"]][["method"]][["libs"]]
  
  libs <- unique(c("tidymodels", mlibs, libs))
  
  libsCode <- paste0('library("',libs, '")', collapse = "\n")

  if (mode == "classification") {
  
    ## TODO alternate code for reponse_type = "response"
    target_labels <- levels(tm_workflow[["pre"]][["mold"]][["outcomes"]][[1]])
    response_type <- "prob"
    p_labels <- glue::glue_collapse(glue::glue('P_<<target>><<target_labels>> = predictions[[".pred_<<target_labels>>"]]', .open = "<<", .close = ">>"), sep = ",\n                    ")
    target_labels_string <- paste0('target_labels <- c(',paste0("'",target_labels, "'", collapse = ", "), ')')
    
    if (is.null(referenceLevel)) {

      referenceLevel <- target_labels[1]
      
    } else {
      
      if (!(referenceLevel %in% target_labels)) {
        stop(glue::glue("referenceLevel must be: ", 
                        glue::glue_collapse(glue::glue("'{target_labels}'"), 
                                            sep = ", ",last = " or "))
        )
      }
    }
        
    p_labels <- glue::glue_collapse(glue::glue('P_<<target>><<target_labels>> = predictions[[".pred_<<target_labels>>"]]', .open = "<<", .close = ">>"), sep = ",\n                    ")
    outputSpec <- glue::glue("EM_CLASSIFICATION, EM_EVENTPROBABILITY, EM_PROBABILITY, I_<<target>>, <<target>>, <<paste0('P_',target, target_labels, collapse = ', ')>>",
                            .open = "<<",
                            .close = ">>")
        
    pred_format <- glue::glue('boolClass <- (predictions == do.call(pmax, predictions))
      predictions[".pred"] <- apply(boolClass, 1 , function(x) target_labels[x])
  
      output_list <- list(EM_CLASSIFICATION = predictions[[".pred"]], 
                          EM_EVENTPROBABILITY = predictions[[".pred_<<referenceLevel>>"]],
                          EM_PROBABILITY = apply(subset(predictions, select = -c(.pred)), 1, max),
                          I_<<target>> = predictions[[".pred"]],
                          <<target>> = predictions[[".pred"]],
                          <<p_labels>>
                          )', 
      .open = "<<",
      .close = ">>")
    
  } else if (mode == "regression") {
    
    outputSpec <- glue::glue("EM_PREDICTION, P_<<target>>", .open = "<<", .close = ">>")
    response_type <- "numeric"
    target_labels_string <- ""
    
    pred_format <- glue::glue("output_list <- list(EM_PREDICTION = predictions[['.pred']], P_<<target>> = predictions[['.pred']])", 
                              .open = "<<",
                              .close = ">>")
  } else {
    
    stop(glue::glue("Workflow scoring code for workflow model '{mode}' mode is not implemented"))
    
  }
  
  
  scorecode <- glue::glue({
    
    '
    <<libsCode>>
    
    scoreFunction <- function(<<paste(predictors, collapse = ", ")>>)
    {
      #output: <<outputSpec>>
      
      if (!exists("sasctlRmodel"))
      {
        assign("sasctlRmodel", readRDS(file = paste(rdsPath, "<<rds>>", sep = "")), envir = .GlobalEnv)
        
      }
      <<target_labels_string>>
    
      data <- data.frame(<<paste(predictors," = ", predictors, collapse = ",\n                         ")>>)
  
      predictions <- predict(sasctlRmodel, new_data = data, type = "<<response_type>>")
    
      <<pred_format>>
    
      return(output_list)
    }
    '
  },
  .open = "<<",
  .close = ">>")
  
  
  message(paste("File written to:", path))
  writeLines(scorecode, path)
  
  invisible(scorecode)
}

