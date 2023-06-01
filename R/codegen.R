
#' SAS standard score code generation \strong{(EXPERIMENTAL)}
#' 
#' @description
#' 
#' \strong{EXPERIMENTAL STATE - MAY NO WORK AS INTENDED}
#' 
#' Score code will only be generated successfully for supported models. 
#' Other models and frameworks will be added in due time. 
#' Use [sasctl::create_scoreSample()] to get a structure sample
#' 
#' @param model model object (lm, glm ...)
#' @param path file name and path to write
#' @param rds .rds file name to be called
#' @param ... to be passes to individual code generators
#' 
#' @return a code string
#' @examples
#' 
#' # SAS viya doesn't play nice with variables with '.' in the names
#' iris <- gsub("\\.", "_", colnames(iris))
#' 
#' # simple regression
#' model <- lm(Petal.Length ~ ., data = iris)
#' 
#' codegen(iris)
#'  
#' 
#' @export

codegen <- function(model, path, rds, ...) {
  UseMethod("codegen")
}

#' Code generator for Linear models
#' @export 

codegen.lm <- function(model, path = "scoreCode.R", rds = "model.rds") {
  
  inputs <- attr(model[['terms']], "term.labels")
  target <- model[['terms']][[2]]
  
  scorecode <- glue::glue({
    
    '
    scoreFunction <- function(<<paste(inputs, collapse = ", ")>>)
    {
      #output: paste0(EM_PREDICTION, P_<<target>>)
      
      if (!exists("sasctlRmodel"))
      {
        assign("sasctlRmodel", readRDS(file = paste(rdsPath, "<<rds>>", sep = "")), envir = .GlobalEnv)
      }
      
      data <- data.frame(<<paste(inputs," = ", inputs, collapse = ",\n                         ")>>)
  
      pred <- predict(sasctlRmodel, newdata = data, type = "response")
    
      output_list <- list("EM_PREDICTION" = pred, P_<<target>> = pred)
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

#' Code generator for General Linear models, specifically logistic regression
#' @export 

codegen.glm <- function(model, path = "scoreCode.R", rds = "model.rds", cutoff = 0.5) {
  
  inputs <- attr(model[['terms']], "term.labels")
  target <- model[['terms']][[2]]
  
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
    scoreFunction <- function(<<paste(inputs, collapse = ", ")>>)
    {
      #output: paste0(EM_CLASSIFICATION, EM_EVENTPROBABILITY, EM_PROBABILITY, I_<<target>>, P_<<target>>1, P_<<target>>0)
      
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



