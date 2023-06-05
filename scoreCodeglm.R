library("tidymodels")
library("stats")

scoreFunction <- function(LOAN, MORTDUE, VALUE, REASON, JOB, YOJ, DEROG, DELINQ, CLAGE, NINQ, CLNO, DEBTINC)
{
  #output: EM_CLASSIFICATION, EM_EVENTPROBABILITY, EM_PROBABILITY, I_BAD, P_BAD0, P_BAD1
  
  if (!exists("sasctlRmodel"))
  {
    assign("sasctlRmodel", readRDS(file = paste(rdsPath, "model.rds", sep = "")), envir = .GlobalEnv)
    target_labels <- c('0', '1')
  }
  
  data <- data.frame(LOAN  =  LOAN,
                     MORTDUE  =  MORTDUE,
                     VALUE  =  VALUE,
                     REASON  =  REASON,
                     JOB  =  JOB,
                     YOJ  =  YOJ,
                     DEROG  =  DEROG,
                     DELINQ  =  DELINQ,
                     CLAGE  =  CLAGE,
                     NINQ  =  NINQ,
                     CLNO  =  CLNO,
                     DEBTINC  =  DEBTINC)
  
  predictions <- predict(sasctlRmodel, new_data = data, type = "prob")

  boolClass <- (predictions == do.call(pmax, predictions))
predictions[".pred"] <- apply(boolClass, 1 , function(x) target_labels[x])
  
output_list <- list(EM_CLASSIFICATION = predictions[[".pred"]], 
                    EM_EVENTPROBABILITY = predictions[[".pred_0"]],
                    EM_PROBABILITY = subset(predictions, select = -c(.pred))[boolClass],
                    I_BAD = predictions[[".pred"]],
                    P_BAD0 = predictions[[".pred_0"]],
                    P_BAD1 = predictions[[".pred_1"]]
                    )

  return(output_list)
}
