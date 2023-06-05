library("tidymodels")
library("stats")

scoreFunction <- function(BAD, LOAN, MORTDUE, VALUE, REASON, JOB, YOJ, DEROG, DELINQ, CLAGE, NINQ, CLNO)
{
  #output: EM_PREDICTION, P_DEBTINC
  
  if (!exists("sasctlRmodel"))
  {
    assign("sasctlRmodel", readRDS(file = paste(rdsPath, "model.rds", sep = "")), envir = .GlobalEnv)
    
  }
  
  data <- data.frame(BAD  =  BAD,
                     LOAN  =  LOAN,
                     MORTDUE  =  MORTDUE,
                     VALUE  =  VALUE,
                     REASON  =  REASON,
                     JOB  =  JOB,
                     YOJ  =  YOJ,
                     DEROG  =  DEROG,
                     DELINQ  =  DELINQ,
                     CLAGE  =  CLAGE,
                     NINQ  =  NINQ,
                     CLNO  =  CLNO)
  
  predictions <- predict(sasctlRmodel, new_data = data, type = "numeric")

  output_list <- list(EM_PREDICTION = predictions[['.pred']], P_DEBTINC = predictions[['.pred']])

  return(output_list)
}
