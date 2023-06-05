library("tidymodels")
library("glmnet")

scoreFunction <- function(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
{
  #output: EM_CLASSIFICATION, EM_EVENTPROBABILITY, EM_PROBABILITY, I_Species, P_Speciessetosa, P_Speciesversicolor, P_Speciesvirginica
  
  if (!exists("sasctlRmodel"))
  {
    assign("sasctlRmodel", readRDS(file = paste(rdsPath, "model.rds", sep = "")), envir = .GlobalEnv)
    target_labels <- c('setosa', 'versicolor', 'virginica')
  }
  
  data <- data.frame(Sepal.Length  =  Sepal.Length,
                     Sepal.Width  =  Sepal.Width,
                     Petal.Length  =  Petal.Length,
                     Petal.Width  =  Petal.Width)
  
  predictions <- predict(sasctlRmodel, new_data = data, type = "prob")

  boolClass <- (predictions == do.call(pmax, predictions))
predictions[".pred"] <- apply(boolClass, 1 , function(x) target_labels[x])
  
output_list <- list(EM_CLASSIFICATION = predictions[[".pred"]], 
                    EM_EVENTPROBABILITY = predictions[[".pred_setosa"]],
                    EM_PROBABILITY = subset(predictions, select = -c(.pred))[boolClass],
                    I_Species = predictions[[".pred"]],
                    P_Speciessetosa = predictions[[".pred_setosa"]],
                    P_Speciesversicolor = predictions[[".pred_versicolor"]],
                    P_Speciesvirginica = predictions[[".pred_virginica"]]
                    )

  return(output_list)
}
