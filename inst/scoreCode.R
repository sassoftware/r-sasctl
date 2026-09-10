mymodel <- readRDS(file = file.path(rdsPath, 'hmeq_classtree_r.rds'))

scoreFunctionName <- function(var1, var2, var3)
{
  #output: outvar1, outvar2

  # Include scoring logic here to get a list of the output variables.
  # The model object is inherited, don't forget to use the 'mymodel' object here
  
  # Older viya versions uses List constructors
  output <- list('outvar1' = outvar1, 'outvar2' = outvar2)
  
  # Newer viya 4 versions recommends using data.frame construtore for speed gains
  # output <- data.frame('outvar1' = outvar1, 'outvar2' = outvar2)

  return(output)
}
