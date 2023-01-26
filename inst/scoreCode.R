scoreFunctionName <- function(var1, var2, var3)
{
  #output: outvar1, outvar2
  
  if (!exists("myClassTree"))
  {
    assign("myClassTree", readRDS(file = paste(rdsPath, 'hmeq_classtree_r.rds', sep = '')), envir = .GlobalEnv)
  }
  
  # Include scoring logic here to get a list of the output variables.
  
  output_list <- list('outvar1' = outvar1, 'outvar2' = outvar2)
  return(output_list)
}
