# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

#' Get list of available models
#' 
#' Return a `data.frame` of metadata of available models/decisions
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @param filters list of of names vectors for filter parameters (createdBy, modifiedBy, name). By default it will use the `contains` operation. For more info visit `https://developer.sas.com/apis/rest/Topics/#filters`.
#' @param limit maximum number of modules to return
#' @param exact boolean, If the filter query should use "contains" for partial match or "eq" for exact match
#' @param start the index of the first module to return
#' @param verbose logical, return print API call information
#' @return A `data.frame` with the list of models
#' @examples 
#' 
#' \dontrun{
#' models <- list_modules(sess, filters = list(createdBy = 'myUser'))
#' models
#' }
#' 
#' @export


list_modules <- function(session, filters = list(), 
                         start = 0, limit = 20 , verbose = FALSE, 
                         exact = FALSE, ...) {
  
  ## creating filter query eg: "or(or(contains(createdBy, '123')),or(contains(name, '123')))"
  query <- build_filter_query(filters, exact = exact)
  
  ## adding query for limits and start index
  query <- append(query, list(start = start, limit = limit))  
  
  ### calling MAS modules
  modules <- vGET(session, "/microanalyticScore/modules", query = query, ..., verbose = verbose)
  
  if (length(modules$items) == 0) {
    return(list())
  }
  
  modules <- create_sasctl_obj(modules$items, "MASlist", append = TRUE)
  
  return(modules)
  
}

#' Get a MAS module/model and steps
#' 
#' Returns a single module `MASmodule` object with module and steps information
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param module `MASmodule` object, module name. If name, will try to find a single model with exact name match. See `exact` parameter
#' @param id module id, will replace `module` parameter if given
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @return `list` with sasctl attribute
#' @examples
#' 
#' \dontrun{
#' my_module <- get_masmodule(sess, model = "name")
#' my_module
#' }
#' 
#' @export

get_masmodule <- function(session, module, id = NULL, exact = TRUE, ...){
  
  if (is.null(id)) {
    module_id <- single_id_from_object(session, module, class = "MASmodule", type = "mas", exact = exact)
  } else {
    module_id <- id # doesn't use UUID, but arbitrary given id
  }
  
  module <- vGET(session,
                 path = paste0("/microanalyticScore/modules/", module_id),
                 ...
  )
  
  module$steps <- vGET(session,
                       path = paste0("/microanalyticScore/modules/", module_id, "/steps"),
                       ...
  )
  
  module$ScoreType <- ifelse(any(basename(do.call(rbind, module$steps$items$links)[,3]) == "execute"), 
                      'execute', ## decision or pmml converted
                      'score') ## model
  
  module$score <- vGET(session,
                       path = paste0("/microanalyticScore/modules/", module_id, "/steps/", module$ScoreType),
                       ...
  )
  

  module <- create_sasctl_obj(module, "MASmodule")
  
  return(module) 
  
}

#' Delete a module/model and steps
#' 
#' Delete a module/model published on MAS.
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param module `MASmodule` object, module/model name. If name, will try to find a single module with exact name match. See `exact` parameter
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @return A [`httr::response`] object.
#' 
#' @examples
#' 
#' \dontrun{
#' deleted_module <- delete_masmodule(sess, module = "ModuleName")
#' deleted_module
#' }
#' 
#' @export

delete_masmodule <- function(session, module, exact = TRUE){
  
  module_id <- single_id_from_object(session, module, class = "MASmodule", type = "mas", exact = exact)
  
  del_module <- vDELETE(session,
                      paste0("microanalyticScore/modules/", module_id))
  
  invisible(del_module)
}


#' Predict MASmodule
#' 
#' score a data.frame MASmodule/model
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param data `data.frame` data.frame of the data
#' @param module `MASmodule` object, module name. If name, will try to find a single module with exact name match. See `exact` parameter
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @param ScoreType `execute` for decision, `score` for model
#' @param forceTrail boolean, if the mas model is a decision (`execute`), it will force add a trailing underscore (_) in variable names.
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @return `data.frame` with scored rows
#' 
#' @details
#' When the `furrr` package is installed, `masScore` uses `furrr::future_map_dfr()`
#' for scoring, which allows parallel execution via a `future` plan. Without `furrr`,
#' scoring runs sequentially using base R. Install both `furrr` and `future` to
#' enable parallel scoring.
#' 
#' @examples
#' 
#' \dontrun{
#' # single row (sequential)
#' scored <- masScore(sess, "module_name", data[1,])
#' scored
#' 
#' # Parallel scoring — requires the furrr and future packages
#' # Not recommended for single rows due to parallelisation overhead
#' future::plan(future::multisession, workers = future::availableCores() - 1)
#' scored <- masScore(sess, "module_name", data)
#' 
#' # Return to sequential execution
#' future::plan(future::sequential)
#' }
#' 
#' @export

masScore <- function(session, module, data, exact = TRUE, ScoreType = 'score', forceTrail = TRUE, ...) {
  
  module_id <- single_id_from_object(session, module, class = "MASmodule", type = "mas", exact = exact)
  
  if (is_sasctl_class(module, 'MASmodule')) {
    ScoreType <- module$ScoreType
  } 
  
  if (!is.data.frame(data)) {
    stop("Data must be a data.frame")
  }
  
  if (ScoreType == "execute" & forceTrail) {
    colnames(data) <- paste0(colnames(data), "_")
  }
  
  json_data <- format_data_json(data) 
  
  path <- paste0("/microanalyticScore/modules/", module_id, "/steps/", ScoreType)
  
  if (requireNamespace("furrr", quietly = TRUE)) {
    scored_data <- furrr::future_map_dfr(json_data,
                          function(x) {
      sasctl::vPOST(session,
                    path = path,
                    payload = x,
                    httr::content_type("application/json"),
                    ...
                    )$outputs},
       .id = "row"
      )
  } else {
    row_ids <- if (!is.null(names(json_data))) names(json_data) else as.character(seq_along(json_data))
    result_list <- lapply(seq_along(json_data), function(i) {
      row_data <- sasctl::vPOST(session,
                                path = path,
                                payload = json_data[[i]],
                                httr::content_type("application/json"),
                                ...
                                )$outputs
      row_data$row <- row_ids[[i]]
      row_data
    })
    scored_data <- do.call(rbind, result_list)
  }

  
  scored_data <- reshape2::dcast(scored_data, row ~ name, value.var = "value")
  
  return(scored_data)
}









