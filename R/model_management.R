# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

#' Publish Model
#' 
#' Publish a model from Model Manager to a given destination
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param model `MMmodel` object, project ID or model name. If name, will try to find a single model with exact name match. See `exact` parameter
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @param destination the publish destination 
#' @param name publish endpoint, if missing, the model name will be used
#' @param force force replace of a published model with the same name
#' @param publishInfo boolean, returns the `publishInfo` object instead of `MASmodule` object
#' @param ... additional parameters to be passed to `httr::POST` such as `httr::add_headers`
#' @return A `MASmodule` object from `get_masmodule()`. If `return_publish_info = TRUE` or `destination != maslocal`, returns a `publishInfo` object 
#' @examples
#' 
#' \dontrun{
#' mas_module <- publish_model(sess, mod, "maslocal", "R_model_published")
#' 
#' scored <- masScore(sess, mas_module, hmeq[1,-1])
#' }
#' 
#' @export

publish_model <-  function(session, model, name, 
                               destination = "maslocal",
                               exact = TRUE,
                               force = FALSE,
                               publishInfo = FALSE,
                               ...) {

  if (!is_sasctl_class(model, "MMmodel")) {
    model <- sasctl::get_model(session, model, exact = exact, ...)
  }  
  
  if (missing(name)) {
    name = model[["name"]] 
  }
  
  payload <- list(
    name = model[["name"]] ,
    notes = "Published from RSasclt", # model$description,
    modelContents = list(list(modelName = name,
                         sourceUri = model$links[model$links$rel == "self",][["uri"]],
                         publishLevel = "model")),
    destinationName = destination
  )
  

  publish <- vPOST(session,
                 path = "modelManagement/publish",
                 query = list(force = tolower(as.character(force)), 
                              reloadModelTable = "true"),
                 ### the following content type must be used or the API will reject it
                 httr::content_type("application/vnd.sas.models.publishing.request.asynchronous+json"),
                 payload = payload,
                 ...
  )
  
  while (publish$state == "pending") {
    
    Sys.sleep(1)

    publish <- vGET(session, publish$links[publish$links$rel == "self",][["uri"]], 
                    ...)
    
  } 
  
  if (publishInfo) {
    
    publish <- create_sasctl_obj(publish, "publishInfo")

    return(publish)
  }
  
  if (publish$state == "failed") {
    logmsg <- vGET(session, publish$links[publish$links$rel == 
                                  "publishingLog", ][["uri"]])[["log"]]
    
    stop(paste("The model publication failed with log:", logmsg))
  }

  
  if (destination == "maslocal") {
    attempt <- 1
    while (!exists("module") && attempt <= 15 ) {
      attempt <- attempt + 1
      Sys.sleep(1)
      try(
        { 
         module <- get_masmodule(session, name)
         },
        silent = TRUE
      )
    }
    
    return(module)
  } else {
    
    return(publish)
  }
  

}

