# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

#' List Publish Destinations
#' 
#' Returns a list of Publish Destinations
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param filters list of of names vectors for filter parameters (createdBy, modifiedBy, name). By default it will use the `contains` operation. For more info visit `https://developer.sas.com/apis/rest/Topics/#filters`.
#' @param limit maximum number of projects to return
#' @param start the index of the first project to return
#' @param exact boolean, If the filter query should use "contains" for partial match or "eq" for exact match
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @return A `data.frame` with the list of projects
#' @examples
#' 
#' \dontrun{
#' destinations <- list_destinations(sess)
#' destinations
#' }
#' 
#' @export

list_destinations <-  function(session, start = 0, limit = 10, filters = list(), exact = FALSE, ...) {
  
  ## creating filter query eg: "or(or(contains(createdBy, '123')),or(contains(name, '123')))"
  query <- build_filter_query(filters, exact = exact)
  
  ## adding query for limits and start index
  query <- append(query, list(start = start, limit = limit))  
  
  destinations <- vGET(session,
                   path = "modelPublish/destinations",
                   query = query,
                   ...
  )
  if (length(destinations$items) == 0) {
    return(list())
  }
  
  destinations <- create_sasctl_obj(destinations$items, "ModelDestinationList", append = TRUE)
  
  return(destinations)
}

#' Get a publishing destination by name 
#' 
#' Returns a publishing destination 
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param name destination name
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @return A `data.frame` with the list of projects
#' @examples
#' 
#' \dontrun{
#' destination <- get_destination(sess, 'maslocal')
#' destination
#' }
#' 
#' @export

get_destination <-  function(session, name, ...) {
    
  
  destination <- vGET(session,
                       path = paste0("modelPublish/destinations/", name),
                       ...
  )
  
  destination <- create_sasctl_obj(destination, "ModelDestination", append = TRUE)
  
  return(destination)
}
