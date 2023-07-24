# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

#' Create a new client
#' 
#' Create a new client for API call
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param client_id name of the new client to be created
#' @param client_secret client secret of the new client
#' @param scope The scopes allowed for the client to obtain on behalf of users, when using any grant type other than "client_credentials". Groups are treated as scopes. Therefore, the scopes that can be obtained by the client on behalf of a user will be the intersection of the user's groups and the scopes registered to the client via this property. Use the wildcard "" to match all groups. Since SAS Viya allows authorization rules to explicitly deny access to specific groups, SAS recommends always using "". The wildcard "*" will not match internal UAA scopes. This list should always include the scope "openid", which is used to assert the identity of the user that the client is acting on behalf of. For clients that only use the grant type "client_credentials" and therefore do not act on behalf of users, use the default scope "uaa.none".
#' @param access_token_validity The time in seconds to access token expiration after it is issued.
#' @param authorized_grant_types The list of grant types that can be used to obtain a token with this client. Types can include authorization_code, password, implicit, and client_credentials.
#' @param authorities The scopes that the client is able to grant itself when using the "client_credentials" grant type. Wildcards are not allowed.
#' @param additional_parameters `list` of parameters and lists to be added to the client creation payload
#' @param ... additional parameters to be passed to `httr::POST` such as `httr::add_headers`
#' @return A `sasClient` object list
#' @examples
#' 
#' \dontrun{
#' new_client <- register_client(sess, 'my_client', 'my_s3cr3t!')
#' new_client
#' }
#' 
#' @export

register_client <- function(session, client_id, client_secret,
                            scope = list("openid"),
                            access_token_validity = 36000,
                            authorized_grant_types = list('client_credentials'),
                            authorities = list('uaa.none'),
                            additional_parameters = NULL,
                            ...) { 
  

  payload <- list(
    "client_id" = client_id,
    "scope" = scope,
    "access_token_validity" = access_token_validity,
    "client_secret" = client_secret,
    "authorities" = authorities,
    "authorized_grant_types" = authorized_grant_types,
    "redirect_uri" = 'urn:ietf:wg:oauth:2.0:oob'
  )
  
  if (!is.null(additional_parameters)) {
    
    if (!is.list(additional_parameters)) {
      stop("additional_paramenters must be a list or a list of lists")  
    }
    payload <- append(payload, additional_parameters)
  }
  
  response <- vPOST(session,
    path = "SASLogon/oauth/clients",
    payload = payload,
    ...
  )
  
  sasclient <- create_sasctl_obj(response, 'sasClient')
  
  return(sasclient)
}

#' Delete a client
#' 
#' Delete a client
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param client `sasClient` object or client ID.
#' @return A [`httr::response`] object.
#' 
#' @examples
#' 
#' \dontrun{
#' new_client <- register_client(sess, 'my_client', 'my_s3cr3t!')
#' delete_client(sess, "my_client")
#' }
#' 
#' @export

delete_client <- function(session, client){
  
  if (is_sasctl_class(client, "sasClient")) {
    client_id <- client$client_id
  } else {
    client_id <- client
  }
  
  del_client <- vDELETE(session,
                        paste0("SASLogon/oauth/clients/", client_id))
  
  invisible(del_client)
}

#' Get a client
#' 
#' Returns a single sasctl `MMclient` from SAS Model Manager
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param client `sasClient` object or client ID.
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @return `list` with sasctl attribute
#' @examples
#' 
#' \dontrun{
#' my_client <- get_client(sess, client = ModelProj)
#' my_client
#' }
#' 
#' @export

get_client <- function(session, client, ...){
  
  if (is_sasctl_class(client, "sasClient")) {
    client_id <- client$client_id
  } else {
    client_id <- client
  }
  
  
  sasclient <- vGET(session,
                  path = paste0("SASLogon/oauth/clients/", client_id),
                  ...
  )
  
  sasclient <- create_sasctl_obj(sasclient, "sasClient")
  
  return(sasclient) 
  
}


#' List Clients
#' 
#' Returns a list of clients
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param filter character string of client_id name to be filtered
#' @param count The number of results per page. The default is 100.
#' @param start the index of the first project to return
#' @param exact boolean, If the filter query should use "co" for partial match or "eq" for exact match
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @return A `data.frame` of `sasclientList` class with the list of clients
#' @examples
#' 
#' \dontrun{
#' clients <- list_clients(sess, filter = list(createdBy = "creatorUser", name = "projectName"))
#' clients
#' }
#' 
#' @export

list_clients <-  function(session, start = 1, count = 100, filter = NULL, exact = FALSE, ...) {
  
  ## creating filter query eg: "or(or(contains(createdBy, '123')),or(contains(name, '123')))"
  
  
  if (!is.null(filter)) {
     query <- list(filter = paste0('client_id ', ifelse(exact, "eq", "co"),  ' "', filter,'"' ))
  } else {
     query <- list()
  }
  
  ## adding query for limits and start index
  query <- append(list(startIndex = start, count = count), query)  
  
  clientlist <- vGET(session,
                   path = "SASLogon/oauth/clients/",
                   query = query,
                   ...
  )
  
  clientlist <- create_sasctl_obj(clientlist, "sasClientList", append = TRUE)
  
  return(clientlist)
}
