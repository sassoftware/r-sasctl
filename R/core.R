# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

#' Viya Session
#' 
#' Creates a Viya session object to be used in other calls
#' 
#' @param hostname string, SAS Viya url
#' @param username string, username for login
#' @param password string, username password
#' @param client_id string, client_id used for authentication, if left blank will use default
#' @param client_secret string, client_secret used for authentication, if left blank will use default
#' @param oauth_token string, if Oauth token is provided, a viya_connection is created
#' @param auth_code logical, if TRUE will open a browser with the user and request the authentication code to continue the authentication process. Viya 2022+ only.
#' @param authinfo A `character` string that specifies an alternative path to a `.authinfo` file that is used for authentication. By default, `~/.authinfo` is used on Linux and `%HOMEDRIVE%` or `\%HOMEPATH%\_authinfo` is used on Windows.
#' @param verbose logical, return print API call information
#' @param cacert ca certificate list
#' @param platform logical, make a get call to get platform information (release, OS, siteName)
#' 
#' @return `viya_connection` class object
#' 
#' @examples 
#' 
#' \dontrun{
#' sess <- session(hostname = "http://myserver.com",
#'          username = "myuser",
#'          password = "mysecret")
#' }
#' 
#' @family authentication
#' 
#' @export
#' 

session <- function(hostname, username = NULL, password = NULL,
                         client_id = NULL, client_secret = NULL,
                         oauth_token = NULL, authinfo = NULL,
                         auth_code = FALSE, verbose = FALSE, 
                         cacert = NULL, platform = TRUE) {
  
  if (!is.null(cacert)) {
    httr::set_config(httr::config(cainfo = cacert))
  }
  
  # hostname info
  hostname <- httr::parse_url(hostname)
  
  if (is.null(hostname$scheme)) {
    stop("hostname must have http:// or https:// protocol")
  }
  
  # authinfo credentials
  
  if (is.null(username) & is.null(client_id)) {
  
  authinfo_ <- query_authinfo(hostname = hostname$hostname,
                              username = username, 
                              filepath = authinfo)
  
  username <- authinfo_$username
  password <- authinfo_$password
  
  }
  
  # check if client_id and client_secret are defined  
  if (is.null(client_id) && !is.null(client_secret) | 
      !is.null(client_id) && is.null(client_secret)) {
        stop("'client_id' and 'client_secret' must be defined.")
  }
      
  # set login type
  if (!is.null(client_id) && !is.null(client_secret) && !is.null(username) && !is.null(password) && !auth_code) {
    grant_type <- "password"
    
  } else {
    if (!is.null(client_id) && !is.null(client_secret)) {
      grant_type <- "client_credentials"
      username <- NULL
      password <- NULL
    }
  }
  
  # use default client    
  if (is.null(client_id) && is.null(client_secret)) {
        
    client_id <- "sas.ec"
    client_secret <- ""
    
    grant_type <- "password"
  } 
  
  ## returning viya_connectio object with oauthToken only
  
  if (!is.null(oauth_token)) {
    
    return(oauth_(oauth_token, hostname, platform))
    
  }
  
  if (auth_code) {

  if (interactive()) {
    url <- paste0(hostname$hostname, "/SASLogon/oauth/authorize?client_id=", client_id,"&response_type=code")
    utils::browseURL(url)
    message(paste0("If a browser don't open automatically, use the following url: ", url))
    code <- readline("Authorization code from browser:")
    
    } else {
      stop("Authentication code method can't be used outside an interactive session.")
    }

  
  grant_type <- "authorization_code"
  
  } else {
    code <- NULL
  }
  
  ## make username password authentication authentication
  #url <- httr::parse_url(hostname)
  url <- hostname
  url$path <- "/SASLogon/oauth/token"
  

  payload <- list(
      client_id = client_id,
      client_secret = client_secret,
      grant_type = grant_type,
      username = if (grant_type %in% c("authorization_code", "client_credentials")) {NULL} else {username},
      password = if (grant_type %in% c("authorization_code", "client_credentials")) {NULL} else {password},
      code = code
    )

  response <- httr::POST(
    url = httr::build_url(url),
    
    httr::content_type("application/x-www-form-urlencoded"),
    body = payload,
    encode = "form",
    if (verbose) httr::verbose()
  )
  
  httr::stop_for_status(response)
  
  authorization <- jsonlite::fromJSON(
                              httr::content(response, as = "text")
                              )
  ## creating a sasctl connection object of viya_connection class
  
  authorization$hostname <- hostname
  authorization$username <- username
  
  authorization <- structure(authorization,
                    class = "viya_connection",
                    package = "sasctl")
  
  if (platform) {
  authorization$platform <- vGET(authorization,
                                 path = "licenses/site")[c("release", 
                                                           "osName",
                                                           "siteName")]
  }
  else {
    authorization$platform <- NA
  }
  
  return(authorization)
}

#' Viya oauth token
#' 
#' Requests a viya oauthtoken
#' 
#' @param hostname string, SAS Viya url
#' @param consul_token consul token uuid
#' @param verbose logical, return print API call information
#' @return list of API call request data
#' @examples 
#' 
#' \dontrun{
#' token <- oauth_consul("http://myserver.com",
#'                   consul_token = "47817a5a-3751-4fad-9558-b12b8a702b69") #token is an uuid
#' 
#' }
#' 
#' @family authentication
#' 
#' @export
#' 

oauth_consul <- function(hostname, consul_token, verbose = FALSE) {
  
  hostname <- httr::parse_url(hostname)
  
  if (is.null(hostname$scheme)) {
    stop("hostname must have http:// or https:// protocol")
  }
  
  hostname$path <- "/SASLogon/oauth/clients/consul"
  hostname$query <- list(
    "callback" = "false",
    "serviceId" = "app"
  )
  
  response <- httr::POST(
    url = httr::build_url(hostname),
    httr::add_headers(
      "X-Consul-Token" = consul_token
    ),
    
    if (verbose) verbose()
  )
  
  httr::stop_for_status(response)
  
  token <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  
  token <- oauth_(oauth_token = token$access_token, 
                  hostname = hostname, 
                  expires_in = token$expires_in, 
                  jti = token$jti,
                  scope = token$scope,
                  username = "consul")
  
  return(token)
}

#' Create viya_connection class object with only the provided oauthToken
#' @param oauth_token ouath token provided by the user
#' @param hostname server hostname
#' @param platform logical, make a get call to get platform information
#' @param username a username to identify the token
#' @return viya_connection class object
#' 
#' @family authentication
#' 
#' @noRd

oauth_ <- function(oauth_token, hostname, expires_in = NA, 
                   scope = NA, jti= NA, platform = TRUE, username = NA) {

  
  authorization <- structure(list(
    access_token = oauth_token,
    token_type = "bearer",
    id_token = NA,
    expires_in = expires_in,
    scope = scope,
    jti = jti,
    hostname = hostname,
    username = username
  ),
    class = "viya_connection",
    package = "sasctl")
  
  if (platform) {
  authorization$platform <- vGET(authorization,
                                 path = "licenses/site")[c("release", "osName",
                                                           "siteName")]
  } else{
    authorization$platform <- NA
  }
  
  return(authorization)
}

#' verify Connection type
#' 
#' @param session a viya_connection object
#' @return nothing, throws error if verification fails.
#' 
#' @noRd
#' 

is_viya_session <- function(session) {
  if (methods::is(session, "viya_connection")) {
    return(TRUE)
  } else {
    stop("session must be a valid viya_connection Class type, obtained using sasctl::session().")
  }
}


#' Viya GET
#' 
#' Wrapper to make generic GET calls to SAS Viya
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param path character, path to the GET api endpoint
#' @param ... additional parameters to be passed to `httr::GET` such as `httr::add_headers`
#' @param query list, additional URL query parameters
#' @param verbose logical, return print API call information
#' @param output string, if `output = "json"` will return `httr::fromJSON(httr::content(response, as = "text"))`,
#' else if `output = "response"` will return a `httr::response()` object
#' @return list if `output = "json"` default. `httr::response()` if `output = response`.
#' @examples 
#' 
#' \dontrun{
#' folders <- vGET(session,
#'                 path = "folders/folders/")
#' 
#' 
#' newFolder <- vPOST(session,
#'                    path = paste0("folders/folders/"),
#'                    query = list(parentFolderUri = folders$items$parentFolderUri[1]),
#'                    payload = list(name = "newFolder"),
#'                    httr::content_type("application/json"))
#' 
#' 
#' deletedFolder <- vDELETE(session,
#'                          path = "folders/folders/",
#'                          resourceID = newFolder$id)
#' 
#' }
#' @family core API requests
#' @export
#' @seealso [httr::GET()], [httr::POST()], [httr::PUT()], [httr::DELETE()]
#' 

vGET <- function(session,
                 path,
                 ...,
                 query,
                 verbose = FALSE,
                 output = "json") {
  
  is_viya_session(session)
  
  if (!(output %in% c("json", "text", "response"))) {
    stop("Output must be 'json', 'text' or 'response'")
  }
  
  url <- session$hostname
  url$path <- paste0("/", path)
  
  if (!missing(query)) {
      url$query <- query
  }
  
  response <- httr::GET(
    url = httr::build_url(url),
    #httr::accept("application/json"),
    httr::add_headers(
      "authorization" = paste("Bearer", session$access_token)
    ),
    
    ...,
    
    if (verbose) httr::verbose()
  )
  
  
  if (httr::status_code(response) == 400) {
    messages <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
    
    warning(messages$message, immediate. = TRUE)
    warning(messages$details, immediate. = TRUE)
  }
  
  httr::stop_for_status(response)
  
  if (output == "json") {
    
    json_out <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
    
      if ("etag" %in% names(httr::headers(response)) ) {
        json_out$etag <- httr::headers(response)$etag
      }
    
    return(json_out)
  }
  
  if (output == "response") {
    
    return(response)
  }
  
  if (output == "text") {
    
    response <- httr::content(response, as = "text", encoding = "UTF-8")
    return(response)
  }
  
}


#' Viya POST
#' 
#' Wrapper to make generic POST calls to SAS Viya
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param path character, path to the GET api endpoint
#' @param payload list or json string, if it is a list, will be transformed in a json string using `jsonlite::toJSON`
#' @param ... additional parameters to be passed to `httr::POST` such as `httr::add_headers`
#' @param query list, additional URL query parameters
#' @param fragment string, additional URL fragment parameter `url.com?query#fragment`
#' @param encode payload encoding type, to be passed to `httr::POST`.
#' @param verbose logical, return print API call information
#' @param output string, if `output = "json"` will return `httr::fromJSON(httr::content(response, as = "text"))`,
#' else if `output = "response"` will return a `httr::response()` object
#' @return list if `output = "json"` default. `httr::response()` if `output = response`.
#' @examples 
#' 
#' \dontrun{
#' folders <- vGET(session,
#'                 path = "folders/folders/")
#' 
#' 
#' newFolder <- vPOST(session,
#'                    path = paste0("folders/folders/"),
#'                    query = list(parentFolderUri = folders$items$parentFolderUri[1]),
#'                    payload = list(name = "newFolder"),
#'                    httr::content_type("application/json"))
#' 
#' 
#' deletedFolder <- vDELETE(session,
#'                          path = "folders/folders/",
#'                          resourceID = newFolder$id)
#' 
#' }
#' @family core API requests
#' @export
#' @seealso [httr::GET()], [httr::POST()], [httr::PUT()], [httr::DELETE()]
#'  

vPOST <- function(session, 
                  path,
                  payload,
                  ...,
                  query,
                  fragment,
                  encode = "json",
                  verbose = FALSE,
                  output = "json") {
    
    is_viya_session(session)
    
    if (!(output %in% c("json", "text", "response"))) {
      stop("Output must be 'json', 'text' or 'response'")
    }
  
    url <- session$hostname
    url$path <- paste0("/", path)
    
    if (!missing(query)) {
    url$query <- query
    }
    
    if (!missing(fragment)) {
      url$fragment <- fragment
    }    
    
    # if (!is.raw(payload)) {
    #   
    #     if (class(payload) != "form_file") {
    #       
    #       if (is.list(payload)) {
    #         
    #         payload <- jsonlite::toJSON(payload,
    #                                     auto_unbox = TRUE)
    #       }
    #     
    #       if (!jsonlite::validate(payload)) {
    #         
    #         stop("JSON payload is malformed")
    #         
    #       }
    #         
    #     }
    # }
    
    response <- httr::POST(
      url = httr::build_url(url),
      httr::add_headers(
        "Authorization" = paste("Bearer", session$access_token)
      ),
      
      body = payload,
      
      encode = encode,
      
      ...,
      
      if (verbose) httr::verbose()
    )
    
    if (httr::status_code(response) == 400) {
      messages <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
      
      warning(messages$message, immediate. = TRUE)
      warning(messages$details, immediate. = TRUE)
    }
        
    httr::stop_for_status(response)
    
    if (output == "json") {
      
      json_out <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
      
      if ("etag" %in% names(httr::headers(response)) ) {
        json_out$etag <- httr::headers(response)$etag
      }
      
      return(json_out)
    }
    
    if (output == "response") {
      return(response)
    }
    
    if (output == "text") {
      
      response <- httr::content(response, as = "text", encoding = "UTF-8")
      return(response)
    }
    
}


#' Viya PUT
#' 
#' Wrapper to make generic DELETE calls to SAS Viya
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param path character, path to the GET api endpoint
#' @param payload list or json string, if it is a list, will be transformed in a json string using `jsonlite::toJSON`
#' @param encode payload encoding type, to be passed to `httr::POST`.
#' @param ... additional parameters to be passed to `httr::PUT` such as `httr::add_headers`
#' @param verbose logical, return print API call information
#' @param output string, if `output = "json"` will return `httr::fromJSON(httr::content(response, as = "text"))`,
#' else if `output = "response"` will return a `httr::response()` object
#' @return list if `output = "json"` default. `httr::response()` if `output = response`.
#' @examples 
#' 
#' \dontrun{
#' folders <- vGET(session,
#'                 path = "folders/folders/")
#' 
#' 
#' newFolder <- vPOST(session,
#'                    path = paste0("folders/folders/"),
#'                    query = list(parentFolderUri = folders$items$parentFolderUri[1]),
#'                    payload = list(name = "newFolder"),
#'                    httr::content_type("application/json"))
#' 
#' 
#' deletedFolder <- vDELETE(session,
#'                          path = "folders/folders/",
#'                          resourceID = newFolder$id)
#' 
#' }
#' @export
#' @family core API requests
#' @details This function in built on top of httr for convenience when calling SAS Viya API endpoints..
#' @seealso [httr::GET()], [httr::POST()], [httr::PUT()], [httr::DELETE()], [httr::response()]
#' 

vPUT <- function(session, 
                 path,
                 payload,
                 ...,
                 verbose = FALSE,
                 encode = "json",
                 output = "json")  {
  
  is_viya_session(session)
  
  url <- session$hostname
  url$path <- paste0("/", path)
  
  
  #### transforming list to json
  
  # if (!missing(payload)) {
  #   if (is.list(payload)) {
  #     
  #     payload <- jsonlite::toJSON(payload,
  #                                 auto_unbox = TRUE)
  #   }
  #   
  #   if (!jsonlite::validate(payload)) {
  #     stop("JSON payload is malformed")
  #   }
  #   
  #   
  # } else {
  #   payload <- list()
  # }
  
  if (missing(payload)) {
    payload <- list()
  }
  
  response <- httr::PUT(
    url = httr::build_url(url),
    httr::add_headers(
      "Authorization" = paste("Bearer", session$access_token)),
    encode = encode,
    body = payload,
    ...,
    
    if (verbose) httr::verbose()
  )
  
  if (httr::status_code(response) == 400) {
    messages <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
    
    warning(messages$message, immediate. = TRUE)
    warning(messages$details, immediate. = TRUE)
  }
  
  httr::stop_for_status(response)
  
  if (output == "json") {
    
    json_out <- jsonlite::fromJSON(httr::content(response, as = "text"))
    
    if ("etag" %in% names(httr::headers(response)) ) {
          json_out$etag <- httr::headers(response)$etag
    }
  
  return(json_out)
  }
  
  if (output == "response") {
    return(response)
  }
  
  if (output == "text") {
    
    response <- httr::content(response, as = "text", encoding = "UTF-8")
    return(response)
  }
  
}


#' Viya DELETE
#' 
#' Wrapper to make generic DELETE calls to SAS Viya
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param path character, path to the GET api endpoint
#' @param ... additional parameters to be passed to `httr::DELETE` such as `httr::add_headers`
#' @param verbose logical, return print API call information
#' @return An `httr::response()` type object
#' @examples 
#' 
#' \dontrun{
#' folders <- vGET(session,
#'                 path = "folders/folders/")
#' 
#' 
#' newFolder <- vPOST(session,
#'                    path = paste0("folders/folders/"),
#'                    query = list(parentFolderUri = folders$items$parentFolderUri[1]),
#'                    payload = list(name = "newFolder"))
#' 
#' 
#' deletedFolder <- vDELETE(session,
#'                          path = "folders/folders/",
#'                          resourceID = newFolder$id)
#' 
#' }
#' @family core API requests
#' @export
#' @seealso [httr::GET()], [httr::POST()], [httr::PUT()], [httr::DELETE()]
#' 

vDELETE <- function(session, 
                    path,
                    ...,
                    verbose = FALSE) {
  
  is_viya_session(session)
  
  url <- session$hostname
  url$path <- paste0("/", path)
  
  response <- httr::DELETE(
    url = httr::build_url(url),
    #httr::accept("application/json"),
    httr::add_headers(
      "authorization" = paste("Bearer", session$access_token)
    ),
    
    ...,
    
    if (verbose) httr::verbose()
  )
  
  if (httr::status_code(response) == 400) {
    messages <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
    
    warning(messages$message, immediate. = TRUE)
    warning(messages$details, immediate. = TRUE)
  }
  
  httr::stop_for_status(response)
  
  if (response$status_code == 204) {
    message(paste0("The resource ", path, " was successfully deleted."))
  } else {
    message(paste0("The resource ", path, " was not deleted or doesn't exists."))
  }
  
  return(response)
}

#' Viya HEAD
#' 
#' Wrapper to make generic DELETE calls to SAS Viya
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param path character, path to the GET api endpoint
#' @param payload list or json string, if it is a list, will be transformed in a json string using `jsonlite::toJSON`
#' @param ... additional parameters to be passed to `httr::PUT` such as `httr::add_headers`
#' @param verbose logical, return print API call information
#' @param output string, if `output = "json"` will return `httr::fromJSON(httr::content(response, as = "text"))`,
#' else if `output = "response"` will return a `httr::response()` object
#' @return list if `output = "json"` default. `httr::response()` if `output = response`.
#' @examples 
#' 
#' \dontrun{
#' folders <- vGET(session,
#'                 path = "folders/folders/")
#' 
#' 
#' newFolder <- vPOST(session,
#'                    path = paste0("folders/folders/"),
#'                    query = list(parentFolderUri = folders$items$parentFolderUri[1]),
#'                    payload = list(name = "newFolder"),
#'                    httr::content_type("application/json"))
#' 
#' 
#' deletedFolder <- vDELETE(session,
#'                          path = "folders/folders/",
#'                          resourceID = newFolder$id)
#' 
#' }
#' @export
#' @family core API requests
#' @details This function in built on top of httr for convenience when calling SAS Viya API endpoints..
#' @seealso [httr::GET()], [httr::POST()], [httr::PUT()], [httr::DELETE()], [httr::response()]
#' 

vHEAD <- function(session, 
                 path,
                 payload,
                 ...,
                 verbose = FALSE,
                 output = "response")  {
  
  is_viya_session(session)
  
  if (!(output %in% c("json", "text", "response"))) {
    stop("Output must be 'json', 'text' or 'response'")
  }
  
  
  url <- session$hostname
  url$path <- paste0("/", path)
  
  
  #### transforming list to json

  response <- httr::HEAD(
    url = httr::build_url(url),
    #httr::accept("application/json"),
    httr::add_headers(
      "Authorization" = paste("Bearer", session$access_token)
    ),
    encode = "json",
    ...,
    
    if (verbose) httr::verbose()
  )
  
  if (httr::status_code(response) == 400) {
    messages <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
    
    warning(messages$message, immediate. = TRUE)
    warning(messages$details, immediate. = TRUE)
  }
  
  httr::stop_for_status(response)
  
  if (output == "json") {
    
    json_out <- jsonlite::fromJSON(httr::content(response, as = "text"))
    
    return(json_out)
  }
  
  if (output == "response") {
    return(response)
  }
  
  if (output == "text") {
    
    response <- httr::content(response, as = "text", encoding = "UTF-8")
    return(response)
  }
  
}

#' Simpler query list
#' 
#' @param query_list a list of filter options according to `https://developer.sas.com/apis/rest/Topics/#filters`.
#' @param exact boolean, if TRUE, will use exact match "equal" instead of "contains"
#' @return a `list` of filters queries in Viya API format 
#' @noRd
#' 

build_filter_query <- function(filters, exact = FALSE){
  
  ## TODO apply case insensivity operator $primary
  
  ## define operator type
  if (exact) {
    operator <- "eq"
  } else {
    operator <- "contains"
  }

  ## creating a filters with "contains"
  query_list <- sapply(seq_along(filters), function(x){
    paste0('or(',
           paste0(paste0(operator, "("),
                  names(filters)[[x]], ", '", filters[[x]], "')", 
                  collapse = ','),
           ')')
  })
  
  ## merging filters
  if (length(query_list)  == 0 ) {
    
    query <- list()
    
  } else if (length(query_list) == 1) {
    query <- list(filter = query_list)
    
  } else if (length(query_list) > 1) {
    
    query <- list(filter = paste0('or(', paste0( 
      query_list,
      collapse = ',' ),")")
      
    )
  }
  
  return(query)
}

#' verify sasctl attribute type
#' 
#' @param object any object
#' @return logical, is sasctl object
#' @noRd
#' 

is_sasctl <- function(object) {
  
  return(any(attr(object, "package") == "sasctl"))
  
}

#' verify sasctl class type
#' 
#' @param object any object
#' @param class class name or vector to verify
#' @return logical, is sasctl object
#' @noRd
#' 

is_sasctl_class <- function(object, class) {
  
  return(any(class(object) %in% class))
  
}

#' verify sasctl type or uuid
#' 
#' MAS modules does not use UUID format
#' 
#' @param object any object
#' @param type sasctl class to verify
#' @param uuid_sufficient check if uuid is sufficient to complete the call
#' @return uuid
#' @noRd
#' 

is_sasctl_uuid <- function(object, type, uuid_sufficient = TRUE) {
  
    warning("is_sasctl_uuid is DEPRECATED and should be replaced for single_id_from_object")
  
    if (class(object) == type) {
      return(object$id)
    } else {
      
      ### check if UUID is sufficient
      if (uuid::UUIDvalidate(object) & !uuid_sufficient) {
        stop(paste0("Must be a ", type, " object, uuid not sufficient"))
      }
      
      if (uuid::UUIDvalidate(object)) {
        return(object)
      } else {
        stop("Invalid uuid format")
      }
    }

}


#' sasctl contructor
#' 
#' @param object any object
#' @param type sasctl class name to add
#' @param append boolean, append to current class
#' @return uuid
#' @noRd
#' 

create_sasctl_obj <- function(object, type, append = FALSE) {
  
  if (append) {
    class(object) <- c(type, class(object))
  } else {
    class(object) <- type
  }
  
  attr(object, 'package') <- "sasctl"
  
  return(object)
}


#' Return project or model uuid from name string object, otherwise return the input object
#' 
#' @param session viya_connection object, obtained through `session` function
#' @param object sasctl object, uuid or name. If name, will try to find a single model with exact name match. See `exact` parameter
#' @param class verify if correct sasctl class
#' @param exact the filter query should use "contains" for partial match or "eq" for exact match
#' @param type string, "project" or "type"
#' @return the object or sasctl object id
#' @noRd
#' 

single_id_from_object <- function(session, object, class ,type, exact = TRUE) {
  
  if (is_sasctl(object)) {
    
    if (!(class(object) %in% class)) {
      stop(paste("Invalid", class, "object"))
    } 
      
    return(object$id)
  }
  
  if (!is.character(object)) {
    stop("Not a valid UUID, Sasctl object or name")
  }
  
  if (uuid::UUIDvalidate(object)) {
    return(object)
  } 
  
  if (!(type %in% c("project", "model", "mas"))) {
      stop("Invalid type for name search")
    }
    
    if (type == "project") {
      objlist <- list_projects(session, filters = list(name = object), exact = exact)
    }
    
    if (type == "model") {
      objlist <- list_models(session, filters = list(name = object), exact = exact)
    }
    
    if (type == "mas") {
      objlist <- list_modules(session, filters = list(name = object), exact = exact)
      type = "mas module"
    }
    
    
    if (length(objlist$id) == 0) {
      stop(paste("No", type, "with the given name"))
    }
    
    if (length(objlist$id) > 1) {
      stop(paste("More than 1 project returned with current name:", paste(objlist$name, collapse = ", ")))
    }
    
    if (length(objlist$id) == 1) {
      single_id <- objlist$id ## project uuid
    }
    
    return(single_id)
}
    
    
    


