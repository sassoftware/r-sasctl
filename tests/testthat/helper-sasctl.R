# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

# ---------------------------------------------------------------------------
# Shared test helpers — loaded automatically by testthat before any test file
# ---------------------------------------------------------------------------

#' Build a minimal viya_connection object without hitting a server.
fake_session <- function(hostname = "http://test.server.com",
                         release = "V04", major = 2024L, minor = 3L) {
  structure(
    list(
      access_token  = "fake_token_abc123",
      token_type    = "bearer",
      id_token      = NA,
      expires_in    = 36000L,
      scope         = "openid",
      jti           = "fake-jti",
      hostname      = httr::parse_url(hostname),
      username      = "testuser",
      cfg           = httr::config(),
      platform      = list(
        release = release,
        osName  = "Linux",
        siteName = "TestSite",
        major   = major,
        minor   = minor
      ),
      clientInfo    = base64enc::base64encode(charToRaw("sas.ec:"))
    ),
    class   = "viya_connection",
    package = "sasctl"
  )
}

#' Create an httr response object with a JSON body.
#'
#' @param status  HTTP status code (integer)
#' @param body    R list/vector that will be serialised to JSON
#' @param headers named list of extra response headers
#' @param url     URL string used in the response object
fake_response <- function(status  = 200L,
                          body    = list(),
                          headers = list(),
                          url     = "http://test.server.com/test") {
  all_headers <- c(
    list(`content-type` = "application/json; charset=UTF-8"),
    headers
  )
  content_raw <- charToRaw(
    jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
  )
  structure(
    list(
      url         = url,
      status_code = as.integer(status),
      headers     = structure(all_headers, class = c("insensitive", "list")),
      content     = content_raw,
      times       = c(redirect = 0, namelookup = 0, connect = 0,
                      pretransfer = 0, starttransfer = 0, total = 0)
    ),
    class = "response"
  )
}

#' Create an httr response that represents a 204 No Content (DELETE success).
fake_delete_response <- function(url = "http://test.server.com/test") {
  structure(
    list(
      url         = url,
      status_code = 204L,
      headers     = structure(list(), class = c("insensitive", "list")),
      content     = raw(0L),
      times       = c(redirect = 0, namelookup = 0, connect = 0,
                      pretransfer = 0, starttransfer = 0, total = 0)
    ),
    class = "response"
  )
}

#' Build a fake MMproject sasctl object.
#'
#' Links match the Model Repository API schema:
#' https://developer.sas.com/rest-apis/modelRepository
fake_project <- function(id   = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee",
                         name = "TestProject",
                         etag = '"abc123"') {
  self_uri <- paste0("/modelRepository/projects/", id)
  obj <- list(id = id, name = name, etag = etag,
              links = data.frame(
                method = "GET",
                rel    = "self",
                href   = self_uri,
                uri    = self_uri,
                type   = "application/vnd.sas.models.project",
                stringsAsFactors = FALSE
              ))
  structure(obj, class = "MMproject", package = "sasctl")
}

#' Build a fake MMmodel sasctl object.
#'
#' Links match the Model Repository API schema:
#' https://developer.sas.com/rest-apis/modelRepository
fake_model <- function(id   = "11111111-2222-3333-4444-555555555555",
                       name = "TestModel",
                       etag = '"model_etag"') {
  self_uri <- paste0("/modelRepository/models/", id)
  obj <- list(id = id, name = name, etag = etag,
              links = data.frame(
                method = "GET",
                rel    = "self",
                href   = self_uri,
                uri    = self_uri,
                type   = "application/vnd.sas.models.model",
                stringsAsFactors = FALSE
              ))
  structure(obj, class = "MMmodel", package = "sasctl")
}

#' Build a fake MASmodule sasctl object.
#'
#' Step links match the Micro Analytic Score API schema:
#' https://developer.sas.com/rest-apis/microanalyticScore
#' Each step carries a links data.frame with columns method/rel/uri/type so
#' that get_masmodule() can correctly determine ScoreType via basename([,3]).
fake_masmodule <- function(id        = "my_module",
                           name      = "my_module",
                           ScoreType = "score") {
  step_uri <- paste0("/microanalyticScore/modules/", id, "/steps/", ScoreType)
  step_links <- data.frame(
    method = c("GET",  "POST"),
    rel    = c("self", ScoreType),
    uri    = c(step_uri, step_uri),
    type   = c("application/vnd.sas.microanalytic.module.step",
               "application/vnd.sas.microanalytic.module.step.output"),
    stringsAsFactors = FALSE
  )
  step_items <- data.frame(id = ScoreType, stringsAsFactors = FALSE)
  step_items$links <- list(step_links)

  obj <- list(id = id, name = name, ScoreType = ScoreType,
              steps = list(items = step_items))
  structure(obj, class = "MASmodule", package = "sasctl")
}
