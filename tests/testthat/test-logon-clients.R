# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

# Tests for R/logon_clients.R: register_client, delete_client, get_client,
# list_clients.  All HTTP calls are replaced with local_mocked_bindings.

library(testthat)
library(sasctl)

sess <- fake_session()

fake_client_payload <- list(
  client_id  = "my_client",
  scope      = list("openid"),
  client_secret = "s3cr3t",
  authorities = list("uaa.none"),
  authorized_grant_types = list("client_credentials"),
  redirect_uri = "urn:ietf:wg:oauth:2.0:oob"
)

# ── register_client ───────────────────────────────────────────────────────────

test_that("register_client: errors when additional_parameters is not a list", {
  expect_error(
    local_mocked_bindings(
      vPOST = function(...) list(),
      .package = "sasctl",
      { register_client(sess, "c", "s", additional_parameters = "bad") }
    ),
    "must be a list"
  )
})

test_that("register_client: passes additional_parameters into payload", {
  captured <- NULL
  local_mocked_bindings(
    vPOST = function(session, path, payload, ...) {
      captured <<- payload
      fake_client_payload
    },
    .package = "sasctl"
  )
  register_client(sess, "c", "s",
                  additional_parameters = list(extra_param = "val"))
  expect_equal(captured$extra_param, "val")
})

# ── delete_client ─────────────────────────────────────────────────────────────

test_that("delete_client: accepts a plain character client_id", {
  local_mocked_bindings(
    vDELETE = function(session, path, ...) fake_delete_response(url = path),
    .package = "sasctl"
  )
  # Should not error
  expect_silent(delete_client(sess, "my_client_id"))
})

test_that("delete_client: extracts client_id from a sasClient object", {
  captured_path <- NULL
  local_mocked_bindings(
    vDELETE = function(session, path, ...) {
      captured_path <<- path
      fake_delete_response()
    },
    .package = "sasctl"
  )
  client_obj <- structure(
    list(client_id = "extracted_id"),
    class   = "sasClient",
    package = "sasctl"
  )
  delete_client(sess, client_obj)
  expect_true(grepl("extracted_id", captured_path))
})

# ── get_client ────────────────────────────────────────────────────────────────

test_that("get_client: uses client_id from a sasClient object", {
  captured_path <- NULL
  local_mocked_bindings(
    vGET = function(session, path, ...) {
      captured_path <<- path
      fake_client_payload
    },
    .package = "sasctl"
  )
  client_obj <- structure(
    list(client_id = "obj_client"),
    class   = "sasClient",
    package = "sasctl"
  )
  get_client(sess, client_obj)
  expect_true(grepl("obj_client", captured_path))
})

# ── list_clients ──────────────────────────────────────────────────────────────

test_that("list_clients: builds exact filter query when exact=TRUE", {
  captured_query <- NULL
  local_mocked_bindings(
    vGET = function(session, path, query, ...) {
      captured_query <<- query
      list(Resources = list(), startIndex = 1,
           itemsPerPage = 100, totalResults = 0)
    },
    .package = "sasctl"
  )
  list_clients(sess, filter = "myclient", exact = TRUE)
  expect_true(grepl("eq", captured_query$filter))
})

test_that("list_clients: builds contains filter query when exact=FALSE", {
  captured_query <- NULL
  local_mocked_bindings(
    vGET = function(session, path, query, ...) {
      captured_query <<- query
      list(Resources = list(), startIndex = 1,
           itemsPerPage = 100, totalResults = 0)
    },
    .package = "sasctl"
  )
  list_clients(sess, filter = "partial", exact = FALSE)
  expect_true(grepl("co", captured_query$filter))
})
