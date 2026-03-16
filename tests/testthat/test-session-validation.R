# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

# Tests for session() and oauth_consul() input validation.
# All paths exercise errors that are raised BEFORE any HTTP call is made.

library(testthat)
library(sasctl)

# ── session() ─────────────────────────────────────────────────────────────────

test_that("session: errors without http/https scheme", {
  expect_error(
    session("myserver.com", username = "u", password = "p"),
    "must have http"
  )
})

test_that("session: errors with only client_id (no client_secret)", {
  expect_error(
    session("http://myserver.com",
            username = "u", password = "p",
            client_id = "my_id"),
    "client_id.*client_secret|client_secret.*client_id"
  )
})

test_that("session: errors with only client_secret (no client_id)", {
  expect_error(
    session("http://myserver.com",
            username = "u", password = "p",
            client_secret = "my_secret"),
    "client_id.*client_secret|client_secret.*client_id"
  )
})

test_that("session: verify_ssl=FALSE emits a warning", {
  # The warning is emitted before any HTTP call so no network is needed here;
  # the function will still attempt HTTP after the warning, so we intercept by
  # also mocking httr::POST to avoid a real connection.
  local_mocked_bindings(
    POST = function(...) fake_response(200L, list(
      access_token = "tok", token_type = "bearer",
      id_token = NA, expires_in = 3600, scope = "openid", jti = "j1"
    )),
    stop_for_status = function(...) invisible(NULL),
    .package = "httr"
  )
  local_mocked_bindings(
    vGET = function(...) list(
      release = "V04", osName = "Linux", siteName = "TestSite",
      major = 2024, minor = 3, cadenceVersion = "2024.03",
      links = NULL, version = NULL
    ),
    .package = "sasctl"
  )
  expect_warning(
    session("http://myserver.com", username = "u", password = "p",
            verify_ssl = FALSE),
    "SSL"
  )
})

# ── oauth_consul() ────────────────────────────────────────────────────────────

test_that("oauth_consul: errors without http/https scheme", {
  expect_error(
    oauth_consul("myserver.com", consul_token = "some-token"),
    "must have http"
  )
})
