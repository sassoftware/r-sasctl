# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

# Tests for vGET / vPOST / vPUT / vDELETE / vHEAD.
# HTTP calls are intercepted with local_mocked_bindings so no server is needed.

library(testthat)
library(sasctl)

sess <- fake_session()

# ── is_viya_session guards ────────────────────────────────────────────────────

test_that("vGET: errors when session is not a viya_connection", {
  expect_error(vGET(list(), "some/path"), "viya_connection")
})

test_that("vPOST: errors when session is not a viya_connection", {
  expect_error(vPOST(list(), "some/path", payload = list()), "viya_connection")
})

test_that("vPUT: errors when session is not a viya_connection", {
  expect_error(vPUT(list(), "some/path", payload = list()), "viya_connection")
})

test_that("vDELETE: errors when session is not a viya_connection", {
  expect_error(vDELETE(list(), "some/path"), "viya_connection")
})

test_that("vHEAD: errors when session is not a viya_connection", {
  expect_error(vHEAD(list(), "some/path"), "viya_connection")
})

# ── output parameter guards ───────────────────────────────────────────────────

test_that("vGET: errors on invalid output parameter", {
  expect_error(
    local_mocked_bindings(
      GET = function(...) fake_response(200L, list()),
      .package = "httr",
      .local_envir = parent.frame(),
      { vGET(sess, "some/path", output = "invalid") }
    ),
    "Output must be"
  )
})

test_that("vPOST: errors on invalid output parameter", {
  expect_error(
    local_mocked_bindings(
      POST = function(...) fake_response(200L, list()),
      .package = "httr",
      .local_envir = parent.frame(),
      { vPOST(sess, "some/path", payload = list(), output = "bad") }
    ),
    "Output must be"
  )
})

test_that("vHEAD: errors on invalid output parameter", {
  expect_error(
    local_mocked_bindings(
      HEAD = function(...) fake_response(200L, list()),
      .package = "httr",
      .local_envir = parent.frame(),
      { vHEAD(sess, "some/path", output = "nope") }
    ),
    "Output must be"
  )
})

# ── vGET happy paths ──────────────────────────────────────────────────────────

test_that("vGET: returns parsed JSON list on 200", {
  local_mocked_bindings(
    GET = function(...) fake_response(200L, list(id = "abc", name = "test")),
    .package = "httr"
  )
  result <- vGET(sess, "some/path")
  expect_type(result, "list")
  expect_equal(result$id, "abc")
  expect_equal(result$name, "test")
})

test_that("vGET: extracts etag from response headers", {
  local_mocked_bindings(
    GET = function(...) fake_response(
      200L,
      list(id = "abc"),
      headers = list(etag = '"abc-etag"')
    ),
    .package = "httr"
  )
  result <- vGET(sess, "some/path")
  expect_equal(result$etag, '"abc-etag"')
})

test_that("vGET: returns raw httr response when output='response'", {
  local_mocked_bindings(
    GET = function(...) fake_response(200L, list(id = "x")),
    .package = "httr"
  )
  result <- vGET(sess, "some/path", output = "response")
  expect_s3_class(result, "response")
})

test_that("vGET: returns text string when output='text'", {
  local_mocked_bindings(
    GET = function(...) fake_response(200L, list(id = "y")),
    .package = "httr"
  )
  result <- vGET(sess, "some/path", output = "text")
  expect_type(result, "character")
})

test_that("vGET: propagates HTTP error status", {
  local_mocked_bindings(
    GET = function(...) fake_response(404L, list(message = "Not Found")),
    .package = "httr"
  )
  expect_error(vGET(sess, "missing/path"))
})

test_that("vGET: warns and propagates on 400 status", {
  local_mocked_bindings(
    GET = function(...) fake_response(
      400L,
      list(message = "Bad Request", details = "invalid param")
    ),
    .package = "httr"
  )
  # vGET issues two consecutive warnings before stop_for_status throws;
  # capture them all to avoid leaked warnings in the test report.
  warnings_seen <- character(0)
  expect_error(
    withCallingHandlers(
      vGET(sess, "bad/path"),
      warning = function(w) {
        warnings_seen <<- c(warnings_seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  )
  expect_true(any(grepl("Bad Request",  warnings_seen)))
  expect_true(any(grepl("invalid param", warnings_seen)))
})

# ── vPOST happy paths ─────────────────────────────────────────────────────────

test_that("vPOST: returns parsed JSON on 201", {
  local_mocked_bindings(
    POST = function(...) fake_response(201L, list(id = "new-id")),
    .package = "httr"
  )
  result <- vPOST(sess, "resource", payload = list(name = "test"))
  expect_equal(result$id, "new-id")
})

test_that("vPOST: returns raw response when output='response'", {
  local_mocked_bindings(
    POST = function(...) fake_response(201L, list(id = "z")),
    .package = "httr"
  )
  result <- vPOST(sess, "resource", payload = list(), output = "response")
  expect_s3_class(result, "response")
})

test_that("vPOST: returns text when output='text'", {
  local_mocked_bindings(
    POST = function(...) fake_response(200L, list(id = "t")),
    .package = "httr"
  )
  result <- vPOST(sess, "resource", payload = list(), output = "text")
  expect_type(result, "character")
})

test_that("vPOST: propagates HTTP error status", {
  local_mocked_bindings(
    POST = function(...) fake_response(403L, list(message = "Forbidden")),
    .package = "httr"
  )
  expect_error(vPOST(sess, "forbidden", payload = list()))
})

# ── vPUT happy paths ──────────────────────────────────────────────────────────

test_that("vPUT: returns parsed JSON on 200", {
  local_mocked_bindings(
    PUT = function(...) fake_response(200L, list(id = "updated")),
    .package = "httr"
  )
  result <- vPUT(sess, "resource/123", payload = list(name = "new"))
  expect_equal(result$id, "updated")
})

test_that("vPUT: propagates HTTP error status", {
  local_mocked_bindings(
    PUT = function(...) fake_response(409L, list(message = "Conflict")),
    .package = "httr"
  )
  expect_error(vPUT(sess, "resource/123", payload = list()))
})

# ── vDELETE happy paths ───────────────────────────────────────────────────────

test_that("vDELETE: returns response object on 204", {
  local_mocked_bindings(
    DELETE = function(...) fake_delete_response(),
    .package = "httr"
  )
  result <- vDELETE(sess, "resource/123")
  expect_s3_class(result, "response")
  expect_equal(result$status_code, 204L)
})

test_that("vDELETE: propagates HTTP error status", {
  local_mocked_bindings(
    DELETE = function(...) fake_response(404L, list(message = "Not Found")),
    .package = "httr"
  )
  expect_error(vDELETE(sess, "resource/999"))
})

# ── vHEAD happy paths ─────────────────────────────────────────────────────────

test_that("vHEAD: returns response object by default", {
  local_mocked_bindings(
    HEAD = function(...) fake_response(200L, list()),
    .package = "httr"
  )
  result <- vHEAD(sess, "resource/123")
  expect_s3_class(result, "response")
})

test_that("vHEAD: propagates HTTP error status", {
  local_mocked_bindings(
    HEAD = function(...) fake_response(404L, list()),
    .package = "httr"
  )
  expect_error(vHEAD(sess, "missing/resource"))
})
