# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

# Tests for R/microanalyticScore.R: list_modules, get_masmodule,
# delete_masmodule, masScore.
# All HTTP calls are intercepted with local_mocked_bindings.

library(testthat)
library(sasctl)

sess   <- fake_session()
masmod <- fake_masmodule()

# ── list_modules ──────────────────────────────────────────────────────────────

test_that("list_modules: returns empty list when no items", {
  local_mocked_bindings(
    vGET = function(...) list(items = list()),
    .package = "sasctl"
  )
  expect_equal(list_modules(sess), list())
})

test_that("list_modules: passes filter and limit in query", {
  captured_query <- NULL
  local_mocked_bindings(
    vGET = function(session, path, query, ...) {
      captured_query <<- query
      list(items = list())
    },
    .package = "sasctl"
  )
  list_modules(sess, filters = list(name = "test"), limit = 5)
  expect_true(grepl("test", captured_query$filter))
  expect_equal(captured_query$limit, 5)
})

# ── get_masmodule ─────────────────────────────────────────────────────────────

test_that("get_masmodule: returns MASmodule with steps and ScoreType (UUID)", {
  # Links follow the Micro Analytic Score API schema: method / rel / uri / type.
  # get_masmodule() inspects basename(links[,3]) == "execute" to set ScoreType;
  # column 3 is "uri" here, so basename gives the step name ("score").
  links_df <- data.frame(
    method = c("GET",  "POST"),
    rel    = c("self", "score"),
    uri    = c("/microanalyticScore/modules/my_module/steps/score",
               "/microanalyticScore/modules/my_module/steps/score"),
    type   = c("application/vnd.sas.microanalytic.module.step",
               "application/vnd.sas.microanalytic.module.step.output"),
    stringsAsFactors = FALSE
  )
  step_items <- data.frame(
    id             = "score",
    stringsAsFactors = FALSE
  )
  step_items$links <- list(links_df)

  local_mocked_bindings(
    vGET = function(session, path, ...) {
      if (grepl("/steps/score$", path)) {
        return(list(id = "score", name = "score"))
      }
      if (grepl("/steps$", path)) {
        return(list(items = step_items))
      }
      list(id = "my_module", name = "my_module")
    },
    .package = "sasctl"
  )
  result <- get_masmodule(sess, id = "my_module")
  expect_s3_class(result, "MASmodule")
  expect_true(!is.null(result$ScoreType))
})

# ── delete_masmodule ──────────────────────────────────────────────────────────

test_that("delete_masmodule: calls vDELETE with correct path", {
  captured_path <- NULL
  local_mocked_bindings(
    vDELETE = function(session, path, ...) {
      captured_path <<- path
      fake_delete_response()
    },
    .package = "sasctl"
  )
  delete_masmodule(sess, masmod)
  expect_true(grepl("microanalyticScore/modules", captured_path))
  expect_true(grepl(masmod$id, captured_path))
})

# ── masScore input validation ─────────────────────────────────────────────────

test_that("masScore: errors when data is not a data.frame", {
  expect_error(
    local_mocked_bindings(
      vPOST = function(...) list(outputs = data.frame()),
      .package = "sasctl",
      { masScore(sess, masmod, data = list(x = 1)) }
    ),
    "data.frame"
  )
})

# ── masScore happy path ───────────────────────────────────────────────────────

test_that("masScore: returns a data.frame with scored rows", {
  input_df <- data.frame(x = c(1.0, 2.0), stringsAsFactors = FALSE)

  local_mocked_bindings(
    vPOST = function(...) list(
      outputs = data.frame(name  = "pred",
                           value = "0.9",
                           stringsAsFactors = FALSE)
    ),
    .package = "sasctl"
  )
  result <- masScore(sess, masmod, input_df)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("masScore: execute ScoreType appends trailing underscore to columns", {
  exec_mod <- fake_masmodule(ScoreType = "execute")
  captured_json <- NULL

  local_mocked_bindings(
    vPOST = function(session, path, payload, ...) {
      captured_json <<- payload
      list(outputs = data.frame(name = "out_", value = "1",
                                stringsAsFactors = FALSE))
    },
    .package = "sasctl"
  )
  input_df <- data.frame(feat = 5.0)
  masScore(sess, exec_mod, input_df, forceTrail = TRUE)
  # The JSON payload should contain the trailing-underscore column name
  expect_true(grepl("feat_", captured_json))
})

# ── list_destinations & get_destination ───────────────────────────────────────

test_that("list_destinations: returns empty list when no items", {
  local_mocked_bindings(
    vGET = function(...) list(items = list()),
    .package = "sasctl"
  )
  expect_equal(list_destinations(sess), list())
})
