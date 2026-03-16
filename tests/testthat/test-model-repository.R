# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

# Tests for R/model_repository.R.
# All HTTP calls are intercepted via local_mocked_bindings.

library(testthat)
library(sasctl)

sess    <- fake_session()
proj    <- fake_project()
mod     <- fake_model()

# ── register_model input validation ──────────────────────────────────────────

test_that("register_model: errors when file is missing", {
  expect_error(
    register_model(sess, name = "M", project = "P", type = "zip"),
    "filepath must be set"
  )
})

test_that("register_model: errors when name is missing", {
  expect_error(
    register_model(sess, file = "m.zip", project = "P", type = "zip"),
    "name must be given"
  )
})

test_that("register_model: errors when project is missing", {
  expect_error(
    register_model(sess, file = "m.zip", name = "M", type = "zip"),
    "project name must be"
  )
})

test_that("register_model: errors on invalid type", {
  expect_error(
    register_model(sess, file = "m.zip", name = "M",
                   project = "P", type = "tar"),
    "SPK.*ZIP.*ASTORE.*PMML|Type must be"
  )
})

test_that("register_model: errors when zip file has wrong extension", {
  tmp_csv <- tempfile(fileext = ".csv")
  writeLines("a,b", tmp_csv)
  on.exit(unlink(tmp_csv))
  expect_error(
    register_model(sess, file = tmp_csv, name = "M",
                   project = "P", type = "zip"),
    "\\.zip"
  )
})

test_that("register_model: errors when pmml file has wrong extension", {
  tmp <- tempfile(fileext = ".csv")
  writeLines("a", tmp)
  on.exit(unlink(tmp))
  expect_error(
    register_model(sess, file = tmp, name = "M",
                   project = "P", type = "pmml"),
    "\\.pmml|\\.xml"
  )
})

test_that("register_model: errors when astore file has wrong extension", {
  tmp <- tempfile(fileext = ".txt")
  writeLines("a", tmp)
  on.exit(unlink(tmp))
  expect_error(
    register_model(sess, file = tmp, name = "M",
                   project = "P", type = "astore"),
    "\\.sasast|\\.ast|\\.astore"
  )
})

test_that("register_model: errors when spk file has wrong extension", {
  tmp <- tempfile(fileext = ".zip")
  writeLines("a", tmp)
  on.exit(unlink(tmp))
  expect_error(
    register_model(sess, file = tmp, name = "M",
                   project = "P", type = "spk"),
    "\\.spk"
  )
})

# ── list_projects ─────────────────────────────────────────────────────────────

test_that("list_projects: returns empty list when no items", {
  local_mocked_bindings(
    vGET = function(...) list(items = list()),
    .package = "sasctl"
  )
  result <- list_projects(sess)
  expect_equal(result, list())
})

# ── list_models ───────────────────────────────────────────────────────────────

test_that("list_models: returns empty list when no items", {
  local_mocked_bindings(
    vGET = function(...) list(items = list()),
    .package = "sasctl"
  )
  expect_equal(list_models(sess), list())
})

# ── list_repositories ─────────────────────────────────────────────────────────

test_that("list_repositories: returns empty list when no items", {
  local_mocked_bindings(
    vGET = function(...) list(items = list()),
    .package = "sasctl"
  )
  expect_equal(list_repositories(sess), list())
})

# ── project_exists / model_exists ─────────────────────────────────────────────

test_that("project_exists: returns TRUE when vHEAD succeeds (UUID)", {
  local_mocked_bindings(
    vHEAD = function(...) fake_response(200L, list()),
    .package = "sasctl"
  )
  expect_true(project_exists(sess, proj$id))
})

test_that("project_exists: returns FALSE when vHEAD errors", {
  local_mocked_bindings(
    vHEAD = function(...) stop("Not Found"),
    .package = "sasctl"
  )
  # UUID avoids extra list_projects call
  expect_false(project_exists(sess, proj$id))
})

test_that("model_exists: returns TRUE when vHEAD succeeds (UUID)", {
  local_mocked_bindings(
    vHEAD = function(...) fake_response(200L, list()),
    .package = "sasctl"
  )
  expect_true(model_exists(sess, mod$id))
})

test_that("model_exists: returns FALSE when vHEAD errors", {
  local_mocked_bindings(
    vHEAD = function(...) stop("Not Found"),
    .package = "sasctl"
  )
  expect_false(model_exists(sess, mod$id))
})

# ── create_project input validation ──────────────────────────────────────────

test_that("create_project: errors when additional_parameters is not a list", {
  local_mocked_bindings(
    list_repositories = function(...) data.frame(
      id = "repo-1", folderId = "fold-1",
      defaultRepository = TRUE,
      stringsAsFactors = FALSE
    ),
    vPOST = function(...) list(id = "pid-new", name = "N"),
    .package = "sasctl"
  )
  expect_error(
    create_project(sess, name = "N", additional_parameters = "bad"),
    "must be a list"
  )
})

test_that("create_project: errors when input_vars is not a data.frame", {
  local_mocked_bindings(
    list_repositories = function(...) data.frame(
      id = "repo-1", folderId = "fold-1",
      defaultRepository = TRUE,
      stringsAsFactors = FALSE
    ),
    .package = "sasctl"
  )
  expect_error(
    create_project(sess, name = "N", input_vars = list(a = 1)),
    "data.frame"
  )
})

# ── delete_project ────────────────────────────────────────────────────────────

test_that("delete_project: calls vDELETE with correct path", {
  captured_path <- NULL
  local_mocked_bindings(
    vDELETE = function(session, path, ...) {
      captured_path <<- path
      fake_delete_response()
    },
    .package = "sasctl"
  )
  delete_project(sess, proj$id)
  expect_true(grepl(proj$id, captured_path))
})

# ── delete_model ──────────────────────────────────────────────────────────────

test_that("delete_model: calls vDELETE with correct path", {
  captured_path <- NULL
  local_mocked_bindings(
    vDELETE = function(session, path, ...) {
      captured_path <<- path
      fake_delete_response()
    },
    .package = "sasctl"
  )
  delete_model(sess, mod$id)
  expect_true(grepl(mod$id, captured_path))
})

# ── list_model_contents ───────────────────────────────────────────────────────

test_that("list_model_contents: returns empty list when no items", {
  local_mocked_bindings(
    vGET = function(...) list(items = list()),
    .package = "sasctl"
  )
  expect_equal(list_model_contents(sess, mod$id), list())
})

# ── update_project_variables input validation ─────────────────────────────────

test_that("update_project_variables: errors when no vars provided", {
  expect_error(
    local_mocked_bindings(
      vPOST = function(...) list(),
      .package = "sasctl",
      { update_project_variables(sess, proj$id) }
    ),
    "No new variables defined"
  )
})

test_that("update_project_variables: errors when input_vars is not a data.frame", {
  expect_error(
    local_mocked_bindings(
      vPOST = function(...) list(),
      .package = "sasctl",
      { update_project_variables(sess, proj$id, input_vars = list(a = 1)) }
    ),
    "data.frame"
  )
})

# ── update_model_variables input validation ───────────────────────────────────

test_that("update_model_variables: errors when no vars provided", {
  expect_error(
    local_mocked_bindings(
      vPOST = function(...) list(),
      .package = "sasctl",
      { update_model_variables(sess, mod$id) }
    ),
    "No new variables defined"
  )
})

# ── add_model_content ─────────────────────────────────────────────────────────

test_that("add_model_content: returns sasctl object on success", {
  local_mocked_bindings(
    vGET  = function(...) list(id = mod$id, name = mod$name, etag = '"etag"'),
    vPOST = function(...) list(items = data.frame(id = "content-new",
                                                  name = "score.R",
                                                  stringsAsFactors = FALSE)),
    .package = "sasctl"
  )
  tmp <- tempfile(fileext = ".R")
  writeLines("cat('hello')", tmp)
  on.exit(unlink(tmp))
  result <- add_model_content(sess, file = tmp, model = mod$id)
  expect_true(inherits(result, "MMmodelContentList"))
})
