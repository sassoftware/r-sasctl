# Copyright © 2022, SAS Institute Inc., Cary, NC, USA.  All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0

# Tests for pure-R helper functions in R/json_files.R
# No server or HTTP mocking required.

library(testthat)
library(sasctl)

# ── write_in_out_json ─────────────────────────────────────────────────────────

test_that("write_in_out_json: errors when data is not a data.frame", {
  expect_error(write_in_out_json(1:5), "data must be a data.frame")
  expect_error(write_in_out_json("string"), "data must be a data.frame")
  expect_error(write_in_out_json(list(a = 1)), "data must be a data.frame")
})

test_that("write_in_out_json: returns a data.frame with correct columns (noFile)", {
  result <- write_in_out_json(iris[, 1:4], input = TRUE, noFile = TRUE)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("name", "length", "type", "level", "role"))
})

test_that("write_in_out_json: numeric columns get type='decimal' and level='interval'", {
  result <- write_in_out_json(iris[, 1:4, drop = FALSE], noFile = TRUE)
  expect_true(all(result$type == "decimal"))
  expect_true(all(result$level == "interval"))
})

test_that("write_in_out_json: character columns get type='string' and level='nominal'", {
  df <- data.frame(name = c("a", "b"), stringsAsFactors = FALSE)
  result <- write_in_out_json(df, noFile = TRUE)
  expect_true(all(result$type == "string"))
  expect_true(all(result$level == "nominal"))
})

test_that("write_in_out_json: factor columns are treated as character/string", {
  df <- iris[, 5, drop = FALSE]       # Species is a factor
  result <- write_in_out_json(df, input = FALSE, noFile = TRUE)
  expect_true(all(result$type == "string"))
})

test_that("write_in_out_json: input=TRUE sets role='input'", {
  result <- write_in_out_json(iris[, 1:2], input = TRUE, noFile = TRUE)
  expect_true(all(result$role == "input"))
})

test_that("write_in_out_json: input=FALSE sets role='output'", {
  result <- write_in_out_json(iris[, 5, drop = FALSE], input = FALSE, noFile = TRUE)
  expect_true(all(result$role == "output"))
})

test_that("write_in_out_json: writes file and returns data.frame", {
  tmp <- tempdir()
  result <- write_in_out_json(iris[, 1:4], input = TRUE, path = tmp, noFile = FALSE)
  expect_true(file.exists(file.path(tmp, "inputVar.json")))
  expect_s3_class(result, "data.frame")
  unlink(file.path(tmp, "inputVar.json"))
})

test_that("write_in_out_json: path without trailing slash is handled", {
  tmp <- tempdir()
  # Remove trailing slash to test the path normalization
  path_no_slash <- sub("/$", "", tmp)
  result <- write_in_out_json(iris[, 1:4], input = TRUE,
                              path = path_no_slash, noFile = FALSE)
  expect_true(file.exists(file.path(tmp, "inputVar.json")))
  unlink(file.path(tmp, "inputVar.json"))
})

# ── write_ModelProperties_json ───────────────────────────────────────────────

test_that("write_ModelProperties_json: returns a data.frame", {
  result <- write_ModelProperties_json(
    modelName          = "TestModel",
    modelFunction      = "Classification",
    algorithm          = "Logistic Regression",
    numTargetCategories = 2,
    targetEvent        = "1",
    targetVariable     = "BAD",
    eventProbVar       = "P_BAD1",
    noFile             = TRUE
  )
  expect_s3_class(result, "data.frame")
  expect_true("value" %in% names(result))
})

test_that("write_ModelProperties_json: numTargetCategories > 2 gives Nominal", {
  result <- write_ModelProperties_json(
    modelName          = "TestModel",
    modelFunction      = "Classification",
    algorithm          = "RF",
    numTargetCategories = 3,
    targetEvent        = "A",
    targetVariable     = "species",
    eventProbVar       = "P_A",
    noFile             = TRUE
  )
  expect_equal(result["targetLevel", "value"], "Nominal")
})

test_that("write_ModelProperties_json: numTargetCategories <= 2 gives Binary", {
  result <- write_ModelProperties_json(
    modelName          = "TestModel",
    modelFunction      = "Classification",
    algorithm          = "GLM",
    numTargetCategories = 2,
    targetEvent        = "1",
    targetVariable     = "BAD",
    eventProbVar       = "P_BAD1",
    noFile             = TRUE
  )
  expect_equal(result["targetLevel", "value"], "Binary")
})

test_that("write_ModelProperties_json: writes file to path", {
  tmp <- tempdir()
  write_ModelProperties_json(
    modelName          = "TestModel",
    modelFunction      = "Prediction",
    algorithm          = "LM",
    numTargetCategories = 2,
    targetEvent        = "1",
    targetVariable     = "Y",
    eventProbVar       = "P_Y",
    path               = tmp,
    noFile             = FALSE
  )
  expect_true(file.exists(file.path(tmp, "ModelProperties.json")))
  unlink(file.path(tmp, "ModelProperties.json"))
})

# ── write_fileMetadata_json ──────────────────────────────────────────────────

test_that("write_fileMetadata_json: errors when names and roles have different lengths", {
  expect_error(
    write_fileMetadata_json(
      additionalFilesNames = c("a.ext", "b.ext"),
      additionalFilesRoles = c("scoreResource"),
      noFile = TRUE
    ),
    "same length"
  )
})

test_that("write_fileMetadata_json: returns data.frame with default entries", {
  result <- write_fileMetadata_json(noFile = TRUE)
  expect_s3_class(result, "data.frame")
  expect_true("role" %in% names(result))
  expect_true("name" %in% names(result))
  # Default: inputVariables, outputVariables, score, scoreResource
  expect_equal(nrow(result), 4L)
})

test_that("write_fileMetadata_json: additional files append correctly", {
  result <- write_fileMetadata_json(
    additionalFilesNames = c("extra.rds", "helper.R"),
    additionalFilesRoles = c("scoreResource", "scoreResource"),
    noFile               = TRUE
  )
  expect_equal(nrow(result), 6L)
})

test_that("write_fileMetadata_json: writes file to path", {
  tmp <- tempdir()
  write_fileMetadata_json(path = tmp, noFile = FALSE)
  expect_true(file.exists(file.path(tmp, "fileMetadata.json")))
  unlink(file.path(tmp, "fileMetadata.json"))
})

# ── format_data_json ──────────────────────────────────────────────────────────

test_that("format_data_json: errors when scr and scr_batch are both TRUE", {
  expect_error(format_data_json(mtcars, scr = TRUE, scr_batch = TRUE))
})

test_that("format_data_json: returns one element per row by default", {
  result <- format_data_json(mtcars[1:3, ])
  expect_length(result, 3L)
})

test_that("format_data_json: default output contains 'inputs' key", {
  result <- format_data_json(mtcars[1, ])
  expect_true(grepl('"inputs"', result[[1]]))
})

test_that("format_data_json: NA values encode as null", {
  df <- data.frame(x = NA_real_)
  result <- format_data_json(df)
  expect_true(grepl("null", result[[1]]))
})

test_that("format_data_json: string values are quoted in JSON", {
  df <- data.frame(label = "hello", stringsAsFactors = FALSE)
  result <- format_data_json(df)
  expect_true(grepl('"hello"', result[[1]]))
})

test_that("format_data_json: numeric values are not quoted in JSON", {
  df <- data.frame(x = 3.14)
  result <- format_data_json(df)
  expect_true(grepl("3.14", result[[1]]))
  expect_false(grepl('"3.14"', result[[1]]))
})

test_that("format_data_json: factor columns are converted to strings", {
  df <- data.frame(cat = factor("setosa"))
  result <- format_data_json(df)
  expect_true(grepl('"setosa"', result[[1]]))
})

test_that("format_data_json: scr_batch returns a single JSON string", {
  result <- format_data_json(mtcars[1:3, ], scr_batch = TRUE)
  expect_length(result, 1L)
  expect_true(jsonlite::validate(result))
})
