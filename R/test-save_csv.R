library(testthat)
library(readr)
library(dplyr)

test_that("save_csv creates a new file", {
  temp_file <- tempfile(fileext = ".csv")
  df <- data.frame(id = 1:3, value = c(10, 20, 30))

  save_csv(df, temp_file)

  expect_true(file.exists(temp_file))

  loaded_df <- read_csv(temp_file, show_col_types = FALSE)
  expect_equal(nrow(loaded_df), 3)
  expect_equal(ncol(loaded_df), 2)

  unlink(temp_file) # Cleanup
})

test_that("save_csv appends data correctly", {
  temp_file <- tempfile(fileext = ".csv")
  df1 <- data.frame(id = 1:3, value = c(10, 20, 30))
  df2 <- data.frame(id = 4:6, value = c(40, 50, 60))

  save_csv(df1, temp_file)
  save_csv(df2, temp_file)

  loaded_df <- read_csv(temp_file, show_col_types = FALSE)
  expect_equal(nrow(loaded_df), 6)
  expect_equal(ncol(loaded_df), 2)

  unlink(temp_file) # Cleanup
})

test_that("save_csv handles missing columns", {
  temp_file <- tempfile(fileext = ".csv")
  df1 <- data.frame(id = 1:3, value = c(10, 20, 30))
  df2 <- data.frame(id = 4:6, other_col = c(100, 200, 300))

  save_csv(df1, temp_file)
  save_csv(df2, temp_file)

  loaded_df <- read_csv(temp_file, show_col_types = FALSE)

  expect_equal(nrow(loaded_df), 6)
  expect_true("value" %in% names(loaded_df))
  expect_true("other_col" %in% names(loaded_df))

  unlink(temp_file) # Cleanup
})

test_that("save_csv removes exact duplicate rows", {
  temp_file <- tempfile(fileext = ".csv")
  df1 <- data.frame(id = 1:3, value = c(10, 20, 30))
  df2 <- data.frame(id = 2:4, value = c(20, 30, 40))

  save_csv(df1, temp_file)
  save_csv(df2, temp_file, remove_duplicates = TRUE)

  loaded_df <- read_csv(temp_file, show_col_types = FALSE)

  expect_equal(nrow(loaded_df), 4) # Only unique rows should remain

  unlink(temp_file) # Cleanup
})

test_that("save_csv ignores specified columns when removing duplicates", {
  temp_file <- tempfile(fileext = ".csv")
  df1 <- data.frame(id = 1:3, value = c(10, 20, 30), timestamp = Sys.time())
  df2 <- data.frame(id = 1:3, value = c(10, 20, 30), timestamp = Sys.time() + 1)

  save_csv(df1, temp_file)
  save_csv(df2, temp_file, remove_duplicates = TRUE, ignore_columns = "timestamp")

  loaded_df <- read_csv(temp_file, show_col_types = FALSE)

  expect_equal(nrow(loaded_df), 3) # Only unique rows (excluding timestamp differences)

  unlink(temp_file) # Cleanup
})

test_that("save_csv creates subfolders if required", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  temp_file <- file.path(temp_dir, "subfolder", "data.csv")

  df <- data.frame(id = 1:3, value = c(10, 20, 30))

  save_csv(df, temp_file, create_subfolders = TRUE)

  expect_true(file.exists(temp_file))

  unlink(temp_dir, recursive = TRUE) # Cleanup
})
