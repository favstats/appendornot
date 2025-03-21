library(testthat)
library(readr)
library(dplyr)

test_that("save_csv creates a new file", {
  temp_file <- tempfile(fileext = ".csv")
  df <- tibble::tibble(x = 1:3, y = 11:13)

  save_csv(df, temp_file)

  expect_true(file.exists(temp_file))

  loaded_df <- read_csv(temp_file, show_col_types = FALSE)
  expect_equal(loaded_df, df)

  unlink(temp_file) # Cleanup
})

test_that("save_csv appends data correctly", {
  temp_file <- tempfile(fileext = ".csv")
  df1 <- tibble::tibble(x = 1:3, y = 11:13)
  df2 <- tibble::tibble(x = 4:5, y = 14:15)

  save_csv(df1, temp_file)
  save_csv(df2, temp_file)

  loaded_df <- read_csv(temp_file, show_col_types = FALSE)
  expected_df <- bind_rows(df1, df2)  # Correct order

  expect_equal(loaded_df, expected_df)

  unlink(temp_file) # Cleanup
})

test_that("save_csv handles missing columns correctly", {
  temp_file <- tempfile(fileext = ".csv")
  df1 <- tibble::tibble(x = 1:3, y = 11:13)
  df2 <- tibble::tibble(x = 4:5, z = 100:101)  # New column 'z'

  save_csv(df1, temp_file)
  save_csv(df2, temp_file)

  loaded_df <- read_csv(temp_file, show_col_types = FALSE)

  # Ensure missing columns are filled with NA
  expected_df <- bind_rows(
    df1 %>% mutate(z = NA),  # Add 'z' column with NA
    df2 %>% mutate(y = NA)   # Add 'y' column with NA
  )

  expect_equal(loaded_df, expected_df)

  unlink(temp_file) # Cleanup
})

test_that("save_csv removes duplicate rows correctly", {
  temp_file <- tempfile(fileext = ".csv")
  df1 <- tibble::tibble(x = 1:3, y = 11:13)
  df2 <- tibble::tibble(x = 2:4, y = 12:14)

  save_csv(df1, temp_file)
  save_csv(df2, temp_file, remove_duplicates = TRUE)

  loaded_df <- read_csv(temp_file, show_col_types = FALSE)

  # Expected output should only keep unique rows
  expected_df <- tibble::tibble(x = c(1, 2, 3, 4), y = c(11, 12, 13, 14))

  expect_equal(loaded_df, expected_df)

  unlink(temp_file) # Cleanup
})

test_that("save_csv ignores specified columns when removing duplicates", {
  temp_file <- tempfile(fileext = ".csv")
  df1 <- tibble::tibble(x = 1:3, y = 11:13, timestamp = Sys.time())
  df2 <- tibble::tibble(x = 1:3, y = 11:13, timestamp = Sys.time() + 10)

  save_csv(df1, temp_file)
  save_csv(df2, temp_file, remove_duplicates = TRUE, ignore_columns = "timestamp")

  loaded_df <- read_csv(temp_file, show_col_types = FALSE)

  # Expected output: Only one copy of each row (ignoring timestamps)
  expected_df <- df1  # Since df1 and df2 are identical except timestamps, only df1 remains

  expect_equal(loaded_df %>% select(-timestamp), expected_df %>% select(-timestamp))

  unlink(temp_file) # Cleanup
})

test_that("save_csv creates subfolders if required", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  temp_file <- file.path(temp_dir, "subfolder", "data.csv")

  df <- tibble::tibble(x = 1:3, y = 11:13)

  save_csv(df, temp_file, create_subfolders = TRUE)

  expect_true(file.exists(temp_file))

  unlink(temp_dir, recursive = TRUE) # Cleanup
})
