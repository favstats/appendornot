# create a test dataset
test_data <- tibble::tibble(x = 1:10, y = 11:20)

# create a test file
test_file <- "test.csv"

# write the test dataset to the test file
save_csv(test_data, test_file)

# check if the test file exists
expect_true(file.exists(test_file))

# check if the test file contains the correct data
expect_equal(readr::read_csv(test_file), test_data)

# append new data to the test file
save_csv(data.frame(x = 11:15, y = 21:25), test_file)

# check if the test file contains the correct data after appending
expect_equal(readr::read_csv(test_file), rbind(tibble::tibble(x = 11:15, y = 21:25), test_data))

# create a test file
test_file2 <- "data/test.csv"

# check if the function creates subfolders correctly
save_csv(data.frame(x = 16:20, y = 26:30), test_file2)
expect_true(file.exists(test_file2))

# check if the function handles new columns correctly
save_csv(data.frame(x = 16:20, y = 26:30, z = 31:35), test_file)
expect_equal(readr::read_csv(test_file), dplyr::bind_rows(tibble::tibble(x = 16:20, y = 26:30, z = 31:35), tibble::tibble(x = 11:15, y = 21:25), test_data))


# remove the test files
unlink(test_file)
# unlink(test_file2)
unlink(unlist(stringr::str_split(test_file2, "/"))[1], recursive = T, force = T)

