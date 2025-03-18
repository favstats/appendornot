#' Save and Append Data to CSV with Column Matching and Duplicate Removal
#'
#' This function appends a dataset to an existing CSV file while ensuring column consistency,
#' optionally removing duplicate rows and ignoring specified columns when checking for duplicates.
#'
#' @param d A data frame or tibble to be saved.
#' @param path A character string specifying the file path to save the dataset.
#' @param create_subfolders Logical. If `TRUE`, creates necessary subfolders if they do not exist.
#' @param remove_duplicates Logical. If `TRUE`, removes exact duplicate rows (excluding ignored columns).
#' @param ignore_columns Character vector. Columns to be ignored when checking for duplicates (e.g., timestamps).
#'
#' @details
#' - If the file does not exist, it creates a new CSV file.
#' - If the file exists, the function ensures the new dataset has all required columns.
#' - Missing columns in either dataset are added with `NA` values.
#' - The function ensures column order remains consistent before appending.
#' - If `remove_duplicates = TRUE`, exact duplicate rows are removed, excluding columns specified in `ignore_columns`.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(id = 1:5, value = c(10, 20, 30, 40, 50), timestamp = Sys.time())
#' save_dataset(df, "data.csv", create_subfolders = TRUE, remove_duplicates = TRUE, ignore_columns = "timestamp")
#' }
#'
#' @importFrom readr read_csv write_csv
#' @importFrom data.table rbindlist
#' @importFrom dplyr select distinct all_of
#' @importFrom stringr str_split str_detect
#' @importFrom purrr discard
#' @export
save_csv <- function(d, path, create_subfolders = FALSE,
                         remove_duplicates = FALSE, ignore_columns = NULL) {

  if (file.exists(path)) {
    # Read existing data
    old_data <- readr::read_csv(path, show_col_types = FALSE)
    old_names <- names(old_data)
    new_names <- names(d)

    # Ensure new data has all old columns (add missing ones as NA)
    missing_old_cols <- setdiff(old_names, new_names)
    if (length(missing_old_cols) > 0) {
      d[missing_old_cols] <- NA
    }

    # Ensure old data has all new columns (add missing ones as NA)
    missing_new_cols <- setdiff(new_names, old_names)
    if (length(missing_new_cols) > 0) {
      old_data[missing_new_cols] <- NA
    }

    # Ensure consistent column order
    final_col_order <- union(old_names, new_names)
    d <- d[, final_col_order, drop = FALSE]
    old_data <- old_data[, final_col_order, drop = FALSE]

    # Bind datasets together
    combined_data <- data.table::rbindlist(list(old_data, d), use.names = TRUE, fill = TRUE)

    # Remove duplicates if requested
    if (remove_duplicates) {
      if (!is.null(ignore_columns)) {
        keep_columns <- setdiff(names(combined_data), ignore_columns)
        if (length(keep_columns) > 0) {
          combined_data <- combined_data %>% dplyr::distinct(dplyr::across(dplyr::all_of(keep_columns)), .keep_all = TRUE)
        } else {
          warning("All columns were ignored for deduplication, skipping deduplication step.")
        }
      } else {
        combined_data <- combined_data %>% dplyr::distinct()
      }
    }

    # Overwrite file with cleaned data
    readr::write_csv(combined_data, file = path)

  } else {
    # Create necessary directories if required
    if (create_subfolders) {
      dir_path <- dirname(path)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }
    }

    # Write new file
    readr::write_csv(d, file = path)
  }
}




add_columns <- function(df, columns) {
  columns %>%
    purrr::map_dfr(~ {
      df %>%
        dplyr::mutate(
          {{ .x }} := NA
        )
    })
}



#' @title Write (new) lines and append if it already exists
#' @description This function writes a text file, creates the necessary (sub-)folders and appends the .csv data if it already is present.
#' \cr \cr Be aware this WILL create new subfolders by default if you specify folders that don't exist. If you don't want this behavior turn it off with \code{create_subfolders = FALSE}.
#' @param l The lines you want to write
#' @param path The path you want to save the csv to
#' @param create_subfolders defaults to `FALSE`. Will create any subfolders you specify in \code{path}.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ## write the lines (or append if it already exists)
#'   save_lines(names(mtcars), "carnames.txt")
#'
#'   ## create a folder called "txt" (if it doesn't exist yet) before it saves the lines
#'   save_lines(names(mtcars), "txt/carnames.txt")
#' }
#' }
#' @rdname save_lines
#' @export
#' @importFrom readr write_lines
#' @importFrom stringr str_split str_detect str_count
#' @importFrom purrr discard
save_lines <- function(l, path, create_subfolders = FALSE) {
  if (file.exists(path)) {
    readr::write_lines(l, append = T, file = path, sep = "\n")
  } else {
    if (create_subfolders) {
      dirs_to_create <- stringr::str_split(path, "\\/") %>%
        unlist() %>%
        purrr::discard(~ stringr::str_detect(.x, "\\.")) %>%
        paste0(collapse = "/")

      if (stringr::str_count(dirs_to_create) != 0) {
        dir.create(dirs_to_create, recursive = T)
      }
    }

    readr::write_lines(l, file = path)
  }
}


### test cases
# cars <- tibble(id = c(1,2), speed = 1:2, dist = 3:4)
#
# new_column_dat <- cars %>% mutate(thisisnownew = "yooo")
# different_order <- cars %>% select(dist, everything())
# different_order_newcolumn <- cars %>% mutate(yy = "yes") %>% select(speed, yy, everything())
#
# write_csv(cars, "cars.csv")
# # readr::read_csv("cars.csv") %>% View
#
#
# debugonce(save_csv)
#
# ## new column is added
# save_csv(new_column_dat, "cars.csv")
#
# ## new column is NOT there
# save_csv(cars, "cars.csv")
#
# ## new column is NOT there in different order
# save_csv(different_order_newcolumn, "cars.csv")
#
# readr::read_csv("cars.csv") %>% View
