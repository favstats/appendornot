#' @title Write (new) csv and append if it already exists
#' @description This function writes a csv, creates the necessary (sub-)folders and appends the .csv data if it already is present.
#' \cr \cr Be aware this WILL create new subfolders by default if you specify folders that don't exist. If you don't want this behavior turn it off with \code{create_subfolders = FALSE}.
#' @param d The dataset you want to write
#' @param path The path you want to save the csv to
#' @param create_subfolders defaults to `TRUE`. Will create any subfolders you specify in \code{path}.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   ## write the csv (or append if it already exists)
#'   save_csv(cars, "cars.csv")
#'
#'   ## create a folder called "data" (if it doesn't exist yet) before it saves the csv
#'   save_csv(cars, "data/cars.csv")
#' }
#' }
#' @export
#' @rdname save_csv
#' @importFrom readr write_csv
#' @importFrom stringr str_split str_detect str_count
#' @importFrom purrr discard
save_csv <- function(d, path, create_subfolders = T) {
  if (file.exists(path)) {
    readr::write_csv(d, append = T, file = path)
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

    readr::write_csv(d, file = path)
  }
}



#' @title Write (new) lines and append if it already exists
#' @description This function writes a text file, creates the necessary (sub-)folders and appends the .csv data if it already is present.
#' \cr \cr Be aware this WILL create new subfolders by default if you specify folders that don't exist. If you don't want this behavior turn it off with \code{create_subfolders = FALSE}.
#' @param l The lines you want to write
#' @param path The path you want to save the csv to
#' @param create_subfolders defaults to `TRUE`. Will create any subfolders you specify in \code{path}.
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
save_lines <- function(l, path, create_subfolders) {
  if (file.exists(path)) {
    readr::write_lines(l, append = T, file = path)
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
