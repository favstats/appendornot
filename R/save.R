
save_csv <- function(d, path) {

  if(file.exists(path)){
    readr::write_csv(d, append = T, file = path)
  } else {

    dirs_to_create <- stringr::str_split(path, "\\/") %>%
      unlist() %>%
      purrr::discard(~stringr::str_detect(.x, "\\.")) %>%
      paste0(collapse = "/")

    if(stringr::str_count(dirs_to_create) != 0){
      dir.create(dirs_to_create, recursive = T)
    }

    readr::write_csv(d, file = path)
  }

}



save_lines <- function(d, path) {

  if(file.exists(path)){
    readr::write_lines(d, append = T, file = path)
  } else {

    dirs_to_create <- stringr::str_split(path, "\\/") %>%
      unlist() %>%
      purrr::discard(~stringr::str_detect(.x, "\\.")) %>%
      paste0(collapse = "/")

    if(stringr::str_count(dirs_to_create) != 0){
      dir.create(dirs_to_create, recursive = T)
    }

    readr::write_lines(d, file = path)
  }

}
