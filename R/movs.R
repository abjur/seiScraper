pegar_id <- function(url) {

  ssl <- httr::config(ssl_verifypeer = FALSE)

  url |>
    httr::GET(ssl) |>
    xml2::read_html() |>
    xml2::xml_find_first("//table/tr[2]") |>
    xml2::xml_text() |>
    stringr::str_remove_all("[a-zA-Z\\.\\:\\/\\-]")

}

# download movs ----------------------------------------------------
download_mov <- function(url, path = "data-raw/movs") {
  # o id é o id do mov que eu quero
  # o path é o path do data-raw>movs

  ssl <- httr::config(ssl_verifypeer = FALSE)
  id <- pegar_id(url)
  files <- fs::path(path, id, ext = "html")

  r <- httr::GET(url, ssl, httr::write_disk(files, overwrite = TRUE))

  invisible(files)
}

# parse movs ----------------------------------------
parse_mov <- function(files) {
  # o arquivo html é o resultado do download movs
  html <- xml2::read_html(files)

  tables_id <- html |>
    xml2::xml_find_all("//table")

  tables_id[2] |>
    rvest::html_table() |>
    as.data.frame()

}
