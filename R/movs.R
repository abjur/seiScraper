# download movs ----------------------------------------------------
download_mov <- function(id, path = "data-raw/movs") {
  # a url é o resultado da função parse_processo
  # o id é o id do mov que eu quero
  # o path é o path do data-raw>movs
  ssl <- httr::config(ssl_verifypeer = FALSE)
  u_id <- parse_processo(id)
  r_id <- httr::GET(u_id, ssl, httr::write_disk(glue::glue("{path}/{id}.html"), overwrite = TRUE))
}

# parse movs ----------------------------------------
parse_mov <- function(id, path = "data-raw/movs") {
  # o arquivo html é o resultado do download movs
  html_id <- xml2::read_html(glue::glue("{path}/{id}.html"))

  tables_id <- html_id |>
    xml2::xml_find_all("//table")

  table_id2 <- tables_id[2] |>
    rvest::html_table() |>
    as.data.frame()

  return(table_id2)
}
