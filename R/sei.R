# download_sei ------------------------------------------------------------

download_sei <- function(ano, path = "data-raw/sei") {

  ssl <- httr::config(ssl_verifypeer = FALSE)
  u_sei <- "https://sei.economia.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php?acao_externa=protocolo_pesquisar&acao_origem_externa=protocolo_pesquisar&id_orgao_acesso_externo=0"
  r_sei <- httr::GET(u_sei, ssl)
  html_sei <- xml2::read_html(r_sei)

  sei_all_numbers <- function() {
    c(
      48:57, #0:9
      65:90, #A-Z
      97:122 #a-z
    )
  }

  sei_all_chars <- function() {
    c(0:9, LETTERS, letters)
  }

  num_to_char <- function(x) {
    nums <- sei_all_numbers()
    char <- sei_all_chars()
    charx <- character(length(x))
    for(xi in seq_along(x)) {
      if (length(char[nums == x[xi]]) > 0) {
        charx[xi] <- char[nums == x[xi]]
      } else {
        charx[xi] <- NA_character_
      }
    }
    charx
  }

  captcha_classify_sei <- function(x, y) {
    l1 <- num_to_char(x)
    l2 <- num_to_char(y)
    l3 <- num_to_char(ceiling((x + y - 48 * 2) / 2 + 48))
    l4 <- num_to_char(floor((x + y - 48 * 2) / 2 + 48))
    if (is.na(l3)) l3 <- ifelse(x > y, l2, l1)
    if (is.na(l4)) l4 <- ifelse(x > y, l1, l2)
    paste(c(l1, l2, l3, l4), collapse = "")
  }

  r0 <- html_sei
  u_captcha_endpoint <- r0 |>
    xml2::xml_find_all("//img[contains(@src,'captcha')]") |>
    xml2::xml_attr("src")
  u_captcha <- paste0("https://sei.economia.gov.br", u_captcha_endpoint)
  xy <- u_captcha_endpoint |>
    urltools::param_get("codetorandom") |>
    stringr::str_split("-") |>
    unlist() |>
    as.numeric()
  ans <- captcha_classify_sei(xy[1], xy[2])

  # parametros ----------------------------------------------------------------------

  dt_inicio <- glue::glue("01/01/{ano}")
  dt_fim <- glue::glue("31/12/{ano}")
  dt_inicio_pf <- lubridate::dmy(dt_inicio)
  dt_fim_pf <- lubridate::dmy(dt_fim)
  partialfields <- glue::glue("id_tipo_proc:100000502 AND sta_prot:P AND dta_ger:[{dt_inicio_pf}T00:00:00Z TO {dt_fim_pf}T00:00:00Z]")
  # body --------------------------------------------------------------------

  body <- list(
    "txtProtocoloPesquisa" = "",
    "txtCaptcha" = ans,
    "sbmPesquisar" = "Pesquisar",
    "q" = "",
    "chkSinProcessos" = "",
    "txtParticipante" = "",
    "hdnIdParticipante" = "",
    "txtUnidade" = "",
    "hdnIdUnidade" = "",
    "selTipoProcedimentoPesquisa" = "100000502",
    "selSeriePesquisa" = "",
    "txtDataInicio" = dt_inicio,
    "txtDataFim" = dt_fim,
    "txtNumeroDocumentoPesquisa" = "",
    "txtAssinante" = "",
    "hdnIdAssinante" = "",
    "txtDescricaoPesquisa" = "",
    "txtAssunto" = "",
    "hdnIdAssunto" = "",
    "txtSiglaUsuario1" = "",
    "txtSiglaUsuario2" = "",
    "txtSiglaUsuario3" = "",
    "txtSiglaUsuario4" = "",
    "hdnSiglasUsuarios" = "",
    "hdnSiglasUsuarios" = "",
    "partialfields" = partialfields,
    "requiredfields" = "",
    "as_q" = "",
    "hdnFlagPesquisa" = "1")

  file <- fs::path(path, ano, ext = "html")

  httr::POST(u_sei, ssl, body = body,
             httr::write_disk(file,overwrite = TRUE))

  invisible(file)

}


# parse_sei ---------------------------------------------------------------

parse_sei <- function(file) {

  html <- xml2::read_html(file)

  ids <- html |>
    xml2::xml_find_all("//table") |>
    xml2::xml_find_all(".//a[2]") |>
    xml2::xml_text() |>
    abjutils::clean_cnj()

  tibble::tibble(
    id = ids
  )

}


# paginas_sei -------------------------------------------------------------

paginas_sei <- function(file) {

  n_casos <- xml2::read_html(file) |>
    xml2::xml_find_first("//div[@class='barra']") |>
    xml2::xml_text() |>
    stringr::str_extract("[0-9]+$") |>
    as.integer()

  total_pag <- ifelse(is.na(n_casos), 1, ceiling(n_casos/10))

  tibble::tibble(
    total_pag
  )

}
