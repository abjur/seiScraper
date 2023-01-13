
# pegar_n_pag -------------------------------------------------------------

pegar_n_pag <- function(file) {

  n_casos <- xml2::read_html(file) |>
    xml2::xml_find_first("//div[@class='barra']") |>
    xml2::xml_text() |>
    stringr::str_extract("[0-9]+$") |>
    as.integer()

  n_pag <- ifelse(is.na(n_casos), 1, ceiling(n_casos/10))
  n_pag

}

# pegar_hash --------------------------------------------------------------

pegar_hash <- function(file) {

  file |>
    xml2::read_html() |>
    xml2::xml_find_first("//a[contains(@href,'hash')]") |>
    xml2::xml_attr("href") |>
    stringr::str_extract("(?<=hash=)[a-z0-9]+")

}

# pegar_pagina_inicial ----------------------------------------------------

pegar_pagina_inicial <- function(ano) {
  # preparação
  url <- "https://sei.economia.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php"
  query <- list(
    "acao_externa"="protocolo_pesquisar",
    "acao_origem_externa"="protocolo_pesquisar_paginado",
    "id_orgao_acesso_externo"="0"
  )
  ssl <- httr::config(ssl_verifypeer = FALSE)

  # captcha
  r0 <- httr::GET(url, ssl, query = query)
  u_captcha_endpoint <- r0 |>
    xml2::read_html() |>
    xml2::xml_find_all("//img[contains(@src,'captcha')]") |>
    xml2::xml_attr("src")
  xy <- u_captcha_endpoint |>
    urltools::param_get("codetorandom") |>
    stringr::str_split("-") |>
    unlist() |>
    as.numeric()
  ans <- captcha_classify_sei(xy[1], xy[2])

  # body

  dt_inicio <- glue::glue("01/01/{ano}")
  dt_fim <- glue::glue("31/12/{ano}")
  dt_inicio_pf <- lubridate::dmy(dt_inicio)
  dt_fim_pf <- lubridate::dmy(dt_fim)
  partialfields <- glue::glue("id_tipo_proc:100000502 AND sta_prot:P AND dta_ger:[{dt_inicio_pf}T00:00:00Z TO {dt_fim_pf}T00:00:00Z]")

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

  # POST
  r_busca <- httr::POST(
    url,
    ssl,
    body = body,
    query = query,
    encode = "form"
  )

  hash <- pegar_hash(r_busca)
  n_pag <- pegar_n_pag(r_busca)

  list(hash = hash, n_pag = n_pag)
}

# download_varias_paginas -----------------------------------------------------------

download_varias_paginas <- function(pag, hash, ano, path) {
  url <- "https://sei.economia.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php"
  query <- list(
    "acao_externa"="protocolo_pesquisar",
    "acao_origem_externa"="protocolo_pesquisar_paginado",
    "inicio"="",
    "id_orgao_acesso_externo"="0",
    "hash"=""
  )
  ssl <- httr::config(ssl_verifypeer = FALSE)

  query$inicio <- (pag-1)*10
  query$hash <- hash

  dt_inicio <- glue::glue("01/01/{ano}")
  dt_fim <- glue::glue("31/12/{ano}")
  dt_inicio_pf <- lubridate::dmy(dt_inicio)
  dt_fim_pf <- lubridate::dmy(dt_fim)
  partialfields <- glue::glue("id_tipo_proc:100000502 AND sta_prot:P AND dta_ger:[{dt_inicio_pf}T00:00:00Z TO {dt_fim_pf}T00:00:00Z]")

  ans <- ""

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

  file <- sprintf("%s/%s_%03d.html", path, ano, pag)

  r <- httr::POST(
    url,
    ssl,
    body = body,
    query = query,
    encode = "form",
    httr::write_disk(file, TRUE)
  )

  file
}

# download_uma_pagina -----------------------------------------------------------

download_uma_pagina <- function(ano, path) {
  # preparação
  url <- "https://sei.economia.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php"
  query <- list(
    "acao_externa"="protocolo_pesquisar",
    "acao_origem_externa"="protocolo_pesquisar_paginado",
    "id_orgao_acesso_externo"="0"
  )
  ssl <- httr::config(ssl_verifypeer = FALSE)

  # captcha
  r0 <- httr::GET(url, ssl, query = query)
  u_captcha_endpoint <- r0 |>
    xml2::read_html() |>
    xml2::xml_find_all("//img[contains(@src,'captcha')]") |>
    xml2::xml_attr("src")
  xy <- u_captcha_endpoint |>
    urltools::param_get("codetorandom") |>
    stringr::str_split("-") |>
    unlist() |>
    as.numeric()
  ans <- captcha_classify_sei(xy[1], xy[2])

  # body

  dt_inicio <- glue::glue("01/01/{ano}")
  dt_fim <- glue::glue("31/12/{ano}")
  dt_inicio_pf <- lubridate::dmy(dt_inicio)
  dt_fim_pf <- lubridate::dmy(dt_fim)
  partialfields <- glue::glue("id_tipo_proc:100000502 AND sta_prot:P AND dta_ger:[{dt_inicio_pf}T00:00:00Z TO {dt_fim_pf}T00:00:00Z]")

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

  file <- glue::glue("{path}/{ano}_001.html")

  # POST
  r_busca <- httr::POST(
    url,
    ssl,
    body = body,
    query = query,
    encode = "form",
    httr::write_disk(file, TRUE)
  )

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

