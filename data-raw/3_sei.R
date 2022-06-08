# 1) download sei ----------------------------------------------------------

ssl <- httr::config(ssl_verifypeer = FALSE)
u_sei <- "https://sei.economia.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php?acao_externa=protocolo_pesquisar&acao_origem_externa=protocolo_pesquisar&id_orgao_acesso_externo=0"
r_sei <- httr::GET(u_sei, ssl)
html_sei <- xml2::read_html(r_sei)

# captcha -----------------------------------------------------------------
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


# body --------------------------------------------------------------------

ano <- "2016"
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

path <- "data-raw/n_sei"
f <- fs::path(path, ano, ext = "html")

# POST --------------------------------------------------------------------

httr::POST(u_sei, ssl, body = body,
           httr::write_disk(f,overwrite = TRUE))

# testando ----------------------------------------------------------------

anos <- c(2016:2022)
path_sei <- "data-raw/sei"

purrr::walk(anos, download_sei, path_sei)

# 2) parse sei ------------------------------------------------------------

path <- "data-raw/sei/2016.html"

html_ano <- xml2::read_html(path)
ids <- html_ano |>
  xml2::xml_find_all("//table") |>
  xml2::xml_find_all(".//a[2]") |>
  xml2::xml_text() |>
  abjutils::clean_cnj()

tibble::tibble(
  id = ids
)


# testando ----------------------------------------------------------------

processos <- path_sei |>
  fs::dir_ls() |>
  purrr::map_dfr(parse_sei, .id = "files")


# 3) total de paginas -----------------------------------------------------
path <- "data-raw/sei/2022.html"
path_sei <- "data-raw/sei"

purrr::walk(anos, download_sei, path_sei)
n_casos <- xml2::read_html(path) |>
  xml2::xml_find_first("//div[@class='barra']") |>
  xml2::xml_text() |>
  stringr::str_extract("[0-9]+$") |>
  as.integer()

total_pag <- ifelse(is.na(n_casos), 1, ceiling(n_casos/10))

path_sei |>
  fs::dir_ls() |>
  purrr::map_dfr(paginas_sei, .id = "files")

# 4) acessando pagina ---------------------------------------------------------------

# query -------------------------------------------------------------------
pag <- 2
inicio <- (pag-1)*10
hash <- "e7ec1ead59daa9f18469245de0f2f7b8bbc98301"

query <- list(
  "acao_externa" = "protocolo_pesquisar",
  "acao_origem_externa" = "protocolo_pesquisar_paginado",
  "inicio" = inicio,
  "id_orgao_acesso_externo" = "0",
  "hash" = "")

# hash --------------------------------------------------------------------

pegar_hash <- function(arq) {
  arq |>
    xml2::read_html() |>
    xml2::xml_find_first("//a[contains(@href,'hash')]") |>
    xml2::xml_attr("href") |>
    stringr::str_extract("(?<=hash=)[a-z0-9]+")
}

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
  n_pag <- paginas_sei(r_busca)

  list(hash = hash, n_pag = n_pag)
}

pegar_pagina <- function(pag, hash, ano, path) {
  u <- "https://sei.economia.gov.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php"
  q <- list(
    "acao_externa"="protocolo_pesquisar",
    "acao_origem_externa"="protocolo_pesquisar_paginado",
    "id_orgao_acesso_externo"="0"
  )
  ssl <- httr::config(ssl_verifypeer = FALSE)

  q$inicio <- (pag-1)*10
  q$hash <- hash

  b <- list(
    txtProtocoloPesquisa = "", txtCaptcha = "",
    sbmPesquisar = "Pesquisar", q = "",
    chkSinProcessos = "P", txtParticipante = "",
    hdnIdParticipante = "", txtUnidade = "",
    hdnIdUnidade = "", selTipoProcedimentoPesquisa = "100000502",
    selSeriePesquisa = "", txtDataInicio = "",
    txtDataFim = "", txtNumeroDocumentoPesquisa = "",
    txtAssinante = "", hdnIdAssinante = "", txtDescricaoPesquisa = "",
    txtAssunto = "", hdnIdAssunto = "", txtSiglaUsuario1 = "",
    txtSiglaUsuario2 = "", txtSiglaUsuario3 = "", txtSiglaUsuario4 = "",
    hdnSiglasUsuarios = "", hdnSiglasUsuarios = "",
    partialfields = "id_tipo_proc:100000502 AND sta_prot:P",
    requiredfields = "", as_q = "", hdnFlagPesquisa = "1"
  )

  f <- sprintf("%s/%s_%03d.html", path, ano, pag)

  r <- httr::POST(
    u,
    ssl,
    body = b,
    query = q,
    encode = "form",
    httr::write_disk(f, TRUE)
  )

  f
}

# testando ----------------------------------------------------------------

path <- "data-raw/paginas"
fs::dir_create(path)

hash <- pegar_pagina_inicial()

anos <- c(2016, 2017, 2018, 2020, 2021, 2022)

for(ano in anos) {
  resultado <- pegar_pagina_inicial(ano)
  paginas <- seq_len(resultado$n_pag)
  hash <- resultado$hash
  if(length(paginas) > 1) {
    for(pag in paginas) {
      arquivo_pagina <- download_varias_paginas(pag, hash, ano, path)
      hash <- pegar_hash(arquivo_pagina)
    }
  } else {
    download_uma_pagina(ano, path)
  }
}

path <- "data-raw/paginas"
ano <- 2019
resultado <- pegar_pagina_inicial(ano)
paginas <- seq_len(resultado$n_pag)
hash <- resultado$hash
if(length(paginas) > 1) {
  for(pag in paginas) {
    arquivo_pagina <- download_varias_paginas(pag, hash, ano, path)
    hash <- pegar_hash(arquivo_pagina)
  }
} else {
  download_uma_pagina(ano, path)
}


# parse

processos <- path |>
  fs::dir_ls() |>
  purrr::map_dfr(parse_sei, .id = "files") |>
  dplyr::pull(id)

ids |>
  dplyr::count(id)
